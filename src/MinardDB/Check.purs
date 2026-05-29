-- | `notothenia check` — the agent-runnable verdict surface.
-- |
-- | The point (see `docs/SYNTHESIS.md`, the linter-at-pace framing, and
-- | the memory `project_linting_engine_vision`): make notothenia's
-- | integrity checks *callable* — one pass/fail, structured findings, a
-- | real exit code — so an agent can run them mid-build and branch on the
-- | result, and a human can read what was built without conversational
-- | context. Every check here is **re-derivable from the repo alone**:
-- | nothing depends on remembering *why*.
-- |
-- | Two tiers, by cost:
-- |
-- |   * **Fast tier** (always on, pure PureScript, no JVM): structural
-- |     lints that catch the rot LLM-speed iteration produces — dangling
-- |     FK targets (rename rot), FK cycles, missing primary keys, and
-- |     (with `--against-yoga`) drift between the schema and an app's
-- |     typed bindings. Instant; safe to run on every edit.
-- |
-- |   * **Audit tier** (`--alloy`): the bounded-model-finding catalog
-- |     (`MinardDB.Properties`) — BCNF, row-level FK-acyclicity proven
-- |     over all instances within scope, coverage probes, satisfiability.
-- |     Heavy artillery; run in CI / on demand.
-- |
-- | Usage:
-- |   spago run -p minard-db --main MinardDB.Check -- --sql schema.sql
-- |   spago run -p minard-db --main MinardDB.Check -- --yoga Tables.purs --alloy
-- |   spago run -p minard-db --main MinardDB.Check -- --sql s.sql --against-yoga Tables.purs --json
-- |
-- | Exit codes: 0 = clean, 1 = findings (an Error-severity failure),
-- | 2 = usage / IO / tool error.
module MinardDB.Check
  ( Severity(..)
  , Status(..)
  , Tier(..)
  , Finding
  , Verdict
  , fastLints
  , verdictOf
  , exitCodeOf
  , renderHuman
  , renderJson
  , main
  ) where

import Prelude

import Data.Argonaut.Core (fromArray, fromBoolean, fromNumber, fromObject, fromString, stringify)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (joinWith)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Foreign.Object as Object
import MinardDB.Alloy.Generate (generate)
import MinardDB.Alloy.Invoke (defaultConfig, runAlloy)
import MinardDB.Alloy.Receipt (CommandResult, Verdict(..), parseReceipt) as Receipt
import MinardDB.Codegen.YogaParse (parseYogaSchema)
import MinardDB.Migration.SQL (schemaFromSql)
import MinardDB.Properties (AlloyCheck, defaultProperties, interpretCommand)
import MinardDB.Properties (CheckBody(..)) as P
import MinardDB.Schema (Schema, Table)
import MinardDB.Schema.Diff (describeDiff, diffSchemas)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process

------------------------------------------------------------------------
-- Finding / verdict model
------------------------------------------------------------------------

data Severity = SevError | SevWarning | SevNote

derive instance eqSeverity :: Eq Severity

data Status = Pass | Fail | Vacuous | Noted | Skipped

derive instance eqStatus :: Eq Status

data Tier = Fast | Audit

-- | One check's outcome. `check` is the locus (e.g. `fk-acyclicity`,
-- | `BCNF_addresses_zip__city`); `detail` is the human explanation.
type Finding =
  { check :: String
  , tier :: Tier
  , status :: Status
  , severity :: Severity
  , detail :: String
  }

type Verdict =
  { src :: String
  , schemaName :: String
  , tableCount :: Int
  , findings :: Array Finding
  , failures :: Int   -- Fail + SevError
  , warnings :: Int   -- Fail + SevWarning
  }

mkPass :: Tier -> String -> String -> Finding
mkPass tier check detail = { check, tier, status: Pass, severity: SevNote, detail }

mkFail :: Tier -> Severity -> String -> String -> Finding
mkFail tier severity check detail = { check, tier, status: Fail, severity, detail }

mkVacuous :: Tier -> String -> String -> Finding
mkVacuous tier check detail = { check, tier, status: Vacuous, severity: SevNote, detail }

mkNoted :: Tier -> String -> String -> Finding
mkNoted tier check detail = { check, tier, status: Noted, severity: SevNote, detail }

verdictOf :: String -> Schema -> Array Finding -> Verdict
verdictOf src schema findings =
  { src
  , schemaName: schema.name
  , tableCount: Array.length schema.tables
  , findings
  , failures: Array.length (Array.filter isFailure findings)
  , warnings: Array.length (Array.filter isWarning findings)
  }
  where
  isFailure f = f.status == Fail && f.severity == SevError
  isWarning f = f.status == Fail && f.severity == SevWarning

exitCodeOf :: Verdict -> Int
exitCodeOf v = if v.failures > 0 then 1 else 0

------------------------------------------------------------------------
-- Fast tier — pure structural lints (no JVM)
------------------------------------------------------------------------

fastLints :: Schema -> Array Finding
fastLints schema =
  fkTargetsExist schema
    <> fkAcyclicFast schema
    <> primaryKeysPresent schema

-- | Every FK must reference a table and columns that actually exist.
-- | A dangling reference is the classic rename-rot artifact: the target
-- | moved and the FK was never updated.
fkTargetsExist :: Schema -> Array Finding
fkTargetsExist schema =
  let problems = schema.tables >>= tableProblems
  in
    if Array.null problems then
      [ mkPass Fast "fk-targets-exist" "every FK references an existing table and columns" ]
    else problems
  where
  tableProblems t = t.foreignKeys >>= \fk ->
    case findTable schema fk.refTable of
      Nothing ->
        [ mkFail Fast SevError ("fk-target/" <> t.name <> "." <> joinWith "," fk.columns)
            ("references missing table `" <> fk.refTable <> "`")
        ]
      Just rt ->
        let missing = Array.filter (\c -> not (columnExists rt c)) fk.refColumns
        in
          if Array.null missing then []
          else
            [ mkFail Fast SevError ("fk-target/" <> t.name <> "." <> joinWith "," fk.columns)
                ("references missing column(s) [" <> joinWith ", " missing
                  <> "] in `" <> fk.refTable <> "`")
            ]

-- | Pure FK-graph cycle detection at the *table* level. This is the
-- | cheap, conservative cousin of the Alloy `NoFKCycle` proof:
-- |
-- |   * A cycle whose every edge is a NOT-NULL FK is a real problem — no
-- |     row can ever be inserted (the orgs⇄people shape). Reported as an
-- |     Error.
-- |   * A cycle with a nullable FK somewhere on it (e.g. a `parent_id`
-- |     tree, or a soft back-reference) is usually fine at the row level —
-- |     the nullable end breaks the chain. Reported as a Warning, with a
-- |     pointer to `--alloy` for the precise per-row verdict.
fkAcyclicFast :: Schema -> Array Finding
fkAcyclicFast schema =
  case findCycle (fkAdjacency schema) of
    Nothing ->
      [ mkPass Fast "fk-acyclicity" "no cycle in the table FK graph" ]
    Just cyc ->
      let path = joinWith " → " cyc
      in
        if isHardCycle schema cyc then
          [ mkFail Fast SevError "fk-acyclicity"
              ("FK cycle with all-NOT-NULL edges — no row can be inserted: " <> path)
          ]
        else
          [ mkFail Fast SevWarning "fk-acyclicity"
              ("FK cycle: " <> path
                <> " — a FK on the cycle is nullable, so row-level acyclicity may still hold; run --alloy to prove")
          ]

-- | A table with no primary key can't have per-row properties localized.
-- | Warning, not error — sometimes intentional (append-only logs).
primaryKeysPresent :: Schema -> Array Finding
primaryKeysPresent schema =
  let without = Array.filter (\t -> Array.null t.primaryKey) schema.tables
  in
    if Array.null without then
      [ mkPass Fast "primary-keys" "every table has a primary key" ]
    else
      map
        ( \t -> mkFail Fast SevWarning "primary-key"
            ("table `" <> t.name <> "` has no primary key (per-row proofs can't be localized to a row)")
        )
        without

------------------------------------------------------------------------
-- Drift (fast, pure) — schema vs an app's typed bindings
------------------------------------------------------------------------

driftLints :: Schema -> String -> Array Finding
driftLints schema yogaText =
  case parseYogaSchema "expected" yogaText of
    Left err ->
      [ mkFail Fast SevError "drift" ("could not parse --against-yoga module: " <> err) ]
    Right expected ->
      let diffs = diffSchemas schema expected
      in
        if Array.null diffs then
          [ mkPass Fast "drift" "schema matches the yoga bindings (no drift)" ]
        else
          map (\d -> mkFail Fast SevError "drift" (describeDiff d)) diffs

------------------------------------------------------------------------
-- Audit tier — the Alloy catalog
------------------------------------------------------------------------

auditLints :: Schema -> Aff (Array Finding)
auditLints schema = do
  let
    stem = "notothenia-check"
    alsPath = Path.concat [ "/tmp", stem <> ".als" ]
  e <- try do
    FS.writeTextFile UTF8 alsPath (generate schema)
    _ <- runAlloy defaultConfig alsPath
    FS.readTextFile UTF8 (Path.concat [ stem, "receipt.json" ])
  case e of
    Left err ->
      pure
        [ mkFail Audit SevError "alloy"
            ("Alloy audit failed: " <> Exception.message err
              <> " (is vendor/alloy.jar present and java on PATH?)")
        ]
    Right receiptText -> case Receipt.parseReceipt receiptText of
      Left perr ->
        pure [ mkFail Audit SevError "alloy" ("could not parse Alloy receipt: " <> perr) ]
      Right cmds ->
        pure (Array.mapMaybe (commandToFinding catalog) cmds)
  where
  catalog = defaultProperties >>= (_ $ schema)

-- | Map one Alloy command result to a finding, using the catalog to
-- | interpret it. The synthetic `show` run is treated as a satisfiability
-- | probe (the `run`-vs-`check` dual from the enforcement note).
commandToFinding :: Array AlloyCheck -> Receipt.CommandResult -> Maybe Finding
commandToFinding catalog cmd =
  if cmd.name == "show" then
    Just case cmd.verdict of
      Receipt.Counterexample ->
        mkNoted Audit "satisfiability" "schema admits a non-empty instance within scope"
      Receipt.NoCounterexample ->
        mkFail Audit SevWarning "satisfiability"
          "schema admits NO instance within scope (over-constrained / unpopulatable)"
  else
    let
      mcheck = Array.find (\c -> c.name == cmd.name) catalog
      detail = interpretCommand mcheck cmd
    in
      Just case mcheck of
        Just c -> case c.body, cmd.verdict of
          P.Assertion _, Receipt.NoCounterexample -> mkPass Audit cmd.name detail
          P.Assertion _, Receipt.Counterexample -> mkFail Audit SevError cmd.name detail
          P.Vacuous _, _ -> mkVacuous Audit cmd.name detail
          P.Probe _, _ -> mkNoted Audit cmd.name detail
        Nothing -> mkNoted Audit cmd.name detail

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

renderHuman :: Verdict -> String
renderHuman v =
  joinWith "\n" $
    [ "notothenia check — " <> v.src
    , "  schema: " <> v.schemaName <> " (" <> show v.tableCount <> " tables)"
    , ""
    ]
      <> map renderFinding v.findings
      <> [ "", verdictLine v ]

renderFinding :: Finding -> String
renderFinding f =
  "  " <> symbol f <> " " <> padR 9 (tierTag f.tier) <> " "
    <> padR 24 f.check <> f.detail
  where
  symbol g = case g.status, g.severity of
    Pass, _ -> "✓"
    Fail, SevError -> "✗"
    Fail, SevWarning -> "!"
    Fail, SevNote -> "!"
    Vacuous, _ -> "~"
    Noted, _ -> "·"
    Skipped, _ -> "⊘"
  tierTag = case _ of
    Fast -> "[fast]"
    Audit -> "[audit]"

verdictLine :: Verdict -> String
verdictLine v =
  if v.failures > 0 then
    "FAIL — " <> show v.failures <> " error(s)" <> warnSuffix
  else if v.warnings > 0 then
    "PASS (with " <> show v.warnings <> " warning(s))"
  else
    "PASS — clean"
  where
  warnSuffix =
    if v.warnings > 0 then ", " <> show v.warnings <> " warning(s)" else ""

renderJson :: Verdict -> String
renderJson v =
  stringify $ fromObject $ Object.fromFoldable
    [ Tuple "ok" (fromBoolean (v.failures == 0))
    , Tuple "source" (fromString v.src)
    , Tuple "schema" (fromString v.schemaName)
    , Tuple "tableCount" (fromNumber (toNumber v.tableCount))
    , Tuple "failures" (fromNumber (toNumber v.failures))
    , Tuple "warnings" (fromNumber (toNumber v.warnings))
    , Tuple "findings" (fromArray (map findingJson v.findings))
    ]
  where
  findingJson f = fromObject $ Object.fromFoldable
    [ Tuple "check" (fromString f.check)
    , Tuple "tier" (fromString (tierStr f.tier))
    , Tuple "status" (fromString (statusStr f.status))
    , Tuple "severity" (fromString (severityStr f.severity))
    , Tuple "detail" (fromString f.detail)
    ]
  tierStr = case _ of
    Fast -> "fast"
    Audit -> "audit"
  statusStr = case _ of
    Pass -> "pass"
    Fail -> "fail"
    Vacuous -> "vacuous"
    Noted -> "noted"
    Skipped -> "skipped"
  severityStr = case _ of
    SevError -> "error"
    SevWarning -> "warning"
    SevNote -> "note"

------------------------------------------------------------------------
-- Schema-graph helpers
------------------------------------------------------------------------

findTable :: Schema -> String -> Maybe Table
findTable schema n = Array.find (\t -> t.name == n) schema.tables

columnExists :: Table -> String -> Boolean
columnExists t c = Array.any (\col -> col.name == c) t.columns

-- | table → the tables it has FKs into.
fkAdjacency :: Schema -> Map String (Array String)
fkAdjacency schema =
  Map.fromFoldable (map (\t -> Tuple t.name (map _.refTable t.foreignKeys)) schema.tables)

-- | Is there an FK from `fromT` to `toT` all of whose columns are
-- | nullable? (Then that edge can be left NULL, breaking a row-level
-- | cycle.)
edgeNullable :: Schema -> String -> String -> Boolean
edgeNullable schema fromT toT =
  case findTable schema fromT of
    Nothing -> false
    Just t ->
      Array.any
        (\fk -> fk.refTable == toT && Array.all (columnNullable t) fk.columns)
        t.foreignKeys
  where
  columnNullable t c =
    case Array.find (\col -> col.name == c) t.columns of
      Just col -> col.nullable
      Nothing -> false

-- | A cycle is "hard" (unpopulatable) when every edge along it is a
-- | NOT-NULL FK.
isHardCycle :: Schema -> Array String -> Boolean
isHardCycle schema cyc =
  let pairs = Array.zip cyc (fromMaybe [] (Array.tail cyc))
  in Array.all (\(Tuple a b) -> not (edgeNullable schema a b)) pairs

-- | Find one cycle in the directed graph, returned as the node path with
-- | the repeated entry node at both ends (e.g. ["orgs","people","orgs"]).
-- | DFS with a per-path stack; `Nothing` means acyclic.
findCycle :: Map String (Array String) -> Maybe (Array String)
findCycle adj = (Array.foldl tryRoot { finished: Set.empty, cycle: Nothing } roots).cycle
  where
  roots = Set.toUnfoldable (Map.keys adj) :: Array String

  neighborsOf node = fromMaybe [] (Map.lookup node adj)

  tryRoot acc node =
    case acc.cycle of
      Just _ -> acc
      Nothing ->
        if Set.member node acc.finished then acc
        else dfs acc.finished [] node

  dfs finished path node =
    let
      path' = Array.snoc path node
      res = Array.foldl (visit path') { finished, cycle: Nothing } (neighborsOf node)
    in
      case res.cycle of
        Just c -> { finished: res.finished, cycle: Just c }
        Nothing -> { finished: Set.insert node res.finished, cycle: Nothing }
    where
    visit path' acc nb =
      case acc.cycle of
        Just _ -> acc
        Nothing ->
          if Array.elem nb path' then
            let idx = fromMaybe 0 (Array.elemIndex nb path')
            in { finished: acc.finished, cycle: Just (Array.drop idx path' <> [ nb ]) }
          else if Set.member nb acc.finished then acc
          else dfs acc.finished path' nb

padR :: Int -> String -> String
padR n s =
  let len = String.length s
  in
    if len >= n then s
    else s <> joinWith "" (Array.replicate (n - len) " ")

------------------------------------------------------------------------
-- CLI
------------------------------------------------------------------------

data Source = FromSql String | FromYoga String

sourcePath :: Source -> String
sourcePath = case _ of
  FromSql f -> f
  FromYoga f -> f

sourceLabel :: Source -> String
sourceLabel = case _ of
  FromSql f -> "DDL " <> f
  FromYoga f -> "yoga types " <> f

type Opts =
  { source :: Maybe Source
  , alloy :: Boolean
  , json :: Boolean
  , againstYoga :: Maybe String
  }

defaultOpts :: Opts
defaultOpts = { source: Nothing, alloy: false, json: false, againstYoga: Nothing }

parseArgs :: Array String -> Either String Opts
parseArgs = go defaultOpts
  where
  go opts xs = case Array.uncons xs of
    Nothing -> Right opts
    Just { head: a, tail } -> case a of
      "--alloy" -> go (opts { alloy = true }) tail
      "--json" -> go (opts { json = true }) tail
      "--sql" -> withArg a tail \v rest -> go (opts { source = Just (FromSql v) }) rest
      "--yoga" -> withArg a tail \v rest -> go (opts { source = Just (FromYoga v) }) rest
      "--against-yoga" -> withArg a tail \v rest -> go (opts { againstYoga = Just v }) rest
      other -> Left ("unknown argument: " <> other)

  withArg flag tail k = case Array.uncons tail of
    Just { head: v, tail: rest } -> k v rest
    Nothing -> Left ("missing value after " <> flag)

usage :: String
usage =
  joinWith "\n"
    [ "usage: notothenia check (--sql FILE | --yoga FILE) [--against-yoga FILE] [--alloy] [--json]"
    , "  --sql FILE           parse a DDL file into a schema"
    , "  --yoga FILE          parse rowtype-yoga Table declarations into a schema"
    , "  --against-yoga FILE  also check the schema for drift vs these typed bindings"
    , "  --alloy              additionally run the Alloy proof catalog (BCNF, row-level acyclicity, …)"
    , "  --json               emit machine-readable JSON instead of a human report"
    , "exit: 0 clean · 1 findings · 2 usage/IO/tool error"
    ]

main :: Effect Unit
main = do
  argv <- Process.argv
  case parseArgs (Array.drop 2 argv) of
    Left err -> die ("notothenia check: " <> err <> "\n" <> usage)
    Right opts -> case opts.source of
      Nothing -> die ("notothenia check: no source given\n" <> usage)
      Just src -> launchAff_ (run opts src)

run :: Opts -> Source -> Aff Unit
run opts src = do
  let path = sourcePath src
  eTxt <- try (FS.readTextFile UTF8 path)
  case eTxt of
    Left err -> liftEffect $ die ("notothenia check: cannot read " <> path <> ": " <> Exception.message err)
    Right txt -> case loadSchema src txt of
      Left err -> liftEffect $ die ("notothenia check: parse failed: " <> err)
      Right schema -> do
        drift <- case opts.againstYoga of
          Nothing -> pure []
          Just f -> do
            e <- try (FS.readTextFile UTF8 f)
            case e of
              Left err ->
                pure [ mkFail Fast SevError "drift" ("cannot read --against-yoga " <> f <> ": " <> Exception.message err) ]
              Right yt -> pure (driftLints schema yt)
        audit <- if opts.alloy then auditLints schema else pure []
        let
          findings = fastLints schema <> drift <> audit
          v = verdictOf (sourceLabel src) schema findings
        liftEffect do
          Console.log (if opts.json then renderJson v else renderHuman v)
          void (Process.exit' (exitCodeOf v))

loadSchema :: Source -> String -> Either String Schema
loadSchema src txt = case src of
  FromSql f -> schemaFromSql (Path.basename f) txt
  FromYoga f -> parseYogaSchema (Path.basename f) txt

-- | Print a message to stderr and exit with code 2 (usage/IO/tool error).
die :: String -> Effect Unit
die msg = do
  Console.error msg
  void (Process.exit' 2)
