-- | Field test for query reach (Phase 4c): point the analysis at a
-- | real schema and real queries from the wild.
-- |
-- | The subject is Marginalia (the project tracker). Its schema lives in
-- | `database/schema.sql`; its queries are SQL string literals embedded
-- | in the PureScript server source (`server/src/**/*.purs`). We:
-- |
-- |   1. parse `schema.sql` into a `Schema` (schemaFromSql);
-- |   2. harvest SQL-looking string literals from the source files —
-- |      both `"..."` and `"""..."""` forms — and parse each;
-- |   3. resolve reach, find dead columns, and write a reach report.
-- |
-- | This is the reach analogue of the registry-dev migration field test.
-- | Like that one, it exists partly to *find the gaps*: real queries use
-- | constructs the heuristic harvester/parser under-handle (dynamically
-- | concatenated WHERE fragments with no SELECT of their own, recursive
-- | CTEs whose names look like tables). Those gaps make reach
-- | *under*-report, which makes dead-column detection err toward false
-- | positives — so every flagged-dead column is a candidate to
-- | scrutinise, not a verdict. The summary prints enough to judge.
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Query.FieldTest
module MinardDB.Query.FieldTest where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (global)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MinardDB.Migration.SQL (schemaFromSql)
import MinardDB.Query.Report (LabeledQuery, buildReachReport, encodeReachReport)
import MinardDB.Schema (Schema)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

------------------------------------------------------------------------
-- Where Marginalia lives
------------------------------------------------------------------------

marginaliaRoot :: String
marginaliaRoot = "/Users/afc/work/afc-work/agent-teams/project-tracker"

schemaPath :: String
schemaPath = Path.concat [ marginaliaRoot, "database", "schema.sql" ]

-- | The server source files that carry SQL. Listed explicitly so the
-- | field test is reproducible and doesn't depend on a recursive walk.
sourceFiles :: Array String
sourceFiles =
  map (\f -> Path.concat [ marginaliaRoot, "server", "src", f ])
    [ "API/Projects.purs"
    , "API/Agent.purs"
    , "API/Subscriptions.purs"
    , "API/Dependencies.purs"
    , "API/Activity.purs"
    , "API/Servers.purs"
    , "API/Stats.purs"
    , "API/Exercise.purs"
    , "API/Deployments.purs"
    , "BlogDrafts.purs"
    ]

------------------------------------------------------------------------
-- Harvesting SQL string literals from PureScript source
------------------------------------------------------------------------

-- | Matches a PureScript string literal: a triple-quoted block (which
-- | may span lines and contain quotes) or an ordinary double-quoted
-- | single-line string. Triple-quoted is tried first so it wins at a
-- | `"""`.
literalRegex :: Maybe Regex
literalRegex = case regex "\"\"\"[\\s\\S]*?\"\"\"|\"[^\"]*\"" global of
  Right r -> Just r
  Left _ -> Nothing

-- | Pull every SQL-looking string literal out of one source file's
-- | text, labelled by file basename + index.
harvest :: String -> String -> Array LabeledQuery
harvest fileLabel src = case literalRegex of
  Nothing -> []
  Just re -> case match re src of
    Nothing -> []
    Just matches ->
      let
        lits = Array.catMaybes (Array.fromFoldable matches)
        sqls = Array.filter looksLikeSql (map stripQuotes lits)
      in
        Array.mapWithIndex
          (\i sql -> { label: fileLabel <> " #" <> show (i + 1), sql })
          sqls

stripQuotes :: String -> String
stripQuotes s =
  if String.take 3 s == "\"\"\"" then
    dropEnds 3 s
  else
    dropEnds 1 s
  where
  dropEnds n str =
    let len = String.length str
    in String.take (len - 2 * n) (String.drop n str)

-- | Keep a literal only if, with whitespace normalised, it reads like a
-- | SQL statement or fragment we can extract refs from.
looksLikeSql :: String -> Boolean
looksLikeSql s =
  let u = String.toUpper (collapseWs s)
  in Array.any (\k -> String.contains (Pattern k) u)
    [ "SELECT ", "INSERT INTO", "UPDATE ", "DELETE FROM", " FROM ", "WITH RECURSIVE" ]

collapseWs :: String -> String
collapseWs =
  String.split (Pattern "\n") >>> map String.trim >>> Array.filter (_ /= "")
    >>> String.joinWith " "

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

reportsDir :: String
reportsDir = "reports"

main :: Effect Unit
main = launchAff_ do
  liftEffect $ Console.log "── Query reach field test: Marginalia ──"

  schemaText <- FS.readTextFile UTF8 schemaPath
  case schemaFromSql "marginalia" schemaText of
    Left err -> liftEffect $ Console.log $ "SCHEMA PARSE FAILED — " <> err
    Right schema -> do
      liftEffect do
        Console.log $ "Parsed schema: " <> show (Array.length schema.tables)
          <> " tables, " <> show (totalColumns schema) <> " columns"
        traverse_
          (\t -> Console.log $ "    " <> t.name <> " (" <> show (Array.length t.columns) <> " cols)")
          schema.tables

      -- Harvest queries from every source file.
      harvested <- traverse readAndHarvest sourceFiles
      let queries = Array.concat harvested
      liftEffect $ Console.log $ "\nHarvested " <> show (Array.length queries)
        <> " SQL literals from " <> show (Array.length sourceFiles) <> " source files"

      -- Build + write the reach report.
      let report = buildReachReport "marginalia"
            ("Real schema + " <> show (Array.length queries)
              <> " queries harvested from the PureScript server source.")
            schema queries
      _ <- attempt (FS.mkdir reportsDir)
      let outPath = Path.concat [ reportsDir, "reach-marginalia.json" ]
      FS.writeTextFile UTF8 outPath (stringify (encodeReachReport report))

      liftEffect do
        Console.log $ "\nReach: " <> show report.reachedCount <> "/"
          <> show report.totalColumns <> " columns reached, "
          <> show report.deadCount <> " dead"
        Console.log "Dead (scrutinise — harvester under-reports dynamic SQL):"
        traverse_ printDead report.tables
        Console.log $ "\nWrote " <> outPath

readAndHarvest :: String -> Aff (Array LabeledQuery)
readAndHarvest path = do
  res <- attempt (FS.readTextFile UTF8 path)
  case res of
    Left _ -> pure []
    Right txt -> pure (harvest (basename path) txt)

basename :: String -> String
basename p = case Array.last (String.split (Pattern "/") p) of
  Just b -> b
  Nothing -> p

printDead :: { name :: String, columns :: Array { name :: String, reachCount :: Int, dead :: Boolean } } -> Effect Unit
printDead t =
  let dead = Array.filter _.dead t.columns
  in case dead of
    [] -> pure unit
    ds -> traverse_ (\c -> Console.log ("    " <> t.name <> "." <> c.name)) ds

totalColumns :: Schema -> Int
totalColumns s = Array.length (Array.concatMap _.columns s.tables)
