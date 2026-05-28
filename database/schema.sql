-- notothenia.duckdb storage schema.
-- One row per analysis run + a child row per inferred FK + per proof.

CREATE SEQUENCE IF NOT EXISTS seq_analyses START 1;

CREATE TABLE IF NOT EXISTS analyses (
    id                    INTEGER     PRIMARY KEY DEFAULT nextval('seq_analyses'),
    name                  VARCHAR     NOT NULL,
    source_path           VARCHAR     NOT NULL,
    captured_at           TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP,
    table_count           INTEGER     NOT NULL,
    declared_fk_count     INTEGER     NOT NULL,
    inferred_fk_count     INTEGER     NOT NULL,
    notes                 VARCHAR
);

CREATE INDEX IF NOT EXISTS idx_analyses_name ON analyses(name);
CREATE INDEX IF NOT EXISTS idx_analyses_captured_at ON analyses(captured_at DESC);

CREATE TABLE IF NOT EXISTS analysis_inferred_fks (
    analysis_id           INTEGER     NOT NULL,
    source_table          VARCHAR     NOT NULL,
    source_columns        VARCHAR     NOT NULL,
    ref_table             VARCHAR     NOT NULL,
    ref_columns           VARCHAR     NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_analysis_inferred_fks ON analysis_inferred_fks(analysis_id);

CREATE TABLE IF NOT EXISTS analysis_proofs (
    analysis_id           INTEGER     NOT NULL,
    command_name          VARCHAR     NOT NULL,
    kind                  VARCHAR     NOT NULL,            -- 'check' | 'run'
    source                VARCHAR     NOT NULL,
    verdict               VARCHAR     NOT NULL,            -- 'SAT' | 'UNSAT'
    interpretation        VARCHAR     NOT NULL,
    witness               VARCHAR,
    scope                 INTEGER     NOT NULL,
    PRIMARY KEY (analysis_id, command_name)
);
