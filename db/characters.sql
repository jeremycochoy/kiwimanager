-- Table: characters

-- DROP TABLE characters;

CREATE TABLE characters
(
  id serial NOT NULL,
  name character varying(20) NOT NULL,
  type smallint NOT NULL,
  lvl smallint NOT NULL DEFAULT 1,
  state_pts integer NOT NULL DEFAULT 0,
  "int" integer NOT NULL DEFAULT 1,
  "str" integer NOT NULL DEFAULT 1,
  dex integer NOT NULL DEFAULT 1,
  agi integer NOT NULL DEFAULT 1,
  vit integer NOT NULL DEFAULT 1,
  exp bigint NOT NULL DEFAULT 0,
  user_id integer NOT NULL,
  slot smallint NOT NULL,
  c_skin smallint NOT NULL DEFAULT 1,
  c_hair smallint NOT NULL DEFAULT 1,
  c_clothes smallint NOT NULL DEFAULT 1,
  pos_x bigint NOT NULL DEFAULT 0,
  pos_y bigint NOT NULL DEFAULT 0,
  CONSTRAINT characters_id PRIMARY KEY (id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE characters
  OWNER TO mmorpg;
