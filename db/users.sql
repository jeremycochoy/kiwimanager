-- Table: users

-- DROP TABLE users;

CREATE TABLE users
(
  id serial NOT NULL,
  name character varying(20) NOT NULL,
  password character varying(65) NOT NULL,
  email text NOT NULL,
  first_name text,
  last_name text,
  birthday timestamp without time zone,
  verified boolean NOT NULL DEFAULT false,
  logged boolean NOT NULL DEFAULT false,
  salt bytea NOT NULL,
  last_login_ip inet,
  last_login_at timestamp without time zone,
  created_at timestamp without time zone,
  CONSTRAINT users_id PRIMARY KEY (id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE users
  OWNER TO mmorpg;
