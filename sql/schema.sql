CREATE TABLE users(
   id serial PRIMARY KEY,
   username VARCHAR (64) UNIQUE NOT NULL,
   password VARCHAR (128) NOT NULL
);


CREATE TABLE persons(
   id serial PRIMARY KEY,
   family_tree_id int NOT NULL REFERENCES family_trees(id),
   level int NOT NULL,
   name VARCHAR (50) NOT NULL,
   last_name VARCHAR (50) NOT NULL,
   birth_date VARCHAR (50) NOT NULL,
   hair_color VARCHAR (50) NOT NULL,
   eye_color VARCHAR (50) NOT NULL,
   skin_color VARCHAR (50) NOT NULL,
   death_date VARCHAR (50) ,
   death_place VARCHAR (50) ,
   profession VARCHAR (50),
   deseases int[]
);

CREATE TABLE parent_relation(
   id serial PRIMARY KEY,
   descendant_id bigint NOT NULL REFERENCES persons(id),
   parent_id bigint NOT NULL REFERENCES persons(id)
);

CREATE TABLE family_trees(
	id serial PRIMARY KEY,
	name text UNIQUE
);