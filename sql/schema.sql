CREATE TABLE users(
   id serial PRIMARY KEY,
   username VARCHAR (64) UNIQUE NOT NULL,
   password VARCHAR (128) NOT NULL
);