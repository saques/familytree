# familytree

## TO RUN (from src/)

- [Install the Haskell Platform](https://www.haskell.org/platform/) (should be able to run Cabal)
- [Follow tutorial](http://snapframework.com/download)
- [Set PATH](http://snapframework.com/docs/quickstart)
- `sudo apt install libpq-dev`
- `cabal install snaplet-postgresql-simple`
- `cabal install`
- Run `cabal install` in backend dir `src/`
- Run with `familytree -p 9000`

## RUN FRONTEND (from webapp/)

- Install elm: `npm install -g elm` (Should be version 0.19.0)
- Build Main.elm: `elm make src/Main.elm --output=main.js`
- Run reactor: `elm reactor`
- Go to: `http://localhost:8000/index.html`
- Enjoy

## Database setup (from sql/)

1. Set password for `postgres` user to `postgres` as specified below.
2. Make sure a `familytree` database exists, as shown below.
2. Run the schema:`psql -h localhost -U postgres -d familytree -a -f schema.sql`

## Devops stuff

- Initialize PostgreSQL: `/usr/lib/postgresql/11/bin/pg_ctl -D /var/lib/postgresql/11/main -l logfile start`
- Login to PostgreSQL: `sudo -u postgres psql` (specify familytree database with `-d familytree`)
- Change password: `\password postgres`, then enter and confirm
- Create database: `CREATE DATABASE familytree;`

## Interesting tutorials

- [Configure PostgreSQL](http://snapforbeginners.com/chapters/postgres-simple.html)
- [Set up PostgreSQL auth](http://snapforbeginners.com/chapters/auth.html)
- [snaplet-postgresql-simple](http://hackage.haskell.org/package/snaplet-postgresql-simple-1.2.0.0/docs/Snap-Snaplet-PostgresqlSimple.html)
- [Binding data in heist with splices](http://snapframework.com/docs/tutorials/heist#heist-programming)

