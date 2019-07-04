# familytree

## TO RUN

- [Install the Haskell Platform](https://www.haskell.org/platform/) (should be able to run Cabal)
- [Follow tutorial](http://snapframework.com/download)
- [Set PATH](http://snapframework.com/docs/quickstart)
- Run `cabal install` in project dir
- Run with `familytree -p <port>`

## Database setup

1. `sudo apt install libpq-dev`
2. `cabal install snaplet-postgresql-simple`
3. On project dir: `cabal install`
4. Set password for `postgres` user to `postgres` as specified below
5. `psql -h localhost -U postgres -d familytree -a -f schema.sql`
6. Run with `familytree -p <port>`

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

