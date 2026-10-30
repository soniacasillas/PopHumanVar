# Database restoration

The dump files are intentionally not stored in Git. Copy the nine `.sql.gz`
files and `SHA256SUMS` from the recovery package into one private directory.
The original tables use `latin1`; the import script preserves that encoding.

## UAB MariaDB

1. Ask the administrator to run `create-uab-databases.sql` after replacing its
   two password placeholders. The loader account is temporary and the runtime
   account has only `SELECT` access.
2. Copy the dumps to the UAB server (at least 11 GB compressed plus temporary
   and database storage).
3. Export the loader connection variables and run the importer:

```sh
export PHV_DUMP_DIR=/private/path/pophumanvar-dumps
export PHV_DB_HOST=127.0.0.1
export PHV_DB_PORT=3306
export PHV_DB_USER=pophumanvar_loader
export PHV_DB_PASSWORD='the-temporary-loader-password'
export PHV_DB_PREFIX=
bash deploy/database/import-databases.sh
bash deploy/database/verify-databases.sh
```

4. Ask the administrator to remove `pophumanvar_loader` after verification.

## DreamHost MySQL

1. In the DreamHost panel, create `dh_functional`, `dh_geva`, `dh_ihs`,
   `dh_ihs_pval`, `dh_isafe`, `dh_isafe_pval`, `dh_nsl`, `dh_nsl_pval`, and
   `dh_phs`. Give one dedicated database user access to all nine.
2. Allow connections from the Connect Cloud outbound IP addresses in the
   DreamHost database access settings. Do not expose the account to arbitrary
   hosts.
3. From the DreamHost SSH shell, set `PHV_DB_HOST` to the database hostname,
   set `PHV_DB_PREFIX=dh_`, and run the same importer. Dump filenames remain
   unprefixed; the importer maps each dump to its prefixed destination.
4. Run `verify-databases.sh`, then use a separate read-only runtime user if the
   DreamHost plan permits it.

The importer resumes at database granularity: if interrupted, rerun it after
removing/recreating the partially imported database or after confirming that
replaying its `DROP TABLE` statements is acceptable.
## DreamHost table shards

The checked-in `config/dreamhost-shards.tsv` keeps every physical database
below approximately 2.8 GB using seven `functional` shards and three `geva`
shards. The remaining seven databases are not split.

Create these ten empty databases in the DreamHost panel and grant the loader
user full table privileges on them:

- `dh_functional_1` through `dh_functional_7`
- `dh_geva_1` through `dh_geva_3`

Keep `dh_functional` and `dh_geva` unchanged during migration. From the
repository root on the DreamHost SSH host, export the loader credentials and
run:

```sh
export PHV_DB_PREFIX=dh_
export PHV_DB_SHARD_MAP_FILE=config/dreamhost-shards.tsv
export PHV_MYSQL_CLIENT=mysql
export PHV_MYSQL_DUMP_CLIENT=mysqldump
bash deploy/database/migrate-dreamhost-shards.sh \
  2>&1 | tee ~/pophumanvar-shard-migration.log
bash deploy/database/verify-dreamhost-shards.sh \
  2>&1 | tee ~/pophumanvar-shard-verification.log
```

The migration copies tables through a stream and never modifies the sources.
Verification compares exact row counts between every source and destination.
After all rows report `OK`, grant the read-only Connect Cloud user `SELECT`
access to all ten shards and add the current Connect Cloud outbound IPs to that
user's allowable hosts.

Configure Connect Cloud with:

```text
PHV_DB_PREFIX=dh_
PHV_DB_LAYOUT=sharded
PHV_DB_SHARD_MAP_FILE=config/dreamhost-shards.tsv
```

Republish and test chromosome queries from every shard before removing the old
`dh_functional` and `dh_geva` databases in the DreamHost panel. Database
removal is deliberately not automated.
