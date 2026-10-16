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
bash deploy/database/import-databases.sh
bash deploy/database/verify-databases.sh
```

4. Ask the administrator to remove `pophumanvar_loader` after verification.

## DreamHost MySQL

1. In the DreamHost panel, create the nine databases with the exact names
   listed in the main README (or record any mandatory prefixes). Give one
   dedicated database user access to all nine.
2. Allow connections from the Connect Cloud outbound IP addresses in the
   DreamHost database access settings. Do not expose the account to arbitrary
   hosts.
3. From the DreamHost SSH shell, set `PHV_DB_HOST` to the database hostname and
   run the same importer. If DreamHost forces prefixed database names, the app
   will need a name mapping before deployment; do not rename dumps silently.
4. Run `verify-databases.sh`, then use a separate read-only runtime user if the
   DreamHost plan permits it.

The importer resumes at database granularity: if interrupted, rerun it after
removing/recreating the partially imported database or after confirming that
replaying its `DROP TABLE` statements is acceptable.

