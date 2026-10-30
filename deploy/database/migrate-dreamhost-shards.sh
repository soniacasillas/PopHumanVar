#!/usr/bin/env bash
set -Eeuo pipefail

: "${PHV_DB_HOST:?Set PHV_DB_HOST}"
: "${PHV_DB_USER:?Set PHV_DB_USER to the loader user}"
: "${PHV_DB_PASSWORD:?Set PHV_DB_PASSWORD}"
PHV_DB_PORT="${PHV_DB_PORT:-3306}"
PHV_DB_PREFIX="${PHV_DB_PREFIX:-dh_}"
PHV_DB_SHARD_MAP_FILE="${PHV_DB_SHARD_MAP_FILE:-config/dreamhost-shards.tsv}"

[[ -r "$PHV_DB_SHARD_MAP_FILE" ]] || {
  echo "Cannot read shard map: $PHV_DB_SHARD_MAP_FILE" >&2
  exit 1
}
[[ "$PHV_DB_PREFIX" =~ ^[A-Za-z0-9_]*$ ]] || {
  echo "PHV_DB_PREFIX may contain only letters, numbers and underscores." >&2
  exit 1
}

mysql_client="${PHV_MYSQL_CLIENT:-mysql}"
dump_client="${PHV_MYSQL_DUMP_CLIENT:-mysqldump}"
command -v "$mysql_client" >/dev/null || { echo "Missing client: $mysql_client" >&2; exit 1; }
command -v "$dump_client" >/dev/null || { echo "Missing dump client: $dump_client" >&2; exit 1; }

defaults_file="$(mktemp)"
trap 'rm -f "$defaults_file"' EXIT
chmod 600 "$defaults_file"
cat >"$defaults_file" <<EOF
[client]
host=$PHV_DB_HOST
port=$PHV_DB_PORT
user=$PHV_DB_USER
password=$PHV_DB_PASSWORD
protocol=tcp
EOF

tail -n +2 "$PHV_DB_SHARD_MAP_FILE" |
while IFS=$'\t' read -r logical_database table shard; do
  [[ -n "$logical_database" ]] || continue
  for identifier in "$logical_database" "$table" "$shard"; do
    [[ "$identifier" =~ ^[A-Za-z0-9_]+$ ]] || {
      echo "Invalid identifier in shard map: $identifier" >&2
      exit 1
    }
  done

  source_database="${PHV_DB_PREFIX}${logical_database}"
  target_database="${PHV_DB_PREFIX}${shard}"
  echo "Copying ${source_database}.${table} -> ${target_database}.${table}"
  "$dump_client" --defaults-extra-file="$defaults_file" \
    --default-character-set=latin1 \
    --no-tablespaces --skip-lock-tables --skip-add-locks --skip-triggers --quick \
    "$source_database" "$table" |
    "$mysql_client" --defaults-extra-file="$defaults_file" \
      --default-character-set=latin1 "$target_database"
done

echo "All mapped tables were copied. The source databases were not modified."