#!/usr/bin/env bash
set -Eeuo pipefail

: "${PHV_DB_HOST:?Set PHV_DB_HOST}"
: "${PHV_DB_USER:?Set PHV_DB_USER}"
: "${PHV_DB_PASSWORD:?Set PHV_DB_PASSWORD}"
PHV_DB_PORT="${PHV_DB_PORT:-3306}"
PHV_DB_PREFIX="${PHV_DB_PREFIX:-dh_}"
PHV_DB_SHARD_MAP_FILE="${PHV_DB_SHARD_MAP_FILE:-config/dreamhost-shards.tsv}"
mysql_client="${PHV_MYSQL_CLIENT:-mysql}"

[[ -r "$PHV_DB_SHARD_MAP_FILE" ]] || {
  echo "Cannot read shard map: $PHV_DB_SHARD_MAP_FILE" >&2
  exit 1
}

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

failed_file="$(mktemp)"
trap 'rm -f "$defaults_file" "$failed_file"' EXIT
printf 'source\ttable\ttarget\tsource_rows\ttarget_rows\tstatus\n'
tail -n +2 "$PHV_DB_SHARD_MAP_FILE" |
while IFS=$'\t' read -r logical_database table shard; do
  [[ -n "$logical_database" ]] || continue
  source_database="${PHV_DB_PREFIX}${logical_database}"
  target_database="${PHV_DB_PREFIX}${shard}"
  result="$("$mysql_client" --defaults-extra-file="$defaults_file" \
    --batch --skip-column-names \
    -e "SELECT (SELECT COUNT(*) FROM \`${source_database}\`.\`${table}\`), (SELECT COUNT(*) FROM \`${target_database}\`.\`${table}\`);")"
  read -r source_rows target_rows <<<"$result"
  status=OK
  if [[ -z "$source_rows" || -z "$target_rows" || "$source_rows" != "$target_rows" ]]; then
    status=MISMATCH
    printf '1\n' >"$failed_file"
  fi
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$source_database" "$table" "$target_database" \
    "${source_rows:-MISSING}" "${target_rows:-MISSING}" "$status"
done

if [[ -s "$failed_file" ]]; then
  echo "Shard verification failed." >&2
  exit 1
fi
echo "Every sharded table has an exact matching row count."