#!/usr/bin/env bash
set -Eeuo pipefail

databases=(functional geva ihs ihs_pval isafe isafe_pval nsl nsl_pval phs)

: "${PHV_DUMP_DIR:?Set PHV_DUMP_DIR to the directory containing the .sql.gz files}"
: "${PHV_DB_HOST:?Set PHV_DB_HOST}"
: "${PHV_DB_USER:?Set PHV_DB_USER}"
: "${PHV_DB_PASSWORD:?Set PHV_DB_PASSWORD}"
PHV_DB_PORT="${PHV_DB_PORT:-3306}"

client="${PHV_MYSQL_CLIENT:-}"
if [[ -z "$client" ]]; then
  if command -v mariadb >/dev/null 2>&1; then
    client="mariadb"
  elif command -v mysql >/dev/null 2>&1; then
    client="mysql"
  else
    echo "Neither the mariadb nor mysql command-line client is installed." >&2
    exit 1
  fi
fi

defaults_file="$(mktemp)"
cleanup() {
  rm -f "$defaults_file"
}
trap cleanup EXIT
chmod 600 "$defaults_file"
cat >"$defaults_file" <<EOF
[client]
host=$PHV_DB_HOST
port=$PHV_DB_PORT
user=$PHV_DB_USER
password=$PHV_DB_PASSWORD
protocol=tcp
EOF

if [[ -f "$PHV_DUMP_DIR/SHA256SUMS" ]] && command -v sha256sum >/dev/null 2>&1; then
  echo "Verifying database dump checksums..."
  (cd "$PHV_DUMP_DIR" && sha256sum --check SHA256SUMS)
fi

for database in "${databases[@]}"; do
  dump="$PHV_DUMP_DIR/$database.sql.gz"
  if [[ ! -r "$dump" ]]; then
    echo "Missing dump: $dump" >&2
    exit 1
  fi

  echo "Importing $database..."
  gzip -dc -- "$dump" | "$client" \
    --defaults-extra-file="$defaults_file" \
    --default-character-set=latin1 \
    "$database"
done

echo "All nine PopHumanVar databases were imported."

