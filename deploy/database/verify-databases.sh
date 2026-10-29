#!/usr/bin/env bash
set -Eeuo pipefail

databases=(functional geva ihs ihs_pval isafe isafe_pval nsl nsl_pval phs)
: "${PHV_DB_HOST:?Set PHV_DB_HOST}"
: "${PHV_DB_USER:?Set PHV_DB_USER}"
: "${PHV_DB_PASSWORD:?Set PHV_DB_PASSWORD}"
PHV_DB_PORT="${PHV_DB_PORT:-3306}"
PHV_DB_PREFIX="${PHV_DB_PREFIX:-}"
client="${PHV_MYSQL_CLIENT:-mariadb}"

if [[ ! "$PHV_DB_PREFIX" =~ ^[A-Za-z0-9_]*$ ]]; then
  echo "PHV_DB_PREFIX may contain only letters, numbers and underscores." >&2
  exit 1
fi

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

printf "database\ttables\trows_estimated\n"
for database in "${databases[@]}"; do
  target_database="${PHV_DB_PREFIX}${database}"
	result="$("$client" --defaults-extra-file="$defaults_file" --batch --skip-column-names \
    -e "SELECT table_schema, COUNT(*), COALESCE(SUM(table_rows),0) FROM information_schema.tables WHERE table_schema='${target_database}' GROUP BY table_schema" \
		|| true)"
	if [[ -n "$result" ]]; then
		printf '%s\n' "$result"
	else
		printf '%s\tMISSING\tMISSING\n' "$target_database"
	fi
done

