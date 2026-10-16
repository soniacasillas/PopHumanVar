-- Run as a MariaDB administrator after replacing both placeholders.
-- The importer account is temporary; the application account remains read-only.

CREATE USER IF NOT EXISTS 'pophumanvar_loader'@'127.0.0.1' IDENTIFIED BY 'REPLACE_LOADER_PASSWORD';
CREATE USER IF NOT EXISTS 'pophumanvar'@'127.0.0.1' IDENTIFIED BY 'REPLACE_APP_PASSWORD';

CREATE DATABASE IF NOT EXISTS functional CHARACTER SET latin1 COLLATE latin1_swedish_ci;
CREATE DATABASE IF NOT EXISTS geva CHARACTER SET latin1 COLLATE latin1_swedish_ci;
CREATE DATABASE IF NOT EXISTS ihs CHARACTER SET latin1 COLLATE latin1_swedish_ci;
CREATE DATABASE IF NOT EXISTS ihs_pval CHARACTER SET latin1 COLLATE latin1_swedish_ci;
CREATE DATABASE IF NOT EXISTS isafe CHARACTER SET latin1 COLLATE latin1_swedish_ci;
CREATE DATABASE IF NOT EXISTS isafe_pval CHARACTER SET latin1 COLLATE latin1_swedish_ci;
CREATE DATABASE IF NOT EXISTS nsl CHARACTER SET latin1 COLLATE latin1_swedish_ci;
CREATE DATABASE IF NOT EXISTS nsl_pval CHARACTER SET latin1 COLLATE latin1_swedish_ci;
CREATE DATABASE IF NOT EXISTS phs CHARACTER SET latin1 COLLATE latin1_swedish_ci;

GRANT ALL PRIVILEGES ON functional.* TO 'pophumanvar_loader'@'127.0.0.1';
GRANT ALL PRIVILEGES ON geva.* TO 'pophumanvar_loader'@'127.0.0.1';
GRANT ALL PRIVILEGES ON ihs.* TO 'pophumanvar_loader'@'127.0.0.1';
GRANT ALL PRIVILEGES ON ihs_pval.* TO 'pophumanvar_loader'@'127.0.0.1';
GRANT ALL PRIVILEGES ON isafe.* TO 'pophumanvar_loader'@'127.0.0.1';
GRANT ALL PRIVILEGES ON isafe_pval.* TO 'pophumanvar_loader'@'127.0.0.1';
GRANT ALL PRIVILEGES ON nsl.* TO 'pophumanvar_loader'@'127.0.0.1';
GRANT ALL PRIVILEGES ON nsl_pval.* TO 'pophumanvar_loader'@'127.0.0.1';
GRANT ALL PRIVILEGES ON phs.* TO 'pophumanvar_loader'@'127.0.0.1';

GRANT SELECT ON functional.* TO 'pophumanvar'@'127.0.0.1';
GRANT SELECT ON geva.* TO 'pophumanvar'@'127.0.0.1';
GRANT SELECT ON ihs.* TO 'pophumanvar'@'127.0.0.1';
GRANT SELECT ON ihs_pval.* TO 'pophumanvar'@'127.0.0.1';
GRANT SELECT ON isafe.* TO 'pophumanvar'@'127.0.0.1';
GRANT SELECT ON isafe_pval.* TO 'pophumanvar'@'127.0.0.1';
GRANT SELECT ON nsl.* TO 'pophumanvar'@'127.0.0.1';
GRANT SELECT ON nsl_pval.* TO 'pophumanvar'@'127.0.0.1';
GRANT SELECT ON phs.* TO 'pophumanvar'@'127.0.0.1';
FLUSH PRIVILEGES;

-- After the import succeeds, remove the temporary loader:
-- DROP USER 'pophumanvar_loader'@'127.0.0.1';

