# Recovery record

This source was recovered from `/home/selector/shiny-server/pophumanvar` in the
original server backup. The service previously ran on CentOS 7 with R 3.6 and
Shiny Server behind Apache.

Deliberate recovery changes are limited to deployment and security needs:

- removed PHPMailer and the upload-and-email workflow;
- replaced that workflow with the two project contact addresses;
- removed embedded database/SMTP credentials;
- made database, VCF, bcftools, and haplotype settings configurable;
- replaced the obsolete `RMySQL` driver with `RMariaDB`;
- added the feature flag needed for a database-only consultation mirror;
- omitted editor artefacts and obsolete `www/appStyle.old`.

No scientific query, filtering, scoring, plotting, browsing, or download logic
has otherwise been intentionally changed. Direct package versions observed in
the old installation are preserved in `original-package-versions.tsv` for
forensic reference; new deployments use versions compatible with R 4.6.0.

