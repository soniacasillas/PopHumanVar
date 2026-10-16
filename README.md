# PopHumanVar

Recovered and cleaned PopHumanVar Shiny application. This repository contains
one codebase for two deployment profiles:

| Profile | Shiny | SQL | VCF/bcftools | `PHV_ENABLE_HAPLOTYPES` |
|---|---|---|---|---|
| UAB (primary) | persistent container | local MariaDB | `/var/www/pophuman/files/vcf` | `true` |
| Connect Cloud (mirror) | Posit Connect Cloud | DreamHost MySQL | not used | `false` |

The consultation functions are SQL-backed. Only the EHH/furcation haplotype
plots require the local VCF collection and `bcftools`.

## Configuration

Copy `.Renviron.example` to a location outside the repository and set:

- `PHV_DB_HOST`, `PHV_DB_PORT`, `PHV_DB_USER`, `PHV_DB_PASSWORD`
- `PHV_ENABLE_HAPLOTYPES=true|false`
- `PHV_VCF_DIR` and `PHV_BCFTOOLS` when haplotypes are enabled

Never commit passwords or a real `.Renviron` file. The application expects the
nine databases `functional`, `geva`, `ihs`, `ihs_pval`, `isafe`,
`isafe_pval`, `nsl`, `nsl_pval`, and `phs` to be accessible using the same SQL
credentials.

## Deployment guides

- Primary UAB service: [`deploy/uab/README.md`](deploy/uab/README.md)
- Connect Cloud mirror: [`deploy/connect-cloud/README.md`](deploy/connect-cloud/README.md)
- Database restoration: [`deploy/database/README.md`](deploy/database/README.md)

## Local syntax check

With the required R packages installed:

```sh
Rscript scripts/check-app.R
```

The recovered application deliberately has no mail-sending or file-upload
workflow. Its contact area directs users to `sonia.casillas@uab.cat` and
`antonio.barbadilla@uab.cat`.
