# Connect Cloud deployment (consultation mirror)

This profile uses the same application code but sets
`PHV_ENABLE_HAPLOTYPES=false`. It provides the database-backed searches,
annotations, scores, tables, downloads, and JBrowse view. It does not read VCF
files or run `bcftools`.

## Before deploying

1. Restore the nine databases to DreamHost using the database guide.
2. Create a dedicated read-only MySQL user for runtime access.
3. In DreamHost, allow only the current outbound IP addresses published by
   Posit Connect Cloud. Recheck that list in Posit's documentation/dashboard
   immediately before configuring the allowlist because it can change.
4. If DreamHost provides a CA certificate for MySQL TLS, add that certificate
   to the deployment and set `PHV_DB_SSL_CA` to its runtime path.
5. Run the repository's **Generate Connect Cloud manifest** GitHub workflow
   once and confirm that it commits `manifest.json` successfully.

## Create the application

In Connect Cloud, create a Shiny deployment from this GitHub repository and
select `app.R` as the primary document. Use R 4.6.0 and configure the variables
shown in `environment.example` as deployment secrets/environment variables.
Do not add `PHV_VCF_DIR` or `PHV_BCFTOOLS` in this profile.

After the build succeeds, test at least:

- one region query and one rsID/gene query;
- each score table (`ihs`, `nsl`, `isafe`, `geva`, and `phs`);
- downloads and the JBrowse panel;
- that the haplotype area shows the UAB-server notice and does not attempt a
  computation.

The GitHub repository should remain private while the recovered code is under
review. A Connect Cloud plan capable of deploying private repositories is
therefore required; making the repository public is a separate publication
decision.

