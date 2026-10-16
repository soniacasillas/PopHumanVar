FROM rocker/r-ver:4.6.0

ENV DEBIAN_FRONTEND=noninteractive \
    INSTALL_HAPLOTYPES=true

RUN apt-get update && apt-get install -y --no-install-recommends \
      bcftools \
      ca-certificates \
      libcurl4-openssl-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libfribidi-dev \
      libgit2-dev \
      libglpk-dev \
      libharfbuzz-dev \
      libicu-dev \
      libjpeg-dev \
      libmariadb-dev \
      libpng-dev \
      libssl-dev \
      libtiff-dev \
      libxml2-dev \
      make \
      pandoc \
      sed \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY scripts/install-packages.R /tmp/install-packages.R
RUN Rscript /tmp/install-packages.R && rm /tmp/install-packages.R

COPY . /app
RUN useradd --create-home --uid 10001 pophumanvar \
    && chown -R pophumanvar:pophumanvar /app

USER pophumanvar
EXPOSE 8080
CMD ["Rscript", "/app/scripts/run-app.R"]

