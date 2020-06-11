FROM rocker/r-base

LABEL maintainer "Are Edvardsen <are.edvardsen@helse-nord.no>"
LABEL no.mongr.cd.enable="true"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libxml2-dev \
    libssl-dev \
    libmariadbclient-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# basic R functionality
RUN R -e "install.packages(c('remotes'), repos='https://cloud.r-project.org/')"

# install package dependencies
RUN R -e "install.packages(c('digest', 'DT', 'pool', 'readr', 'RMariaDB', 'yaml'))"

RUN R -e "remotes::install_github('rstudio/shiny')"

RUN R -e "install.packages(c('shinyalert', 'shinycssloaders'))"


## add package tarball
COPY *.tar.gz .

## install package
RUN R CMD INSTALL --clean *.tar.gz

## clean up
RUN rm *.tar.gz

EXPOSE 3838

CMD ["R", "-e", "options(shiny.port=3838,shiny.host='0.0.0.0'); imongr::run_app()"]
