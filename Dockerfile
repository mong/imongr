FROM hnskde/imongr-base-r:4.0.0

LABEL maintainer "Arnfinn Hykkerud Steindal <arnfinn.hykkerud.steindal@helse-nord.no>"
LABEL no.mongr.cd.enable="true"

WORKDIR /app/R

## add package tarball
# hadolint ignore=DL3010
COPY *.tar.gz .

## install package, clean up and make sure sufficient latex tools exists in base image
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')" \
    # imongr did not work with bslib v6 when setting bootstrap version to 4
    # See issue https://github.com/mong/imongr/issues/380
    && R -e "remotes::install_github('rstudio/bslib@v0.5.1')" \
    && R CMD INSTALL --clean ./*.tar.gz \
    && rm ./*.tar.gz \
    && R -e "rmarkdown::render(input = system.file('terms.Rmd', package = 'imongr'), output_format = 'html_fragment')"

EXPOSE 3838

CMD ["R", "-e", "options(shiny.port=3838,shiny.host='0.0.0.0'); imongr::run_app()"]
