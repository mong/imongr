FROM hnskde/imongr-base-r:1.5.0

LABEL maintainer "Are Edvardsen <are.edvardsen@helse-nord.no>"
LABEL no.mongr.cd.enable="true"

WORKDIR /app/R

## add package tarball
# hadolint ignore=DL3010
COPY *.tar.gz .

## install package
RUN R CMD INSTALL --clean ./*.tar.gz

## tinytex pre-setup
RUN R -e "rmarkdown::render(input = system.file('terms.Rmd', package = 'imongr'), output_format = 'pdf_document')"

## clean up
RUN rm ./*.tar.gz

EXPOSE 3838

CMD ["R", "-e", "options(shiny.port=3838,shiny.host='0.0.0.0'); imongr::run_app()"]
