FROM hnskde/imongr-base-r:4.2.0

LABEL maintainer="Arnfinn Hykkerud Steindal <arnfinn.hykkerud.steindal@helse-nord.no>"
LABEL no.mongr.cd.enable="true"

WORKDIR /app/R

RUN installr -d shinyvalidate

# hadolint ignore=DL3018,DL3013
RUN apk add --no-cache --update python3 py3-pip \
    && apk add --no-cache --update --virtual=build gcc musl-dev python3-dev \
    libffi-dev openssl-dev cargo make \
    && pip3 install --no-cache-dir --prefer-binary azure-cli \
    && apk del build

## add package tarball
# hadolint ignore=DL3010
COPY *.tar.gz .

## install package, clean up and make sure sufficient pandoc tools exists in base image
RUN R CMD INSTALL --clean ./*.tar.gz \
    && rm ./*.tar.gz \
    && R -e "rmarkdown::render(input = system.file('terms.Rmd', package = 'imongr'), output_format = 'html_fragment')"


EXPOSE 3838

CMD ["R", "-e", "options(shiny.port=3838,shiny.host='0.0.0.0'); imongr::run_app()"]
