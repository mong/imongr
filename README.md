# imongr <img src="man/figures/logo.png" align="right" height="150" />

<!-- badges: start -->
[![Version](https://img.shields.io/github/v/release/mong/imongr?sort=semver)](https://github.com/mong/imongr/releases)
[![R build status](https://github.com/mong/imongr/workflows/R-CMD-check/badge.svg)](https://github.com/mong/imongr/actions)
[![Codecov test coverage](https://codecov.io/gh/mong/imongr/branch/main/graph/badge.svg)](https://codecov.io/gh/mong/imongr?branch=main)
[![GitHub open issues](https://img.shields.io/github/issues/mong/imongr.svg)](https://github.com/mong/imongr/issues)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://mong.github.io/imongr/)
<!-- badges: end -->

Primarily a tool to update data used by the [mongts](https://github.com/mong/mongts/) application.

## Install

You can install the released version of imongr from [GitHub](https://github.com/mong/imongr) with:

``` r
remotes::install_github("mong/imongr")
```

## Development

The easiest way to develop `imongr` is to fire up the `docker-compose.yml` file:

```sh
docker-compose up
```

This file consist of six different services:
- three mariadb databases (`prod` at port `3331`, `verify` at port `3332`, and `qa` at port `3333`)
- Adminer, a tool for database management, at port [8888](http://localhost:8888/)
- RStudio at port [8787](http://localhost:8787/)
- The app, based on the `hnskde/imongr:latest` image, at port [3838](http://localhost:3838/)

Open [localhost:8787](http://localhost:8787/) with your favorite browser and login with `rstudio` and `password`. Go into the `imongr` folder, open `imongr.Rproj`, and press **Yes** to *Do you want to open the project ~/imongr?*. Start coding.

Populate the databases by using Adminer, either through `imongr` (*Administrative verktøy* - *Adminer*) or through [port 8888](http://localhost:8888/) (the password is the same as username/db/repository name).

The data can be visualized by using the `mongts` app:

```bash
export DB_PORT=3331 # or 3332 for the verify database
yarn install && yarn dev # inside the mongts folder
```

The data can then be seen at [localhost:3000/kvalitetsregistre/alle/sykehus/](http://localhost:3000/kvalitetsregistre/alle/sykehus/)

### Build docker image and run the container app locally

```bash
R CMD build .
docker build -t hnskde/imongr:latest .
docker-compose up
```

Navigate a browser to [localhost:3838/](http://localhost:3838/).

### Getting out of some dirty states

If the environment variables have been changed an you want to change them back to default:

```r
readRenviron("~/.Renviron")
```

If your pool have not been closed properly:

```r
... # TBA
```

If you have not cleaned up after testing with database:

```r
readRenviron("~/.Renviron") # get back to default env var
... # TBA
```


## Ethics
Please note that the 'imongr' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
