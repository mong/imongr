# imongr <img src="man/figures/logo.svg" align="right" height="150" />

<!-- badges: start -->
[![Version](https://img.shields.io/github/v/release/mong/imongr?sort=semver)](https://github.com/mong/imongr/releases)
[![Travis build status](https://travis-ci.org/mong/imongr.svg?branch=master)](https://travis-ci.org/mong/imongr)
[![R build status](https://github.com/mong/imongr/workflows/R-CMD-check/badge.svg)](https://github.com/mong/imongr/actions)
[![Codecov test coverage](https://codecov.io/gh/mong/imongr/branch/master/graph/badge.svg)](https://codecov.io/gh/mong/imongr?branch=master)
[![GitHub open issues](https://img.shields.io/github/issues/mong/imongr.svg)](https://github.com/mong/imongr/issues)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://mong.github.io/imongr/)
<!-- badges: end -->

Primarily a tool to update data used by the [qmongr](https://github.com/mong/qmongr/) application.

## Install

You can install the released version of imongr from [GitHub](https://github.com/mong/imongr) with:

``` r
remotes::install_github("mong/imongr")
```

## Development

1. Fire up docker-compose (`docker-compose up`)
2. Enter *RStudio* on http://localhost:8787/
3. Set up `imongr` inside docker. Either do `git clone git@github.com:mong/imongr` and create new project based on folder, or create new project based on git repository. Dependencies can be installed by first install `remotes` and then install `imongr` from `github`: `remotes::install_github("mong/imongr")`.
4. Define user, groups etc.
```
Sys.setenv(SHINYPROXY_USERNAME="<my-user-name>")
Sys.setenv(SHINYPROXY_USERGROUPS="MANAGER,PROVIDER")
```

## Ethics
Please note that the 'imongr' project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
