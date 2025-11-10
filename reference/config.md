# Functions handling imongr R package config

Functions handling imongr R package config

## Usage

``` r
create_config(dir = ".")

write_config(config, dir = ".", filename = "_imongr.yml")

get_config(dir = ".")

check_config(config)
```

## Arguments

- dir:

  string providing path to configuration file

- config:

  list containing configuration

- filename:

  string defining config filename

## Value

A status message or list of config
