# Shiny module providing UI and server functions for registry status overview

Shiny module providing UI and server functions for registry status
overview

## Usage

``` r
add_arrows(dat_format)

status_ui(id)

status_server(id, pool, pool_verify)

status_app(pool, pool_verify)
```

## Arguments

- id:

  Character string module namespace

- pool:

  A database pool object connecting to production data

- pool_verify:

  A database pool object connecting to staging data

## Value

Shiny objects for the imongr app
