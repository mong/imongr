# Shiny module providing GUI and server logic for the download data tab

Shiny module providing GUI and server logic for the download data tab

## Usage

``` r
download_ui(id)

download_server(id, registry_tracker, pool, pool_verify)

download_app(pool, pool_verify)
```

## Arguments

- id:

  Character string module namespace

- registry_tracker:

  Integer defining registry id

- pool:

  A database pool object

- pool_verify:

  A database pool object

## Value

Shiny objects for the imongr app
