# Shiny module providing GUI and server logic for the indicator tab

Shiny module providing GUI and server logic for the indicator tab

## Usage

``` r
indicator_ui(id)

indicator_server(id, registry_tracker, pool, pool_verify)

indicator_app(pool_verify)
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
