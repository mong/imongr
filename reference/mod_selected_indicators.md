# Shiny module providing GUI and server logic for the selected indicators tab

Shiny module providing GUI and server logic for the selected indicators
tab

## Usage

``` r
selected_indicators_ui(id)

selected_indicators_server(id, registry_tracker, pool, pool_verify)
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
