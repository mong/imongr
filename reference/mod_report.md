# Shiny module providing GUI and server logic for reports

Shiny module providing GUI and server logic for reports

## Usage

``` r
report_ui(id)

report_server(id, pool, pool_verify)

report_app(pool, pool_verify)
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
