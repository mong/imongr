# Shiny module providing GUI and server logic for the upload data tab

Shiny module providing GUI and server logic for the upload data tab

## Usage

``` r
upload_ui(id)

upload_server(id, registry_tracker, pool_verify)

upload_app(pool)
```

## Arguments

- id:

  Character string module namespace

- registry_tracker:

  Integer defining registry id

- pool_verify:

  A database pool object

- pool:

  A database pool object

## Value

Shiny objects for the imongr app
