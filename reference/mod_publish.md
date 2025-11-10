# Shiny module providing GUI and server logic for the publish indicator tab

Shiny module providing GUI and server logic for the publish indicator
tab

## Usage

``` r
publish_ui(id)

publish_server(id, tab_tracker, registry_tracker, pool, pool_verify)

publish_app(pool, pool_verify)
```

## Arguments

- id:

  Character string module namespace

- tab_tracker:

  String defining tab

- registry_tracker:

  Integer defining registry id

- pool:

  A database pool object

- pool_verify:

  A database pool object

## Value

Shiny objects for the imongr app
