# Shiny module providing GUI and server logic for (user) profile

Shiny module providing GUI and server logic for (user) profile

## Usage

``` r
profile_ui(id)

profile_server(id, pool, pool_verify)

profile_app(pool, pool_verify)
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
