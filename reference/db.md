# Low level database functions for imongr

Database metadata are read from config. If one or more of these are
defined 'env' corresponding values will be fetched from the
environmental variables IMONGR_DB_HOST, IMONGR_DB_NAME, IMONGR_DB_USER
and IMONGR_DB_PASS. If these are undefined the function providing
connection handles will exit with an error. What the function expects
from table names and variable names in the data is also defined by the
configuration. Thus, functions can be left unchanged when the data model
is altered as long as such changes are reflected by configuration.

## Usage

``` r
make_pool(context = "prod")

drain_pool(pool)

insert_table(pool, table, df)

get_table(pool, table, sample = NA)

get_table_raw(pool, table, sample = NA)
```

## Arguments

- context:

  Character string defining the environment context. Must be one of
  `c("prod", "verify", "qa")`. Default value is `"prod"`.

- pool:

  a database connection pool object

- table:

  string defining target database table

- df:

  data frame containing data to be inserted into a database

- sample:

  Numeric in the range 0 to 1 defining the relative sub-sample size,
  *e.g.* when `sample = 0.1` approximately 10% of observations are
  returned. Default is `NA` which will return all data

## Value

Database pool object, data frame or status message

## Details

Ordinary user interactions with data should have their own functions,
but may be built ontop of these ones. For instance, such functions must
make sure consistency (*e.g.* foreign keys) between database tables are
kept in order.
