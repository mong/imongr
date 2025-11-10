# Functions for data operations in imongr

Functions for data operations in imongr

## Usage

``` r
delete_indicator_data(pool, df)

delete_agg_data(pool, df)

insert_data_verify(pool, df, update = NA, affirm = NA)

insert_data_prod(
  pool_verify,
  pool_prod,
  df,
  registry_delivery_ids,
  terms_version = NA
)

insert_agg_data(pool, df)

update_aggdata_delivery(pool, indicator)

agg_all_data(pool)

clean_agg_data(pool)

create_imongr_user(pool, df)

update_registry_medfield(pool, registry_id, df)

update_registry_user(pool, registry_id, df)

update_registry_user_role(pool, user_id, registry_id, role)
```

## Arguments

- pool:

  Database pool object

- df:

  A data frame with new user data for overwriting the old user list

- update:

  Character string of format YYYY-MM-DD providing date until data are
  regarded as updated. Default value is NA.

- affirm:

  Character string of format YYYY-MM-DD providing date until data are
  regarded as final. Default value is NA.

- pool_verify:

  A database pool object

- pool_prod:

  A database pool object

- registry_delivery_ids:

  Integer delivery ids

- terms_version:

  Character string providing version stamp of of the terms accepted when
  data are published. Default value is NA that will normally apply for
  all uploads prior to publishing.

- indicator:

  Character vector of indicator ids

- registry_id:

  The numeric id of the selected registry

- role:

  A string with the role to be stored

## Value

Relevant values from the current environment and database
