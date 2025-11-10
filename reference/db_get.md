# Retreiv data from imongr database

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
get_all_user_data(pool)

get_user_data(pool)

get_user_id(pool)

get_user_registries(pool)

get_user_registry_select(pool)

get_user_deliveries(pool)

get_registry_data(pool, registry)

get_indicators_registry(pool, indicator)

get_registry_ind(pool, registry)

get_registry_name(pool, registry)

get_registry_short_name(pool, registry, full_name = FALSE, short_name = FALSE)

get_registry_full_name(pool, registry, full_name = FALSE, short_name = FALSE)

get_org_name(pool, orgnr)

get_hospitals(pool)

get_hfs(pool)

get_rhfs(pool)

get_flat_org(pool)

get_all_orgnr(pool, include_short_name = FALSE)

get_user(pool, sample = NA)

get_users(pool, valid = TRUE)

get_registry_indicators(pool, registry)

get_dg_indicators(pool, registry)

get_registry_medfield(pool, registry)

get_medfield_registry(pool, medfield)

get_registry_user(pool, registry)

get_user_registry(pool, user)

get_users_per_registry(pool)

get_aggdata_delivery(pool, indicator)

get_aggdata(pool, registry)

get_review_collaborators(pool, registry)

get_registry_projects(pool, registry, indicator)

get_project_hospitals(pool, project)

get_publications(pool, registry)

get_ind_agg_data(pool, ind_id)

get_ind_units(pool, ind_id)

get_ind_limits(pool, ind_id)
```

## Arguments

- pool:

  a database connection pool object

- registry:

  Integer defining registry id

- indicator:

  Character vector of indicator ids

- full_name:

  Logical defining if full names is to be returned

- orgnr:

  Integer id of organization

- include_short_name:

  Logical if variable 'short_name' is to be returned

- sample:

  Integer in range \\0, 1\\ defining data set subsample size. Defaults
  to NA in which case all data is returned

- valid:

  Logical if to select valid user only. TRUE by default

- medfield:

  Integer defining medfield id

- user:

  Integer defining user id

## Value

Data object from database
