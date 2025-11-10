# Get relevant settings either from config og environmental variables

Get relevant settings either from config og environmental variables

## Usage

``` r
get_user_name()

get_user_groups()

db_host(context = "prod")

db_name()

db_username()

db_password()

adminer_url()
```

## Arguments

- context:

  Character string defining the environment context. Must be one of
  `c("prod", "verify", "qa")`. Default value is `"prod"`.

## Value

Character string with settings
