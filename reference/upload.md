# Upload data to imongr

Various functions used when data are being uploaded to imongr. All
checks regarding consistency goes here

## Usage

``` r
check_report(registry, df, ind, pool)

mail_check_report(pool, registry, mail_msg)

check_upload(registry, df, ind, pool)

check_missing_registry(registry, df, ind, conf, pool)

check_mixing_ind(registry, df, ind, conf, pool)

check_missing_var(registry, df, ind, conf, pool)

check_invalid_var(registry, df, ind, conf, pool)

check_invalid_context(registry, df, ind, conf, pool)

check_invalid_org(registry, df, ind, conf, pool)

check_invalid_ind(registry, df, ind, conf, pool)

check_numeric_var(registry, df, ind, conf, pool)

check_natural_var(registry, df, ind, conf, pool)

check_overflow_var(registry, df, ind, conf, pool)

check_numeric_denominator(registry, df, ind, conf, pool)

check_natural_denominator(registry, df, ind, conf, pool)

check_zero_denominator(registry, df, ind, conf, pool)

check_duplicated_inds(registry, df, ind, conf, pool)

check_values_exists(registry, df, ind, conf, pool)

check_numeric_year(registry, df, ind, conf, pool)

check_natural_year(registry, df, ind, conf, pool)

csv_to_df(path, sep = ",", dec, encoding = "UTF-8")

sample_df(df, skip = c(""), n, random = FALSE)

indicator_is_fraction(pool, df, conf, return_ind = FALSE, ind = NULL)

filter_fraction_indicator(pool, df, conf, ind)
```

## Arguments

- registry:

  Integer registry id

- df:

  Data frame holding indicator data

- ind:

  Data frame with indicators

- pool:

  Data base pool object

- mail_msg:

  Character vector holding (part of) email body

- conf:

  List of configuration

- path:

  Character path to a file

- sep:

  Character filed sep

- dec:

  Character decimal sep

- encoding:

  Character encoding

- skip:

  character vector defining data frame variables to skip

- n:

  Numeric sample size

- random:

  Logical sample method

- return_ind:

  Logical whether indicators should be returned. FALSE by default

## Value

whatever
