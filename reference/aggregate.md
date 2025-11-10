# Aggregate hospital data

Aggregation of indicators as provided for each unit (hospital) at
organization levels above hospital: health trust, regional health trust
and national.

## Usage

``` r
agg(df, org, ind, ind_noagg = character(), orgnr_name_map)

agg_dg(aggs, ind)

agg_from_level(df, org, ind, conf, from_level)
```

## Arguments

- df:

  Data frame

- org:

  Data frame holding the flat org table with all levels needed. May be
  obtained by `get_flat_org(pool)`

- ind:

  Data frame holding the indicator db table providing all data on each
  indicator

- ind_noagg:

  Character vector identifying indicator (ids) that are not to be
  aggregated (used as is). Default is
  [`character()`](https://rdrr.io/r/base/character.html)

- orgnr_name_map:

  Data frame with global mapping of organization id and name. Applied on
  data to be used as is (no aggregation).

- aggs:

  Data frame of (pre) aggregated data

- conf:

  List of configuration

- from_level:

  Integer specifying from what level to aggregate from

- diff:

  Data frame with diff data

## Value

Data frame in raw, grouped or aggregated form

## Details

All functions are adapted from qmongr/qmongrdata. However, the
nomenclature of function arguments are changed somewhat. Main source of
underlying data is the qmongr database and where tables are used in
these function their names are kept as is (*org* and *ind*). Other data
frames passed into or between function is denoted *df*. The aggregate
consists of the following variables:

- year:

  The year of the current record

- ind_id:

  Indicator ID

- orgnr:

  Organization ID

- context:

  How orgnr is to be understood, *e.g.* caregiver, or patient residency

- count:

  Number of observations of the current record

- var:

  Summarised indicator value, for instance mean or median

- unit_level:

  Code providing level of aggregation such as 'hospital', 'hf' (health
  trust), 'rhf' (regional health trust) and 'national'

- unit_name:

  Name of the organization unit
