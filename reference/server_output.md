# Functions that provide server output

Functions that provide server output

## Usage

``` r
select_registry_ui(
  pool,
  conf,
  input_id,
  context,
  current_reg = character(),
  show_context = TRUE,
  pool0 = NULL
)

submit_ui(input_id, conf, pool, upload_file, registry, df, ind, context)

error_report_ui(pool, df, ind, upload_file, registry)

warning_report_ui(pool, df, upload_file, registry)

upload_sample_text_ui(pool, conf, upload_file, registry, indicators)

upload_sample_ui(df, upload_file, registry, sample_size, sample_type)

var_doc_ui(conf)

medfield_summary_text_ui(pool, conf, df)
```

## Arguments

- pool:

  Database pool object

- conf:

  List of configuration

- input_id:

  Character string with shiny ui id

- context:

  Character string with user selected (db) context

- current_reg:

  Character string defining previously selected registry

- show_context:

  Logical stating if context is to be shown in GUI. TRUE by default.

- pool0:

  Alternative database pool object to be used when an intersect of
  values are to be returned.

- upload_file:

  Character string with file to upload

- registry:

  Character string defining registry

- df:

  Data frame containing indicator data

- ind:

  Data frame containing indicators

- indicators:

  Character vector of indicators

- sample_size:

  Numeric sample size

- sample_type:

  Character string of sampling type

## Value

shiny ui objects to be provided by the shiny server function
