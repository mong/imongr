# Tools and whatever

Tools and whatever

Function for removing empty rows in csv input. In a row ";;;;;" the
values will translate to NA or an empty string depending on whether the
columns are numeric or strings.

## Usage

``` r
natural(vals, tolerance = .Machine$double.eps^0.5)

md5_checksum(df, ind = "")

remove_empty_rows(df)

user_widget()

version_info(newline = "<br>")

no_opt_out_ok()

insert_sample_data(include_data_table = TRUE)

delete_all_data(prompt = TRUE)

invalidate_cache()
```

## Arguments

- vals:

  A numeric vector

- tolerance:

  A (small) positive floating point number used to evaluate if a numeric
  can be regarded as a whole number. Default depends on the running
  environment and set to `.Machine$double.eps^0.5`

- df:

  A data frame

- ind:

  A data frame holding indicator data.

- newline:

  String element defining line break for formatting. Default is `<br>`

- include_data_table:

  Logical defining if the data table is to be populated by data too. By
  default TRUE

- prompt:

  Logical to prompt for user input. Default is TRUE

## Value

Invisible
