## code to prepare `indicator` dataset goes here

indicator <- qmongrdata::IndBeskr

# ditch nra because Register and IndID seems to be swapped in indBeskr
ind <- indicator$IndID == "nra"
indicator <- indicator[!ind, ]

usethis::use_data(indicator, overwrite = TRUE)
