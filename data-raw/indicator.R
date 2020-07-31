## code to prepare `indicator` dataset goes here

indicator <- qmongrdata::IndBeskr

# ditch nra because Register and IndID seems to be swapped in indBeskr
ind <- indicator$IndID == "nra"
indicator <- indicator[!ind, ]

# add registry ids
indicator$registry_id <-
  dplyr::left_join(indicator, imongr::registry, by = c("Register" = "name"))$id

# select whatever vars to bring
indicator <- indicator %>%
  dplyr::select(., IndID, Register, IndTittel, IndNavn, MaalNivaaGronn,
                MaalNivaaGul, MaalRetn, BeskrivelseKort, BeskrivelseLang,
                registry_id)


usethis::use_data(indicator, overwrite = TRUE)
