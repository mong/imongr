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

# make the actual (renamed) table
ind <- data.frame(id = indicator$IndID,
                  title = indicator$IndTittel,
                  name = indicator$IndNavn,
                  level_green = indicator$MaalNivaaGronn,
                  level_yellow = indicator$MaalNivaaGul,
                  level_direction = indicator$MaalRetn,
                  short_description = indicator$BeskrivelseKort,
                  long_description = indicator$BeskrivelseLang,
                  registry_id = indicator$registry_id,
                  stringsAsFactors = FALSE)

# add sg_id (self reference)
ind <- cbind(ind, dg_id = NA)

usethis::use_data(ind, overwrite = TRUE)
