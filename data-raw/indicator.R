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
                indikatorType, registry_id)


# make the actual (renamed) table
ind <- data.frame(id = indicator$IndID,
                  title = indicator$IndTittel,
                  name = indicator$IndNavn,
                  level_green = indicator$MaalNivaaGronn,
                  level_yellow = indicator$MaalNivaaGul,
                  level_direction = indicator$MaalRetn,
                  short_description = indicator$BeskrivelseKort,
                  long_description = indicator$BeskrivelseLang,
                  type = indicator$indikatorType,
                  registry_id = indicator$registry_id,
                  stringsAsFactors = FALSE)

# until present in data, add new field 'include'
ind <- cbind(ind, include = TRUE)

# add dg_id (self reference)
ind <- cbind(ind, dg_id = NA)

## until present, fake a norgast dg indicator
dg_norgast <- data.frame(id = "norgast_dg",
                         dg_id = NA,
                         title = "Dekningsgrad for NoRGast",
                         name = "Dekningsgrad for NoRGast",
                         level_green = .8,
                         level_yellow = .6,
                         level_direction = 1,
                         short_description = "Dekningsgrad for NoRGast",
                         long_description = "Dekningsgrad for NoRGast",
                         type = "dg",
                         registry_id = 10,
                         include = TRUE,
                         stringsAsFactors = FALSE)

ind$dg_id[ind$registry_id == 10] <- "norgast_dg"

# NB due to self-reference of dg indicators place them first in table
ind <- rbind(dg_norgast, ind)

usethis::use_data(ind, overwrite = TRUE)
