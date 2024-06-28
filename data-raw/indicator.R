## code to prepare `indicator` data set goes here

indicator <- qmongrdata::IndBeskr

# add registry ids
indicator$registry_id <-
  dplyr::left_join(
    indicator, imongr::registry, by = c("registry_name" = "name")
  )$id.y

# select whatever vars to bring
indicator <- indicator |>
  dplyr::select(id, name, title, name, level_green, level_yellow,
                level_direction, short_description, long_description,
                type, registry_id)


# make the actual (renamed) table
ind <- data.frame(id = indicator$id,
                  title = indicator$title,
                  name = indicator$name,
                  level_green = indicator$level_green,
                  level_yellow = indicator$level_yellow,
                  level_direction = indicator$level_direction,
                  short_description = indicator$short_description,
                  long_description = indicator$long_description,
                  type = indicator$type,
                  registry_id = indicator$registry_id,
                  stringsAsFactors = FALSE)

# until present in data, add new field 'include' and 'sformat'
ind <- cbind(ind, include = TRUE)
ind <- cbind(ind, sformat = ",.0%")

# stunt for min_denominator
ind <- cbind(ind, min_denominator = NA)

# add dg_id (self reference)
ind <- cbind(ind, dg_id = NA)

# add dummy indicator for norgast that is not a fraction type
dummy_norgast <- data.frame(id = "norgast_dummy",
                            dg_id = "norgast_db",
                            title = "Dummy for NoRGast",
                            name = "Dummy for NoRGast",
                            level_green = .8,
                            level_yellow = .6,
                            level_direction = 1,
                            short_description = "Dummy for NoRGast",
                            long_description = "Dummy for NoRGast",
                            type = "verdi",
                            sformat = ",.0%",
                            registry_id = 10,
                            include = TRUE,
                            min_denominator = NA,
                            stringsAsFactors = FALSE)
ind <- rbind(dummy_norgast, ind)

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
                         sformat = ",.0%",
                         registry_id = 10,
                         include = TRUE,
                         min_denominator = NA,
                         stringsAsFactors = FALSE)

ind$dg_id[ind$registry_id == 10] <- "norgast_dg"

# NB due to self-reference of dg indicators place them first in table
ind <- rbind(dg_norgast, ind)


usethis::use_data(ind, overwrite = TRUE)
