## code to prepare `nation` dataset goes here

dat <- readr::read_delim("data-raw/hospital_structure.csv",
                         ";", escape_double = FALSE, trim_ws = TRUE)

# pick levels
dat_nation <- dat[dat$Nivaa == 0, ]
dat_rhf <- dat[dat$Nivaa == 1, ]
dat_hf <- dat[dat$Nivaa == 2, ]
dat_shus <- dat[dat$Nivaa == 3, ]

# add ids, increment by 1
dat_nation$id <- seq_len(dim(dat_nation)[1])
dat_rhf$id <- seq_len(dim(dat_rhf)[1])
dat_hf$id <- seq_len(dim(dat_hf)[1])
dat_shus$id <- seq_len(dim(dat_shus)[1])

# add parent ids (foreign keys)
dat_rhf$nation_id <-
  dplyr::left_join(dat_rhf, dat_nation, by = c("NivaaOpp" = "OrgNr"))$id.y
dat_hf$rhf_id <-
  dplyr::left_join(dat_hf, dat_rhf, by = c("NivaaOpp" = "OrgNr"))$id.y
dat_shus$hf_id <-
  dplyr::left_join(dat_shus, dat_hf, by = c("NivaaOpp" = "OrgNr"))$id.y


nation <- data.frame(id = dat_nation$id,
                     orgnr = dat_nation$OrgNr,
                     full_name = dat_nation$OrgNavnEnhetsreg,
                     short_name = dat_nation$Kortnavn)

rhf <- data.frame(id = dat_rhf$id,
                  orgnr = dat_rhf$OrgNr,
                  full_name = dat_rhf$OrgNavnEnhetsreg,
                  short_name = dat_rhf$Kortnavn,
                  nation_id = dat_rhf$nation_id)

hf <- data.frame(id = dat_hf$id,
                 orgnr = dat_hf$OrgNr,
                 full_name = dat_hf$OrgNavnEnhetsreg,
                 short_name = dat_hf$Kortnavn,
                 rhf_id = dat_hf$rhf_id)

shus <- data.frame(id = dat_shus$id,
                  orgnr = dat_shus$OrgNr,
                  full_name = dat_shus$OrgNavnEnhetsreg,
                  short_name = dat_shus$Kortnavn,
                  hf_id = dat_shus$hf_id)


usethis::use_data(nation, overwrite = TRUE)
usethis::use_data(rhf, overwrite = TRUE)
usethis::use_data(hf, overwrite = TRUE)
usethis::use_data(shus, overwrite = TRUE)
