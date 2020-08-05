## code to prepare `nation` dataset goes here

dat <- readr::read_delim("data-raw/hospital_structure.csv",
                         ";", escape_double = FALSE, trim_ws = TRUE)

# pick levels
dat_nation <- dat[dat$Nivaa == 0, ]
dat_rhf <- dat[dat$Nivaa == 1, ]
dat_hf <- dat[dat$Nivaa == 2, ]
dat_hospital <- dat[dat$Nivaa == 3, ]

# # add ids, increment by 1
# dat_nation$id <- seq_len(dim(dat_nation)[1])
# dat_rhf$id <- seq_len(dim(dat_rhf)[1])
# dat_hf$id <- seq_len(dim(dat_hf)[1])
# dat_hospital$id <- seq_len(dim(dat_hospital)[1])
#
# add parent id (orgnr as foreign key)
dat_rhf$nation_orgnr <-
  dplyr::left_join(dat_rhf, dat_nation, by = c("NivaaOpp" = "OrgNr"),
                   keep = TRUE)$OrgNr.y
dat_hf$rhf_orgnr <-
  dplyr::left_join(dat_hf, dat_rhf, by = c("NivaaOpp" = "OrgNr"),
                   keep = TRUE)$OrgNr.y
dat_hospital$hf_orgnr <-
  dplyr::left_join(dat_hospital, dat_hf, by = c("NivaaOpp" = "OrgNr"),
                   keep = TRUE)$OrgNr.y


nation <- data.frame(orgnr = dat_nation$OrgNr,
                     full_name = dat_nation$OrgNavnEnhetsreg,
                     short_name = dat_nation$Kortnavn)

rhf <- data.frame(orgnr = dat_rhf$OrgNr,
                  full_name = dat_rhf$OrgNavnEnhetsreg,
                  short_name = dat_rhf$Kortnavn,
                  nation_orgnr = dat_rhf$nation_orgnr)

hf <- data.frame(orgnr = dat_hf$OrgNr,
                 full_name = dat_hf$OrgNavnEnhetsreg,
                 short_name = dat_hf$Kortnavn,
                 rhf_orgnr = dat_hf$rhf_orgnr)

hospital <- data.frame(orgnr = dat_hospital$OrgNr,
                       full_name = dat_hospital$OrgNavnEnhetsreg,
                       short_name = dat_hospital$Kortnavn,
                       hf_orgnr = dat_hospital$hf_orgnr)


usethis::use_data(nation, overwrite = TRUE)
usethis::use_data(rhf, overwrite = TRUE)
usethis::use_data(hf, overwrite = TRUE)
usethis::use_data(hospital, overwrite = TRUE)
