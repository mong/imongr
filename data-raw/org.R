## code to prepare `org` dataset goes here

org <- qmongrdata::SykehusNavnStruktur

# stranger_strings <- function(data, vars) {
#
#   subs <- function(vec) {
#     vec <- gsub("\xe6", "æ", vec)
#     vec <- gsub("\xc6", "Æ", vec)
#     vec <- gsub("\xf8", "ø", vec)
#     vec <- gsub("\xd8", "Ø", vec)
#     vec <- gsub("\xe5", "å", vec)
#     vec <- gsub("\xc5", "Å", vec)
#   }
#
#   for (i in seq_len(length(vars))) {
#     data[vars[i]] <- subs(data[vars[i]])
#   }
#   data
# }
#
# vars <- c("RHF", "HF", "Hfkortnavn", "OrgNavnEnhetsreg", "SykehusnavnLang",
#           "SykehusNavn")
# org <- stranger_strings(org, vars)

# make OrgNrs integers
org$OrgNrRHF <- as.integer(org$OrgNrRHF)
org$OrgNrHF <- as.integer(org$OrgNrHF)
org$OrgNrShus <- as.integer(org$OrgNrShus)

usethis::use_data(org, overwrite = TRUE)
