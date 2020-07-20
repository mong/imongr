## code to prepare `data` dataset goes here

data <- qmongrdata::KvalIndData

# fake a delivery
data <- cbind(data, delivery_id=rep(1, dim(data)[1]))

# ditch nra because Register and IndID seems to be swapped in indBeskr
ind <- data$KvalIndID == "nra"
data <- data[!ind, ]

# add registry field that is (currently) not part of data set from qmongrdata
ind_reg <- data.frame(ind = qmongrdata::IndBeskr$IndID,
                      reg = qmongrdata::IndBeskr$Register,
                      stringsAsFactors = FALSE)
reg <- rep(NA, dim(data)[1])
for (i in seq_len(dim(ind_reg)[1])) {
  ind <- data$KvalIndID == ind_reg$ind[i]
  reg[ind] <- ind_reg$reg[i]
}
data <- cbind(data, Register = reg, stringsAsFactors = FALSE)

# add nevner field that is (currently) not part of data set from qmongrdata
data <- cbind(data[, c(1:5)], nevner = NA, data[, 6:8])

# currently OrgNrShus from qmongrdata contains NAs and currently do not conform
# to the database consistency constraints
ind <- is.na(data$OrgNrShus)
data <- data[!ind, ]

# currently, OrgNrShus in data is not consistent with SykehusNavnStruktur
# thus, remove all "unknown" OrgNrShus entries from data
org <- qmongrdata::SykehusNavnStruktur$OrgNr
org_data <- data$OrgNrShus
ind <- is.element(org_data, org)
data <- data[ind, ]

# curently, OrgNrShus is of class character in qmongrdata while integers in db
# thus, convert it
data$OrgNrShus <- as.integer(data$OrgNrShus)

# substitute some strange codes (latin1?) with proper chars
data$ShNavn <- gsub("\xe6", "æ", data$ShNavn)
data$ShNavn <- gsub("\xc6", "Æ", data$ShNavn)
data$ShNavn <- gsub("\xf8", "ø", data$ShNavn)
data$ShNavn <- gsub("\xd8", "Ø", data$ShNavn)
data$ShNavn <- gsub("\xe5", "å", data$ShNavn)
data$ShNavn <- gsub("\xc5", "Å", data$ShNavn)

# then, change to ascii substitutes
data$ShNavn <- gsub("æ", "ae", data$ShNavn)
data$ShNavn <- gsub("Æ", "Ae", data$ShNavn)
data$ShNavn <- gsub("ø", "oe", data$ShNavn)
data$ShNavn <- gsub("Ø", "Oe", data$ShNavn)
data$ShNavn <- gsub("å", "aa", data$ShNavn)
data$ShNavn <- gsub("Å", "Aa", data$ShNavn)


usethis::use_data(data, overwrite = TRUE)
