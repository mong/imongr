## code to prepare `data` dataset goes here

data <- qmongrdata::KvalIndData
data <- cbind(data, delivery_id=rep(1, dim(data)[1]))

# currently OrgNrShus from qmongrdata contains NAs and currently do not conform
# to the database consistency constraints
ind <- is.na(data$OrgNrShus)
data <- data[!ind, ]

# substitute some strange codes (latin1?) with proper chars
data$ShNavn <- gsub("\xe6", "æ", data$ShNavn)
data$ShNavn <- gsub("\xc6", "Æ", data$ShNavn)
data$ShNavn <- gsub("\xf8", "ø", data$ShNavn)
data$ShNavn <- gsub("\xd8", "Ø", data$ShNavn)
data$ShNavn <- gsub("\xe5", "å", data$ShNavn)
data$ShNavn <- gsub("\xc5", "Å", data$ShNavn)

usethis::use_data(data, overwrite = TRUE)
