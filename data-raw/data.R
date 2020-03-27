## code to prepare `data` dataset goes here

data <- qmongrdata::KvalIndData
data <- cbind(data, delivery_id=rep(1, dim(data)[1]))

# currently OrgNrShus from qmongrdata contains NAs and currently do not conform
# to the database consistency constraints
ind <- is.na(data$OrgNrShus)
data <- data[!ind, ]

usethis::use_data(data, overwrite = TRUE)
