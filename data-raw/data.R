## code to prepare `data` dataset goes here

data <- data.frame()

### add registires, one by one ... ###
norgast <- read.csv2("../qmongrdata/data-raw/norgastdata.csv")

# add delivery_id, denominator and unit_level
norgast <- cbind(norgast, delivery_id = rep(1, dim(norgast)[1]))
norgast <- cbind(norgast, denominator = rep(1, dim(norgast)[1]))
norgast <- cbind(norgast, unit_level = rep("hospital", dim(norgast)[1]))


### merge registries
data <- rbind(norgast)

usethis::use_data(data, overwrite = TRUE)
