## code to prepare `data` dataset goes here

data <- data.frame()

### add registires, one by one ... ###
norgast <- read.csv2("../qmongrdata/data-raw/norgastdata.csv")

# add context, delivery_id, denominator and unit_level
norgast <- cbind(norgast, context = rep("caregiver", dim(norgast)[1]))
norgast <- cbind(norgast, delivery_id = rep(1, dim(norgast)[1]))
norgast <- cbind(norgast, denominator = rep(1, dim(norgast)[1]))
norgast <- cbind(norgast, unit_level = rep("hospital", dim(norgast)[1]))


### add a fake dg for each year and orgnr
dg <- norgast |>
  dplyr::distinct(year, orgnr) |>
  dplyr::mutate(ind_id = "norgast_dg",
                denominator = floor(runif(dplyr::n(), min = 100, max = 1000)),
                var =
                  floor(runif(dplyr::n(), min = .3, max = .99) * denominator),
                context = "caregiver",
                delivery_id = 1,
                unit_level = "hospital") |>
  dplyr::select(year, ind_id, var, denominator, orgnr, unit_level, delivery_id,
                context) |>
  dplyr::arrange(year, ind_id, orgnr)

norgast <- dplyr::bind_rows(norgast, dg)

### merge registries
data <- rbind(norgast)


usethis::use_data(data, overwrite = TRUE)
