## code to prepare `registry` dataset goes here

# get all registries except nra (where IndID and Register is currently swapped)
data <- qmongrdata::IndBeskr
ind <- data$IndID == "nra"
data <- data[!ind, ]
regs <- levels(factor(data$Register))


registry <- data.frame(id = seq_along(regs),
                       name = regs)

# add full name
df <- read.csv2("../qmongrdata/data-raw/IndBeskr.csv")
df <- df |> dplyr::select(Register, FulltRegisterNavn)
df <- df |> dplyr::rename(full_name = FulltRegisterNavn)

registry <- dplyr::left_join(registry, unique(df), by = c("name" = "Register"))


usethis::use_data(registry, overwrite = TRUE)
