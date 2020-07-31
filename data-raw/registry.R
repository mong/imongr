## code to prepare `registry` dataset goes here

# get all registries except nra (where IndID and Register is currently swapped)
data <- qmongrdata::IndBeskr
ind <- data$IndID == "nra"
data <- data[!ind, ]
regs <- levels(factor(data$Register))

registry <- data.frame(id = seq_len(length(regs)),
                       name = regs,
                       full_name = rep("", length(regs)))

usethis::use_data(registry, overwrite = TRUE)
