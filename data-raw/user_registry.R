## code to prepare `user_registry` dataset goes here
## uses data from internal data sets, please create this data set as last step

# get all registries from internal Register data set
regs <- imongr::registry$Register

user_registry <- data.frame(Register = regs, user_id = rep(1, length(regs)))

usethis::use_data(user_registry, overwrite = TRUE)
