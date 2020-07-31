## code to prepare `user_registry` dataset goes here
## uses data from internal data sets, please create this data set as last step

# get all registry ids from internal registry data set
registry_id <- imongr::registry$id

user_registry <- data.frame(registry_id = registry_id,
                            user_id = rep(1, length(regs)))

usethis::use_data(user_registry, overwrite = TRUE)
