## code to prepare `orgnr_shortname` dataset goes here

pool <- make_pool()
orgnr_shortname <- get_all_orgnr(pool, include_short_name = TRUE)
drain_pool(pool)

usethis::use_data(orgnr_shortname, overwrite = TRUE)
