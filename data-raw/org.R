## code to prepare `org` dataset goes here

# do need a db populated with correspponding test data. If not, following
# test will fail

pool <- make_pool()
org <- get_flat_org(pool)
drain_pool(pool)

usethis::use_data(org, overwrite = TRUE)
