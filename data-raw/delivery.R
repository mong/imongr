## code to prepare `delivery` dataset goes here

delivery <- data.frame(latest = 1,
                       latest_update = "2022-03-02",
                       latest_affirm = "2022-03-01",
                       md5_checksum = "06195aa345d5f8889bc7de7dfef92e30",
                       terms_version = "imongr v0.0.1.9000",
                       user_id = 1)

usethis::use_data(delivery, overwrite = TRUE)
