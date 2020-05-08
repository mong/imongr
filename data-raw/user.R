## code to prepare `user` dataset goes here

user <- data.frame(user_name="mongr", name="Mongr No", phone="+4747474747",
                   email="jesus@sky.com", valid=1)

usethis::use_data(user, overwrite = TRUE)
