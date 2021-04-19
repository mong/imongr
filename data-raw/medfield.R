## code to prepare `medfield` dataset goes here

medfield <- data.frame(
  id = seq(7),
  name = c("hjerte", "kreft", "luft", "diabetes", "nerve", "muskel", "tarm"),
  full_name = c("", "", "", "", "", "", ""),
  stringsAsFactors = FALSE
)

usethis::use_data(medfield, overwrite = TRUE)
