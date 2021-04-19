## code to prepare `registry_medfield` dataset goes here

registry_medfield <- data.frame(
  id = seq(11),
  registry_id = c(seq(7), seq(from = 7, by = -2)),
  medfield_id = c(seq(7), c(6, 4, 5, 2))
)

usethis::use_data(registry_medfield, overwrite = TRUE)
