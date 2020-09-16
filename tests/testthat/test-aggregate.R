conf <- get_config()
df <- imongr::data
org <- imongr::org #flattened organization table as obtained by get_flat_org()
ind <- imongr::ind
orgnr_shortname <- imongr::orgnr_shortname

test_that("example data can be aggregated", {
  expect_equal(class(agg(df, org, ind, orgnr_shortname)), "data.frame")
})

test_that("error is provided when compulsory varaibles are missing", {
  vars <- conf$db$tab$data$insert[conf$aggregate$data_var_ind[1:3]]
  df <- df[, vars]
  expect_error(agg(df, org, ind))
})
