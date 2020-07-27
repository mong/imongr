conf <- get_config()
df <- imongr::data
org <- imongr::org
indicator <- imongr::indicator

test_that("example data can be aggregated", {
  expect_equal(class(agg(df, org, indicator)), "data.frame")
})

test_that("error is provided when compulsory varaibles are missing", {
  vars <- conf$db$tab$data$insert[conf$aggregate$data_var_ind[1:3]]
  df <- df[, vars]
  expect_error(agg(df, org, indicator))
})