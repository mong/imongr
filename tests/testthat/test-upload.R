pool <- list()
conf <- get_config()

l <- check_missing_var(data.frame(), conf, pool)
test_that("all vars are missing om empty data frame", {
  expect_equal(l$report, conf$db$tab$data$insert[conf$upload$data_var_ind])
})
test_that("check reports failed status", {
  expect_true(l$fail)
})

l <- check_invalid_var(data.frame(offBounce = 0), conf, pool)
test_that("invalid var is reported and that check status is failed", {
  expect_equal(l$report, "offBounce")
  expect_true(l$fail)
})

df <- imongr::data[conf$upload$data_var_ind]
l <- check_missing_var(df, conf, pool)
test_that("an empty report and ok status is provided from a valid data set", {
  expect_false(l$fail)
  expect_equal(l$report, character())
})

# next up will call for a test-db
