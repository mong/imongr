conf <- get_config()
df <- imongr::data
org <- imongr::org #flattened organization table as obtained by get_flat_org()
ind <- imongr::ind

test_that("example data can be aggregated", {
  expect_true("data.frame" %in% class(agg(df, org, ind)))
})

test_that("error is provided when compulsory varaibles are missing", {
  vars <- conf$db$tab$data$insert[conf$aggregate$data_var_ind[1:3]]
  df <- df[, vars]
  expect_error(agg(df, org, ind))
})

test_that("data from higher lever org can be spread downwards in agg_data", {
  df <- rbind(df, data.frame(year = 2018,
                             orgnr = 1,
                             var = 33,
                             ind_id = "norgast_saarruptur",
                             delivery_id = 11,
                             denominator = 100,
                             unit_level = "nation"))
  expect_true("data.frame" %in% class(agg(df, org, ind)))
})

test_that("data from mid level can be spread both ways (up and down orgs)", {
  df <- rbind(df, data.frame(year = 2018,
                             orgnr = 983974899,
                             var = 11,
                             ind_id = "norgast_saarruptur",
                             delivery_id = 11,
                             denominator = 33,
                             unit_level = "nation"))
  expect_true("data.frame" %in% class(agg(df, org, ind)))
})





test_that("aggregation can handle missin level 'colour' for ind", {
  ind$level_green[1] <- NA
  expect_true("data.frame" %in% class(agg(df, org, ind)))
})

test_that("aggregation can handle missin level direction for ind", {
  ind$level_direction[1] <- NA
  expect_true("data.frame" %in% class(agg(df, org, ind)))
})
