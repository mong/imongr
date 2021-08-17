conf <- get_config()
df <- imongr::data
org <- imongr::org #flattened organization table as obtained by get_flat_org()
ind <- imongr::ind

onm <- data.frame(orgnr = 999999999, unit_name = "for_the_test",
                             stringsAsFactors = FALSE)

test_that("example data can be aggregated", {
  expect_true("data.frame" %in% class(agg(df, org, ind, orgnr_name_map = onm)))
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
                             context = "caregiver",
                             delivery_id = 11,
                             denominator = 100,
                             unit_level = "nation"))
  expect_true("data.frame" %in% class(agg(df, org, ind, orgnr_name_map = onm)))
})

test_that("data from mid level can be spread both ways (up and down orgs)", {
  df <- rbind(df, data.frame(year = 2018,
                             orgnr = 983974899,
                             var = 11,
                             ind_id = "norgast_saarruptur",
                             context = "caregiver",
                             delivery_id = 11,
                             denominator = 33,
                             unit_level = "nation"))
  expect_true("data.frame" %in% class(agg(df, org, ind, orgnr_name_map = onm)))
})





test_that("aggregation can handle missin level 'colour' for ind", {
  ind$level_green[1] <- NA
  expect_true("data.frame" %in% class(agg(df, org, ind, orgnr_name_map = onm)))
})

test_that("aggregation can handle missin level direction for ind", {
  ind$level_direction[1] <- NA
  expect_true("data.frame" %in% class(agg(df, org, ind, orgnr_name_map = onm)))
})

test_that("child (data record) inherits dg within context", {
  df_cargiver_parent <- data.frame(
    year = 2014,
    orgnr = 874716782,
    var = 1,
    ind_id = "norgast_dg",
    context = "caregiver",
    delivery_id = 1,
    denominator = 2,
    unit_level = "hospital"
  )
  df_resident_parent <- data.frame(
    year = 2014,
    orgnr = 874716782,
    var = 1,
    ind_id = "norgast_dg",
    context = "resident",
    delivery_id = 1,
    denominator = 4,
    unit_level = "hospital"
  )
  df_child <- data.frame(
    year = 2018,
    orgnr = 874716782,
    var = 1,
    ind_id = "norgast_saarruptur",
    context = "resident",
    delivery_id = 1,
    denominator = 1,
    unit_level = "hospital"
  )
  test_data <- agg(rbind(df_cargiver_parent, df_child), org, ind,
                   orgnr_name_map = onm)
  test_child <- test_data[test_data$year == 2018, ]
  expect_true(all(is.na(test_child$dg)))
  test_data <- agg(rbind(df_resident_parent, df_child), org, ind,
                   orgnr_name_map = onm)
  test_child <- test_data[test_data$year == 2018, ]
  expect_true(all(test_child$dg == .25))

})
