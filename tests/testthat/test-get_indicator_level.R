context("get_indicator_level")

test_that("get_indicator_level() is working with realistic input", {
  data <- readRDS(file = "data/get_indicator_level_gdf.rds")
  ref_data <- data
  ref_data$level <- ""
  ref_data$level_direction <- NA

  testthat::expect_equal(get_indicator_level(gdf = data, ind = data.frame()),
                         ref_data)
})
