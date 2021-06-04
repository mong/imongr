context("get_indicator_level")

test_that("get_indicator_level() is working with realistic input", {
  data <- readRDS(file = "data/get_indicator_level_gdf.rds")
  descr <- readRDS(file = "data/get_indicator_level_ind.rds")

  testthat::expect_equal_to_reference(get_indicator_level(gdf = data, ind = descr),
                                      "data/get_indicator_level_ref.rds")
})
