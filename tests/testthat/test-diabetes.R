# Test created because of issue #75
# https://github.com/mong/imongr/issues/75
#
# Will fail until this bug has been fixed

test_that("Has issue 75 been fixed", {
  diabetes <- csv_to_df("data/diabetes_voksne.csv", sep = ";", dec = ",")

  testthat::expect_equal_to_reference(diabetes, "data/diabetes.rda")

  ind_descr <- readRDS(file = "data/ind.rds")
  org_structure <- readRDS(file = "data/org.rds")
  all_orgnr <- readRDS(file = "data/all_orgnr.rds")

  diabetes <- dplyr::left_join(diabetes, all_orgnr, by = "orgnr")
  testthat::expect_equal_to_reference(agg(diabetes, org_structure, ind_descr), "data/diabetes_agg.rda")

})
