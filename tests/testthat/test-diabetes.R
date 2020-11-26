# Test created because of issue #75
# https://github.com/mong/imongr/issues/75
#
# Will fail until this bug has been fixed

test_that("Has issue 75 been fixed", {
#  diabetes <- csv_to_df("data/diabetes_voksne.csv", sep = ";", dec = ",")

#  testthat::expect_equal_to_reference(diabetes, "data/diabetes.rda")

  diabetes <- readRDS(file = "data/diabetes.rds")
  ind_descr <- readRDS(file = "data/ind.rds")
  Encoding(ind_descr$title) <- "UTF-8"
  Encoding(ind_descr$short_description) <- "UTF-8"
  Encoding(ind_descr$long_description) <- "UTF-8"
  org_structure <- readRDS(file = "data/org.rds")
  Encoding(org_structure$hospital) <- "UTF-8"
  Encoding(org_structure$hf) <- "UTF-8"
  Encoding(org_structure$rhf) <- "UTF-8"
  all_orgnr <- readRDS(file = "data/all_orgnr.rds")

  diabetes <- dplyr::left_join(diabetes, all_orgnr, by = "orgnr")
  testthat::expect_equal_to_reference(diabetes, "data/diabetes.rda")
  diabetes_agg <- agg(diabetes, org_structure, ind_descr)
  print(diabetes_agg)
  testthat::expect_equal_to_reference(diabetes_agg, "data/diabetes_agg.rda")

})
