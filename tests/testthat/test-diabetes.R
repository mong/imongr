# Test created because of issue #75
# https://github.com/mong/imongr/issues/75
#
# Will fail until this bug has been fixed

test_that("Test csv_to_df() with file", {
  diabetes <- csv_to_df("data/diabetes_voksne.csv", sep = ";", dec = ",")
  testthat::expect_equal_to_reference(diabetes, "data/diabetes_csv.rda")
})

test_that("Test agg() with files", {
  diabetes <- readRDS(file = "data/diabetes.rds")
  ind_descr <- readRDS(file = "data/ind.rds")
  org_structure <- readRDS(file = "data/org.rds")
  all_orgnr <- readRDS(file = "data/all_orgnr.rds")
  diabetes <- dplyr::left_join(diabetes, all_orgnr, by = "orgnr")
  testthat::expect_equal_to_reference(diabetes, "data/diabetes.rda")
  diabetes_agg <- agg(diabetes, org_structure, ind_descr) %>%
    dplyr::arrange(orgnr, var)
  testthat::expect_equal_to_reference(diabetes_agg, "data/diabetes_agg.rda")

})
