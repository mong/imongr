context("csv_to_df and agg")

test_that("csv_to_df() read file as expected", {
  diabetes <- csv_to_df("data/diabetes_voksne.csv", sep = ";", dec = ",")
  testthat::expect_equal_to_reference(diabetes, "data/diabetes_csv.rds")
})

onm <- data.frame(orgnr = 999999999, unit_name = "for_the_test",
                  stringsAsFactors = FALSE)

test_that("agg() is working with realistic input", {
  diabetes <- readRDS(file = "data/diabetes.rds")
  ind_descr <- readRDS(file = "data/ind.rds")
  org_structure <- readRDS(file = "data/org.rds")
  all_orgnr <- readRDS(file = "data/all_orgnr.rds")
  diabetes <- dplyr::left_join(diabetes, all_orgnr, by = "orgnr")
  testthat::expect_equal_to_reference(diabetes, "data/diabetes_ref.rds")
  diabetes_agg <- agg(diabetes, org_structure, ind_descr,
                      orgnr_name_map = onm) %>%
    dplyr::arrange(orgnr, var) # arrange to get the same result on all OS
  testthat::expect_equal_to_reference(diabetes_agg, "data/diabetes_agg.rds")

})
