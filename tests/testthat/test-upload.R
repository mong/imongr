pool <- list()
create_config()
conf <- get_config()
registry <- 10 # may be norgast

l <- check_missing_var(registry, data.frame(), data.frame(), conf, pool)
test_that("all vars are missing om empty data frame", {
  expect_equal(l$report, conf$upload$file$vars)
})
test_that("check reports failed status", {
  expect_true(l$fail)
})

l <- check_invalid_var(
  registry, data.frame(offBounce = 0), data.frame(), conf, pool
)
test_that("invalid var is reported and that check status is failed", {
  expect_equal(l$report, "offBounce")
  expect_true(l$fail)
})

df <- imongr::data[, names(imongr::data) %in% conf$upload$file$vars]
df <- cbind(df, orgnr = rep(1, dim(df)[1]))
l <- check_missing_var(registry, df, data.frame(), conf, pool)
test_that("an empty report and ok status is provided from a valid data set", {
  expect_false(l$fail)
  expect_equal(l$report, character())
})


# preserve initial state
env_context <- Sys.getenv("IMONGR_CONTEXT")
env_user <- Sys.getenv("IMONGR_DB_USER")
env_pass <- Sys.getenv("IMONGR_DB_PASS")
env_name <- Sys.getenv("IMONGR_DB_NAME")
env_host <- Sys.getenv("IMONGR_DB_HOST")
env_user_name <- Sys.getenv("SHINYPROXY_USERNAME")
env_user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")

# env that need to be set for below tests
Sys.setenv(SHINYPROXY_USERNAME = "mongr")
Sys.setenv(SHINYPROXY_USERGROUPS = "PROVIDER")

# Database infrastructure is only guaranteed at Github Actions and
# our own dev env. Tests running on other environments should be skipped.
check_db <- function(is_test_that = TRUE) {
  if (Sys.getenv("IMONGR_CONTEXT") == "DEV") {
    NULL
  } else if (Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") == "true") {
    NULL
  } else {
    if (is_test_that) {
      testthat::skip("Possible lack of database infrastructure")
    } else {
      1
    }
  }
}

# make a sample df to be used for the remaining tests
df <- data.frame(
  context = "caregiver",
  year = 2018,
  orgnr = 974633574,
  ind_id = "norgast_andel_avdoede_bykspytt_tolv",
  var = 0,
  denominator = 1
)

test_that("valid vars pass the check", {
  expect_true(
    length(
      check_invalid_var(registry, df, data.frame(), conf, pool)$report
    ) == 0
  )
})

test_that("an upload csv can be converted to a data frame", {
  csv_file <- tempfile(fileext = ".csv")
  write.csv(df, file = csv_file)
  expect_error(csv_to_df(tempfile(), dec = "."))
  expect_equal(class(csv_to_df(csv_file, dec = ".")), "data.frame")
  # check when varaiable 'nevner' is not present
  write.csv(df[, !names(df) %in% "nevner"], file = csv_file)
  expect_equal(class(csv_to_df(csv_file, dec = ".")), "data.frame")
  file.remove(csv_file)
})

test_that("encoding magic can be performed on csv file", {
  df_nordic <- rbind(
    df[, !names(df) %in% "nevner"],
    data.frame(
      context = "caregiver",
      year = 2018,
      orgnr = 974633574,
      ind_id = "æøåÆØÅ",
      var = 0,
      denominator = 1
    )
  )
  csv_file <- tempfile(fileext = ".csv")
  write.csv(df_nordic, file = csv_file, fileEncoding = "LATIN1")
  expect_warning(csv_to_df(csv_file, dec = "."))
  file.remove(csv_file)
})

test_that("a (sub) sample df can be provided", {
  expect_equal(class(sample_df(df, skip = c("nevner"), n = 2)), "data.frame")
  expect_equal(class(sample_df(df, n = NA)), "data.frame")
  expect_equal(class(sample_df(df, n = 1, random = TRUE)), "data.frame")
})

# For the remaining tests we need a test database
## first off with no data
if (is.null(check_db(is_test_that = FALSE))) {
  pool <- make_pool()
  query <- paste(
    "CREATE DATABASE IF NOT EXISTS testdb CHARACTER SET = 'utf8'",
    "COLLATE = 'utf8_danish_ci';"
  )
  pool::dbExecute(pool, query)

  # new connections to testdb
  drain_pool(pool)
  Sys.setenv(IMONGR_DB_NAME = "testdb")
  pool <- make_pool()

  # add tabs to testdb
  fc <- file(system.file("2_create_tabs.sql", package = "imongr"), "r")
  t <- readLines(fc)
  close(fc)
  sql <- paste0(t, collapse = "\n")
  queries <- strsplit(sql, ";")[[1]]
  for (i in seq_len(length(queries))) {
    pool::dbExecute(pool, queries[i])
  }

  insert_sample_data()
}

# test that depend on db
conf <- get_config()

df_mix <- rbind(
  df,
  data.frame(
    context = "caregiver",
    year = 2018,
    orgnr = 974633574,
    ind_id = "norgast_dummy",
    var = 0,
    denominator = 1
  )
)

# get indicators for norgast (id = 10), but only if we have a db
if (is.null(check_db(is_test_that = FALSE))) {
  ind <- get_registry_ind(pool, 10)
}

test_that("mixed indicator type check is working", {
  check_db()
  expect_true(check_mixing_ind(registry, df_mix, ind, conf, pool)$fail)
  expect_false(check_mixing_ind(registry, df, ind, conf, pool)$fail)
})

test_that("(valid) context check is working", {
  check_db()
  expect_false(check_invalid_context(registry, df, ind, conf, pool)$fail)
  expect_equal(
    check_invalid_context(
      registry, df[, !names(df) %in% "context"], ind, conf, pool
    )$report,
    conf$upload$check_empty
  )
  # set an invalid context
  df$context <- "careprovider"
  expect_true(check_invalid_context(registry, df, ind, conf, pool)$fail)
  expect_true(
    length(check_invalid_context(registry, df, ind, conf, pool)$report) > 0
  )
  df$context <- "caregiver"
})

test_that("org id checks is working", {
  check_db()
  expect_false(check_invalid_org(registry, df, ind, conf, pool)$fail)
  expect_equal(check_invalid_org(
    registry, df[, !names(df) %in% "orgnr"], ind,
    conf, pool
  )$report, conf$upload$check_empty)
  # set an org id that (assumable) does not exist
  df$orgnr <- -1
  expect_true(check_invalid_org(registry, df, ind, conf, pool)$fail)
  expect_true(
    length(check_invalid_org(registry, df, ind, conf, pool)$report) > 0
  )
  df$orgnr <- 974633574
})

test_that("indicator id check is working", {
  check_db()
  expect_false(check_invalid_ind(registry, df, ind, conf, pool)$fail)
  expect_equal(
    check_invalid_ind(
      registry, df[, !names(df) %in% "ind_id"], ind, conf, pool
    )$report, conf$upload$check_empty
  )
  # set an indicator id that does not exist
  df$ind_id <- "nothing"
  expect_true(
    length(check_invalid_ind(registry, df, ind, conf, pool)$report) > 0
  )
  df$ind_id <- "norgast_andel_avdoede_bykspytt_tolv"
})

test_that("numeric check on var is working", {
  check_db()
  expect_false(check_numeric_var(registry, df, ind, conf, pool)$fail)
  expect_equal(
    check_numeric_var(
      registry, df[, !names(df) %in% "var"], ind, conf, pool
    )$report, conf$upload$check_empty
  )
  df$var <- as.character(df$var)
  expect_true(check_numeric_var(registry, df, ind, conf, pool)$fail)
  df$var <- as.numeric(df$var)
})

test_that("numeric check on denominator is working", {
  check_db()
  expect_false(check_numeric_denominator(registry, df, ind, conf, pool)$fail)
  expect_equal(
    check_numeric_denominator(
      registry, df[, !names(df) %in% "denominator"], ind, conf, pool
    )$report, conf$upload$check_empty
  )
  df$denominator <- as.character(df$denominator)
  expect_true(check_numeric_denominator(registry, df, ind, conf, pool)$fail)
  df$denominator <- as.numeric(df$denominator)
})

test_that("natural number check on var is working", {
  check_db()
  expect_false(check_natural_var(registry, df, ind, conf, pool)$fail)
  df$var <- df$var + .1
  expect_true(check_natural_var(registry, df, ind, conf, pool)$fail)
  df$var <- df$var - 1000000.1
  expect_true(check_natural_var(registry, df, ind, conf, pool)$fail)
  df$var <- df$var + 1000000
  expect_equal(
    check_natural_var(
      registry, df[, !names(df) %in% "var"], ind, conf, pool
    )$report, conf$upload$check_impossible
  )
  # check dismissed when containing none-fraction indicator data
  expect_false(check_natural_var(registry, df_mix, ind, conf, pool)$fail)
  # checks ok when no data left after filtering
  expect_false(check_natural_var(
    registry,
    data.frame(
      context = "caregiver",
      year = 2018,
      orgnr = 974633574,
      ind_id = "norgast_dummy",
      var = 0,
      denominator = 1
    ),
    ind,
    conf,
    pool
  )$fail)
})

test_that("check on var <= denominator is working", {
  check_db()
  expect_false(check_overflow_var(registry, df, ind, conf, pool)$fail)
  df$var <- 2
  df$denominator <- 1
  expect_true(check_overflow_var(registry, df, ind, conf, pool)$fail)
  expect_equal(
    check_overflow_var(
      registry, df[, !names(df) %in% "var"], ind, conf, pool
    )$report, conf$upload$check_impossible
  )
  # filters when data contains none-fraction indicators
  expect_false(check_overflow_var(registry, df_mix, ind, conf, pool)$fail)
  # checks ok when no data left after filtering
  expect_false(check_overflow_var(
    registry,
    data.frame(
      context = "caregiver",
      year = 2018,
      orgnr = 974633574,
      ind_id = "norgast_dummy",
      var = 0,
      denominator = 1
    ),
    ind,
    conf,
    pool
  )$fail)
})

test_that("natural number check on denominator is working", {
  check_db()
  expect_false(check_natural_denominator(registry, df, ind, conf, pool)$fail)
  df$denominator <- df$denominator + .1
  expect_true(check_natural_denominator(registry, df, ind, conf, pool)$fail)
  df$denominator <- df$denominator - 1000000.1
  expect_true(check_natural_denominator(registry, df, ind, conf, pool)$fail)
  df$denominator <- df$denominator + 1000000
  expect_equal(
    check_natural_denominator(
      registry, df[, !names(df) %in% "denominator"], ind, conf, pool
    )$report, conf$upload$check_impossible
  )
})

test_that("zero check on denominator is working", {
  check_db()
  expect_false(check_zero_denominator(registry, df, ind, conf, pool)$fail)
  orig_denominator <- df$denominator
  df$denominator <- 0
  expect_true(check_zero_denominator(registry, df, ind, conf, pool)$fail)
  df$denominator <- orig_denominator
  expect_equal(
    check_zero_denominator(
      registry, df[, !names(df) %in% "denominator"], ind, conf, pool
    )$report, conf$upload$check_impossible
  )
})


test_that("check report (wrapper) function is working", {
  check_db()
  expect_equal(class(check_report(registry, df, ind, pool)), "character")
  expect_equal(
    class(
      check_report(registry, df[, !names(df) %in% "orgnr"], ind, pool)
    ),
    "character"
  )
})

test_that("reports are provided even if registry is not defined", {
  check_db()
  expect_equal(class(check_report("", df, ind, pool)), "character")
})

test_that("pro forma check on missing registry returns a list", {
  check_db()
  expect_equal(class(check_missing_registry("", df, ind, conf, pool)), "list")
})

test_that("true fractions can be detected", {
  check_db()
  expect_equal(class(indicator_is_fraction(pool, df, conf)), "logical")
  expect_equal(
    class(
      indicator_is_fraction(pool, df, conf, return_ind = TRUE)
    ),
    "data.frame"
  )
})

test_that("true fractions can be filtered from mixing data", {
  check_db()
  expect_identical(
    filter_fraction_indicator(pool, df_mix, conf, ind),
    df
  )
})

# clean up
## drop tables (in case tests are re-run on the same instance)
if (is.null(check_db(is_test_that = FALSE))) {
  pool::dbExecute(pool, "ALTER TABLE `delivery` DROP FOREIGN KEY `fk_delivery_publish`;")
  pool::dbExecute(
    pool,
    paste(
      "DROP TABLE",
      paste(names(conf$db$tab), collapse = ", "), ";"
    )
  )
}
## if db dropped on Github Actions the coverage reporting will fail...
if (is.null(check_db(is_test_that = FALSE)) &&
      Sys.getenv("GITHUB_ACTIONS_RUN_DB_UNIT_TESTS") != "true") {
  pool::dbExecute(pool, "drop database testdb;")
}
## drain pool
if (is.null(check_db(is_test_that = FALSE))) {
  drain_pool(pool)
}
## and finally, remove local config
file.remove("_imongr.yml")


# recreate initial state
Sys.setenv(IMONGR_CONTEXT = env_context)
Sys.setenv(IMONGR_DB_USER = env_user)
Sys.setenv(IMONGR_DB_PASS = env_pass)
Sys.setenv(IMONGR_DB_NAME = env_name)
Sys.setenv(IMONGR_DB_HOST = env_host)
Sys.setenv(SHINYPROXY_USERNAME = env_user_name)
Sys.setenv(SHINYPROXY_USERGROUPS = env_user_groups)
