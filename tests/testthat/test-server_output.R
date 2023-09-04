create_config()

# preserve initial state
env_context <- Sys.getenv("IMONGR_CONTEXT")
env_user <- Sys.getenv("IMONGR_DB_USER")
env_pass <- Sys.getenv("IMONGR_DB_PASS")
env_name <- Sys.getenv("IMONGR_DB_NAME")
env_host <- Sys.getenv("IMONGR_DB_HOST")
env_user_name <- Sys.getenv("SHINYPROXY_USERNAME")
env_user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")

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

  # add a user with no deliveries
  query <- paste(
    "INSERT INTO user VALUES (10, 'nobody@nowhere.com',",
    "'Mr Nobody',",
    "'+0000000000', 'nobody@nowhere.com', 1);"
  )
  pool::dbExecute(pool, query)
}

conf <- get_config()

# make a sample df
df <- data.frame(
  context = "caregiver",
  year = 2018,
  orgnr = 974633574,
  ind_id = "norgast_andel_avdoede_bykspytt_tolv",
  var = 0,
  denominator = 1
)

# get indicators for norgast (id = 10), but only if we have a db
if (is.null(check_db(is_test_that = FALSE))) {
  ind <- get_registry_ind(pool, 10)
}

# we need a user and groups defined
Sys.setenv(SHINYPROXY_USERNAME = "mongr")
Sys.setenv(SHINYPROXY_USERGROUPS = "G1,G2")


test_that("select list of registries is provided (if any)", {
  check_db()
  expect_true("shiny.tag.list" %in% class(
    select_registry_ui(pool, conf, "test", "prod")
  ))
  expect_true("shiny.tag.list" %in% class(
    select_registry_ui(pool, conf, "test", "verify")
  ))
  expect_true("shiny.tag.list" %in% class(
    select_registry_ui(pool, conf, "test", "qa")
  ))
  expect_true("shiny.tag.list" %in% class(
    select_registry_ui(pool, conf, "test", "prod", "reg")
  ))
  # test when intersecting registries
  expect_true("shiny.tag.list" %in% class(
    select_registry_ui(pool, conf, "test", "prod", "reg", pool0 = pool)
  ))
  # test when no deliveries
  Sys.setenv(SHINYPROXY_USERNAME = "nobody@nowhere.com")
  expect_true("shiny.tag.list" %in% class(
    select_registry_ui(pool, conf, "test", "prod")
  ))
  # test when manager
  Sys.setenv(SHINYPROXY_USERGROUPS = "PROVIDER,MANAGER")
  expect_true("shiny.tag.list" %in% class(
    select_registry_ui(pool, conf, "test", "prod")
  ))
  expect_true("shiny.tag.list" %in% class(
    select_registry_ui(pool, conf, "test", "prod", "reg", pool0 = pool)
  ))
  # reset
  Sys.setenv(SHINYPROXY_USERNAME = "mongr")
  Sys.setenv(SHINYPROXY_USERGROUPS = "G1,G2")
})

test_that("submit ui is provided", {
  check_db()
  expect_null(submit_ui("input_id", conf, pool, NULL, 10, df, "prod"))
  expect_equal(
    class(
      submit_ui(
        "input_id", conf, pool, TRUE, 10,
        df[, !names(df) %in% c("denominator", "registry_id")], ind,
        "prod"
      )
    )[1],
    "shiny.tag.list"
  )
  expect_equal(
    class(
      submit_ui(
        "input_id", conf, pool, TRUE, 10,
        df[, !names(df) %in% c("denominator", "registry_id")], ind,
        "verify"
      )
    )[1],
    "shiny.tag.list"
  )
  expect_equal(
    class(
      submit_ui(
        "input_id", conf, pool, TRUE, 10,
        df[, !names(df) %in% c("denominator", "registry_id")], ind,
        "qa"
      )
    )[1],
    "shiny.tag.list"
  )
})

test_that("error report is provided", {
  check_db()
  expect_null(error_report_ui(pool, df, ind, NULL, "norgast"))
  expect_equal(
    class(
      error_report_ui(
        pool, df[, !names(df) %in% c("nevner", "Register")], ind,
        TRUE, 10
      )
    ),
    "character"
  )
})

test_that("upload sample text is provided", {
  check_db()
  expect_null(upload_sample_text_ui(pool, conf, NULL, 4))
  expect_equal(
    class(
      upload_sample_text_ui(pool, conf, TRUE, 4, indicators = vector())
    ),
    "character"
  )
})

test_that("upload sample ui is provided", {
  expect_null(upload_sample_ui(df, NULL, "norgast", 1, FALSE))
  expect_equal(
    class(
      upload_sample_ui(df, TRUE, "norgast", 1, FALSE)
    ),
    "data.frame"
  )
})

test_that("text for var doc ui is provided", {
  expect_equal(class(var_doc_ui(conf)), "character")
})

test_that("text for medfield summary is properly provided", {
  check_db()
  expect_null(medfield_summary_text_ui(pool, conf, data.frame()))
  df <- imongr::medfield
  expect_equal(
    class(medfield_summary_text_ui(pool, conf, df)),
    "character"
  )
  df <- rbind(df, data.frame(
    id = 100, name = "anon",
    full_name = "medfield with no registries"
  ))
  expect_equal(
    class(medfield_summary_text_ui(pool, conf, df)),
    "character"
  )
})

test_that("text for reguser summary is properly provided", {
  check_db()
  expect_null(reguser_summary_text_ui(pool, conf, data.frame()))
  df <- imongr::user
  df <- cbind(data.frame(id = seq(1, dim(df)[1])), df)
  expect_equal(
    class(reguser_summary_text_ui(pool, conf, df)),
    "character"
  )
  df <- rbind(
    df,
    data.frame(
      id = 100, user_name = "nobody@nowhere.com",
      name = "Mr Nobody", phone = "+0000000000",
      email = "nobody@nowhere.com", valid = 1
    )
  )
  expect_equal(
    class(medfield_summary_text_ui(pool, conf, df)),
    "character"
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
