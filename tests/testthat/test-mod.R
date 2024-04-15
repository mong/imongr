create_config()

conf <- get_config()

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

# Testing

## profile

test_that("profile module provides a shiny app object", {
  check_db()
  expect_equal(class(profile_app()), "shiny.appobj")
})

test_that("profile module ui returns returns a shiny tag list object", {
  check_db()
  expect_true("shiny.tag.list" %in% class(profile_ui("id")))
})

Sys.setenv(SHINYPROXY_USERNAME = "unknown")
test_that("profile module server provides sensible output for not known user", {
  check_db()
  shiny::testServer(
    profile_server,
    args = list(pool = pool, pool_verify = pool),
    {
      # when unknown user, status should be reported accordingly
      session$setInputs(upload_history = TRUE, publish_history = TRUE)
      expect_equal(profile(), conf$profile$pending)
    }
  )
})

Sys.setenv(SHINYPROXY_USERNAME = "nobody@nowhere.com")
Sys.setenv(SHINYPROXY_USERGROUPS = "PROVIDER,MANAGER")
test_that("known user with no previous deliveries get relevant message", {
  check_db()
  shiny::testServer(
    profile_server,
    args = list(pool = pool, pool_verify = pool),
    {
      session$setInputs(upload_history = TRUE, publish_history = TRUE)
      expect_true(
        grepl(
          conf$profile$delivery$none, profile(),
          fixed = TRUE, useBytes = TRUE
        )
      )
    }
  )
})

Sys.setenv(SHINYPROXY_USERNAME = "mongr")
Sys.setenv(SHINYPROXY_USERGROUPS = "PROVIDER,MANAGER")
test_that("profile module server provides sensible output for known user", {
  check_db()
  shiny::testServer(
    profile_server,
    args = list(pool = pool, pool_verify = pool),
    {
      session$setInputs(upload_history = FALSE, publish_history = FALSE)
      expect_null(upload_history())
      expect_null(publish_history())

      session$setInputs(upload_history = TRUE, publish_history = TRUE)
      expect_true("htmlwidget" %in% class(upload_history()))
      expect_true("htmlwidget" %in% class(publish_history()))
    }
  )
})


## report

test_that("report module provides a shiny app object", {
  check_db()
  expect_equal(class(report_app()), "shiny.appobj")
})

test_that("report module ui returns returns a shiny tag list object", {
  check_db()
  expect_true("shiny.tag.list" %in% class(report_ui("id")))
})

test_that("report module server provides sensible output", {
  check_db()
  shiny::testServer(
    report_server,
    args = list(pool = pool, pool_verify = pool),
    {
      # when unknown report an empty data frame should be returned
      session$setInputs(
        report = "",
        file_format = "csv"
      )
      expect_true("data.frame" %in% class(df()))
      expect_equal(dim(df()), c(0, 0))

      session$setInputs(
        report = "Registerstatus",
        file_format = "csv"
      )
      expect_true("data.frame" %in% class(df()))
      expect_true(dim(df())[1] > 0)
    }
  )
})


## indicator

test_that("indicator module provides a shiny app object", {
  check_db()
  expect_equal(class(indicator_app()), "shiny.appobj")
})

test_that("indicator module ui returns returns a shiny tag list object", {
  check_db()
  expect_true("shiny.tag.list" %in% class(indicator_ui("id")))
})

test_that("indicator module server provides sensible output", {
  check_db()
  shiny::testServer(
    indicator_server,
    args = list(pool = pool, pool_verify = pool),
    {
      # use package data from imongr, relevant part looks like
      # imongr::ind[imongr::ind$id == "norgast_saarruptur",
      #             c("id",
      #               "level_green",
      #               "level_yellow",
      #               "level_direction",
      #               "type",
      #               "registry_id",
      #               "include",
      #               "min_denominator")]

      # direction 1 levels should be consistent
      session$setInputs(
        indicator_registry = 10,
        indicator = "norgast_kikkhullsteknikk_endetarm",
        include = TRUE,
        dg_id = NA,
        level_direction = TRUE,
        level_green = 0.7,
        level_yellow = 0.6,
        min_denominator = 10,
        type = "andel"
      )
      expect_true(level_consistent())

      # if levels are not yet set (empty) we should get consistent levels
      session$setInputs(
        indicator_registry = 10,
        indicator = "norgast_saarruptur",
        include = TRUE,
        dg_id = NA,
        level_green = NA,
        level_yellow = NA,
        min_denominator = 10,
        type = "andel"
      )
      expect_true(level_consistent())

      # if one level is not set levels should be consistent
      session$setInputs(level_yellow = 0.04)
      expect_true(level_consistent())

      # equal levels should be consistent
      session$setInputs(
        level_direction = FALSE,
        level_green = 0.04,
        level_yellow = 0.04
      )
      expect_true(level_consistent())

      # if levels are consistent with defined level direction we are consistent
      session$setInputs(
        level_green = 0.03,
        level_yellow = 0.04
      )
      expect_true(level_consistent())
      session$setInputs(
        level_direction = TRUE,
        level_green = 0.05,
        level_yellow = 0.04
      )
      expect_true(level_consistent())

      # and, if levels do not follow direction we are inconsistent
      session$setInputs(
        level_direction = TRUE,
        level_green = 0.03,
        level_yellow = 0.04
      )
      expect_false(level_consistent())
      session$setInputs(
        level_direction = FALSE,
        level_green = 0.05,
        level_yellow = 0.04
      )
      expect_false(level_consistent())

      # indicator values for aggregation can be updated
      session$setInputs(
        indicator_registry = 10,
        indicator = "norgast_saarruptur",
        include = TRUE,
        dg_id = NA,
        level_direction = FALSE,
        level_green = 0.03,
        level_yellow = 0.04,
        min_denominator = 10,
        type = "andel"
      )
      expect_true(is.na(rv$ind_data$min_denominator))
      session$setInputs(update_val = 1)
      expect_equal(rv$ind_data$min_denominator, 10)

      # indicator title, short and long can be edited and lengths checked
      session$setInputs(
        ind_title = "title",
        ind_short = "short",
        ind_long = "long"
      )
      expect_false(rv$title_oversize)
      expect_false(rv$short_oversize)
      expect_false(rv$long_oversize)
      expect_true(session$setInputs(update_txt = 1))
      session$setInputs(
        ind_title = paste0(rep("a", 256), collapse = ""),
        ind_short = paste0(rep("a", 1024), collapse = ""),
        ind_long = paste0(rep("a", 2048), collapse = "")
      )
      expect_true(rv$title_oversize)
      expect_true(rv$short_oversize)
      expect_true(rv$long_oversize)
    }
  )
})

test_that("indicator module has output...", {
  check_db()
  shiny::testServer(
    indicator_server,
    args = list(pool = pool, pool_verify = pool),
    {
      session$setInputs(
        indicator_registry = 10,
        indicator = "norgast_saarruptur"
      )
      expect_false(is.null(output))
    }
  )
})

test_that("publish module has output...", {
  check_db()
  shiny::testServer(

    publish_server,
    args = list(pool = pool, pool_verify = pool, tab_tracker = {}),
    {
      session$setInputs(
        publish_registry = 10
      )

      # Get out norgast data from pool
      expect_equal(nrow(publish_data()), 19773)
      expect_equal_to_reference(publish_ind(), "data/norgast_ind.rds")

      session$setInputs(
        publish_registry = 1
      )

      expect_equal(nrow(publish_data()), 0)
    }
  )
})

test_that("upload module has output...", {
  check_db()
  shiny::testServer(
    upload_server,
    args = list(pool = pool),
    {
      session$setInputs(
        registry = 10,
        sep = ";",
        dec_sep = ",",
        sample_size = 20,
        sample_type = TRUE,
        latest_update = "2022-12-24",
        latest_affirm = "2022-01-01"
      )
      expect_equal_to_reference(ind(), "data/norgast_ind.rds")
      expect_equal(df(), data.frame())
    }
  )
})

test_that("download module has output...", {
  check_db()
  shiny::testServer(
    download_server,
    args = list(pool = pool, pool_verify = pool),
    {
      session$setInputs(
        download_registry = 10,
        tab_set = "ind",
        download_context = "verify",
        file_format = "rds"
      )
      expect_equal_to_reference(db_table(), "data/norgast_ind.rds")

      session$setInputs(
        tab_set = "data"
      )
      expect_equal(nrow(db_table()), 19773)
      session$setInputs(
        tab_set = "ind",
        download_context = "prod"
      )
      expect_equal_to_reference(db_table(), "data/norgast_ind.rds")
      session$setInputs(
        download_registry = "qwerty"
      )
      expect_error(db_table())
      # Try with download_context = null
      session$setInputs(
        download_registry = 10,
        download_context = NULL
      )
      expect_equal_to_reference(db_table(), "data/norgast_ind.rds")
      # Try with empty registry id
      session$setInputs(
        download_registry = ""
      )
      expect_equal(db_table(), data.frame())
      session$setInputs(
        download_registry = 10,
        tab_set = "hf"
      )
      expect_equal_to_reference(db_table(), "data/hf.rds")
    }
  )
})

test_that("review module has output...", {
  check_db()
  shiny::testServer(
    review_server,
    args = list(pool = pool),
    {
      session$setInputs(selected_registry = "", selected_year = "2018")

      expect_equal(verdict(), "1")

      session$setInputs(selected_year = "2019")

      expect_equal(verdict(), "1C")

      session$setInputs(
        requirement_1 = TRUE,
        requirement_2 = TRUE,
        requirement_3 = TRUE,
        requirement_4 = TRUE,
        requirement_5 = TRUE
      )

      expect_equal(verdict(), "2C")

      session$setInputs(
        requirement_18 = TRUE
      )

      expect_equal(verdict(), "2B")

      session$setInputs(
        requirement_6 = TRUE,
        requirement_7 = TRUE,
        requirement_8 = TRUE,
        requirement_9 = TRUE,
        requirement_10 = TRUE,
        requirement_11 = TRUE
      )

      expect_equal(verdict(), "3B")

      session$setInputs(
        requirement_17 = TRUE
      )

      expect_equal(verdict(), "3A")

      session$setInputs(
        requirement_12 = TRUE,
        requirement_13 = TRUE,
        requirement_14 = TRUE,
        requirement_15 = TRUE,
        requirement_16 = TRUE
      )

      expect_equal(verdict(), "4A")
    }
  )
})

# clean up
## drop tables (in case tests are re-run on the same instance)
if (is.null(check_db(is_test_that = FALSE))) {
  conf <- get_config()
  pool::dbExecute(pool, "ALTER TABLE `delivery` DROP FOREIGN KEY `fk_delivery_publish`;")
  pool::dbExecute(pool, "ALTER TABLE `evaluation` DROP FOREIGN KEY `fk_evaluation_user`;")
  pool::dbExecute(pool, "ALTER TABLE `evaluation` DROP FOREIGN KEY `fk_evaluation_registry`;")
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
