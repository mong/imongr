if (!exists("check_db", mode = "function")) {
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
}

test_that("indicator sorting save updates include/exclude and included name", {
  check_db()

  local_pool <- FALSE

  if (exists("pool", inherits = TRUE)) {
    test_pool <- get("pool", inherits = TRUE)
  } else {
    create_config()
    test_pool <- make_pool()
    local_pool <- TRUE
  }

  is_open <- tryCatch({
    DBI::dbGetQuery(test_pool, "SELECT 1")
    TRUE
  }, error = function(e) FALSE)

  if (!is_open) {
    create_config()
    test_pool <- make_pool()
    local_pool <- TRUE
  }

  if (local_pool) {
    on.exit(drain_pool(test_pool), add = TRUE)
  }

  recover_pool <- function() {
    create_config()
    test_pool <<- make_pool()
    if (!local_pool) {
      local_pool <<- TRUE
      on.exit(drain_pool(test_pool), add = TRUE)
    }
  }

  safe_execute <- function(sql) {
    tryCatch(
      DBI::dbExecute(test_pool, sql),
      error = function(e) {
        if (grepl("bad_weak_ptr|pool has been closed|closed", conditionMessage(e), ignore.case = TRUE)) {
          recover_pool()
          DBI::dbExecute(test_pool, sql)
        } else {
          stop(e)
        }
      }
    )
  }

  safe_query <- function(sql) {
    tryCatch(
      DBI::dbGetQuery(test_pool, sql),
      error = function(e) {
        if (grepl("bad_weak_ptr|pool has been closed|closed", conditionMessage(e), ignore.case = TRUE)) {
          recover_pool()
          DBI::dbGetQuery(test_pool, sql)
        } else {
          stop(e)
        }
      }
    )
  }

  ind_exists <- tryCatch(
    DBI::dbExistsTable(test_pool, "ind"),
    error = function(e) {
      if (grepl("bad_weak_ptr|pool has been closed|closed", conditionMessage(e), ignore.case = TRUE)) {
        recover_pool()
        DBI::dbExistsTable(test_pool, "ind")
      } else {
        stop(e)
      }
    }
  )

  if (!ind_exists) {
    testthat::skip("Table 'ind' is not available in the configured test database")
  }

  registry_id <- 10
  indicator_ids <- c(
    "zz_sorting_include",
    "zz_sorting_exclude"
  )

  cleanup_query <- paste0(
    "DELETE FROM ind WHERE id IN ('",
    paste(indicator_ids, collapse = "', '"),
    "');"
  )

  safe_execute(cleanup_query)
  on.exit(safe_execute(cleanup_query), add = TRUE)

  escape_sql <- function(x) {
    gsub("'", "''", x, fixed = TRUE)
  }

  insert_indicator <- function(id, title, name, include) {
    query <- paste0(
      "INSERT INTO ind (id, registry_id, include, title, name, type) VALUES ('",
      escape_sql(id), "', ", registry_id, ", ", include, ", '",
      escape_sql(title), "', '", escape_sql(name), "', 'andel');"
    )
    safe_execute(query)
  }

  insert_indicator(indicator_ids[1], "Include", "x", 1)
  insert_indicator(indicator_ids[2], "Exclude", "y", 0)

  shiny::testServer(
    indicator_server,
    args = list(
      registry_tracker = list(current_registry = registry_id),
      pool = test_pool,
      pool_verify = test_pool
    ),
    {
      session$setInputs(
        indicator_registry = registry_id,
        indicator_tabs = "Sortering",
        indicator_sorting = list(
          c(indicator_ids[1]),
          c(indicator_ids[2])
        ),
        save_sorting = 1
      )
    }
  )

  saved <- safe_query(
    paste0(
      "SELECT id, name, include FROM ind WHERE id IN ('",
      paste(indicator_ids, collapse = "', '"),
      "');"
    )
  )

  saved <- saved[match(indicator_ids, saved$id), ]

  expect_equal(saved$include[1], 1)
  expect_equal(saved$name[1], "a")
  expect_equal(saved$include[2], 0)
  expect_equal(saved$name[2], "y")
})
