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

if (!exists("pool", inherits = TRUE) && is.null(check_db(is_test_that = FALSE))) {
  create_config()
  pool <- make_pool()
}

test_that("indicator sorting bucket list saves include order and excludes hidden indicators", {
  check_db()

  registry_id <- 10
  indicator_ids <- c(
    "zz_sorting_gamma",
    "zz_sorting_alpha",
    "zz_sorting_beta",
    "zz_sorting_hidden"
  )

  cleanup_query <- paste0(
    "DELETE FROM ind WHERE id IN ('",
    paste(indicator_ids, collapse = "', '"),
    "');"
  )

  pool::dbExecute(pool, cleanup_query)
  on.exit(pool::dbExecute(pool, cleanup_query), add = TRUE)

  escape_sql <- function(x) {
    gsub("'", "''", x, fixed = TRUE)
  }

  insert_indicator <- function(id, title, name, include) {
    query <- paste0(
      "INSERT INTO ind (id, registry_id, include, title, name, type) VALUES ('",
      escape_sql(id), "', ", registry_id, ", ", include, ", '",
      escape_sql(title), "', '", escape_sql(name), "', 'andel');"
    )
    pool::dbExecute(pool, query)
  }

  insert_indicator(indicator_ids[1], "Gamma", "c", 1)
  insert_indicator(indicator_ids[2], "Alpha", "a", 1)
  insert_indicator(indicator_ids[3], "Beta", "b", 1)
  insert_indicator(indicator_ids[4], "Hidden", "z", 0)

  shiny::testServer(
    indicator_server,
    args = list(
      registry_tracker = list(current_registry = registry_id),
      pool = pool,
      pool_verify = pool
    ),
    {
      session$setInputs(
        indicator_registry = registry_id,
        indicator_tabs = "Sortering",
        indicator_sorting = list(
          c(indicator_ids[3], indicator_ids[2], indicator_ids[1]),
          c(indicator_ids[4])
        ),
        save_sorting = 1
      )
    }
  )

  saved <- pool::dbGetQuery(
    pool,
    paste0(
      "SELECT id, name, include FROM ind WHERE id IN ('",
      paste(indicator_ids, collapse = "', '"),
      "');"
    )
  )

  saved <- saved[match(indicator_ids, saved$id), ]

  expect_equal(saved$name[1:3], c("c", "b", "a"))
  expect_equal(saved$include[1:3], c(1, 1, 1))
  expect_equal(saved$name[4], "z")
  expect_equal(saved$include[4], 0)
})
