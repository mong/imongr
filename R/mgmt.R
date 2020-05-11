#' Functions for organization and user management in imongr
#'
#' @param pool Database connection pool object
#' @param df Data frame corresponding to database table fields
#' @name mgmt
#' @aliases create_imongr_org create_imongr_user
NULL


#' @rdname mgmt
#' @export
create_imongr_org <- function(pool, df) {

  org <- get_org(pool)
  if (df$OrgNrShus %in% org$OrgNrShus) {
    return("Organization already exists. Nothing to do!")
  } else {
    msg <- insert_tab(pool, "org", df)
    return(msg)
  }
}


#' @rdname mgmt
#' @export
create_imongr_user <- function(pool, df) {

  user <- get_user(pool)
  if (df$user_name %in% user$user_name) {
    return("User already exists. Nothing to do!")
  } else {
    msg <- insert_tab(pool, "user", df)
    return(msg)
  }
}
