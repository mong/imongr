% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_review.R
\name{mod_review}
\alias{mod_review}
\alias{review_ui}
\alias{get_last_year}
\alias{update_graph_data}
\alias{toggle_button}
\alias{render_checkboxes}
\alias{on_update_form}
\alias{review_server}
\alias{review_app}
\title{Shiny module providing GUI and server logic for the expert group tab}
\usage{
review_ui(id)

get_last_year()

update_graph_data(input, pool, rv)

toggle_button(input, session, rv, event, requirements)

render_checkboxes(input, output, df_requirements, ns, id_numbers)

on_update_form(
  session,
  input,
  pool,
  n_requirements,
  fetch_previous_year = FALSE
)

review_server(id, registry_tracker, pool)

review_app(pool)
}
\description{
Shiny module providing GUI and server logic for the expert group tab
}
