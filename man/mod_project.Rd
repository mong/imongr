% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_project.R
\name{mod_project}
\alias{mod_project}
\alias{indicator_ui}
\alias{indicator_server}
\alias{indicator_app}
\alias{project_ui}
\alias{get_hospitals_orgnr}
\alias{project_server}
\title{Shiny module providing GUI and server logic for the indicator tab}
\usage{
project_ui(id)

get_hospitals_orgnr(pool)

project_server(id, registry_tracker, pool, pool_verify)
}
\arguments{
\item{id}{Character string module namespace}

\item{pool}{A database pool object}

\item{registry_tracker}{Integer defining registry id}

\item{pool_verify}{A database pool object}
}
\value{
Shiny objects for the imongr app

A named list of hospital organization numbers with their short names as names
}
\description{
Shiny module providing GUI and server logic for the indicator tab
}
