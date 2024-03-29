% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_download.R
\name{mod_download}
\alias{mod_download}
\alias{download_ui}
\alias{download_server}
\alias{download_app}
\title{Shiny module providing GUI and server logic for the download data tab}
\usage{
download_ui(id)

download_server(id, registry_tracker, pool, pool_verify)

download_app(pool, pool_verify)
}
\arguments{
\item{id}{Character string module namespace}

\item{registry_tracker}{Integer defining registry id}

\item{pool}{A database pool object}

\item{pool_verify}{A database pool object}
}
\value{
Shiny objects for the imongr app
}
\description{
Shiny module providing GUI and server logic for the download data tab
}
