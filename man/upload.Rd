% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upload.R
\name{upload}
\alias{upload}
\alias{csv_to_df}
\alias{sample_df}
\alias{check_report}
\alias{check_upload}
\alias{check_missing_var}
\alias{check_invalid_var}
\alias{check_invalid_org}
\alias{check_invalid_ind}
\alias{check_none_numeric_var}
\alias{check_duplicate_delivery}
\title{Title}
\usage{
check_report(df, pool)

check_upload(df, pool)

check_missing_var(df, conf, pool)

check_invalid_var(df, conf, pool)

check_invalid_org(df, conf, pool)

check_invalid_ind(df, conf, pool)

check_none_numeric_var(df, conf, pool)

check_duplicate_delivery(df, conf, pool)

csv_to_df(path, sep = ",", dec, encoding = "UTF-8")

sample_df(df, skip = c(""), n, random = FALSE)
}
\arguments{
\item{df}{Data frame}

\item{path}{Character path to a file}

\item{sep}{Character filed sep}

\item{dec}{Character decimal sep}

\item{encoding}{Character encoding}

\item{skip}{character vector defining data frame variables to skip}

\item{n}{Numeric sample size}

\item{random}{Logical sample method}
}
\value{

}
\description{
Title
}