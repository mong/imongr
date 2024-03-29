% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate.R
\name{aggregate}
\alias{aggregate}
\alias{agg}
\alias{agg_dg}
\alias{agg_from_level}
\alias{agg_residual}
\alias{agg_udef}
\title{Aggregate hospital data}
\usage{
agg(df, org, ind, ind_noagg = character(), orgnr_name_map)

agg_dg(aggs, ind)

agg_from_level(df, org, ind, conf, from_level)

agg_residual(aggs, conf)

agg_udef(diff, conf)
}
\arguments{
\item{df}{Data frame}

\item{org}{Data frame holding the flat org table with all levels needed. May
be obtained by \code{get_flat_org(pool)}}

\item{ind}{Data frame holding the indicator db table providing all
data on each indicator}

\item{ind_noagg}{Character vector identifying indicator (ids) that are not to
be aggregated (used as is). Default is \code{character()}}

\item{orgnr_name_map}{Data frame with global mapping of organization id and
name. Applied on data to be used as is (no aggregation).}

\item{aggs}{Data frame of (pre) aggregated data}

\item{conf}{List of configuration}

\item{from_level}{Integer specifying from what level to aggregate from}

\item{diff}{Data frame with diff data}
}
\value{
Data frame in raw, grouped or aggregated form
}
\description{
Aggregation of indicators as provided for each unit (hospital) at
organization levels above hospital: health trust, regional health trust and
national.
}
\details{
All functions are adapted from qmongr/qmongrdata. However, the nomenclature
of function arguments are changed somewhat. Main source of underlying data
is the qmongr database and where tables are used in these function their
names are kept as is (\emph{org} and \emph{ind}). Other data frames
passed into or between function is denoted \emph{df}.
The aggregate consists of the following variables:
\describe{
  \item{year}{The year of the current record}
  \item{ind_id}{Indicator ID}
  \item{orgnr}{Organization ID}
  \item{context}{How orgnr is to be understood, \emph{e.g.} caregiver,
  or patient residency}
  \item{count}{Number of observations of the current record}
  \item{var}{Summarised indicator value, for instance mean or median}
  \item{unit_level}{Code providing level of aggregation such as 'hospital',
  'hf' (health trust), 'rhf' (regional health trust) and 'national'}
  \item{unit_name}{Name of the organization unit}
}
}
