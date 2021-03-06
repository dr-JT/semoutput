% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sem_factorloadings.R
\name{sem_factorloadings}
\alias{sem_factorloadings}
\title{Factor loadings}
\usage{
sem_factorloadings(
  x,
  standardized = TRUE,
  ci = "standardized",
  ci.level = 0.95,
  digits = 3,
  print = TRUE
)
}
\arguments{
\item{x}{a cfa() or sem() lavaan model}

\item{standardized}{logical. Include standardized loadings? (default = TRUE)}

\item{ci}{logical. display standardized or unstandardized confidence
intervals? (default = "standardized"). Not needed if standardized=FALSE}

\item{ci.level}{What level of confidence interval to use (default = 0.95)}

\item{digits}{How many digits to display? (default = 3)}

\item{print}{Create a knitr table for displaying as html table? (default = TRUE)}
}
\description{
This function will display a table of Factor loadings
}
