% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mpn_ci.R
\name{get_mpn_ci}
\alias{get_mpn_ci}
\title{Get MPN Confidence Intervals for E.Coli}
\usage{
get_mpn_ci(e.coli)
}
\arguments{
\item{e.coli}{numeric. A vector for e.coli values}
}
\value{
A data.frame with 3 columns: e.coli, lo, up
}
\description{
Lookup laboratory tables for MPN values for E.Coli to get upper and lower
0.95 confidence interval for the given values. If value is not directly
found in table it will be generated by interpolating nearest neighbors.
}
\examples{
\dontrun{
print(get_mpn_ci(c(15,30,35,60,61,71,120,1959,25000,369990)))
}

}
