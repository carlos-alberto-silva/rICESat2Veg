% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class.icesat2.R
\name{close,icesat2.atl03_h5-method}
\alias{close,icesat2.atl03_h5-method}
\alias{close,icesat2.atl08_h5-method}
\title{Safely closes the \code{\linkS4class{icesat2.atl03_h5}}}
\usage{
\S4method{close}{icesat2.atl03_h5}(con, ...)

\S4method{close}{icesat2.atl08_h5}(con, ...)
}
\arguments{
\item{con}{An object of class \code{icesat2.atl08_h5}}

\item{...}{Inherited from base}
}
\description{
Closing files will avoid locking HDF5 ATL03 files.

Closing files will avoid locking HDF5 ATL08 files.
}
