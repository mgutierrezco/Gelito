\name{readFile}

\alias{readFile}

\title{
Read tab file with header to console, by default.
}

\description{
Read tab file with header to console, by default.
}

\usage{
gm <- GelitoMain()
gm$readFile(filename = "", header = TRUE, console = FALSE, all = FALSE)
}

\arguments{
  \item{filename}{
filename than contents data.
}
  \item{header}{
if header exists or not (FALSE).
}
  \item{console}{
by default, results are not sent to console.
}
  \item{all}{
by default, one expression valid is enough. FALSE is like "and" on all expressions.
}
}

\value{
if console = TRUE, show data read, else nothing
}

\author{
Manuel Ramón Gutiérrez Covarrubias
}

\seealso{
GelitoMain
}
