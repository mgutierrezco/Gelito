\name{readFile}

\alias{readFile}

\title{
Read file and append.
}

\description{
Read file and append to lists.
}

\usage{
gm <- Gelito()
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
no
}

\author{
Manuel Ramón Gutiérrez Covarrubias
}

\seealso{
Gelito
}
