\name{readIntegerList}

\alias{readIntegerList}

\title{
Read a list of integers.
}

\description{
Read a list of integers, separated by comma. Only unique integer are permitted.
}

\usage{
gm <- GelitoMain()
gm$readIntegerList(text, distinct)
}

\arguments{
  \item{text}{
text for prompt. It can be blank.
}
  \item{distinct}{
if not NA, all integers must be distinct than this integer. Optional.
}
}

\value{
  \item{integer}{
number list
}
}

\author{
Manuel Ramón Gutiérrez Covarrubias
}

\seealso{
GelitoMain
}
