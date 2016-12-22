\name{readValueList}

\alias{readValueList}

\title{
Read a list of values.
}

\description{
Read a list of values, separated by comm. Only unique values are permitted.
}

\usage{
gm <- GelitoMain()
gm$readValueList(text, distinct)
}

\arguments{
  \item{text}{
text for prompt. It can be blank.
}
  \item{distinct}{
if not NA, all values must be distinct than this value. Optional.
}
}

\value{
  \item{list}{
value list
}
}

\author{
Manuel Ramón Gutiérrez Covarrubias
}

\seealso{
GelitoMain
}
