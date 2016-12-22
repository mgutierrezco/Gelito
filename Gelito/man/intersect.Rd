\name{intersect}

\alias{intersect}

\title{
List Intersection.
}

\description{
List Intersection. By default, intersect will be done by first column in list.
}

\usage{
gm <- Gelito()
gm$intersect(lists = list(), ncol = 1)
}

\arguments{
  \item{lists}{
list of number of list to intersect; count must be greater than 0.
}
  \item{ncol}{
intersect will be done by number of ncol.
}
}

\value{
  \item{list}{
list result
}
}

\author{
Manuel Ramón Gutiérrez Covarrubias
}

\seealso{
Gelito
}
