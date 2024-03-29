% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cogmapr.R
\name{ggCMap.bg}
\alias{ggCMap.bg}
\title{Ghost cognitive map}
\usage{
ggCMap.bg(
  data,
  size.concepts = 4,
  size.labels = 4,
  size.edges = 4,
  size.arrows = 4,
  map.color = "grey50"
)
}
\arguments{
\item{data}{A list, the output of the 'data.ggCMap' function, containing all useful vertex and edge information for the cognitive maps.}

\item{size.concepts}{Size of the dot linked to vertices}

\item{size.labels}{Size of the labels of vertices}

\item{size.edges}{Size of the labels of the weight of edges}

\item{size.arrows}{Size of arrows (head)}

\item{map.color}{The unique color of all concepts and labels of the ghost map}
}
\value{
A plot
}
\description{
ggplot background of a map (used in highlighted maps)
}
\details{
ggplot background of a map (used in highlighted maps). It is like a ghost map.
}
\examples{
project_name <- "a_new_project"
main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
my.project <- ProjectCMap(main_path, project_name)

df.scm <- data.ggCMap(my.project, edge.filter = "4")
ggCMap.bg(df.scm)
}
