% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cogmapr.R
\name{SocCMap}
\alias{SocCMap}
\title{Social Cognitive Mapping}
\usage{
SocCMap(
  data.edges.soc,
  project,
  label = "num",
  minlen = 1,
  fontsize = 16,
  shape = "box"
)
}
\arguments{
\item{data.edges.soc}{A data.frame as produced by the EdgSocCMap function}

\item{project}{A QDA project, a list as generated by the ProjectCMap function.}

\item{label}{A character string that defines the text that will be print in the variables (vertex) of the cognitive maps. It can be "num", "name" or "numname" (which is of the form "NUM - Name"). The default is "num"}

\item{minlen}{A graphical parameter that defines a relative lenght between the variable of the cognitive maps. See help from RGraphViz package.}

\item{fontsize}{The fontsize of vertices (concepts), in r-base plot}

\item{shape}{The shape of the verices (concepts), in r-base plot}
}
\value{
a 'SocCMap' object, a list containing various information that could be use for plotting an Individual Cognitive Map. The most important elements are :
\describe{
\item{"vertex"}{A list of information on Cognitive Map's variables (i.e. vertices)}
\item{"edg"}{A list of information about relationships}
##' \item{"graph"}{A graphNEL object}
\item{"eAttrs"}{A list of graphical attributes of edges}
\item{"nAttrs"}{A list of graphical attributes of nodes (vertices)}
\item{"gAttrs"}{A list of graphical attributes of the whole graph}
}
}
\description{
Formatting the data for plotting an Social Cognitive Map
}
\examples{
project_name <- "a_new_project"
main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
my.project <- ProjectCMap(main_path, project_name)

edg.scm <- EdgSocCMap(my.project, min.weight=6, weighted.icm=TRUE)
SocCMap(edg.scm, my.project)
}
