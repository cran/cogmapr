% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cogmapr.R
\name{plotSocCMap}
\alias{plotSocCMap}
\title{Plotting a Social Cognitive Map}
\usage{
plotSocCMap(soc.cmap, layoutType = "neato", ..., main = "Social map")
}
\arguments{
\item{soc.cmap}{An object of class SocCMap, as an output of the SocCMap function}

\item{layoutType}{Type of graph. See detail in RGraphViz. Can be 'neato', 'dot', 'twopi', 'circo', and 'fdp'. The default is 'neato'.}

\item{...}{other graphical parameters}

\item{main}{The title of the map. By default it is "Individual map - Agent's name"}
}
\value{
A plot
}
\description{
Plotting a Social Cognitive Map
}
\examples{
project_name <- "a_new_project"
main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
my.project <- ProjectCMap(main_path, project_name)

edg.scm <- EdgSocCMap(my.project, min.weight=6, weighted.icm=TRUE)
scm <- SocCMap(edg.scm, my.project)
plotSocCMap(scm)
scm <- SocCMap(edg.scm, my.project, label = "name", shape = "plaintext")
plotSocCMap(scm)
}
