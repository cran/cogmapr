% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cogmapr-indic.R
\name{ConceptOutdegreeIndiv}
\alias{ConceptOutdegreeIndiv}
\title{Outdegree of vertices by document}
\usage{
ConceptOutdegreeIndiv(project, min.weight = 1, weighted.icm = FALSE)
}
\arguments{
\item{project}{A QDA project, a list as generated by the ProjectCMap function.}

\item{min.weight}{A integer that will determine the minimum (>=) weight of relationships that will be taken into account. Relationships with a lower weight (<) will not be shown. Default is set to 1 (\emph{i.e.} all relationships are shown).}

\item{weighted.icm}{A boolean. If FALSE, the weight of the relationships in the ICM will be fixed to 1.}
}
\value{
A data frame of Outdegree by document (ICM)
}
\description{
Outdegree of vertices by document
}
\details{
Outdegree of vertices by document
}
\examples{
project_name <- "a_new_project"
main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
my.project <- ProjectCMap(main_path, project_name)

ConceptOutdegreeIndiv(my.project)
}
