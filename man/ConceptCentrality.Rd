% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cogmapr-indic.R
\name{ConceptCentrality}
\alias{ConceptCentrality}
\title{Centralities of concepts}
\usage{
ConceptCentrality(project, filters = NULL, units = "all", weighted.icm = FALSE)
}
\arguments{
\item{project}{A QDA project, a list as generated by the ProjectCMap function.}

\item{filters}{A list of named strings that will filter the relationships showed in the SCM. e.g. =list(coding_class = "A_coding_class", document_part = "A_document_part")=. To date, these filters are linked to the nature of relationships.}

\item{units}{A string vector giving the names of the units (i.e. classes linked to documents) that will be include in the SCM. It is a second type of filter.}

\item{weighted.icm}{A boolean. If FALSE, the weight of the relationships in the ICM will be fixed to 1.}
}
\value{
A data frame with the value of the centrality (n) of vertices.
}
\description{
Compute the centrality of concepts
}
\details{
Compute the centrality of concepts
}
\examples{
project_name <- "a_new_project"
main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
my.project <- ProjectCMap(main_path, project_name)

ConceptCentrality(my.project)

}
