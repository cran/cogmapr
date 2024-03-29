% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cogmapr-comp.R
\name{ConceptTest}
\alias{ConceptTest}
\title{Compare value of concepts indicators between maps}
\usage{
ConceptTest(project, units, output = "p.value", sep = ">", coder = "qcoder")
}
\arguments{
\item{project}{A QDA project, a list as generated by the ProjectCMap function.}

\item{units}{The units to compare}

\item{output}{"p.value" (default) or "raw.data".}

\item{sep}{Separation used in the relationships definition. Default is ">" (ex : 1>3)}

\item{coder}{Coding tool used for this project. Default is "qcoder" (only implemented now)}
}
\value{
A data frame (if output = "p.value"), a list of data frame (if output = "raw.data").
}
\description{
This function test the differences between the properties of concepts
}
\details{
This function test the differences between the properties of concepts (indegree, outdegree, centrality) between groups of documents (i.e. between social cognitive maps). Till now, only two excluding groups can be tested (ex. document from one country vs another country, from a group of players vs another group of players).  It is not possible to compare non exclusive groups (ex. map from one country vs map from one group of players, as some documents can be in the two groups!). For this test, the 'wilcoxon.test' is done. If output = 'p.value', the function returns the results of the tests, one test for each concepts of the map. If output = 'raw.data', the function returns the raw data on which the tests are done, one data frame by concept. This option can be used to export data and perform other statistical tests.
}
\examples{
project_name <- "a_new_project"
main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
my.project <- ProjectCMap(main_path, project_name)

## need more documents
ConceptTest(my.project, units = c("Belgium", "Québec"))
}
