## This file contains the R code blocks from the README file
## and can be use to easily run commands. Please refer carefully
## to the cautions and comments in the REAMDE file.

## Installation

devtools::install_github('FrdVnW/qcoder', ref = "cogmap-dev", upgrade = 'never')

install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"), update = FALSE)

install.packages("cogmapr")

devtools::install_git("https://gitlab.com/FrdVnW/cogmapr", ref = "dev", upgrade = 'never')
devtools::install_git("https://gitlab.com/FrdVnW/cogmapr", ref = "RELEASE_0.9.1", upgrade = 'never')

devtools::install_git("https://gitlab.com/FrdVnW/cogmapvisualizr", ref = "master", upgrade = 'never')

## Usage

library(qcoder)

create_qcoder_cogmap_project("PROJECT_NAME", sample = TRUE)

import_project_data("PROJECT_NAME")

project_document_part <- c(
    "Subject A",
    "Subject B",
    "Subject C"
)
project_coding_class <- c(
    "Relationship Class x",
    "Relationship Class y",
    "Relationship Class z"
)
project_concept_class <- c(
    "Concept Class i",
    "Concept Class j",
    "Concept Class k"
)

qcode()

library("cogmapr")
library("dplyr")
library("pander")
library("ggplot2")
library("Rgraphviz")

project_name <- "PROJECT_NAME"
main_path <- paste0(getwd(),'/')
my.cogmap.project <- ProjectCMap(main_path,project_name)

plot(IndCMap(my.cogmap.project, doc.id = '1'))

QuotesIndCMap(my.cogmap.project,1)

plot(
    SocCMap(
        EdgSocCMap(my.cogmap.project,
                   min.weight = 2
                   ),
        my.cogmap.project,
        label="name"
    )
)

SheetCoding(my.cogmap.project)
## ReportICM(my.cogmap.project) ## bug
## ReportSCM(my.cogmap.project) ## bug

library(cogmapvisualizr)
cogmapvisualizr()
