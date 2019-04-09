## ==================================================================
## Package : cogmapr
## Frederic M. Vanwindekens, CRA-W
## GPL (>= 3)
## ==================================================================

##' Remove codings of a QDA project
##'
##' This function removes one or many codings of a Qualitative Data Analysis (QDA) project. The codings are listed using the 'id' of codings.
##' @title Remove codings of a QDA project
##' @param project A QDA project (as created by the qcoder package)
##' @param codings_id A vector of integer corresponding with the id of the codings to remove
##' @return A QDA project
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' my.cleansed.project <- RemoveCodings(my.project, 1)
##' @export
RemoveCodings <- function(project,
			  codings_id) {

    clean.project <- project
    clean.project$codings <- clean.project$codings %>%
        dplyr::filter(!(coding_id %in% codings_id))

    return(clean.project)
}
