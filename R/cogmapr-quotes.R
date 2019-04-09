## ==================================================================
## Package : cogmapr
## Frederic M. Vanwindekens, CRA-W
## GPL (>= 3)
## ==================================================================

##' Extract all quotes of a QDA project
##'
##' This function creates a data frame with all quotes of a Qualitative Data Analysis (QDA) project
##' @title Extract all quotes of a QDA project
##' @inheritParams EdgCMap
##' @return A data frame with relationships and quotes
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' QuotesCMap(my.project)
##' @export
QuotesCMap <- function(project) {
    df.quotes <- EdgCMap(project) %>%
        dplyr::left_join(project$concepts[c("num","name")],
                         by = c("concept_from" = "num")) %>%
        dplyr::rename(concept_from_name = name) %>%
        dplyr::left_join(project$concepts[c("num","name")],
                         by = c("concept_to" = "num")) %>%
        dplyr::rename(concept_to_name = name) %>%
        dplyr::arrange(concept_from_name,concept_to_name) ## %>%
    return(df.quotes)
}

##' Extract all quotes of a document
##'
##' This function creates a data frame with all quotes of a one document of a Qualitative Data Analysis (QDA) project
##' @title Extract all quotes of a document (or an Individual Cognitivce Map)
##' @inheritParams QuotesCMap
##' @param doc.id The id of a document (id of documents can be found in the data frame "documents" in the QDA project)
##' @return A data frame with relationships and quotes
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' QuotesIndCMap(my.project, 2)
##' @export
QuotesIndCMap <- function(project, doc.id){
    df.quotes <- QuotesCMap(project) %>%
        dplyr::filter(doc_id == doc.id)
    return(df.quotes)
}


##' Extract all quotes of a group of documents (or of an Social Cognitive Map)
##'
##' This function creates a data frame with all quotes of a a group of documents of a Qualitative Data Analysis (QDA) project
##' @title Extract all quotes of documents (or a Social Cognitivce Map)
##' @inheritParams EdgSocCMap
##' @return A data frame of relationships and quotes
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' QuotesSocCMap(my.project)
##' QuotesSocCMap(my.project, units = 'Québec')
##' @export
QuotesSocCMap <- function(project, min.weight = 1, filters = NULL, units = 'all'){
    df.quotes <- QuotesCMap(project) %>%
        dplyr::full_join(
                   EdgSocCMap(project),
                   by = "edge"
               ) ## TODO weighted ICM
    if (!is.null(filters)) {
        for (n.filter in names(filters)) {
            df.quotes  <- df.quotes %>%
                dplyr::filter(get(n.filter) %in% filters[[n.filter]])
        }
    }
    if (!('all' %in% units)) {
        units_doc_id <- project$documents_unit %>%
            tidyr::spread(unit_name,unit_id) %>%
            dplyr::filter_at(units, dplyr::all_vars(!is.na(.))) %>%
            dplyr::select(doc_id) %>%
            dplyr::pull()
        df.quotes <- df.quotes %>%
            dplyr::filter(doc_id %in% units_doc_id)
    }
    df.quotes  <- df.quotes %>%
        dplyr::filter(coding_weight.y >= min.weight)
    return(df.quotes)
}


##' Extract the quotes of a project linked to selected relationships
##'
##' Extract the quotes of a project linked to selected relationships. Units is a mandatory parameter as this function was initially developped for given the quotes linked to significantly different relationships between groups of documents (i.e.  units).
##' @title Extract the quotes of a project linked to selected relationships
##' @inheritParams EdgSocCMap
##' @param selected.edge A vector of character value(s), the names of one or many relationship(s) of the map
##' @return A data frame of relationships and quotes
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' df.quotes.scm.edge(my.project, "Belgium", "1~2")
##' @export
df.quotes.scm.edge <- function(project, units, selected.edge){
    
  df.quotes <- QuotesCMap(project) %>%
    dplyr::full_join(project$documents_units, by = "doc_id") %>%
    dplyr::filter(unit_name %in% units) %>%
    dplyr::filter(edge %in% selected.edge) %>%
    dplyr::group_by(unit_name) %>%
    ## dplyr::summarise(quotes = paste("-", selected_text, collapse = "\n\n")) %>%
    dplyr::select(coding_id, edge, concept_from_name, concept_to_name, unit_name, selected_text) %>%
    tidyr::spread(unit_name, selected_text) %>%
    dplyr::mutate(coding_id = as.integer(coding_id))
    
    return(df.quotes)
}


##' Extract the quotes of a project linked to selected concepts
##'
##' Extract the quotes of a project linked to selected concepts. Units is a mandatory parameter as this function was initially developped for given the quotes linked to significantly different concepts between groups of documents (i.e.  units).
##' @title Extract the quotes of a project linked to selected concepts
##' @inheritParams EdgSocCMap
##' @param selected.concept A vector of character/integer value(s), the id(s) of one or many concept(s) of the map
##' @return A data frame of relationships and quotes
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' df.quotes.scm.concept(my.project, "Québec", 4)
##' df.quotes.scm.concept(my.project, "Québec", "2")
##' @export
df.quotes.scm.concept <- function(project, units, selected.concept){

    df.quotes <- QuotesCMap(project) %>%
    dplyr::full_join(project$documents_units, by = "doc_id") %>%
    dplyr::filter(unit_name %in% units) %>%
    dplyr::filter(concept_from %in% selected.concept | concept_to %in% selected.concept) %>%
    dplyr::group_by(unit_name) %>%
    ## dplyr::summarise(quotes = paste("-", selected_text, collapse = "\n\n")) %>%
    dplyr::select(coding_id, edge, concept_from_name, concept_to_name, unit_name, selected_text) %>%
    tidyr::spread(unit_name, selected_text) %>%
    dplyr::mutate(coding_id = as.integer(coding_id))
  
    return(df.quotes)
}
