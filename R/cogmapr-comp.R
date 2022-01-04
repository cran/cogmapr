## ==================================================================
## Package : cogmapr
## Frederic M. Vanwindekens, CRA-W
## GPL (>= 3)
## ==================================================================

##' This function test the differences between the properties of concepts
##'
##' This function test the differences between the properties of concepts (indegree, outdegree, centrality) between groups of documents (i.e. between social cognitive maps). Till now, only two excluding groups can be tested (ex. document from one country vs another country, from a group of players vs another group of players).  It is not possible to compare non exclusive groups (ex. map from one country vs map from one group of players, as some documents can be in the two groups!). For this test, the 'wilcoxon.test' is done. If output = 'p.value', the function returns the results of the tests, one test for each concepts of the map. If output = 'raw.data', the function returns the raw data on which the tests are done, one data frame by concept. This option can be used to export data and perform other statistical tests.
##' @title Compare value of concepts indicators between maps
##' @inheritParams ConceptIndicIndiv
##' @param units The units to compare
##' @param output "p.value" (default) or "raw.data".
##' @param sep Separation used in the relationships definition. Default is ">" (ex : 1>3)
##' @param coder Coding tool used for this project. Default is "qcoder" (only implemented now)
##' @return A data frame (if output = "p.value"), a list of data frame (if output = "raw.data").
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ## need more documents
##' ConceptTest(my.project, units = c("Belgium", "Québec"))
##' @export
ConceptTest <- function(project,
                        units,
                        output = "p.value",
                        sep = ">",
                        coder = "qcoder") {

    l.concept.test <- list()

    if (length(units) < 2) {stop("Two of more units are needed ...")}

    if (length(units) == 2) {test <- "wilcox.test"}
    else {stop("Not yes implemented ...")} ## test <- "kruskal.test"
    
    df <- ConceptIndicIndiv(project) %>%
        dplyr::left_join(dplyr::select(project$documents_units,doc_id, unit_name)) %>%
        dplyr::filter(unit_name %in% units) %>%
        tidyr::gather("indic","value",4:6) %>%
        dplyr::group_by(num, name, indic)

    all.concepts.doc.units <- expand.grid(
        list(
            doc_id = project$documents$doc_id,
            num = project$concepts$num,
            indic = c("centrality", "indegree", "outdegree")),
        stringsAsFactors=FALSE) %>%
        dplyr::right_join(project$concepts, by = "num") %>%
        dplyr::full_join(project$documents_units %>% dplyr::select(doc_id, unit_name),
                         by="doc_id")

    if (output == "p.value") {
        
        p.value <- df %>%
            dplyr::full_join(all.concepts.doc.units %>%
                             dplyr::filter(unit_name %in% units),
                             by = c("doc_id", "name", "num", "indic", "unit_name")
                             ) %>%
            dplyr::do(test = tryCatch(
                   expr = do.call(test, args=(list(formula = value ~ unit_name,
                                                   data=.))),
                   error = function(e) {
                       warning(paste("Test failed for var", .$indic[1], .$name[1]))
                       data.frame(p.value = NA)
                       }
                   )
               ) %>%     
            dplyr::summarise(num, name, indic, p.value = signif(test$p.value,3)) %>%
            as.data.frame()

        l.concept.test[["p.value"]] <- p.value
        
    }

    if (output == "raw.data") {
        raw.data <- df %>%
            dplyr::group_split()
        l.concept.test[["raw.data"]] <- raw.data
    }

    return(l.concept.test[[output]])
}


##' This function test the differences between the properties of relationships
##'
##' This function test the differences between the weight of relationships between groups of documents (i.e. between social cognitive maps). Till now, only two excluding groups can be tested (ex. document from one country vs another country, from a group of players vs another group of players). It is not possible to compare non exclusive groups (ex. map from one country vs map from one group of players, as some documents can be in the two groups!). For this test, the 'fisher.test' is done. If output = 'p.value', the function returns the results of the tests, one test for each relationships of the map. If output = 'raw.data', the function returns the raw data on which the tests are done, one data frame by concept. This option can be used to export data and perform other statistical tests.
##'
##' If more then 2 groups, 'anova' can be used as test (to be confirmed).
##' @title Compare relationships weight between maps
##' @inheritParams EdgCMap
##' @inheritParams EdgSocCMap
##' @param units The units to compare
##' @param output "p.value" (default) or "raw.data".
##' @return A data frame (if output = "p.value"), a list of data frame (if output = "raw.data").
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ## need more documents
##' RelationshipTest(my.project, units = c("Belgium", "Québec"))
##' @export
RelationshipTest  <- function(project,
			      units,
			      output = "p.value",
			      weighted.icm = FALSE,
			      sep = ">",
			      coder = "qcoder") {

    l.relationship.test <- list()

    if (weighted.icm == TRUE) {stop("Not sure how to do with weighted icm, development is needed ...")}
    if (length(units) < 2) {stop("Two of more units are needed ...")}

    if (weighted.icm) {
        test <- "anova"
    } else {
        test <- "fisher.test"
        if (length(units) > 2) {
            stop("Not yes implemented ...")
        }
    }

    ## test <- "chisq.test"

    all.edges <- expand.grid(project$concepts$num,project$concepts$num) %>%
        dplyr::rename(from = Var1, to = Var2) %>%
        dplyr::mutate(edge = paste(from,to,sep="~")) %>%
        dplyr::select(edge) %>%
        dplyr::pull()

    all.edges.units <- all.edges %>%
        expand.grid(project$units$name, stringsAsFactors = FALSE) %>%
        dplyr::rename(edge = Var1, unit_name = Var2)

    args <- sapply(names(formals(EdgCMap)), get, environment())

    all.edges.doc.units <- expand.grid(
        list(
            doc_id = project$documents$doc_id,
            edge = all.edges),
        stringsAsFactors=FALSE) %>%
        dplyr::full_join(project$documents_units %>% dplyr::select(doc_id, unit_name), by="doc_id")

    if (test == "fisher.test") {
        df <- do.call(EdgCMap, args) %>%
            dplyr::select(doc_id, concept_from, concept_to, edge, coding_weight) %>%
            dplyr::left_join(dplyr::select(project$documents_units, doc_id, unit_name),
                             by = "doc_id") %>%
            {if (!weighted.icm) dplyr::mutate(., coding_weight = 1)} %>%
            dplyr::group_by(edge, unit_name) %>%
            ## dplyr::summarise(weight = sum(coding_weight)) %>%
            dplyr::full_join(all.edges.doc.units,
                             by = c("doc_id", "edge", "unit_name")) %>%
            dplyr::filter(unit_name %in% units) %>%
            tidyr::replace_na(list(coding_weight = 0)) %>%
            as.data.frame()

        comparable.edges <- df %>%
            dplyr::group_by(edge) %>%
            dplyr::summarise(test = length(unique(coding_weight))) %>%
            dplyr::filter(test >= 2) %>%
            dplyr::select(edge) %>%
            dplyr::pull()

        non.null.edges <- df %>%
            dplyr::group_by(edge) %>%
            dplyr::summarise(weight = sum(coding_weight)) %>%
            dplyr::filter(weight > 0) %>%
            dplyr::select(edge) %>%
            dplyr::pull()

        raw.data <- df %>%
            dplyr::group_by(edge) %>%
            dplyr::filter(edge %in% comparable.edges) %>%
            dplyr::group_split()

        df.test <- df %>%
            dplyr::group_by(edge) %>%
            dplyr::filter(edge %in% comparable.edges) %>%
            dplyr::summarise(
                       p.value = signif(stats::fisher.test(coding_weight, unit_name)$p.value)) %>%
            dplyr::right_join(data.frame(edge = non.null.edges,
                                         stringsAsFactors=FALSE),
                              by = "edge") %>%
            dplyr::mutate(p.value = as.character(p.value)) %>%
            tidyr::replace_na(list(p.value = "NR"))
    }

    if (test == "anova") {

        df <- do.call(EdgCMap, args) %>%
            dplyr::select(doc_id, concept_from, concept_to, edge, coding_weight) %>%
            dplyr::left_join(
                       dplyr::select(project$documents_units,
                                     doc_id, unit_name)
                   ) %>%
            {if (!weighted.icm) dplyr::mutate(., coding_weight = 1)} %>%
            dplyr::group_by(edge, unit_name) %>%
            ## dplyr::summarise(weight = sum(coding_weight)) %>%
            dplyr::full_join(all.edges.units) %>%
            dplyr::filter(unit_name %in% units) %>%
            dplyr::mutate(p.value = as.character(p.value)) %>%
            tidyr::replace_na(list(coding_weight = 0)) %>%
            as.data.frame()

        comparable.edges <- df %>%
            dplyr::filter(coding_weight > 0) %>%
            dplyr::group_by(edge,unit_name) %>%
            dplyr::count() %>%
            dplyr::group_by(edge) %>%
            dplyr::summarise(nmax = max(n)) %>%
            dplyr::filter(nmax >= 2) %>%
            dplyr::select(edge) %>%
            dplyr::pull()

        raw.data <- df %>%
            dplyr::group_by(edge) %>%
            dplyr::group_split()

        df.test <- df %>%
            dplyr::group_by(edge) %>%
            dplyr::filter(edge %in% comparable.edges) %>%
            dplyr::do(stats::aov(coding_weight ~ unit_name,
                          data = .) %>%
                      car::Anova(type = "III") %>%
                      dplyr::slice(1)
                      ) %>%
            dplyr::ungroup() %>%
            as.data.frame() %>%
            dplyr::select(edge, "Pr(>F)") %>%
            dplyr::rename(p.value = "Pr(>F)") %>%
            dplyr::mutate(p.value = signif(p.value,2)) %>%
            dplyr::mutate(test = test) %>%
            tidyr::separate(edge, c("concept_from","concept_to"),
                            remove = FALSE)
    }

    p.value <- df.test

    return(get(output))
}

##' Summary table on relationship comparisons
##'
##' This function produce a summary table based on relationship comparisons and is reactive to a limit of p.value beyond which differences are considered as significant and are reported in the table
##' @title Summary table on relationship comparisons
##' @inheritParams RelationshipTest
##' @param limit.p.value A numeric.
##' @return A data frame
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ## Here 0.6 is used only for producing an output. No signif. diff. is reported.
##' RelationshipTestSummary(my.project, units = c("Belgium", "Québec"), 0.6)
##' @export 
RelationshipTestSummary <- function(project,
                                    units,
                                    limit.p.value = 0.05) {
    
    RelationshipTest(project,
                     units = units,
                     output = "p.value"
                     ) %>%
        as.data.frame() %>% ### transform to a data frame (sometimes better then tibble)
        dplyr::filter(p.value < limit.p.value) %>% ### filter only signif. diff. relations
        dplyr::left_join(unique(project$relationship), by = "edge")  %>% ### join with information on relationships
        dplyr::select(edge, edge_name, p.value) %>%
        dplyr::left_join(EdgSocCMap(project, units=units[1]) %>%
                         dplyr::select(edge, coding_weight)
                         ) %>%
        dplyr::rename(.dots=stats::setNames('coding_weight', units[1])) %>%
        dplyr::left_join(EdgSocCMap(project, units=units[2]) %>%
                         dplyr::select(edge, coding_weight)
                         ) %>%
        dplyr::rename_(.dots=stats::setNames('coding_weight', units[2])) %>%
        replace(is.na(.), 0)
}


##' Summary table on concept comparisons by indicator
##'
##' This function produce a summary table based on concept comparisons by indicator.
##' @title Summary table on concept comparisons by indicator
##' @inheritParams ConceptIndicators
##' @return A data frame
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ## More documents are needed for running this function
##' ## ConceptIndicSummary(my.project, units = c("Belgium", "Québec"))
##' @export 
ConceptIndicSummary <- function(project,
                               units){
  df <- lapply(units,
         function(x) {
           ConceptIndicators(project, units = x) %>% dplyr::mutate(unit=x)
         }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(num, name, unit, centrality, outdegree, indegree) %>%
    tidyr::gather(indic, value, 4:6) %>%
    as.data.frame()
  
  return(df)
}


##' Summary table on concept comparisons
##'
##' This function produce a summary table based on concept comparisons and is reactive to a limit of p.value beyond which differences are considered as significant and are reported in the table
##' @title Summary table on concept comparisons
##' @inheritParams ConceptTest
##' @inheritParams RelationshipTestSummary
##' @return A data frame
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ## More documents are needed for running this function
##' ## ConceptTestSummary(my.project, units = c("Belgium", "Québec"), 0.6)
##' @export 
ConceptTestSummary <- function(project,
                               units,
                               limit.p.value = 0.05) {
       
        df <- ConceptTest(project,
                      units = units,
                      output = "p.value"
                      ) %>%
            as.data.frame() %>% ### transform to a data frame (sometimes better then tibble)
            dplyr::filter(p.value < limit.p.value) %>%
            dplyr::left_join(ConceptIndicSummary(project, units)) %>%
            tidyr::spread(unit,value)
    
    return(df)
}
