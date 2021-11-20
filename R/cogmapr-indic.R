## ==================================================================
## Package : cogmapr
## Frederic M. Vanwindekens, CRA-W
## GPL (>= 3)
## ==================================================================

## =====================================
## == Indicator at the concepts level ==
## =====================================

##' Compute the indegree of concepts
##  --------------------------------
##'
##' Compute the indegree of concepts
##' @title Indegrees of concepts
##' @inheritParams EdgSocCMap
##' @return A data frame with the value of the indegree (n) of vertices.
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ConceptIndegree(my.project)
##' @export
ConceptIndegree <- function(project,
                             filters = NULL,
                             units = 'all',
                             weighted.icm = FALSE) {
    ## environment(EdgSocCMap)  <- environment()
    min.weight = 1
    args <- sapply(names(formals(EdgSocCMap)), get, environment())
    df.indegree <- do.call(EdgSocCMap, args) %>%
        dplyr::group_by(to) %>%
        dplyr::summarise(n = sum(coding_weight)) %>%
        dplyr::rename(num = to) %>%
        dplyr::left_join(project$concepts, by = "num") %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::select(num, name, n)
    ## dplyr::join(
    return(df.indegree)
}

##' Compute the outdegree of concepts
##  ---------------------------------
##' 
##' Compute the outdegree of concepts##' @title Outdegrees of concepts
##' @inheritParams EdgSocCMap
##' @return A data frame with the value of the outdegree (n) of vertices.
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ConceptOutdegree(my.project)
##'
##' @export
ConceptOutdegree <- function(project,
                              filters = NULL,
                              units = 'all',
                              weighted.icm = FALSE) {
    ## environment(EdgSocCMap)  <- environment()
    min.weight = 1
    args <- sapply(names(formals(EdgSocCMap)), get, environment())
    df.outdegree <- do.call(EdgSocCMap, args) %>%
        dplyr::group_by(from) %>%
        dplyr::summarise(n = sum(coding_weight)) %>%
        dplyr::rename(num = from) %>%
        dplyr::left_join(project$concepts, by = "num") %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::select(num, name, n)
    ## dplyr::join(
    return(df.outdegree)
}

##' Compute the centrality of concepts
##  ----------------------------------
##'
##' Compute the centrality of concepts
##' @title Centralities of concepts
##' @inheritParams EdgSocCMap
##' @return A data frame with the value of the centrality (n) of vertices.
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ConceptCentrality(my.project)
##'
##' @export
ConceptCentrality <- function(project,
                               filters = NULL,
                               units = 'all',
                               weighted.icm = FALSE) {
    ## environment(EdgSocCMap)  <- environment()
    args.in <- sapply(names(formals(ConceptIndegree)), get, environment())
    args.out <- sapply(names(formals(ConceptOutdegree)), get, environment())

    df.centrality <- do.call(ConceptIndegree, args.in) %>%
        dplyr::bind_rows(do.call(ConceptOutdegree, args.out)) %>%
        dplyr::group_by(num, name) %>%
        dplyr::summarise(n = sum(n)) %>%
        dplyr::arrange(desc(n))
    df.centrality
}

##' Compute the indicators of concepts of a Social Cognitive Map
##  ------------------------------------------------------------
##'
##' Compute the indicators of concepts of a Social Cognitive Map (centrality, indegree, outdegree). It build a user friendly data frame. It includes the 'receiver' and the transmitter character of each vetex. The receiver character of a concept is calculated as the part of the indegree of this concept on its centrality. The transmitter character of a concept is calculated as the part of the outdegree of this concept on its centrality.
##'
##' (add formulae)
##' 
##' @title Concepts indicators of a Social Cognitive Map
##' @inheritParams EdgSocCMap
##' @return A data frame with the value of some indicators linked to vertices of a map.
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ConceptIndicators(my.project)
##' @export
ConceptIndicators <- function(project,
			      filters = NULL,
			      units = 'all',
			      weighted.icm = FALSE) {
  args <- sapply(names(formals(ConceptCentrality)), get, environment())

  df.concept.indic <- do.call(ConceptCentrality, args) %>%
    dplyr::rename(centrality = n) %>%
    dplyr::left_join(do.call(ConceptOutdegree, args), by = c("num", "name")) %>%
    dplyr::rename(outdegree = n) %>%
    dplyr::left_join(do.call(ConceptIndegree, args), by = c("num", "name")) %>%
    dplyr::rename(indegree = n) %>%
    tidyr::replace_na(list(outdegree=0)) %>%
    tidyr::replace_na(list(indegree=0)) %>%
    dplyr::arrange(desc(centrality)) %>%
    dplyr::mutate(receiver = signif(indegree/(indegree+outdegree), 2)) %>%
    dplyr::mutate(transmitter = signif(outdegree/(indegree+outdegree), 2))
  return(df.concept.indic)
}

## ==================================
## == Indicator at the graph level ==
## ==================================

##' Compute the graph indicators of a Social Cognitive Map (at graph level)
##  -----------------------------------------------------------------------
##'
##' Compute some indicators from the graph theory and applies them to a Social Cognitive Map :
##' - dimension : the number of vertices
##' - n_transmitter : the number of transmitter vertices
##' - n_receiver : the number of receiver vertices
##' - n_ordinary : the number of ordinary vertices (transmitter & receiver)
##' - connections : the number of edges
##' - density : ...
##' - complexity_a : ....
##' - complexity_b : ...
##' - hierarchy : ...
##'
##' (== add formulae ==)
##' 
##' Source : Oezesmi & Oezesmi, 2004
##' 
##' @title Graph indicators of a social cognitive map
##' @inheritParams EdgSocCMap
##' @return A data frame with the value of some indicators linked to the map
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' GraphIndicators(my.project)
##' GraphIndicators(my.project, units = "Belgium")
##' GraphIndicators(my.project, units = "Québec")
##' @export
GraphIndicators <- function(project,
			     filters = NULL,
			     units = 'all',
			     weighted.icm = FALSE){

  args <- sapply(names(formals(ConceptCentrality)), get, environment())
  min.weight = 1
  df.concept.indic <- do.call(ConceptIndicators, args)

  l.graph.indic <- list(
    "dimension" = sum(df.concept.indic$centrality != 0),
    "n_transmitter" = sum(df.concept.indic$transmitter == 1),
    "n_receiver" = sum(df.concept.indic$receiver == 1),
    "n_ordinary" = sum(df.concept.indic$transmitter != 0 &
			       df.concept.indic$receiver != 0),
    "connections" = dim(do.call(EdgSocCMap,args))[1]
    )
  df.graph.indic  <- as.data.frame(l.graph.indic) %>%
    dplyr::mutate(density = connections/(dimension * (dimension -1))) %>%
    ## dplyr::mutate(density = connections/(dimension^2)) %>% ## if causal effect on themselves
    dplyr::mutate(complexity_a = (n_receiver / n_transmitter)) %>%
    dplyr::mutate(complexity_b =
		    (sum(df.concept.indic$receiver > 0.5) /
		    sum(df.concept.indic$transmitter > 0.5))
		  ) %>%
    dplyr::mutate(hierarchy = signif((12/((dimension-1)*dimension*(dimension+1)))*
		    sum(((df.concept.indic$outdegree -
			  sum(df.concept.indic$outdegree))/dimension
		    )^2
		    ),2)
		  )
  return(df.graph.indic)
  # mean degree ? variance indegree, variance outdegree ?
}

##' Table of graph indicators of a social cognitive map
##  ---------------------------------------------------
##' 
##' Table of graph indicators of a social cognitive map
##' @title Table of graph indicators of a social cognitive map
##' @param df.graph.indic A data frame, as the output of the function 'GraphIndicators'
##' @return A data frame of graph thery indicator, easier to read (long format)
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' df.graph.indic <- GraphIndicators(my.project)
##' GraphIndicatorsTable(df.graph.indic)
##' @export
GraphIndicatorsTable <- function(df.graph.indic){
  df.graph.indic.table <- as.data.frame(t(df.graph.indic)) %>%
    dplyr::mutate(indicator = c(
		    "Dimension",
		    "Number of transmitter variable(s)",
		    "Number of receiver variable(s)",
		    "Number of ordinary variable(s)",
		    "Number of connection(s)",
		    "Density",
		    "Complexity a",
		    "Complexity b",
		    "Hierarchy")) %>%
    dplyr::rename(value = V1) %>%
    dplyr::select(indicator, value)

  return(df.graph.indic.table)
  }


## ===========================
## == Indicator for the ICM ==
## ===========================

## TODO ADAPT DOC
##' Indegree of vertices by document
##'
##' Indegree of vertices by document
##' @title Indegree of vertices by document
##' @inheritParams EdgIndCMap
##' @return A data frame of Indegree by document (ICM)
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' ConceptIndegreeIndiv(my.project)
##' @export
ConceptIndegreeIndiv <- function(project, min.weight = 1, weighted.icm = FALSE) {

args <- sapply(names(formals(EdgIndCMap)), get, environment())

df.indegree <- do.call(EdgIndCMap, args) %>%
  dplyr::group_by(doc_id,to) %>% 
  dplyr::summarise(n=sum(coding_weight),.groups='keep') %>%
  dplyr::rename(num = to) %>%
  dplyr::left_join(project$concepts, by = "num") %>%
  dplyr::select(doc_id, name, num, n) %>%
  dplyr::rename(indegree = n) %>%
  dplyr::ungroup() %>%
  as.data.frame()

  return(df.indegree)
}


## TODO ADAPT DOC
##' Outdegree of vertices by document
##'
##' Outdegree of vertices by document
##' @title Outdegree of vertices by document
##' @inheritParams EdgIndCMap
##' @return A data frame of Outdegree by document (ICM)
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' ConceptOutdegreeIndiv(my.project)
##' @export
ConceptOutdegreeIndiv <- function(project, min.weight = 1, weighted.icm = FALSE) {

args <- sapply(names(formals(EdgIndCMap)), get, environment())

df.outdegree <- do.call(EdgIndCMap, args) %>%
  dplyr::group_by(doc_id,from) %>% 
  dplyr::summarise(n=sum(coding_weight),.groups='keep') %>%
  dplyr::rename(num = from) %>%
  dplyr::left_join(project$concepts, by = "num") %>%
  dplyr::select(doc_id, name, num, n) %>%
  dplyr::rename(outdegree = n) %>%
  dplyr::ungroup() %>%
  as.data.frame()

  return(df.outdegree)
}


## TODO ADAPT DOC
##' Centrality of vertices by document
##'
##' Centrality of vertices by document
##' @title Centrality of vertices by document
##' @inheritParams EdgIndCMap
##' @return A data frame of Centrality by document (ICM)
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' ConceptCentralityIndiv(my.project)
##' @export
ConceptCentralityIndiv <- function(project, min.weight = 1, weighted.icm = FALSE) {
  ## environment(EdgSocCMap)  <- environment()
  args <- sapply(names(formals(EdgIndCMap)), get, environment())

  df.centrality <- do.call(ConceptIndicIndiv, args) %>%
    dplyr::select(doc_id, name, num, centrality)

  return(df.centrality)
}

## TODO ADAPT DOC
##' Concept Indicators of vertices by document
##'
##' Concept Indicators of vertices by document
##' @title Concept Indicators of vertices by document
##' @inheritParams EdgIndCMap
##' @return A data frame of Concept Indicators by document (ICM)
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' ConceptIndicIndiv(my.project)
##' @export
ConceptIndicIndiv <- function(project, min.weight = 1, weighted.icm = FALSE) {
  ## environment(EdgSocCMap)  <- environment()
  args <- sapply(names(formals(EdgIndCMap)), get, environment())

  df.centrality <- do.call(ConceptIndegreeIndiv, args) %>%
    dplyr::full_join(do.call(ConceptOutdegreeIndiv, args)) %>%
    tidyr::replace_na(list(outdegree=0)) %>%
    tidyr::replace_na(list(indegree=0)) %>%
    dplyr::mutate(centrality = indegree + outdegree)

  return(df.centrality)
}


##' Concept Indicators of vertices by document (tidy data)
##'
##' Concept Indicators of vertices by document (tidy data)
##' @title Concept Indicators of vertices by document (tidy data)
##' @inheritParams EdgCMap
##' @param doc_id "all" (default) or the ID of document to include.
##' @return A data frame (tidy data) with all indicators, their values by document (ICM)
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' ConceptIndicatorsICM(my.project)
##' ConceptIndicatorsICM(my.project, 2)
##' @export
ConceptIndicatorsICM <- function(project, doc_id = "all") {
    
    big.df.icm.indic <- data.frame()

    if (doc_id == "all") {
        doc_id <- project$documents$doc_id
    } else {}

    for (doc_id in doc_id) {
        big.df.icm.indic <- big.df.icm.indic %>%
            dplyr::bind_rows(
                       tidyr::gather(
                                  ConceptIndicators(
                                      project,
                                      filters = list(doc_id = doc_id)),
                                  "indicator", "n", 3:7) %>%
                       dplyr::mutate(doc_id = doc_id)
                   )
    }
    
    return(big.df.icm.indic)
}

##' Graph Indicators of vertices by document (tidy data)
##'
##' Graph Indicators of vertices by document (tidy data)
##' @title Graph Indicators of vertices by document (tidy data)
##' @inheritParams EdgCMap
##' @param doc_id "all" (default) or the ID of document to include.
##' @return A data frame (tidy data) with all indicators, their values by document (ICM)
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' GraphIndicatorsICM(my.project)
##' GraphIndicatorsICM(my.project, 2)
##' @export
GraphIndicatorsICM <- function(project, doc_id = "all") {
  big.df.icm.indic <- data.frame()

  if (doc_id == "all") {
    doc_id <- project$documents$doc_id
  } else {}

  for (doc_id in doc_id) {
    big.df.icm.indic <- big.df.icm.indic %>%
      dplyr::bind_rows(
               tidyr::gather(
                        GraphIndicators(
                          project,
                          filters = list(doc_id = doc_id)),
                        "indicator", "n", 1:9) %>%
               dplyr::mutate(doc_id = doc_id)
             )
  }
  return(big.df.icm.indic)
}
