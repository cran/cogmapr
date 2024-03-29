## ==================================================================
## Package : cogmapr
## Frederic M. Vanwindekens, CRA-W
## GPL (>= 3)
## ==================================================================

## ===============================
## Open Project & Edges Extraction
## ===============================

##' This function import and format the data from a coding tool in a Qualitatve Data Analysis.
##'
##'This function import and format the data from a coding tool in a Qualitatve Data Analysis (QDA).
##' @title Import a Qualitatve Data Analysis project
##' @param main_path The main to your QDA project. It must ended with '/'
##' @param project_name The name of your project (as defined in 'qcoder', the name of the subfolder in your main_path). This name is also used in some filenames in the data_frame subsubfolders.
##' @param coder A character string indicating the coding tool used for coding the QDA. The only tool supported now is 'qcoder'. Earlier version of cogmapr worked with "RQDA" project (no more maintened).
##' @param sep (==Depreciated, with RQDA project==) A character string (often a single character) that is used in RQDA in order to express the relationships between two variables. Default is "_" if codes used in RQDA are of the form : "x_y" (\emph{i.e.} relationship from x to y), but it is possible to use ">", "->", "-->" or even "--->"
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' @return A list
##' @export

ProjectCMap <- function(main_path, project_name, coder = "qcoder", sep = ">"){
    l.project <- list()
    l.project[["name"]] <- project_name
    l.project[["project_path"]] <- paste0(main_path,project_name,"/data_frames/qcoder_documents_",project_name,".rds")
    l.project[["units"]] <- unique(readRDS(paste0(main_path,project_name,"/data_frames/qcoder_units_",project_name,".rds")))
    l.project[["units_documents"]] <- unique(readRDS(paste0(main_path,project_name,"/data_frames/qcoder_unit_document_map_",project_name,".rds"))) %>% ## TODO
        dplyr::mutate(unit_id = as.numeric(as.character(unit_id))) %>%
        dplyr::mutate(doc_path= as.character(doc_path))
    l.project[["documents"]] <- readRDS(l.project[["project_path"]])[c("doc_id","doc_path")]
    l.project[["documents_units"]] <- l.project[["documents"]] %>%
        dplyr::left_join(l.project[["units_documents"]], by = "doc_path") %>%
        dplyr::left_join(l.project[["units"]], by = "unit_id") %>%
        dplyr::rename(unit_name = name) %>%
        dplyr::mutate(name = as.character(doc_id)) ## name is needed for compatibilty with rqda in EdgSocCMap
    l.project[["concepts"]] <- readRDS(paste0(main_path,project_name,"/data_frames/qcoder_concepts_",project_name,".rds")) %>%
        as.data.frame()
    names(l.project[["concepts"]])[1:2] <- c("num","name")
    l.project[["concepts"]]$num <- as.character(l.project[["concepts"]]$num)
    l.project[["codings"]] <- readRDS(paste0(main_path, project_name,
                                             "/data_frames/qcoder_codings_",
                                             project_name, ".rds")) %>%
        dplyr::mutate(edge = paste(concept_from, concept_to, sep="~")) %>%
        as.data.frame()
    l.project[["relationships"]] <- unique(l.project[["codings"]][c("concept_from","concept_to","edge")]) %>%
        dplyr::left_join(l.project[["concepts"]][c("num","name")], by = c("concept_from" = "num")) %>%
        dplyr::rename(concept_from_name = name) %>%
        dplyr::left_join(l.project[["concepts"]][c("num","name")], by = c("concept_to" = "num")) %>%
        dplyr::rename(concept_to_name = name) %>%
        dplyr::mutate(edge_name = paste(concept_from_name,
                                        concept_to_name,
                                        sep=" --> ")
                      )
    l.project[["codes_path"]] <- paste0(main_path,project_name,"/data_frames/qcoder_codes_",project_name,".rds")
    ## l.project[["data_edges"]] <- EdgCMap(l.project[["project_path"]],l.project[["codes_path"]],sep,coder)[c("name","edge","edge.a","from","to")]

    return(l.project)

}


##' Extract all edges from a Qualitative Data Analysis project
##'
##' This function opens a Qualitative Data Analysis (QDA) project and extracts edge information.
##'
##' The coding used in the QDA have to be done using the 'cogamp-dev' branch of the qcoder package (github : 'FrdVnW/qcoder'). devtools::install_github('FrdVnW/qcoder', ref = "cogmap-dev", upgrade = 'never')
##' 
##' @param project A QDA project, a list as generated by the ProjectCMap function.
##' @inheritParams ProjectCMap
##' @return A data.frame with the relationships identified in the interviews. Each relationships ('coding_id') is linked to an agent ('doc_ed'), an edge's name ('edge'), the varible at the origin of the relationships ('concept_from'), the variable at the end of the relationships ('concept_to'), other properties of the relationships ('coding_sign', 'coding_weight', 'coding_class' and 'document_part') and the quotes linked to relationships ('selected_text').
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' EdgCMap(my.project)
##' @export 
EdgCMap <- function(project, sep = ">", coder = "qcoder") {
    if (coder == "qcoder") {
        data.edges <- project[['codings']] %>%
            dplyr::mutate(edge = paste(concept_from, concept_to, sep="~"))
    } else {return(warning("coder not known, sorry!"))}
    ## class(data.edges) <- "EdgCMap"
    return(data.edges)
}


## ==================
## Individual Mapping
## ==================




## TODO : better document

##' Extract all edges from a Qualitative Data Analysis project for individual cognitive mapping
##'
##' This function opens a Qualitative Data Analysis (QDA) project and extracts edge information for individual cognitive mapping
##'
##' The coding used in the QDA have to be done using the 'cogamp-dev' branch of the qcoder package (github : 'FrdVnW/qcoder'). devtools::install_github('FrdVnW/qcoder', ref = "cogmap-dev", upgrade = 'never')
##' 
##' @inheritParams EdgCMap
##' @param min.weight A integer that will determine the minimum (>=) weight of relationships that will be taken into account. Relationships with a lower weight (<) will not be shown. Default is set to 1 (\emph{i.e.} all relationships are shown).
##' @param weighted.icm A boolean. If FALSE, the weight of the relationships in the ICM will be fixed to 1.
##' @return A data.frame 
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' EdgIndCMap(my.project)
##' @export 

EdgIndCMap <- function(project, min.weight = 1, weighted.icm = FALSE) {
    data.edges <- EdgCMap(project)[c("coding_id", "doc_id", "concept_from", "concept_to", "coding_sign", "coding_weight", "coding_class", "document_part","edge")]
    if (weighted.icm == TRUE) {
    } else {
        data.edges$coding_weight <- 1
    }
    data.edges.ind <- data.edges %>%
        dplyr::group_by(doc_id,edge) %>%
        dplyr::summarise(coding_weight=ifelse(weighted.icm,
                                              sum(coding_weight),
                                              as.numeric(coding_weight > 0)),.groups='keep') %>%
        dplyr::summarise(coding_weight=sum(coding_weight)) %>%
        dplyr::filter(coding_weight >= min.weight) %>%
        dplyr::mutate(from = gsub("(\\d+)~(\\d+)","\\1", edge)) %>%
        dplyr::mutate(to = gsub("(\\d+)~(\\d+)","\\2", edge)) %>%    
        as.data.frame()
    return(data.edges.ind)
}





## Creating Individual Cognitive Map
## ---------------------------------

##' Individual Cognitive Mapping
##'
##' 
##' Formatting the data for plotting an Individual Cognitive Map
##'
##' @inheritParams EdgCMap
##' @param doc.id The id of a document 
##' @return  a 'IndCMap' object, a list containing various information that could be use for plotting an Individual Cognitive Map. The most important elements are :
##' \describe{
##' \item{"vertex"}{A list of information on Cognitive Map's variables (i.e. vertices or concepts)}
##' \item{"edg"}{A list of information about relationships}
##' \item{"graph"}{A graphNEL object}
##' \item{"eAttrs"}{A list of graphical attributes of edges}
##' \item{"nAttrs"}{A list of graphical attributes of nodes (vertices)}
##' \item{"gAttrs"}{A list of graphical attributes of the whole graph}
##' }
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' IndCMap(my.project, 1)
##' @export

IndCMap <- function(project, doc.id) {
    data.edges <- project[["codings"]]
    data.vertex <- project[["concepts"]]
    cmap <- list()
    cmap[["agent"]][["name"]] <- doc.id
    cmap[["vertex"]][["num"]] <- data.edges %>%
        dplyr::filter(doc_id == doc.id) %>%
        dplyr::select(doc_id, concept_from, concept_to) %>%
        tidyr::gather("direction","num",2:3) %>%
        dplyr::select(num) %>%
        unique() %>%
        dplyr::pull() %>%
        as.numeric() %>%
        sort()
    cmap[["vertex"]][["numname"]] <- paste(data.vertex$num[data.vertex$num %in%cmap[["vertex"]][["num"]]]," - ",data.vertex$name[data.vertex$num %in% cmap[["vertex"]][["num"]]])
    cmap[["vertex"]][["name"]] <- as.vector(data.vertex$name[data.vertex$num %in% cmap[["vertex"]][["num"]]])
    cmap[["vertex"]][["label"]] <- as.vector(data.vertex$name[data.vertex$num %in% cmap[["vertex"]][["num"]]])
    names(cmap[["vertex"]][["label"]]) <- as.vector(cmap[["vertex"]][["name"]])
    cmap[["graph"]] <- methods::new("graphNEL", nodes = as.character(cmap[["vertex"]][["name"]]),edgemode="directed")   
    cmap[["graph"]] <- graph::addEdge(
        as.vector(data.vertex$name[match(as.vector(data.edges %>%
                                                   dplyr::filter(doc_id == doc.id) %>%
                                                   dplyr::select(concept_from) %>%
                                                   dplyr::pull() %>%
                                                   as.numeric()),
                                         data.vertex$num)]),
        as.vector(data.vertex$name[match(as.vector(data.edges %>%
                                                   dplyr::filter(doc_id == doc.id) %>%
                                                   dplyr::select(concept_to) %>%
                                                   dplyr::pull() %>%
                                                   as.numeric()),
                                         data.vertex$num)]),
        cmap[["graph"]],
        1)
    cmap[["edg"]] <- list()
    cmap[["edg"]][["name"]] <- graph::edgeNames(cmap[["graph"]],recipEdges = "distinct")
    ## cmap[["eAttrs"]] <- list()
    cmap[["eAttrs"]]$dir <- cmap[["edg"]][["dir"]]
    cmap[["eAttrs"]]$arrowhead <- "open"
    cmap[["eAttrs"]]$arrowtail <- "none"
    cmap[["nAttrs"]]$label <- cmap[["vertex"]][["label"]]
    cmap[["gAttrs"]] <- list(
        graph=list(overlap=FALSE,splines=TRUE,rankdir="TB",sep=0.2,ratio="fill") #,size="7,14" before : ,sep=0.2,ratio="fill"
       ,node= list(fixedsize=FALSE,shape="box") ## style="rounded",
       ,edge=list(minlen=1,lwd=1) ## before : minlen=5
        )
    class(cmap) <- "IndCMap"

    return(cmap)
}


## Plotting of Individual Cognitive Map
## ------------------------------------

##' Plotting an Individual Cognitive Map
##'
##' Plotting an Individual Cognitive Map
##'
##' @param ind.cmap An object of class IndCMap, as an output of the IndCMap function
##' @param layoutType Type of graph. See detail in RGraphViz. Can be 'neato', 'dot', 'twopi', 'circo', and 'fdp'. The default is 'neato'. 
##' @param main The title of the map. By default it is "Individual map - Agent's name"
##' @param ... other graphical parameters
##' @return A plot
## ##' @method plot IndCMap
##' @importFrom Rgraphviz plot
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' icm <- IndCMap(my.project, 1)
##'
##' plotIndCMap(icm)
##' @export
plotIndCMap <- function(ind.cmap, layoutType="neato", main = paste('Individual map -', ind.cmap[["agent"]][["name"]]), ...) {
    Rgraphviz::plot(ind.cmap[["graph"]], layoutType,
         attrs = ind.cmap[["gAttrs"]],
         ## nodeAttrs=ind.cmap[["nAttrs"]], ## bug "the character vector must have names"
         ## edgeAttrs=ind.cmap[["eAttrs"]],
         main=main,...)
}


## ==============
## Social Mapping
## ==============

## Edges and weights of a Social Cognitive Map
## ------------------------------------------

##' Aggregation of the relationships identified in a serie of Individual Cognitive Maps
##'
##' This function will produce a data frame that contains all relationships of a serie of Individual Cognitive Maps. The weights of these relationships are calculated.
##'
##' The function can be used to produce a data frame that contains only the relation with a minimum weight or that concerns only a type of agents.
##'
##' @inheritParams EdgCMap
##' @inheritParams EdgIndCMap
##' @param filters A list of named strings that will filter the relationships showed in the SCM. e.g. =list(coding_class = "A_coding_class", document_part = "A_document_part")=. To date, these filters are linked to the nature of relationships.
##' @param units A string vector giving the names of the units (i.e. classes linked to documents) that will be include in the SCM. It is a second type of filter.
##' @return A data frame with four or five variables :
##' ##' \describe{
##' \item{$edge}{The name of the relationship, of the generic form \emph{x~y}}
##' \item{$FACTOR}{If used, the factor the parameter \emph{variable}]. This variable contains then the levels of the factor defined in the parameter \emph{group} used as subset criteria}
##' \item{$weight}{The weight of each relationship}
##' \item{$from}{The number of the vertex at the origin of the relationship}
##' \item{$to}{The number of the vertex at the end of the relationship}
##' }
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' EdgSocCMap(my.project)
##' EdgSocCMap(my.project)
##' EdgSocCMap(my.project, min.weight=3)
##' EdgSocCMap(my.project, min.weight=6, weighted.icm=TRUE)
##' @export
EdgSocCMap <- function(project, min.weight = 1, filters = NULL, units = 'all', weighted.icm = FALSE) {
    data.edges <- EdgCMap(project)[c("coding_id", "doc_id", "concept_from", "concept_to", "coding_sign", "coding_weight", "coding_class", "document_part","edge")]
    if (weighted.icm == TRUE) {
    } else {
        data.edges$coding_weight <- 1
    }
    if (!is.null(filters)) {
        for (n.filter in names(filters)) {
            data.edges  <- data.edges %>%
                dplyr::filter(get(n.filter) %in% filters[[n.filter]])
        }
    }
    if (!('all' %in% units)) {
        units_doc_id <- project$documents_unit %>%
            tidyr::spread(unit_name,unit_id) %>%
            dplyr::filter_at(units, dplyr::all_vars(!is.na(.))) %>%
            dplyr::select(doc_id) %>%
            dplyr::pull()
        
        data.edges  <- data.edges %>%
            dplyr::filter(doc_id %in% units_doc_id)
    }
    data.edges.soc <- data.edges %>%
        dplyr::group_by(doc_id,edge) %>%
        dplyr::summarise(coding_weight=ifelse(weighted.icm,
                                              sum(coding_weight),
                                              as.numeric(coding_weight > 0))) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(edge) %>%
        dplyr::summarise(coding_weight=sum(coding_weight)) %>%
        dplyr::filter(coding_weight >= min.weight) %>%
        dplyr::mutate(from = gsub("(\\d+)~(\\d+)","\\1", edge)) %>%
        dplyr::mutate(to = gsub("(\\d+)~(\\d+)","\\2", edge)) %>%    
        as.data.frame()
    return(data.edges.soc)
}


## EdgSocCMap <- function(data.edges, data.agent, min.weight = 1, group = "all", variable="") {
##     if (group=="all") {
##         data.edges.soc <- aggregate(data.edges$name,by=list(data.edges$edge.a),"length")
##         names(data.edges.soc) <- c("edge","weight")
##     }
##     else {
##         data.edges.b <- merge(data.edges,data.agent[,c("name",variable)])
##         data.edges.soc.b <- aggregate(data.edges.b$name,by=list(data.edges.b$edge.a,data.edges.b[[variable]]),"length")
##         names(data.edges.soc.b) <- c("edge",variable,"weight")
##         data.edges.soc <- data.edges.soc.b[data.edges.soc.b[[variable]]==group,]
##     }
    
##     data.edges.soc <- subset(data.edges.soc,data.edges.soc$weight>=min.weight)
##     data.edges.soc$from <- gsub("(\\d+)~(\\d+)","\\1",data.edges.soc$edge)
##     data.edges.soc$to <- gsub("(\\d+)~(\\d+)","\\2",data.edges.soc$edge)
##     ## data.edges.soc$edge.b <- gsub(sep,"~",data.edges.soc$edge)
##     ## class(data.edges.soc) <- "EdgSocCMap"
##     return(data.edges.soc)
## }


## Creating Social Cognitive Map
## -----------------------------

##' Social Cognitive Mapping
##'
##' Formatting the data for plotting an Social Cognitive Map
##'
##' @param data.edges.soc A data.frame as produced by the EdgSocCMap function
##' @inheritParams EdgSocCMap
##' @param label A character string that defines the text that will be print in the variables (vertex) of the cognitive maps. It can be "num", "name" or "numname" (which is of the form "NUM - Name"). The default is "num"
##' @param minlen A graphical parameter that defines a relative lenght between the variable of the cognitive maps. See help from RGraphViz package.
##' @param fontsize The fontsize of vertices (concepts), in r-base plot
##' @param shape The shape of the verices (concepts), in r-base plot
##' @return a 'SocCMap' object, a list containing various information that could be use for plotting an Individual Cognitive Map. The most important elements are :
##' \describe{
##' \item{"vertex"}{A list of information on Cognitive Map's variables (i.e. vertices)}
##' \item{"edg"}{A list of information about relationships}
##' ##' \item{"graph"}{A graphNEL object}
##' \item{"eAttrs"}{A list of graphical attributes of edges}
##' \item{"nAttrs"}{A list of graphical attributes of nodes (vertices)}
##' \item{"gAttrs"}{A list of graphical attributes of the whole graph}
##' }
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' edg.scm <- EdgSocCMap(my.project, min.weight=6, weighted.icm=TRUE)
##' SocCMap(edg.scm, my.project)
##' @export
SocCMap <- function(data.edges.soc, project, label = "num", minlen = 1, fontsize = 16, shape = "box") {
    data.vertex <- project[["concepts"]]
    soc.cmap <- list()
    
    soc.cmap[["vertex"]][["num"]] <- sort(as.numeric(unique(c(data.edges.soc$from,data.edges.soc$to))))
    soc.cmap[["vertex"]][["numname"]] <- paste(data.vertex$num[data.vertex$num %in%soc.cmap[["vertex"]][["num"]]]," - ",data.vertex$name[data.vertex$num %in% soc.cmap[["vertex"]][["num"]]])
    soc.cmap[["vertex"]][["name"]] <- as.vector(data.vertex$name[data.vertex$num %in% soc.cmap[["vertex"]][["num"]]])
    soc.cmap[["vertex"]][["label"]] <- soc.cmap[["vertex"]][[label]] # as.vector(data.vertex$NUM[data.vertex$NUM %in% soc.cmap[["vertex"]][["num"]]])
    names(soc.cmap[["vertex"]][["label"]]) <- as.vector(soc.cmap[["vertex"]][["num"]])

    soc.cmap[["edg"]][["weight"]] <- data.edges.soc$coding_weight
    names(soc.cmap[["edg"]][["weight"]]) <- data.edges.soc$edge
    
    soc.cmap[["graph"]] <- methods::new("graphNEL", nodes = as.character(soc.cmap[["vertex"]][["num"]]),edgemode="directed")
    
    soc.cmap[["graph"]] <- graph::addEdge(
        as.character(data.edges.soc$from),
        as.character(data.edges.soc$to),
        soc.cmap[["graph"]],
        1)

    soc.cmap[["edg"]][["name"]] <- graph::edgeNames(soc.cmap[["graph"]],recipEdges = "distinct")
    soc.cmap[["eAttrs"]] <- list()
    soc.cmap[["eAttrs"]]$label <-  soc.cmap[["edg"]][["weight"]]
    soc.cmap[["eAttrs"]]$color <- sapply(soc.cmap[["edg"]][["weight"]],FUN = function (x) {grDevices::grey(1-(x/(max(soc.cmap[["edg"]][["weight"]]))))},simplify = FALSE,USE.NAMES = TRUE)
    soc.cmap[["nAttrs"]]$label <- soc.cmap[["vertex"]][["label"]]
    soc.cmap[["gAttrs"]] <- list(
        graph=list(overlap=FALSE,splines=TRUE, rankdir="TB"), 
       node= list(fixedsize = FALSE, labelloc = "t", shape = shape, fontsize = fontsize, style = "rounded"), ## style="rounded",
       edge=list(minlen=minlen,color="darkgrey",arrowsize=0.5,arrowhead="open",arrowtail="none",dir="forward",labelfontsize=8,lwd=1) # lwd=0.5
        )
    class(soc.cmap) <- "SocCMap"
    return(soc.cmap)
}

## Plotting of Social Cognitive Map
## ------------------------------------

##' Plotting a Social Cognitive Map
##'
##' Plotting a Social Cognitive Map
##'
##' @param soc.cmap An object of class SocCMap, as an output of the SocCMap function
##' @param layoutType Type of graph. See detail in RGraphViz. Can be 'neato', 'dot', 'twopi', 'circo', and 'fdp'. The default is 'neato'. 
##' @param main The title of the map. By default it is "Individual map - Agent's name"
##' @param ... other graphical parameters
##' @return A plot
## ##' @method plot SocCMap
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' edg.scm <- EdgSocCMap(my.project, min.weight=6, weighted.icm=TRUE)
##' scm <- SocCMap(edg.scm, my.project)
##' plotSocCMap(scm)
##' scm <- SocCMap(edg.scm, my.project, label = "name", shape = "plaintext")
##' plotSocCMap(scm)
##' @export
plotSocCMap <- function(soc.cmap, layoutType = "neato", ..., main = "Social map") {
    Rgraphviz::plot(soc.cmap[["graph"]], layoutType,
         attrs = soc.cmap[["gAttrs"]],
         nodeAttrs = soc.cmap[["nAttrs"]],
         edgeAttrs = soc.cmap[["eAttrs"]],
         main = main, ...)   
}


## Get the coordinates of the vertex of a Cognitive Map
## ----------------------------------------------------

##' Coordinates of the vertices of a Cognitive Map
##'
##' Get the coordinates of the vertices of a Cognitive Map. The output of this function can be useful for plotting Cognitive Maps in a personalize ways (as with ggplot2 as done by the ggCMap function of this package)
##'
##' @param soc.cmap An object of class SocCMap, as an output of the SocCMap function
##' @param layoutType Type of graph. See detail in RGraphViz. Can be 'neato', 'dot', 'twopi', 'circo', and 'fdp'. The default is 'neato'.
##' @return A data frame with three variable :
##' \describe{
##' \item{$vertex}{The number of the vertex)}
##' \item{$x}{The x coordinate of the vertex} 
##' \item{$y}{The y coordinate of the vertex} 
##' }
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' edg.scm <- EdgSocCMap(my.project, min.weight=6, weighted.icm=TRUE)
##' 
##' scm <- SocCMap(edg.scm, my.project, label = "name", shape = "plaintext")
##' coordCMap(scm)
##' @export
coordCMap <- function(soc.cmap, layoutType = "neato") {
    l.points <- lapply(
        Rgraphviz::AgNode(
                       Rgraphviz::agopen(soc.cmap[["graph"]], name = "foo", layoutType = layoutType
                                         )), Rgraphviz::getNodeCenter)

    names(l.points) <- unlist(
        lapply(Rgraphviz::AgNode(Rgraphviz::agopen(
                                                soc.cmap[["graph"]], name = "foo", layoutType = layoutType
                                            )), Rgraphviz::name))

    ds.points <- as.data.frame(t(as.data.frame(lapply(l.points, Rgraphviz::getPoints))))
    ds.points$concept <- names(l.points)
    names(ds.points)[c(1,2)] <- c("x","y")
    ## class(ds.points) <- coordCMap     
    return(ds.points)
}


## Plotting of Social Cognitive Map as a ggplot Graph
## --------------------------------------------------

##' Get all important data for plotting a Cognitive Map in ggplot
##'
##' Get all important data for plotting a Cognitive Map in ggplot
##' @title Data collection for ggCMap
##' @inheritParams EdgSocCMap
##' @inheritParams SocCMap
##' @inheritParams coordCMap
##' @param vertex.filter A vector of integers or characters given the 'id' of vertices (concepts) that will be included in the map. By default, all vertices are included (vertex.filter = NULL) 
##' @param edge.filter A vector of characters given the name "i~j" of edges (relationships from "i" to "j") that will be included in the map. By default, all edges are included (edge.filter = NULL)
##' @param limit.to.filters A logical that will impact the position of the vertices. FALSE (the default) will filter vertices and edges (vertex.filter, edge.filter) keeping the position they would have in the unfiltered cognitive map (interesting with background). TRUE will fully re-compute the position of the vertices, building a cognitive map in its own (better readability).
##' @param level 0 or 1. Filter the edge/vertices at x level around the filtered edges/vertices (==Not implemented yet==)
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' data.ggCMap(my.project)
##' data.ggCMap(my.project, min.weight = 3)
##' data.ggCMap(my.project, edge.filter = "4")
##' data.ggCMap(my.project, units = "Belgium")
##' @return A list of two data frames :
##' -edges
##' -vertex.
##' In each of these data frames, the main columns are linked to the coordinates of vertex (x, y, x.from, y.from, x.to, y.to)
##' @export 
data.ggCMap <- function(project, min.weight = 1,
                   filters = NULL, units = 'all', weighted.icm = FALSE,
                   label = 'name', minlen = 1, fontsize = 16, shape = "box",
                   layoutType = 'neato',
                   vertex.filter = NULL, edge.filter = NULL,
                   limit.to.filters = FALSE, level = 0
                   ) {
    
    args <- sapply(names(formals(EdgSocCMap)), get, environment())

    if (limit.to.filters) {
        
        vertex.hl <- character()

        if (!is.null(vertex.filter)) {
            vertex.hl <- c(vertex.hl, vertex.filter)
        }
        
        if (!is.null(edge.filter)) {
            vertex.hl <- unique(c(vertex.hl, unlist(strsplit(edge.filter,'~'))))
        }
        
        args$project$codings  <- args$project$codings %>%
            dplyr::filter(concept_from %in% vertex.hl | concept_to %in% vertex.hl)
        
    }

    data.edges.soc <- do.call(EdgSocCMap, args)

    args.b <- sapply(names(formals(SocCMap)), get, environment())

    if (dim(data.edges.soc)[1] < 2) {stop("Only one relationship, try lower min.weigth")}
    
    data.vertex.soc <- project$concepts %>%
        dplyr::left_join(coordCMap(do.call(SocCMap, args.b)), by = c("num" = "concept"))
    
     data.edges.soc <- data.edges.soc %>%
        merge(data.vertex.soc[,c("num","name", "x","y")],
              by.x="to",by.y="num") %>%
        merge(data.vertex.soc[,c("num","name","x","y")],
              by.x="from",by.y="num",
              suffixe=c(".to",".from"))

    data.edges.soc$midX <- apply(data.edges.soc[c("x.from","x.to")], 1, function(x) {jitter(mean(x))})
    data.edges.soc$midY <- apply(data.edges.soc[c("y.from","y.to")], 1, function(x) {jitter(mean(x))})
   
    if (all(!limit.to.filters,
            any(!is.null(vertex.filter),!is.null(edge.filter)))) {
        vertex.hl <- character()
        if (!is.null(vertex.filter)) {
            vertex.hl  <- c(vertex.hl, vertex.filter)
        }
        if (!is.null(edge.filter)) {
            vertex.hl  <- unique(c(vertex.hl, unlist(strsplit(edge.filter,'~'))))
        }
        
        data.edges.soc  <- data.edges.soc %>%
            dplyr::filter(from %in% vertex.hl | to %in% vertex.hl)
                
        data.vertex.soc  <- data.vertex.soc %>%
            dplyr::filter(num %in% c(data.edges.soc$from, data.edges.soc$to))
    }
    if (dim(data.edges.soc)[1] == 0) {stop("Too filtered, no edge")}
    if (dim(data.vertex.soc)[1] == 0) {stop("Too filtered, no vertex")}

    l.data.ggCMap <- list(
                      edges = data.edges.soc,
                      vertex = data.vertex.soc)

    return(l.data.ggCMap)
}


##' Plotting the a social cognitive map using ggplot2
##'
##' Plotting the a social cognitive map using ggplot2
##' @title Plot a social cognitive map using ggplot2
##' @param data A list, the output of the 'data.ggCMap' function, containing all useful vertex and edge information for the cognitive maps. 
##' @param size.concepts Size of the dot linked to vertices
##' @param size.labels Size of the labels of vertices
##' @param size.edges Size of the labels of the weight of edges
##' @param size.arrows Size of arrows (head)
##' @param alpha.arrows The transparency of arrows.
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' df.scm <- data.ggCMap(my.project, edge.filter = "4")
##' ggCMap(df.scm)
##' @return A ggplot of a social cognitive map
##' @export 
ggCMap <- function(data,
                   size.concepts = 4, size.labels = 4, size.edges = 4, size.arrows = 4,
                   alpha.arrows = 0.3
                   ) {
    
    data.edges.soc <- data$edges
    data.vertex.soc <- data$vertex
    
    pnet <- (ggplot2::ggplot()
        + geom_point(aes(x, y, color = concept.class),
                     size = size.concepts, data = data.vertex.soc) # , size=degree
        + geom_segment(aes(x = x.from , y = y.from,
                                  xend = x.to, yend = y.to, size = coding_weight),
                       arrow = arrow(angle = 15, length = unit((size.arrows/5), "inches"),
                                     type = "closed"),
                       data = data.edges.soc, colour = "black", alpha = alpha.arrows,
                       show.legend = FALSE) # ,alpha=0.1 alpha : for pdf only
        + geom_text(aes(x, y, label = name, hjust = (x/max(x)), color = concept.class), ## 
                    data = data.vertex.soc,
                    size = size.labels,
                    vjust = -1, show.legend = FALSE) #,size=4
        + geom_label(aes(x = midX, y = midY, label = coding_weight),
                    data = data.edges.soc,
                    size = size.edges
                    )
        + scale_colour_discrete(l = 40, name = "Concept category : ")
        ## + labs(title=paste("Social map - Unit(s) =", units, " - min. weight=", min.weight))
        + theme(
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              plot.margin = unit(c(2,2,2,2), "pt"),
              ## legend.position = "bottom",
              legend.title = element_text(size=(3*size.labels), face="bold"),
              legend.text = element_text(size=(3*size.labels)),
              plot.title = element_text(hjust=0,size=(3*size.labels), face="bold"),
              axis.ticks =  element_blank(),
              axis.title.x= element_blank(),
              axis.title.y= element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()
          )
    )
    return(pnet)   
}


##' ggplot background of a map (used in highlighted maps)
##'
##' ggplot background of a map (used in highlighted maps). It is like a ghost map.
##' @title Ghost cognitive map
##' @inheritParams ggCMap  
##' @param map.color The unique color of all concepts and labels of the ghost map 
##' @return A plot
##' @import ggplot2
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' df.scm <- data.ggCMap(my.project, edge.filter = "4")
##' ggCMap.bg(df.scm)
##' @export 
ggCMap.bg <- function(data,
                   size.concepts = 4, size.labels = 4, size.edges = 4, size.arrows = 4,
                   map.color = "grey50"
                   ) {
    
    data.edges.soc <- data$edges
    data.vertex.soc <- data$vertex
    
    pnet <- (ggplot()
        + ggplot2::geom_point(aes(x, y),
                     size = size.concepts, data = data.vertex.soc,
                     colour = map.color) # , size=degree
        + geom_segment(aes(x = x.from , y = y.from,
                                  xend = x.to, yend = y.to, size = coding_weight),
                       arrow = grid::arrow(angle = 15, length = unit((size.arrows/5), "inches"),
                                     type = "closed"),
                       data = data.edges.soc,
                       colour = map.color,
                       show.legend = FALSE) # ,alpha=0.1 alpha : for pdf only
        + geom_text(aes(x, y, label = name), ## , hjust = (x/max(x))
                    data = data.vertex.soc,
                    size = size.labels,
                    colour = map.color,
                    vjust = -1, show.legend = FALSE) #,size=4
        ## + labs(title=paste("Social map - Unit(s) =", units, " - min. weight=", min.weight))
        + theme(
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              plot.margin = unit(c(2,2,2,2), "pt"),
              ## legend.position = "bottom",
              legend.title = element_text(size=(3*size.labels), face="bold"),
              legend.text = element_text(size=(3*size.labels)),
              plot.title = element_text(hjust=0,size=(3*size.labels), face="bold"),
              axis.ticks =  element_blank(),
              axis.title.x= element_blank(),
              axis.title.y= element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()
          )
    )
    return(pnet)   
}


##' A cognitive map where some concepts or relationships are highlighted
##'
##' A cognitive map where some concepts or relationships are highlighted. The highlighted elements are those who are filtered (see edge.filter, vertex.filter). A background (ghost map) will be show by default. The parameter 'limit.to.filters' can be set as 'TRUE' for only showing the filtered elements. 
##' @title Highlighted cognitive map
##' @inheritParams data.ggCMap
##' @inheritParams ggCMap
##' @inheritParams ggCMap.bg
##' @return A plot
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' ggCMap.hl(my.project, vertex.filter = 2)
##' @export 
ggCMap.hl <- function(project, min.weight = 1,
                   filters = NULL, units = 'all', weighted.icm = FALSE,
                   label = 'name', minlen = 1, fontsize = 16, shape = "box", layoutType = "neato",
                   vertex.filter = NULL, edge.filter = NULL,
                   limit.to.filters = FALSE, level = 0,
                   size.concepts = 4, size.labels = 4, size.edges = 4, size.arrows = 4,
                   alpha.arrows = 0.3, map.color = grDevices::grey(0.8)
                   ) {
    args.data <- sapply(names(formals(data.ggCMap)), get, environment())

    if (limit.to.filters) {
        pnet <- ggCMap(
            do.call(data.ggCMap,
                    args.data),
            size.concepts = size.concepts,
            size.labels = size.labels,
            size.edges = size.edges,
            size.arrows = size.arrows,
            alpha.arrows = alpha.arrows
        )
        ## todo : better code here above !!
    } else {    
        args.data.bg <- args.data
        args.data.bg$vertex.filter <- NULL
        args.data.bg$edge.filter <- NULL
        
        args.bg <- sapply(names(formals(ggCMap.bg)), get, environment())
        args.bg$data <- do.call(data.ggCMap, args.data.bg)

        data.hl <- do.call(data.ggCMap, args.data)
        data.edges.soc <- data.hl$edges
        data.vertex.soc <- data.hl$vertex
        
        pnet <- (do.call(ggCMap.bg, args.bg)
            + geom_point(aes(x, y, color = concept.class),
                         size = size.concepts, data = data.vertex.soc) # , size=degree
            + geom_segment(aes(x = x.from , y = y.from,
                               xend = x.to, yend = y.to, size = coding_weight),
                           arrow = arrow(angle = 15, length = unit((size.arrows/5), "inches"),
                                         type = "closed"),
                           data = data.edges.soc, colour = "black", alpha = alpha.arrows,
                           show.legend = FALSE) # ,alpha=0.1 alpha : for pdf only
            + geom_text(aes(x, y, label = name, color = concept.class), ## hjust = (x/max(x)), 
                        data = data.vertex.soc,
                        size = size.labels,
                        vjust = -1, show.legend = FALSE) #,size=4
            + geom_label(aes(x = midX, y = midY, label = coding_weight),
                         data = data.edges.soc,
                         size = size.edges
                         )
            + scale_colour_discrete(l = 40, name = "Concept category : ")
            ## + labs(title=paste("Social map - Unit(s) =", units, " - min. weight=", min.weight))
            + theme(
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  plot.margin = unit(c(2,2,2,2), "pt"),
                  ## legend.position = "bottom",
                  legend.title = element_text(size=(3*size.labels), face="bold"),
                  legend.text = element_text(size=(3*size.labels)),
                  plot.title = element_text(hjust=0,size=(3*size.labels), face="bold"),
                  axis.ticks =  element_blank(),
                  axis.title.x= element_blank(),
                  axis.title.y= element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank()
              )
        )
    }
    return(pnet)   
}
