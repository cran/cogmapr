#' ---
#' title: 'Individual Cognitive Maps and quotes'
#' author: "CogMapR"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' params:
#'  id.agent: 2
#'  project: null
#' ---

##+ echo = FALSE, warning = FALSE, error = FALSE, message = FALSE

knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      error = FALSE,
                      message = FALSE,
                      fig.width = 7,
                      fig.height = 4,
                      fig.align = 'center',
                      results = "asis"
                      )

##                dpi = 100,
##                dev.args=list(bg='white'), 
##                echo=TRUE,
##                results= "markup",
##                warning = FALSE, 
##                error = TRUE,
##                message = FALSE,
##                cache = FALSE,
##                tidy = FALSE, 
##                tidy.opts=list(width.cutoff=95))


pander::panderOptions("table.style" , "multiline")
## panderOptions("table.style" , "rmarkdown")
## panderOptions("table.split.cells" , 60)
## panderOptions("table.alignment.default" , "right")

## panderOptions("table.split.table" , Inf)
## panderOptions("table.alignment.rownames" , "right")

pander::panderOptions('knitr.auto.asis', FALSE)


##+ echo = FALSE, warning = FALSE, error = FALSE, message = FALSE


pander::pandoc.header(
            paste0("Agent/Interview/Document : ",
                   params$project$documents$doc_path[params$project$documents$doc_id == params$id.agent]," (id n° : ", params$id.agent,")"),
            level=1)


pander::pandoc.header("Individual map",
              level=2)

pander::pandoc.header("Individual map -- 1 year",
              level=3)

plotIndCMap(
    IndCMap(params$project,
            params$id.agent)
)


## TODO check that with the new dev in package
## pandoc.header("Individual map - clustered by concepts classes",
##               level = 2)

## pandoc.header("Individual map - clustered by concepts classes",
##               level = 2)

## ClusteredConceptsICM(params$project,params$id.agent)

## pandoc.header("Individual map - subgraphs by concepts classes",
##               level = 2)

## for (concepts.class in levels(df.vertex.2$class)) {
##     pandoc.header(paste0("Concepts class : ", concepts.class),
##                   level = 3)
    
##     SubgraphClusteredConceptsICM(params$project,
##                                  as.character(params$id.agent), ## without as.character, bug if only 1 relationship
##                                  concepts.class)
    
## }

pander::pandoc.header("Quotes",
              level=2)

## apply(QuotesIndCMap(params$project, params$id.agent),
##       1,
##       function(x) {
##           pandoc.header(paste0(x[["concept_from_name"]]," --> ", x[["concept_to_name"]]), level=3)
##           pander(x[["selected_text"]])
##           pander("\n\n")
##       }
##       )


silent <- lapply(QuotesIndCMap(params$project, params$id.agent) %>%
       dplyr::mutate(selected_text_b = paste0(selected_text, " *(weight = ",coding_sign,coding_weight," - id = ",coding_id,")*")) %>%
       dplyr::group_by(edge) %>%
       dplyr::group_split(),
       function(x) {
           pander::pandoc.header(paste0(x[["concept_from_name"]][1]," --> ", x[["concept_to_name"]][1]), level=3)
           pander::pander(paste(x[["selected_text_b"]], collapse = "\n\n -- \n\n"))
           pander::pander("\n\n")
       }
       )
