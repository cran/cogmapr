#' ---
#' title: 'Social Cognitive Map and quotes'
#' author: "CogMapR"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: pdf_document
#' params:
#'  project: null
#'  min.weight: null
#'  filters: null
#'  units: null
#'  weighted.icm : null
#' ---

##+ echo = FALSE, warning = FALSE, error = FALSE, message = FALSE

knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      error = FALSE,
                      message = FALSE,
                      fig.width = 7,
                      fig.height = 4,
                      fig.align = 'center',
                      results = "asis")

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


pander::pandoc.header("Parameters", level = 1)

t.params <- rbind(c("min.weight", params$min.weight),
                  c("filters", params$filters),
                  c("units", params$units),
                  c("weighted.icm", params$weighted.icm)
                  )


names(t.params) <- c("Parameter","Value")

pander::pander(t.params)

pander::pandoc.header("Social map",
              level=2)

plotSocCMap(
    SocCMap(
        EdgSocCMap(params$project,
                   min.weight = params$min.weight,
                   filters = NULL,
                   units = params$units,
                   weighted.icm = params$weighted.icm
                   ),
        params$project,
        label="name"
    )
)


pander::pandoc.header("Quotes",
              level=2)

## apply(QuotesSocCMap(params$project,
##                    min.weight = params$min.weight,
##                    filters = NULL,
##                    units = params$units
##                    ),
##       1,
##       function(x) {
##           pander::pandoc.header(paste0(x[["from"]]," --> ",x[["to"]]),level=3)
##           pander::pander("\n\n")
##           pander::pander(paste(x[["doc_id"]]," : ",x[["selected_text"]]))
##           pander::pander("\n\n")
##       }
##       )

silent <- lapply(QuotesSocCMap(params$project,
                   min.weight = params$min.weight,
                   filters = NULL,
                   units = params$units
                   ) %>%
       dplyr::mutate(selected_text_b = paste0(selected_text, " *(weight = ",coding_sign,coding_weight.x," - id = ",doc_id," - coding_id = ",coding_id,")*")) %>%
       dplyr::group_by(edge) %>%
       dplyr::group_split(),
       function(x) {
           pander::pandoc.header(paste0(x[["concept_from_name"]][1]," --> ", x[["concept_to_name"]][1]), level=3)
           pander::pander(paste(x[["selected_text_b"]], collapse = "\n\n -- \n\n"))
           pander::pander("\n\n")
       }
       )
