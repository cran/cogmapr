#' ---
#' title: 'Coding sheet'
#' author: "CogMapR"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output: pdf_document
#' params:
#'  project: null
#' ---


##+ echo = FALSE, warning = FALSE, error = FALSE, message = FALSE
library(knitr)
library(pander)
## library(gridExtra)

opts_chunk$set(echo = FALSE,
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


panderOptions("table.style" , "multiline")
## panderOptions("table.style" , "rmarkdown")
## panderOptions("table.split.cells" , 60)
## panderOptions("table.alignment.default" , "right")

## panderOptions("table.split.table" , Inf)
## panderOptions("table.alignment.rownames" , "right")

panderOptions('knitr.auto.asis', FALSE)


##+ echo = FALSE, warning = FALSE, error = FALSE, message = FALSE


#' # Concepts

pander(params$project$concepts)

#' # Documents & Units

#' ## Documents"
pander(params$project$documents)

#' ## Units
pander(params$project$units)

#' ## Documents - Units
pander(
params$project$units_documents %>%
    dplyr::left_join(params$project$units) %>%
    dplyr::group_by(doc_path) %>%
    dplyr::summarise(units_list = paste(name, collapse = " - ")) %>%
    dplyr::ungroup() %>%
    as.data.frame()
)




