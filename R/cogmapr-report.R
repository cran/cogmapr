## ==================================================================
## Package : cogmapr
## Frederic M. Vanwindekens, CRA-W
## GPL (>= 3)
## ==================================================================

##' Create a serie of reports on Individual Cognitive Maps and quotes
##' ##'
##' Create a serie of reports on Individual Cognitive Maps and quotes. The 'pdf' format will depend on a LaTeX installation. The reports will be stored in a subfolder of your working directory ('./data-output/icm/...').
##' @title Create a serie of reports on Individual Cognitive Maps and quotes
##' @inheritParams EdgCMap
##' @param format A character string, the format of the report you want. 'html' (default), 'pdf'. 
##' @param output_dir A character string, the output directory of the rendered file (see rmarkdown::render())
##' @return A cute message.
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' ReportICM(my.project)
##' ## ReportICM(my.project, "pdf") ## need LaTeX to be installed and working with R.
##' @export
ReportICM <- function(project, output_dir = tempdir(), format = "html"){
    package_location <- system.file(package = "cogmapr")
    n <- 0
    pb <- utils::txtProgressBar(style=3)
    for (id.doc in as.character(project$documents$doc_id)) {
        doc_path <- gsub(".txt","",project$documents$doc_path[project$documents$doc_id == id.doc])
        rmarkdown::render(paste0(package_location,"/rmarkdown/report_icm.R"),
                          params = list(
                              project = project,
                              id.agent = id.doc
                          ),
                          output_dir = output_dir,
                          output_file=paste0("icm_map_quotes_doc_", doc_path,
                                switch(
                                    format,
                                    pdf = '.pdf',
                                    html = '.html',
                                    word = '.docx'
                                )),
                          output_format = switch(
                              format,
                              pdf = rmarkdown::pdf_document(),
                              html = rmarkdown::html_document()
                          ),
                          quiet = TRUE
                          )
        n <- n+1
        utils::setTxtProgressBar(pb, n/length(project$documents$doc_id))
    }
    cat("\n")
    return(paste("Reports of Individual Cognitive Maps produced in",output_dir,"\n\n"))
}


##' Create a serie of reports on Social Cognitive Maps and quotes
##' 
##' Create a serie of reports on Social Cognitive Maps and quotes. The 'pdf' format will depend on a LaTeX installation. The reports will be stored in a subfolder of your working directory ('./data-output/icm/...').
##' @title Create a serie of reports on Individual Cognitive Maps and quotes
##' @inheritParams EdgSocCMap
##' @inheritParams ReportICM
##' @return A cute message.
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##' 
##' ReportSCM(my.project)
##' ReportSCM(my.project, units = "Farmer")
##' ## pdf : need LaTeX to be installed and working with R.
##' ## ReportSCM(my.project, min.weight = 3, format = "pdf")
##' @export
ReportSCM <- function(project, min.weight = 1, filters = NULL, units = "all", weighted.icm = FALSE, format = "html", output_dir = tempdir()){
    package_location <- system.file(package = "cogmapr")
    rmarkdown::render(paste0(package_location,"/rmarkdown/report_scm.R"),
                      params = list(
                          project = project,
                          min.weight = min.weight,
                          filters = filters,
                          units = units,
                          weighted.icm = weighted.icm
                      ),
                      output_dir = output_dir,
                      output_file = paste0("scm_map_quotes_", units,
                                           "_", min.weight,
                                           switch(
                                               format,
                                               pdf = '.pdf',
                                               html = '.html',
                                               word = '.docx'
                                           )),
                      output_format = switch(
                          format,
                          pdf = rmarkdown::pdf_document(),
                          html = rmarkdown::html_document()
                      ),
                      quiet = TRUE
                      )

    return(paste("Reports of Social Cognitive Map produced in ",output_dir))
}


##' Create a useful informational sheet for coding
##' 
##' Create a document with main information of the Qualitative Date Analysis project (id of documents, of edges, of vertices, units, ...). The 'pdf' format will depend on a LaTeX installation. The reports will be stored in a subfolder of your working directory ('./data-output/sheet_coding.ext').
##' @title Create a useful informational sheet for coding
##' @inheritParams EdgCMap
##' @inheritParams ReportICM
##' @return A cute message.
##' @examples
##' project_name <- "a_new_project"
##' main_path <- paste0(system.file("testdata", package = "cogmapr"), '/')
##' my.project <- ProjectCMap(main_path, project_name)
##'
##' SheetCoding(my.project)
##' ## Need a pdf installation
##' ## SheetCoding(my.project, "pdf")
##' @export
SheetCoding <- function(project, format = "html", output_dir = tempdir()){
    package_location <- system.file(package = "cogmapr")
    rmarkdown::render(paste0(package_location,"/rmarkdown/sheet_coding.R"),
                      params = list(
                          project = project
                      ),
                      output_dir = output_dir,
                      output_file = paste0("sheet_coding",
                                           switch(
                                               format,
                                               pdf = '.pdf',
                                               html = '.html',
                                               word = '.docx'
                                           )),
                      output_format = switch(
                          format,
                          pdf = rmarkdown::pdf_document(),
                          html = rmarkdown::html_document()
                      ),
                      quiet = TRUE
                      )

    return(paste("Sheet coding produced in ", output_dir))
}
