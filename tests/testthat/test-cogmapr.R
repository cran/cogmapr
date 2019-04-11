project_name <- "a_new_project"
main_path <- paste0(system.file("testdata",package="cogmapr"),'/')
project <- ProjectCMap(main_path,project_name)

context("test-the-cogmapr-package")

test_that("The project is a not empty list", {
    expect_is(project,"list")
    expect_gt(dim(project$documents)[1],0)
})

context("Test social cognitive maps")

test_that("The functions are reactive to options", {
    ## max weight <= n doc
    expect_lte(
        max(EdgSocCMap(project,
                       min.weight = 1,
                       units="all",
                       weighted.icm = FALSE
                       )$coding_weight),
        dim(project$documents)[1])
    expect_lte(
        sum(EdgSocCMap(project,
                       min.weight = 1,
                       units="Farmer",
                       weighted.icm = FALSE,
                       )$coding_weight),
        sum(EdgSocCMap(project,
                       min.weight = 1,
                       units="all",
                       weighted.icm = FALSE,
                       )$coding_weight),
        )
    expect_gt(dim(EdgSocCMap(project,
                             units=c("Farmer","Belgium")
                             )
                  )[1],
              0)
})

context("Test graph theory indicators")

test_that("The functions are reactive to options", {
    expect_is(ConceptIndegree(project, units = 'Québec'), "data.frame")
    expect_is(ConceptOutdegree(project, units = 'all'), "data.frame")
    expect_is(ConceptCentrality(project, units = 'Farmer'), "data.frame")
    expect_is(ConceptIndicators(project), "data.frame")
    expect_is(GraphIndicators(project), "data.frame")
    expect_is(GraphIndicatorsTable(GraphIndicators(project)), "data.frame")
    expect_error(ConceptTest(project,c("Belgium"), "centrality"))

    expect_warning(ConceptTest(project,c("Belgium", "Québec")))
    
    expect_is(ConceptTest(project, c("Belgium", "Québec"), "indegree", output = "raw.data"), "list")
    expect_is(RelationshipTest(project, units = c("Belgium", "Québec")), "data.frame")
    expect_is(RelationshipTest(project,
		    units = c("Belgium", "Québec")
		    ), "data.frame")
    expect_is(RelationshipTest(project,
		    units = c("Farmer", "Stakeholder2"),
		    output = "raw.data"
		    ), "list")
    expect_is(ConceptIndicatorsICM(project), "data.frame")
    expect_is(GraphIndicatorsICM(project), "data.frame")
})


    cmap.soc  <- 

context("Specific functions")

test_that("The functions are reactive to options", {
    ## p <- ggCMap(data.ggCMap(project, min.weight = 2))
    ## expect_true(is.ggplot(p))
    expect_is(SocCMap(
        EdgSocCMap(project, min.weight = 2),
        project
    ), "SocCMap")
})

context("Specific functions")
    
test_that("The functions are reactive to options", {
    expect_false(any(c(1,2,3) %in% RemoveCodings(project,c(1,2,3))$codings$coding_id))
    expect_true(any(c(1,2,3,4) %in% RemoveCodings(project,c(1,2,3))$codings$coding_id))
    })
