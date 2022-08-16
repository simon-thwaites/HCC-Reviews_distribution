# Module for selection review year

selectSeason_ui <- function(id, season.list) {
  ns <- NS(id)
  
  tagList(
    title = strong("Select Season for Review Data"),
    selectInput(ns("selectSeason"), "Season Selection", c("Select ..."="", season.list$response.year))#,
    # selectInput(ns("selectID"), "StudyID", c("Select by StudyID"="", mixedsort(ID.list$studyID))), # already ordered but mixedsort() was used here before ordering the studyIDs
    # selectInput(ns("selectUR"), "UR Number", c("Select by UR Number"="", mixedsort(UR.list$`UR Number`)))
  )
}

selectSeason_server <- function(input, output, session, data) {
  
  observe({

    x <- input$selectSeason
    season <- dplyr::filter(data(), "response.year" == x) %>%
      select("response.year")

    season <- season[[1]]
    cat(file=stderr(), "selected season is", season, "\n")

    # # Update selected
    # updateSelectInput(session, "selectUR",
    #                   selected = UR
    # )
  })
  # 
  # 
  # observe({
  #   
  #   x <- input$selectID
  #   UR <- dplyr::filter(data(), `Study ID` == x) %>%
  #     select(`UR Number`)
  #   
  #   UR <- UR[[1]]
  #   
  #   # Update selected
  #   updateSelectInput(session, "selectUR",
  #                     selected = UR
  #   )
  # })
  # 
  # observe({
  #   
  #   x <- input$selectID
  #   UR <- dplyr::filter(data(), `Study ID` == x) %>%
  #     select(`UR Number`)
  #   
  #   UR <- UR[[1]]
  #   
  #   # Update selected
  #   updateSelectInput(session, "selectUR",
  #                     selected = UR
  #   )
  # })
  # 
  # # observer to update the selection of ID based on UR <---
  # observe({
  #   
  #   x <- input$selectUR
  #   ID <- dplyr::filter(data(), `UR Number` == x) %>%
  #     select(`Study ID`)
  #   
  #   ID <- ID[[1]]
  #   
  #   # Update selected
  #   updateSelectInput(session, "selectID",
  #                     selected = ID
  #   )
  # })
  # 
  return(
    list(
      selectedSeason <-  reactive({ input$selectSeason })
    )
  )
}