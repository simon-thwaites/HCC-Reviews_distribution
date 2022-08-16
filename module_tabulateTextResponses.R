# Module for selection review year

responseTable_ui <- function(id, wc.plot) {
  ns <- NS(id)
  
  tagList(
    title = strong("Responses"),
    
  )
}

responseTable_server <- function(input, output, session, data) {
  
  req(input$season)
  
  ## table data
  t.data <- data.frame(df_userSelected_seasonData()$`What were the standout positives from the season?`)
  colnames(t.data) <- "Responses"
  t.data
  
}