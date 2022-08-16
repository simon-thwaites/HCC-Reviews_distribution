# Hectorville Cricket Club
# Player Review app
# simon.thwaites.biomech@gmail.com
# Created: 22/11/2021

library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)

# library(wordcloud2)
library(wordcloud)
library(tm) # text mining
library(dplyr)
library(textclean) # to handle contractions

source("getResponses.R")
source("pasteYearSummary.R")
source("createTDM.R")

# library("lubridate")
# library("timevis")

# initialise var to store each year as list of tibble
allYears <- list()

yearChoices <- matrix(data = NA, nrow = nResponses + 1, ncol = 2)
yearChoices[1, 1] <- "Select season ..."
yearChoices[1, 2] <- ""
for (ii in 1:nResponses){
  string <- substring(all_urls$response.year[ii], 4)
  string <- gsub("\\.", "/", string) # want to display in app as season 20/21, etc, anmd need the \\ to escape the '.'
  
  ### appear old > recent
  # yearChoices[ii + 1, 1] <- paste0("Season ", string)
  # yearChoices[ii + 1, 2] <- all_urls$response.year[[ii]]
  
  ### appear recent > old
  yearChoices[(nResponses + 2) - ii, 1] <- paste0("Season ", string)
  yearChoices[(nResponses + 2) - ii, 2] <- all_urls$response.year[[ii]]
  
  
  ## adding to the list of tibbles for all the data
  yr.temp <- all_urls$response.year[[ii]] # get the yr.18.19 string as a a temp variable
  addYearColTemp <- resp[[yr.temp]] %>%   # add a Season column to the data
    add_column(Season = string)
  allYears[[yr.temp]] <- addYearColTemp   # assign to the list of tibbles
  
}

# bind the rows of the tibble list for all years
allYears <- bind_rows(allYears)

choicesNamed <- yearChoices[, 2]
names(choicesNamed) <- yearChoices[, 1]

# names to override existing Gform columns
newNames <- c("teamPerformance",                                       
              "yourPerformance",                                                                    
              "training",                                                                             
              "presentationNights",                                                      
              "selectionNights",                                                         
              "clubFunctions",                                                                        
              "gamedayPrep")

# map the new colnames to the selection on screen
plotChoices <- c("Team Performance" = "teamPerformance",                                       
                 "Your Performance" = "yourPerformance",                                                                    
                 "Training" = "training",                                                                             
                 "Presentation Nights" = "presentationNights",                                                      
                 "Selection Nights" = "selectionNights",                                                         
                 "Club Functions" = "clubFunctions",                                                                        
                 "Gameday Preparation" = "gamedayPrep")

text.colnames <- c("text.positives",
                   "text.negatives",
                   "text.training",
                   "text.presentations",
                   "text.selections",
                   "text.gamedayprep",
                   "text.funcideas",
                   "text.comments",
                   "text.moment")


# plot module ----
summaryPlots_ui <- function(id) {
  
  fluidRow(
    plotlyOutput(NS(id, "plot"))
  )
  
}

summaryPlots_server <- function(id, allYears_df, allYears_df_selection, allYears_df_newName, summaryPlotYearsSelection, summaryGroupByGrade) {
  # summaryPlotYearsSelection is a reacctive from outside the module so must be passed directly as they cannpt access reactives outside of the module namespace
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderPlotly({
      plotData <- select(allYears_df, allYears_df_selection, `Grade most played`, Season) %>% 
        filter(Season %in% summaryPlotYearsSelection())
      # filter(Season %in% summaryPlotYearsSelection())
      
      colnames(plotData) <- c(allYears_df_newName, "Grade", "Season") # assign new col names
      
      plotData <- plotData %>%
        mutate(Season = factor(Season), # make Season and Grade factors for plotting
               Grade = factor(Grade))
      
      if (summaryGroupByGrade() == TRUE) {
        
        df_mean <- plotData %>%
          group_by(Grade, Season) %>%
          summarize(average = round(mean(.data[[allYears_df_newName]]), 2) ) %>%
          ungroup()
        
        p <- plotData %>%
          ggplot(aes(x = Season, y = .data[[allYears_df_newName]], fill = Grade)) +
          geom_boxplot(alpha = 0.5) +
          
          geom_point(data = df_mean,
                     mapping = aes(x = Season, y = average),
                     color="red",
                     position = position_dodge2(width = 0.7,
                                                preserve = "single"),
                     show.legend = FALSE) +
          geom_line(data = df_mean,
                    mapping = aes(x = Season, y = average, group=Grade, color = Grade),
                    position = position_dodge2(width = 0.7,
                                               preserve = "single"),
                    show.legend = FALSE)
        
        ggplotly(p) %>%
          layout(boxmode = "group")
        
      } else {
        df_mean <- plotData %>%
          group_by(Season) %>%
          summarize(average = round(mean(.data[[allYears_df_newName]]), 2) ) %>%
          ungroup()
        
        p <- plotData %>%
          ggplot(aes(x = Season, y = .data[[allYears_df_newName]])) +
          geom_boxplot(alpha = 0.5) +
          
          geom_point(data = df_mean,
                     mapping = aes(x = Season, y = average),
                     color="red",
                     show.legend = FALSE) +
          geom_line(data = df_mean,
                    mapping = aes(x = Season, y = average, group = 1), # want to connect all the data points and we can specify that using group=1
                    color = "red",
                    show.legend = FALSE)
        
        ggplotly(p)
      }
      
      
      
    })
  })
}



# Dashboard UI ------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "HCC Player Reviews"),
  dashboardSidebar(
    sidebarMenu(
      # setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      # Menu items
      menuItem(
        "By Season", tabName = "bySeason", icon = icon("binoculars")
      ),
      menuItem(
        "Overall Summary", tabName = "summary", icon = icon("chart-line")
      ),
      menuItem(
        "Generate Reports", tabName = "report", icon = icon("clipboard-check")
      )
    )
  ),
  
  # Dashboard body ----------------------------------------------------------
  dashboardBody(
    tabItems(
      ##### By season UI ####
      tabItem(tabName = "bySeason",
              fluidRow(
                box(
                  width = 12,
                  selectInput(inputId = "season",        # select season input
                              label = "Select Season:",
                              choices = choicesNamed)
                )
              ),
              fluidRow(
                tabBox(          # create some tabs to select from quantitative, text, and all data
                  width = 12,
                  # title = "Select Output",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1",
                  tabPanel(
                    "Quantitative Summary", 
                    # this will have grey 'code like box'
                    verbatimTextOutput("yearSummary"),
                    
                    fluidRow(
                      column(width = 6,
                             plotlyOutput("plotPieTeamPlayed")), # for a/b/c/other grade most played
                      column(width = 6,
                             plotlyOutput("plotPieNextSeason"))  # playing next season?
                      
                    ),
                    checkboxGroupInput("varSelection",
                                       "Select metrics to plot:",
                                       choices = plotChoices,
                                       selected = newNames),
                    plotlyOutput("plotYearStats")
                  ),
                  tabPanel(
                    "Text Responses",
                    sliderInput("wordcloud.freq",
                                "Minimum Frequency:",
                                min = 1,  max = 10, value = 2),
                    sliderInput("wordcloud.max",
                                "Maximum Number of Words:",
                                min = 1,  max = 100,  value = 50),
                    
                    h4("What were the standout positives from the season?"),
                    fluidRow(column(width = 6, align = "center", plotOutput("wc.plot.pos", height="700px")), # add in width = "100%", height="700px" into plotOutput calls to chagne size of the image
                             column(width = 6, tableOutput("pos.table"))),
                    h4("What were the standout negatives from the season?"),
                    fluidRow(column(width = 6, align = "center", plotOutput("wc.plot.neg", height="700px")),
                             column(width = 6, tableOutput("neg.table"))),
                    h4("How could we improve training?"),
                    fluidRow(column(width = 6, align = "center", plotOutput("wc.plot.training", height="700px")),
                             column(width = 6, tableOutput("train.table"))),
                    h4("How could we improve presentation nights?"),
                    fluidRow(column(width = 6, align = "center", plotOutput("wc.plot.pres", height="700px")),
                             column(width = 6, tableOutput("pres.table"))),
                    h4("How could we improve selection nights?"),
                    fluidRow(column(width = 6, align = "center", plotOutput("wc.plot.selections", height="700px")),
                             column(width = 6, tableOutput("selec.table"))),
                    h4("How could we improve game day preparation?"),
                    fluidRow(column(width = 6, align = "center", plotOutput("wc.plot.gamedayprep", height="700px")),
                             column(width = 6, tableOutput("gameday.table"))),
                    h4("Do you have any ideas for Club functions?"),
                    fluidRow(column(width = 6, align = "center", plotOutput("wc.plot.clubfunc", height="700px")),
                             column(width = 6, tableOutput("clubfunc.table"))),
                    h4("Any other comments?"),
                    fluidRow(column(width = 6, align = "center", plotOutput("wc.plot.comment", height="700px")),
                             column(width = 6, tableOutput("comment.table"))),
                    h4("What was your moment of the season?"),
                    fluidRow(column(width = 6, align = "center", plotOutput("wc.plot.momentseason", height="700px")),
                             column(width = 6, tableOutput("moment.table")))
                  ),
                  tabPanel("All Responses", DT::dataTableOutput("table"))
                )
              )
      ),
      
      #### Overall summary UI ####
      tabItem(tabName = "summary",
              h2("Summary tab"),
              fluidRow(
                # make checkboxes for seasons to include in summary plots
                column(6, checkboxGroupInput("summary.yearSelection",
                                             "Select seasons to include:",
                                             choices = unique(allYears$Season),
                                             selected = unique(allYears$Season))
                ),
                # check box to select if split by grades
                column(6, checkboxInput("summary.groupByGrade",
                                        "Group by grade played?",
                                        value = FALSE))
              ),
              h3("Team Performance"),
              summaryPlots_ui("summary.plot.teamPerf"),
              h3("Individual Performance"),
              summaryPlots_ui("summary.plot.yourPerf"),
              h3("Training"),
              summaryPlots_ui("summary.plot.training"),
              h3("Presentation Nights"),
              summaryPlots_ui("summary.plot.presentationNights"),
              h3("Selection Nights"),
              summaryPlots_ui("summary.plot.selectionNights"),
              h3("Club Functions"),
              summaryPlots_ui("summary.plot.clubFunctions"),
              h3("Gameday Preparation"),
              summaryPlots_ui("summary.plot.gamedayPrep")
      ),
      tabItem(tabName = "report",
              fluidRow(
                box(width = 12,
                    h3("Generate Report"),
                    h4("Generate report based on all dashboard selections (season selection, group by grade played, etc"),
                    downloadButton("reportSummary", "Generate Report")
                )
              )
      )
      
    )
  )
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #### define reactives ####
  
  # reactive dataframe for selected season data
  df_userSelected_seasonData <- reactive({
    req(input$season)
    resp[[input$season]]
  })
  
  # reactive dataframe with only the quantitative metrics
  df_quant_seasonData <- reactive({
    req(input$season)
    
    df_userSelected_seasonData()[, 3:9] %>% 
      `colnames<-` (newNames) %>%        # special piping with setting colnames
      tibble::rowid_to_column("ID") %>%  # add id column
      pivot_longer(!ID, names_to = "ScoreName", values_to = "ScoreValue")  # make long
  })
  
  # reactive to change the levels of the quant_data for plotting based on only selected variables
  levels_react <- reactive({
    req(input$season)
    input$varSelection
  })
  
  # output DT table for selected season data
  output$table <- DT::renderDataTable(
    df_userSelected_seasonData()[, -1], # lose timestamp column 
    options = list(scrollX = TRUE)
  )
  
  # output text for year summaries
  output$yearSummary <- renderPrint({
    req(input$season)
    pasteYearSummary(df_userSelected_seasonData)
  })
  
  # reactives for summary plotting selections
  # summaryPlotYearsSelection <- reactive({
  #     req(input$summary.yearSelection)
  #     input$summary.yearSelection
  # })
  
  ## boxplots
  # output$plotYearStats <- renderPlotly({
  #     req(input$season)
  #     plotData <- df_quant_seasonData() %>%
  #         filter(ScoreName %in% input$varSelection)
  #     
  #     # make order of Name an ordered factor to stop ggplot reordering the xData
  #     plotData$ScoreName <- ordered(plotData$ScoreName, levels = newNames)
  # 
  #     p <- plotData %>%
  #         plot_ly(y = ~ScoreValue, color = ~ScoreName, type = "box", boxpoints = "all", jitter = 0.2) %>% 
  #         hide_legend()
  # })
  
  
  #### violin plots ####
  output$plotYearStats <- renderPlotly({
    req(input$season)
    
    plotData <- df_quant_seasonData() %>%
      filter(ScoreName %in% input$varSelection)
    # make order of Name an ordered factor to stop ggplot reordering the xData
    plotData$ScoreName <- ordered(plotData$ScoreName, levels = levels_react())
    
    # change the display names of the metrics
    plotData <- plotData %>% 
      mutate(ScoreName = recode(ScoreName, 
                                "teamPerformance" = "Team Performance",                                       
                                "yourPerformance" = "Your Performance",
                                "training" = "Training",
                                "presentationNights" = "Presentation Nights",
                                "selectionNights" = "Selection Nights",
                                "clubFunctions" = "Club Functions",
                                "gamedayPrep" = "Gameday Preparation"))
    
    p <- plotData %>%
      plot_ly(
        x = ~ScoreName,
        y = ~ScoreValue,
        split = ~ScoreName,
        type = 'violin',
        box = list(visible = T),
        opacity = 0.75,
        points = 'all',
        jitter = 0.2,
        meanline = list(visible = T)
      ) %>%
      layout(
        xaxis = list(
          title = "Metric"
        ),
        yaxis = list(
          title = "Score",
          zeroline = T,
          range = c(-1,11)
        )
      ) %>%
      hide_legend()
  })
  
  #### Pie Charts ####
  output$plotPieTeamPlayed <- renderPlotly({
    req(input$season)
    plotData <- df_userSelected_seasonData()
    
    p <- plotData %>% 
      plot_ly(labels = ~`Grade most played`, type = 'pie') %>% 
      layout(title = "Grade most played?")
  })
  
  output$plotPieNextSeason <- renderPlotly({
    req(input$season)
    plotData <- df_userSelected_seasonData()
    
    p <- plotData %>% 
      plot_ly(labels = ~`Do you plan on playing next season?`, type = 'pie') %>% 
      layout(title = "Playing next season?")
  })
  
  #### WORDCLOUD SERVER ####
  ## Define a reactive expression for the text data and do some pre-processing on the contraction terms
  text.data <- reactive({
    req(input$season)
    
    df_userSelected_seasonData() %>% 
      rename(text.positives = `What were the standout positives from the season?`,
             text.negatives = `What were the standout negatives from the season?`,
             text.training = `How could we improve training?`,
             text.presentations = `How could we improve presentation nights?`,
             text.selections = `How could we improve selection nights?`,
             text.gamedayprep = `How could we improve game day preparation?`,
             text.funcideas = `Do you have any ideas for Club functions?`,
             text.comments = `Any other comments?`,
             text.moment = `What was your moment of the season?`) %>% 
      select(all_of(text.colnames)) %>% 
      # Note: Using an external vector in selections is ambiguous.
      # Use `all_of(text.colnames)` instead of `text.colnames` to silence this message.
      # See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      replace_contraction() %>% 
      { gsub("[’]", "", .) }
    # With pipe your data are passed as a first argument to the next function, 
    # so if you want to use it somewhere else you need to wrap the next line in {} and use . as a data "marker".
    
  })
  
  ## Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  word.cloud.scale <- c(3,0.5)
  
  #### wordcloud plots ####
  output$wc.plot.pos <- renderPlot({
    words <- createTDM(text.data(), 1)
    wordcloud_rep(names(words), words,  
                  scale = word.cloud.scale,
                  min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
                  colors = brewer.pal(8, "Dark2"))
  })
  output$wc.plot.neg<- renderPlot({
    words <- createTDM(text.data(), 2)
    wordcloud_rep(names(words), words, scale = word.cloud.scale,
                  min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
                  colors = brewer.pal(8, "Dark2"))
  })
  output$wc.plot.training<- renderPlot({
    words <- createTDM(text.data(), 3)
    wordcloud_rep(names(words), words, scale = word.cloud.scale,
                  min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
                  colors = brewer.pal(8, "Dark2"))
  })
  output$wc.plot.pres<- renderPlot({
    words <- createTDM(text.data(), 4)
    wordcloud_rep(names(words), words, scale = word.cloud.scale,
                  min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
                  colors = brewer.pal(8, "Dark2"))
  })
  output$wc.plot.selections<- renderPlot({
    words <- createTDM(text.data(), 5)
    wordcloud_rep(names(words), words, scale = word.cloud.scale,
                  min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
                  colors = brewer.pal(8, "Dark2"))
  })
  output$wc.plot.gamedayprep<- renderPlot({
    words <- createTDM(text.data(), 6)
    wordcloud_rep(names(words), words, scale = word.cloud.scale,
                  min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
                  colors = brewer.pal(8, "Dark2"))
  })
  output$wc.plot.clubfunc<- renderPlot({
    words <- createTDM(text.data(), 7)
    wordcloud_rep(names(words), words, scale = word.cloud.scale,
                  min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
                  colors = brewer.pal(8, "Dark2"))
  })
  output$wc.plot.comment<- renderPlot({
    words <- createTDM(text.data(), 8)
    wordcloud_rep(names(words), words, scale = word.cloud.scale,
                  min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
                  colors = brewer.pal(8, "Dark2"))
  })
  output$wc.plot.momentseason<- renderPlot({
    words <- createTDM(text.data(), 9)
    wordcloud_rep(names(words), words, scale = word.cloud.scale,
                  min.freq = input$wordcloud.freq, max.words=input$wordcloud.max,
                  colors = brewer.pal(8, "Dark2"))
  })
  
  #### wordcloud all responses ####
  ## tabulate responses for each long answer question
  output$pos.table <- renderTable({
    req(input$season)
    ## make table data a dataframe and change colname for app display
    t.data <- data.frame(df_userSelected_seasonData()$`What were the standout positives from the season?`)
    colnames(t.data) <- "Responses"
    t.data
  })
  output$neg.table <- renderTable({
    req(input$season)
    ## table data
    t.data <- data.frame(df_userSelected_seasonData()$`What were the standout negatives from the season?`)
    colnames(t.data) <- "Responses"
    t.data
  })
  output$train.table <- renderTable({
    req(input$season)
    ## table data
    t.data <- data.frame(df_userSelected_seasonData()$`How could we improve training?`)
    colnames(t.data) <- "Responses"
    t.data
  })
  output$pres.table <- renderTable({
    req(input$season)
    ## table data
    t.data <- data.frame(df_userSelected_seasonData()$`How could we improve presentation nights?`)
    colnames(t.data) <- "Responses"
    t.data
  })
  output$selec.table <- renderTable({
    req(input$season)
    ## table data
    t.data <- data.frame(df_userSelected_seasonData()$`How could we improve selection nights?`)
    colnames(t.data) <- "Responses"
    t.data
  })
  output$gameday.table <- renderTable({
    req(input$season)
    ## table data
    t.data <- data.frame(df_userSelected_seasonData()$`How could we improve game day preparation?`)
    colnames(t.data) <- "Responses"
    t.data
  })
  output$clubfunc.table <- renderTable({
    req(input$season)
    ## table data
    t.data <- data.frame(df_userSelected_seasonData()$`Do you have any ideas for Club functions?`)
    colnames(t.data) <- "Responses"
    t.data
  })
  output$comment.table <- renderTable({
    req(input$season)
    ## table data
    t.data <- data.frame(df_userSelected_seasonData()$`Any other comments?`)
    colnames(t.data) <- "Responses"
    t.data
  })
  output$moment.table <- renderTable({
    req(input$season)
    ## table data
    t.data <- data.frame(df_userSelected_seasonData()$`What was your moment of the season?`)
    colnames(t.data) <- "Responses"
    t.data
  })
  
  #### summary plots ####
  ## Team performance
  summaryPlots_server("summary.plot.teamPerf", allYears, "Team performance (from your grade most played):", "TeamPerformance", reactive(input$summary.yearSelection), reactive(input$summary.groupByGrade))
  summaryPlots_server("summary.plot.yourPerf", allYears, "Your performance:", "yourPerformance", reactive(input$summary.yearSelection), reactive(input$summary.groupByGrade))
  summaryPlots_server("summary.plot.training", allYears, "Training:", "training", reactive(input$summary.yearSelection), reactive(input$summary.groupByGrade))
  summaryPlots_server("summary.plot.presentationNights", allYears, "Presentation nights (Saturdays):", "presentationNights", reactive(input$summary.yearSelection), reactive(input$summary.groupByGrade))
  summaryPlots_server("summary.plot.selectionNights", allYears, "Selection nights (Thursdays):", "selectionNights", reactive(input$summary.yearSelection), reactive(input$summary.groupByGrade))
  summaryPlots_server("summary.plot.clubFunctions", allYears, "Club Functions", "clubFunctions", reactive(input$summary.yearSelection), reactive(input$summary.groupByGrade))
  summaryPlots_server("summary.plot.gamedayPrep", allYears, "Game day preparation", "gamedayPrep", reactive(input$summary.yearSelection), reactive(input$summary.groupByGrade))
  
  #### report generators ####
  output$reportSummary <- downloadHandler(
    
    # For PDF output, change this to "report.pdf"
    # filename = "HCC-Report.pdf",
    filename = "HCC-Report.html",
    content = function(file) {
      req(input$season)
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Markdown-template_reportOverallSummary.Rmd")
      file.copy("Markdown-template_reportOverallSummary.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(review_years = input$summary.yearSelection,
                     groupByGrade = input$summary.groupByGrade,
                     allYears = allYears,
                     selected_season = input$season,
                     nResponses = nrow(resp[[input$season]]),
                     season_data = resp[[input$season]],
                     levels_react = input$varSelection,
                     wc.min.freq = input$wordcloud.freq,
                     wc.max.words = input$wordcloud.max
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
