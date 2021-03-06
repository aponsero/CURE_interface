library(shiny)
library(viridis)
library(hrbrthemes)
library(treemapify)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

# ----------------------------------
# load dataset
datasetJL <- read_csv("../datasets/JL_database_2018.csv") 
datasetJL$article_year <- as.numeric(datasetJL$article_year)
articlesJL <- read_csv("../datasets/JL_article_2018.csv") 

datasetAV <- read_csv("../datasets/AV_tools_2021.csv") 
articlesAV <- read_csv("../datasets/AV_articles_2021.csv") 

ui <- navbarPage(title = "DS Heroes",
                 theme = "style/style.css",
                 footer = includeHTML("footer.html"),
                 fluid = TRUE,
                 
                 # ----------------------------------
                 # tab panel 1 - Home
                 tabPanel("Home",
                          includeHTML("home.html"),
                          tags$script(src = "plugins/scripts.js"),
                          tags$head(
                            tags$link(rel = "stylesheet", 
                                      type = "text/css", 
                                      href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                            tags$link(rel = "icon", 
                                      type = "image/png", 
                                      href = "images/logo_icon.png")
                          ),
                 ),
                 # ----------------------------------
                 # tab panel 2 - DB JL
                 tabPanel(title = "DB Justice League",
                         sidebarLayout(fluid=TRUE,
                           sidebarPanel(h2("Selection databases"),
                                        sliderInput("DB_year", "date of first publication:",
                                                    min = 1991, max = 2020,
                                                    value = c(2010,2020)),
                                        sliderInput("DB_citation", "number of citations:",
                                                    min = 0, max = 18000,
                                                    value = c(0,18000)),
                                        sliderInput("nb_publi", "number of publications:",
                                                    min = 0, max = 25,
                                                    value = c(0,25)),
                                        checkboxGroupInput("access", "Availability:",
                                                           c("available" = "yes",
                                                             "not available" = "no"),
                                                           selected=c("yes", "no")),
                                        h4(),
                                        downloadButton("downloadData", "Download databases"),
                                        h4(),
                                        downloadButton("downloadArticles", "Download articles"),
                                        ),
                           
                           
                           mainPanel(width = 8,
                                     fluidRow(
                                       column(5,plotOutput("avaibility_chart")),
                                       column(5,plotOutput("nb_citations_chart")),
                                     ),
                                     fluidRow(column(10, 
                                                     h2("List of selected ressources"),
                                                     DT::dataTableOutput("tableview"),))
                                    ) 
                         ),
                  ),
                # ----------------------------------
                # tab panel 3 - Code Av
                tabPanel("Code Avenger",
                         sidebarLayout(fluid=TRUE,
                                       sidebarPanel(h2("Selection tools"),
                                                    checkboxGroupInput("accessT", "Code Availability:",
                                                                       c("available" = "yes",
                                                                         "not available" = "no"),
                                                                       selected=c("yes", "no")),
                                                    h4(),
                                                    downloadButton("downloadTool", "Download tools"),
                                                    h4(),
                                                    downloadButton("downloadArticlesT", "Download articles"),
                                       ),
                                       mainPanel(width = 8,
                                                 fluidRow(
                                                   column(5,plotOutput("avaibilityT_chart")),
                                                 ),
                                                 fluidRow(column(10, 
                                                                 h2("List of selected ressources"),
                                                                 DT::dataTableOutput("tableviewT")))
                                       ) 
                         ),
                )

)
                 

# ----------------------------------
# ----------------------------------
# ----------------------------------
# SERVER SIDE
# ----------------------------------
# ----------------------------------

server <- function(input, output) {
  
  
  # tab panel 2-A - DB JL
  # ----------------------------------
    db_selection <- reactive({
      datasetJL %>%  filter(year_first_publication>=input$DB_year[1] & year_first_publication<=input$DB_year[2]) %>%
        filter(total_citation>=input$DB_citation[1] & total_citation<=input$DB_citation[2]) %>%
        filter(nb_articles>=input$nb_publi[1] & nb_articles<=input$nb_publi[2]) %>%
        filter(available %in% input$access)
    })
    
    article_selection <- reactive({
      list<- db_selection() %>% select(db_id)
      inner_join(articlesJL, list)
    })
    
    output$avaibility_chart <- renderPlot({  
      display_dataset <- db_selection() %>% group_by(available) %>% tally()
      
      display_dataset  %>%  ggplot(aes(x="", y=n, fill=available)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void()+
        ggtitle("Availability of ressources")
    })
    
    output$nb_citations_chart <- renderPlot({  
      display_dataset <- db_selection() 
      
      display_dataset %>% ggplot(aes(x=total_citation)) +
        geom_histogram(fill="#00BFC4") +
        theme_minimal()+
        ggtitle("Distribution of citations")
    })
    
    
    output$tableview <- renderDataTable({
      db_selection() %>% select(db_id, resource_name, link_HTML)
    }, escape = FALSE)
    
  # ----------------------------------
    # Download button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("DataJL-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(db_selection(), file)
      }
    )
    
    output$downloadArticles <- downloadHandler(
      filename = function() {
        paste("ArticleJL-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(article_selection(), file)
      }
    )
    
  # ----------------------------------
    # tab panel 2-B - Code Av
    
  # ----------------------------------
    tool_selection <- reactive({
      datasetAV %>% 
        filter(Code_availability %in% input$accessT)
    })
    
    articleT_selection <- reactive({
      list<- tool_selection() %>% select(tool_ID)
      inner_join(datasetAV, list)
    })
    
    output$avaibilityT_chart <- renderPlot({  
      display_dataset <- tool_selection() %>% group_by(Code_availability) %>% tally()
      
      display_dataset  %>%  ggplot(aes(x="", y=n, fill=Code_availability)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void()+
        ggtitle("Availability of the code")
    })
    
    output$tableviewT <- renderDataTable({
      tool_selection() %>% select(tool_ID, Name, link_code)
    }, escape = FALSE)
    
    # ----------------------------------
    # Download button
    output$downloadtool <- downloadHandler(
      filename = function() {
        paste("DataAV-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(articleT_selection(), file)
      }
    )
    
    output$downloadtoolArt <- downloadHandler(
      filename = function() {
        paste("ArticleAV-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(article_selection(), file)
      }
    )
  
}

shinyApp(server = server, ui = ui)