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
datasetJL <- read_csv("datasets/JL_Databases.2016.csv") 
datasetJL$year_first_publication <- as.numeric(datasetJL$year_first_publication)
articlesJL <- read_csv("datasets/JL_articles_DB.2016.csv", guess_max = 3200) 

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
                           sidebarPanel(
                                        # database selectors
                                        h2("Selection databases"),
                                        sliderInput("DB_year", "date of first publication:",
                                                    min = 1991, max = 2020,
                                                    value = c(2010,2020)),
                                        sliderInput("DB_year_last", "date of last publication (as of 2016):",
                                                    min = 1991, max = 2020,
                                                    value = c(2010,2020)),
                                        sliderInput("DB_citation", "number of citations in 2016:",
                                                    min = 0, max = 18000,
                                                    value = c(0,18000)),
                                        sliderInput("nb_publi", "number of publications in 2016:",
                                                    min = 0, max = 25,
                                                    value = c(0,25)),
                                        checkboxGroupInput("access", "Availability in 2016:",
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
                                                     DT::dataTableOutput("tableview"),)),
                                    ) 
                         ),
                  ),
                # ----------------------------------
                # tab panel 3 - Code Av
                tabPanel("Help",
                         includeHTML("help.html"),
                         tags$script(src = "plugins/scripts.js"),
                         tags$head(
                           tags$link(rel = "stylesheet", 
                                     type = "text/css", 
                                     href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                           tags$link(rel = "icon", 
                                     type = "image/png", 
                                     href = "images/logo_icon.png")
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
        filter(year_last_publication>=input$DB_year_last[1] & year_last_publication<=input$DB_year_last[2]) %>%
        filter(total_citation_2016>=input$DB_citation[1] & total_citation_2016<=input$DB_citation[2]) %>%
        filter(nb_articles_2016>=input$nb_publi[1] & nb_articles_2016<=input$nb_publi[2]) %>%
        filter(available_2016 %in% input$access)
      # here change to 2021 when update dataset complete
    })
    
    article_selection <- reactive({
      list<- db_selection() %>% select(db_id)
      inner_join(articlesJL, list)
    })
    
    output$avaibility_chart <- renderPlot({  
      display_dataset <- db_selection() %>% group_by(available_2016) %>% tally()
      
      display_dataset  %>%  ggplot(aes(x="", y=n, fill=available_2016)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void()+
        ggtitle("Availability of ressources")
    })
    
    output$nb_citations_chart <- renderPlot({  
      display_dataset <- db_selection() 
      
      display_dataset %>% ggplot(aes(x=total_citation_2021)) +
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
  
}

shinyApp(server = server, ui = ui)
