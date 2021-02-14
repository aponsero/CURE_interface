library(shiny)
library(viridis)
library(hrbrthemes)
library(treemapify)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)


# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
  if (sci) {
    // scientific style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
    })
  } else {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.pow(10, num)); }
    })
  }
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    logifySlider('DB_citation', sci = false)
  }, 5)})
"



# ----------------------------------
# load dataset
datasetJL <- read_csv("../datasets/JL_raw_2020.csv") 
datasetJL$article_year <- as.numeric(datasetJL$article_year)


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
                          )
                 ),
                 # ----------------------------------
                 # tab panel 2 - DB JL
                 tabPanel(title = "DB JL",
                         sidebarLayout(fluid=TRUE,
                           sidebarPanel(h2("Selection databases"),
                                        sliderInput("DB_year", "date of publication:",
                                                    min = 1991, max = 2020,
                                                    value = c(2010,2020)),
                                        sliderInput("DB_citation", "number of citations:",
                                                    min = 0, max = 17000,
                                                    value = c(0,17000)),
                                        downloadButton("downloadData", "Download selection")
                                        ),
                           
                           
                           mainPanel(width = 8,
                                     fluidRow(
                                       column(6,plotOutput("avaibility_chart")),
                                       column(6,plotOutput("nb_citations_chart")),
                                     ),
                                     fluidRow(column(12,tableOutput("tableview")))
                                    ) 
                         ),
                  ),
                # ----------------------------------
                # tab panel 3 - Code Av
                tabPanel("Code Av",
                         includeHTML("more_information.html"),
                         tags$script(src = "plugins/scripts.js"),
                         tags$head(
                           tags$link(rel = "stylesheet", 
                                     type = "text/css", 
                                     href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                           tags$link(rel = "icon", 
                                     type = "image/png", 
                                     href = "images/logo_icon.png")
                         )
                )

)
                 

# ----------------------------------
# ----------------------------------
# ----------------------------------
# SERVER SIDE
# ----------------------------------
# ----------------------------------

server <- function(input, output) {
  
  # ----------------------------------
  # tab panel 2-A - DB JL
    db_selection <- reactive({
      datasetJL %>%  filter(article_year>=input$DB_year[1] & article_year<=input$DB_year[2]) %>%
        filter(citations>=input$DB_citation[1] & citations<=input$DB_citation[2]) 
    })
    
    output$avaibility_chart <- renderPlot({  
      display_dataset <- db_selection() %>% group_by(available) %>% tally()
      
      display_dataset  %>%  ggplot(aes(x="", y=n, fill=available)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void()
    })
    
    output$nb_citations_chart <- renderPlot({  
      display_dataset <- db_selection() 
      
      display_dataset %>% ggplot(aes(x=citations)) +
        geom_histogram(colour="black", fill="blue")
    })
    
    
    output$tableview <- renderTable({
      db_selection() %>% select(article_global_id, resource_name, access)
    }, digits = 0)
    
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
  

}

shinyApp(server = server, ui = ui)