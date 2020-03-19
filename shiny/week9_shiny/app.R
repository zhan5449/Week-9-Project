library(shiny)
library(tidyverse)
library(lubridate)

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("App for Assessment Data"),
   
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("respondents",
                        "Would you like to see the data of males only, females only, or all participants?",
                        c("Male"="Male",
                          "Female"="Female",
                          "All"="Male,Female"),
                        selected="Male,Female"),
            selectInput("aug17",
                        "Would you like to include participants that completed the assessment before August 1, 2017?",
                        c("Yes"="include",
                          "No"="exclude"),
                        selected="include")
        ),
        

        # Show table
        mainPanel(
            plotOutput("scatterplot"),
            tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    week9_tbl <- readRDS("data.rds")
    output$scatterplot <- renderPlot({
        
        # draw the scatterplot
        ggplot(week9_tbl,aes(x=Q1_Q5,y=Q6_Q10))+
            geom_point()+
            geom_smooth(method="lm",se=F)+
            labs(x="Mean scores of Q1 to Q5",
                 y="Mean scores of Q6 to Q10",
                 title="Scatterplot between the Q1-Q5 mean and the Q6-Q10 mean")
    })
    output$table <- renderTable({
        # display part of the table according to input
      if(input$aug17=="exclude"){
        week9_tbl[week9_tbl$gender %in% unlist(strsplit(input$respondents,",")) & week9_tbl$timeEnd>=ymd_hms("2017-08-01 00:00:00"),]
      }else{
        week9_tbl[week9_tbl$gender %in% unlist(strsplit(input$respondents,",")),]
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Link to online app
## https://zhan5449.shinyapps.io/week9_shiny/
