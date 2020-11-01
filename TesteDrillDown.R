library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(RODBC)
library(dplyr)
library(lubridate)

TesteCal <- readRDS("TesteCal.RDS")
TesteCal$Data <- as.Date(TesteCal$Data)

ui <- fluidPage(
  plotlyOutput("mes", height = "300px"),
  plotlyOutput("dia", height = "300px")
)

server <- function(input, output, session) {
  
  # for maintaining the state of drill-down variables
  mes <- reactiveVal()
  #dia <- reactiveVal()
  
  
  # when clicking on a category, 
  observeEvent(event_data("plotly_click", source = "mes"), {
    mes(event_data("plotly_click", source = "mes"))
    #dia(NULL)
  })
  
  # observeEvent(event_data("plotly_click", source = "dia"), {
  #   dia(event_data("plotly_click", source = "dia")$x)
  # })
  
  
  output$mes <- renderPlotly({
    
    ggplotly({
      
      TesteCal %>%
        dplyr::group_by(mes = month(Data, label = TRUE), Equipamento) %>%
        dplyr::summarise(Energia = mean(Energia)) %>%
        ggplot(aes(x = mes, y=Energia, group = Equipamento)) +
        geom_col(aes(fill = Equipamento), position = "dodge") +
        ylim(0,NA)+
        xlab("2020")
      
    },
    source = "mes")
    
    # TesteCal %>% 
    #     dplyr::group_by(mes, Equipamento) %>% 
    #     dplyr::summarise(Energia = mean(Energia)) %>% 
    #     plot_ly(x=~mes, y=~Energia, type = "bar", source = "mes")
  })
  
  output$dia <- renderPlotly({
    if (is.null(mes())) return(NULL)
    
    ggplotly({
      
      TesteCal %>%
        filter(month(Data) %in% month(round(mes()$x))) %>%
        ggplot(aes(x = factor(dia), y=Energia, group = Equipamento)) +
        geom_col(aes(fill = Equipamento), position = "dodge") +
        ylim(0,NA) +
        xlab(as.character(month(round(mes()$x),label = TRUE)))
    })
    
    # TesteCal %>% 
    #   filter(mes %in% mes()) %>% 
    #   plot_ly(x=~dia, y=~Energia)
    
  })
  
}

shinyApp(ui, server)