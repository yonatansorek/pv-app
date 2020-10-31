library(shiny)
library(shinythemes)
library(dplyr)


UI<-(fluidPage(    
  
  # Give the page a title
  titlePanel("interactive pv curve analize"),
  
  # Generate a row with a sidebar
  sidebarLayout( 
    sidebarPanel(
      selectInput(inputId = "rep", label = "Sample n:",
                  choices = levels(factor(pv_r$rep)),
                  selected = "1"),
      # Select variable for x-axis
      selectInput(inputId = "sp", label = "Species:",
                  choices = levels(factor(pv_r$var)),
                  selected = "clil"),
      # Select variable for color
      selectInput(inputId = "date", label = "Date:",
                  choices =levels(factor(pv_r$time)), 
                  selected = "April"),
      h4("Brushed points"),
      tableOutput("plot_clickedpoints"),
      
      h4("Results"),
      textOutput("Power")),
    
    mainPanel(
      h4("Demo - brushedPoints - Interactive plots - select data points in plot - return the rows of data that are selected by brush"),
      plotOutput(outputId = "rawplot", brush = "plot_brush_",   
                 click = "plot_click" # Equiv, to click=clickOpts(id="plot_click")
                 
      ))
  )
)
)



pv<-read.csv("c:/shiny_pv/data/pv summer 2020.csv")

pv_r<-mutate(pv,wp=water_pot_Mpa/-10,wp_m=-1/wp,M=fresh_mass_g-extra_mass_g-dry_mass_g) 

server <- function(input, output,session) {
  
  mt <- reactiveValues(data=pv_r)
  
  output$rawplot <- renderPlot({
    mt$data%>% 
      filter(time==input$date,var==input$sp,rep==input$rep ) %>% 
      ggplot()+
      aes(x=fresh_mass_g,y=water_pot_Mpa)+
      geom_point(size=3)+
      theme_bw() 
  })
  
  observe({
    df = brushedPoints(mt$data, brush = input$plot_brush_, allRows = TRUE) 
    mt$data = df[df$selected_== FALSE,  ] 
  })
  

}
shinyApp(UI,server)
