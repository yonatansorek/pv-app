library(shiny)
library(shinythemes)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(segmented)
library(data.table)

pv<-read.csv("c:/shiny_pv/data/pv summer 2020.csv")

de<-mutate(pv,wp_mpa=wp/-10,wp_m=-1/wp_mpa,M=fresh_mass_g-extra_mass_g-dry_mass_g) 

  
  ui1 <- fluidPage(
 #   fluidRow(      tags$head(
     #   tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } 
    #            #inline .form-group { display: table-row;}")))
      #   tags$div(id = "inline", textInput(inputId = "txtInp", label = "Label:"))
    
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "rep", label = "Sample n:",
                      choices = levels(factor(de$rep)),
                      selected = "1"),
          # Select variable for x-axis
          selectInput(inputId = "sp", label = "Species:",
                      choices = levels(factor(de$var)),
                      selected = "clil"),
          # Select variable for color
          selectInput(inputId = "date", label = "Date:",
                      choices =levels(factor(de$time)), 
                      selected = "April"),
            h4("full weter weight"),
            textOutput("ww"),
          h4("Your Current Data"), 
          verbatimTextOutput("brush_info"),
          tableOutput("values"),
          h4("Laef P-V Parameters"), 
          tableOutput("final_results")
    ),
  
      
    
    mainPanel(
            actionButton('DeleteSelectedData', 'Delete Incorect Data'), 
            actionButton('ResetData', 'Reset Data'),
            plotOutput("myPlot", click = "plot1_click", 
                       brush = brushOpts(id = "plot1_brush")),
            plotOutput("myPlot2")
            )
    )
  )
  
  
server1 <- function(input, output, session) {
 
  rx_de <- reactiveVal(de)
  fildf<-reactive(rx_de() %>% 
    filter(time==input$date,var==input$sp,rep==input$rep ))

  
  output$myPlot <- renderPlot({
    my.lm <- lm(M~wp , data = fildf())
    my.seg <- segmented(my.lm, 
                        seg.Z = ~ wp)
    
    my.slopes <- coef(my.seg)
    b0 <- coef(my.seg)[[1]]
    b1 <- coef(my.seg)[[2]]
    ggplot(fildf())+
      ggtitle("interactive plot - brush outlyer and press 'delete'")+
      geom_point(size=3,aes(x=wp,y=M))+
      geom_abline(intercept = b0, slope = b1,linetype="dashed")+
      theme_bw()+
      theme(text = element_text(size=20))
  })
  output$myPlot2 <- renderPlot({
    my.lm <- lm(M~wp , data = fildf())
    my.seg <- segmented(my.lm, 
                        seg.Z = ~ wp)
    
    my.slopes <- coef(my.seg)
    intercept2 <- coef(my.seg)[[1]]

    all_d<- brushedPoints(fildf(), input$plot1_brush, allRows = TRUE)#[, c(input$sp, input$date)]
    pv_data<-all_d %>% mutate(rwc=round(M/intercept2*100,2))
    
    my.lm2 <- lm(rwc~wp_m , data = pv_data)
    my.seg2 <- segmented(my.lm2, 
                        seg.Z = ~ wp_m)
   
    my.lines <- my.seg2$psi[, 2]
    my.fitted2 <- fitted(my.seg2)
    my.model2 <- data.frame(wp_m_f = pv_data$wp_m,  rwc_f= my.fitted2)
    pv_data<-cbind(pv_data,my.model2)
    
    lm_d<-subset(pv_data,wp_m<(summary.segmented(my.seg2)$psi [1,2]))
    osmo_mod<-lm(wp_m~rwc,data=lm_d)
    TLP<--1/(summary.segmented(my.seg2)$psi [1,2])
    TLP<-round(TLP,2)
    
    pv_data %>% 
      ggplot()+
      geom_point(aes(colour = "Data",y=wp_m,x=rwc),size=3)+
      geom_line(aes(y=wp_m_f,x=rwc_f, colour = "TLP"),size=2)+
      geom_smooth(data=lm_d, aes(y=wp_m,x=rwc,colour = "lm Model"),method=lm, fullrange=TRUE,se=F,linetype="dashed",size=1.5)+
      scale_color_manual(values=c( "black", "darkgreen","tomato"))+
      ggtitle(label = paste("TLP:",TLP))+
      #annotate(geom = 'text', label = paste("TLP:",TLP), x = -Inf, y = Inf, hjust = 1, vjust = 1,size=10)+
      theme_cleveland()+
      theme(text = element_text(size=20))+
      theme(plot.title = element_text(size=24,hjust = 0.5,face = "bold",margin = margin(t = 10, b = -20)))+
      geom_hline(yintercept =my.lines, linetype = "dashed",size=1.5)+
      scale_x_reverse()
    
   })

  output$brush_info <- renderPrint({
    my.lm <- lm(M~wp , data = fildf())
    my.seg <- segmented(my.lm, 
                        seg.Z = ~ wp)
    
    my.slopes <- coef(my.seg)
    b0 <- coef(my.seg)[[1]]
    
    all_d<- brushedPoints(fildf(), input$plot1_brush, allRows = TRUE)#[, c(input$sp, input$date)]
    data<-all_d %>% mutate(rwc=round(M/b0*100,2)) 
    data[, c("obs","rwc","wp")]
  })
  
  output$final_results <- renderTable({
    my.lm <- lm(M~wp , data = fildf())
    my.seg <- segmented(my.lm,  seg.Z = ~ wp)
    
    my.slopes <- coef(my.seg)
    b0 <- coef(my.seg)[[1]]
    
    all_d<- brushedPoints(fildf(), input$plot1_brush, allRows = TRUE)#[, c(input$sp, input$date)]
    data<-all_d %>% mutate(rwc=round(M/b0*100,2)) 
    
    my.lm2 <- lm(rwc ~ wp_m, data = data)
    my.seg2 <- segmented(my.lm2, seg.Z = ~ wp_m)
    
    TLP<--1/(summary.segmented(my.seg2)$psi [1,2])
    
    lm_d<-subset(data,wp_m<(summary.segmented(my.seg2)$psi [1,2]))
    model_after_tlp <- lm(rwc ~ wp_m , data =lm_d )
    new.speeds <- data.frame(
      wp_m = summary.segmented(my.seg2)$psi [1,2]
    )
    
    rwc_tlp<-predict(model_after_tlp, newdata = new.speeds)
    rwc_tlp<-as.data.frame(rwc_tlp)[1,1]
    
    model_for_osm <- lm(wp_m ~ rwc , data =lm_d )
    
    new.rwc <- data.frame(
      rwc = 100)
    #100% osmolytes
    
    osm_full=-1/predict(model_for_osm, newdata = new.rwc)
    osm_full<-as.data.frame(osm_full)[1,1]
    
    #rwc_apoplast
    rwc_apoplast<-as.data.frame(coef(model_after_tlp))[1,1]
    
    Parameter<-c("TLP","RWC at TLP","Osm full","Apoplast f")
    Value<-c(TLP,rwc_tlp,osm_full,rwc_apoplast)
    output<-data.table(Parameter,Value)
    output
  })
  
  output$ww<-renderPrint({
             #dfreact<- brushedPoints(fildf(), input$plot1_brush, allRows = TRUE)
             my.lm <- lm(M~wp , data = fildf())
             my.seg <- segmented(my.lm, 
                                 seg.Z = ~ wp)
            # my.rsq<-summary(my.seg)$adj.r.squared
             my.slopes <- coef(my.seg)[1]
             my.slopes
            
             })
  

  observeEvent(input$DeleteSelectedData, {
    Var1 <- brushedPoints(rx_de(), input$plot1_brush, allRows = TRUE)
    rx_de(Var1[!Var1$selected_, names(Var1) != "selected_", drop = FALSE])
  })
  
  observeEvent(input$ResetData, {
    rx_de(de)
  })
}

shinyApp(ui1, server1)

