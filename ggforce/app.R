#install.packages(c("reshape2", "ggforce", "tidyverse", "rhandsontable", "shiny", "shinyjqui"))

library(reshape2)
library(ggforce)
library(tidyverse)
library(rhandsontable)
library(shiny)
library(shinyjqui)
library(colourpicker)

ui <- fluidPage(
  titlePanel("Probability of Success Demonstration"),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  br(),
  # column(width=3,
  #        sliderInput("work", "Prior: out of 100 compounds, how many are assumed to work?", min=0, max=100, value=20)
  #        ),
  # column(width=3,
  #        sliderInput("ph1TP", "Phase 1: out of all counpounds assumed to work, what proportion gives positive results in Phase I?", min=0, max=1, value=0.9, step=0.01),
  #        sliderInput("ph1FP", "Phase 1: out of all counpounds assumed not to work, what proportion gives positive results in Phase I?", min=0, max=1, value=0.05, step=0.01)
  #        ),
  # column(width=3,
  #        sliderInput("ph2TP", "Phase 2: out of all positive Phase I compounds assumed to work, what proportion gives positive results in Phase II?", min=0, max=1, value=0.9, step=0.01),
  #        sliderInput("ph2FP", "Phase 2: out of all positive Phase I compounds assumed not to work, what proportion gives positive results in Phase II?", min=0, max=1, value=0.05, step=0.01)
  # ),
  # column(width=3,
  #        sliderInput("ph3TP", "Phase 3: out of all positive Phase II compounds assumed to work, what proportion gives positive results in Phase III?", min=0, max=1, value=0.9, step=0.01),
  #        sliderInput("ph3FP", "Phase 3: out of all positive Phase II compounds assumed not to work, what proportion gives positive results in Phase III?", min=0, max=1, value=0.05, step=0.01)
  # ),
  
  
  
  column(width=3,
         sliderInput("work", "How many out of 100 assumed to work?", min=0, max=100, value=20)
  ),
  column(width=3,
         sliderInput("ph1TP", "Phase 1: true positive", min=0, max=1, value=0.9, step=0.01),
         sliderInput("ph1FP", "Phase 1: false positive", min=0, max=1, value=0.05, step=0.01)
  ),
  column(width=3,
         sliderInput("ph2TP", "Phase 2: true positive", min=0, max=1, value=0.9, step=0.01),
         sliderInput("ph2FP", "Phase 2: false positive", min=0, max=1, value=0.05, step=0.01)
  ),
  column(width=3,
         sliderInput("ph3TP", "Phase 3: true positive", min=0, max=1, value=0.9, step=0.01),
         sliderInput("ph3FP", "Phase 3: false positive", min=0, max=1, value=0.05, step=0.01)
  ),
  
  
  br(),
  hr(),
  fluidRow(
    column(width=4,
           br(),
           column(width=12, 
                  strong("Probability of success table"),
                  rHandsontableOutput("hot")),
           column(width=6, br(), uiOutput("barUI")),
           column(width=6, br(), sliderInput("space", "White space", min=10, max=40, value=20)),
           br(),
           column(width=6, colourInput("col1", "Work", "dodgerblue3")),
           column(width=6, colourInput("col2", "Not work", "darkorchid3"))),
    column(width=6, 
           br(),
           jqui_resizable(plotOutput("plot"))))
)

server <- function(input, output, session) {
  
  
  input_data<-reactive({
    work<-input$work
    
    ph1TP<-input$ph1TP
    ph1FP<-input$ph1FP
    
    ph2TP<-input$ph2TP
    ph2FP<-input$ph2FP
    
    ph3TP<-input$ph3TP
    ph3FP<-input$ph3FP
    
    data.frame(X=c("TP","FP","FN","TN", "Cost"),
               Portfolio = as.character(c(work,100-work,NA,NA," ")),
               Phase1b = as.character(c(work*ph1TP, (100-work)*ph1FP, work*(1-ph1TP), (100-work)*(1-ph1FP)," ")),
               Phase2 = as.character(c(work*ph1TP*ph2TP, (100-work)*ph1FP*ph2FP, work*ph1TP*(1-ph2TP), (100-work)*ph1FP*(1-ph2FP)," ")),
               Phase3 = as.character(c(work*ph1TP*ph2TP*ph3TP, (100-work)*ph1FP*ph2FP*ph3FP, work*ph1TP*ph2TP*(1-ph3TP), (100-work)*ph1FP*ph2FP*(1-ph3FP)," ")), 
               stringsAsFactors = F, row.names = 1:5)
  })
  
  output$hot <- renderRHandsontable(rhandsontable(input_data(), useTypes = FALSE))

  
  
  
  dat<-reactive({
    if(!is.null(input$hot))
    sample_data <- hot_to_r(input$hot)[-5,] %>%
                   mutate(Portfolio=as.numeric(Portfolio),
                          Phase1b=as.numeric(Phase1b),
                          Phase2=as.numeric(Phase2),
                          Phase3=as.numeric(Phase3))
    
    
    if(!is.null(input$hot))
    gather(sample_data, key="phase", value="value", Portfolio, Phase1b, Phase2, Phase3) %>%
         mutate(lab=paste0(rep(LETTERS[1:4], each=4), rep(1:4, 4))) %>%
         filter(!is.na(value))
    
  })

  output$barUI<-renderUI(numericInput("bar", "Horizonal bar position", min=20, max=120, value=dat()$value[2]+input$space*0.5))
  
 
  
  data<-reactive({
    
    for(i in seq_along(dat()$lab)){
      assign(dat()$lab[i], dat()$value[i])
    }
    
    S<-input$space
    H<-100+input$space
    
    wave <- data.frame(
      x = c(20, 80, 20, 80, 
            20, 80, 20, 80,
            20, 80, 20, 80, 
            20, 80, 20, 80,
            100, 160, 100, 160,
            100, 160, 100, 160,
            100, 160, 100, 160,
            100, 160, 100, 160,
            180, 240, 180, 240,
            180, 240, 180, 240,
            180, 240, 180, 240,
            180, 240, 180, 240),
      y = c(0, 0, B4, B4, 
            B4, H-(B1+B2), A2, H-B1,
            H-A1, B4, H-B1, B3+B4, 
            H-B1, H-B1, H, H,
            
            H-(B1+B2), H-(C1+C2)-S-(C3+C4), H-(B1+B2)+C4, H-(C1+C2)-S-C3,
            H-(B1+B2)+C4, H-(C1+C2), H-B1, H-C1,
            H-B1, H-(C1+C2)-S-C3, H-C1, H-(C1+C2)-S,
            H-C1, H-C1, H, H,
            
            H-(C1+C2), H-(D1+D2)-S-(D3+D4), H-(C1+C2)+D4, H-(D1+D2)-S-D3,
            H-(C1+C2)+D4, H-(D1+D2), H-C1, H-D1,
            H-C1, H-(D1+D2)-S-D3, H-D1, H-(D1+D2)-S,
            H-D1, H-D1, H, H 
      ),
      group = rep(1:12, each=4),
      col = rep(rep(c("Purple", "Purple", "Blue", "Blue"), 3), each=4),
      alpha = rep("3", length(col))
    )
    
    positions <- data.frame(
      group = rep(1:14, each = 4),
      col = rep(rep(c("Purple", "Blue"), each=4), 7),
      alpha = rep(c("1", "1", rep(c("2", "2", "1", "1"), 3)), each=4),
      x = c(0, 20, 20, 0, #portfolio FP
            0, 20, 20, 0, #portfolio TP
            80, 100, 100, 80, #ph1b TN
            80, 100, 100, 80, #ph1b FN
            80, 100, 100, 80, #ph1b FP
            80, 100, 100, 80, #ph1b TP
            160, 180, 180, 160, #ph2 TN
            160, 180, 180, 160, #ph2 FN
            160, 180, 180, 160, #ph2 FP
            160, 180, 180, 160, #ph2 TP
            240, 260, 260, 240, #ph3 TN
            240, 260, 260, 240, #ph3 FN
            240, 260, 260, 240, #ph3 FP
            240, 260, 260, 240), #ph3 TP
      y = c(0, 0, A2, A2, #portfolio FP
            H-A1, H-A1, H, H, #portfolio TP
            
            0, 0, B4, B4, #ph1b TN
            B4, B4, B3+B4, B3+B4, #ph1b FN
            H-(B1+B2), H-(B1+B2), H-B1, H-B1, #ph1b FP 
            H-B1, H-B1, H, H, #ph1b TP
            
            H-(C1+C2)-S-(C3+C4), H-(C1+C2)-S-(C3+C4), H-(C1+C2)-S-C3, H-(C1+C2)-S-C3, #ph2 TN
            H-(C1+C2)-S-C3, H-(C1+C2)-S-C3, H-(C1+C2)-S, H-(C1+C2)-S, #ph2 FN
            H-(C1+C2), H-(C1+C2), H-C1, H-C1, #ph2 FP
            H-C1, H-C1, H, H, #ph2 TP
            
            H-(D1+D2)-S-(D3+D4), H-(D1+D2)-S-(D3+D4), H-(D1+D2)-S-D3, H-(D1+D2)-S-D3, #ph3 TN
            H-(D1+D2)-S-D3, H-(D1+D2)-S-D3, H-(D1+D2)-S, H-(D1+D2)-S, #ph3 FN
            H-(D1+D2), H-(D1+D2), H-D1, H-D1, #ph3 FP
            H-D1, H-D1, H, H) #ph3 TP
    )
    
    
    legnd <- data.frame(
      group = rep(15:18, each=4),
      col = rep(c("Blue", "Purple", "Blue", "Purple"), each=4),
      alpha = rep(c("1", "1", "2", "2"), each=4),
      x = c(200, 200, 220, 220, 
            200, 200, 220, 220,
            200, 200, 220, 220,
            200, 200, 220, 220),
      y = c(29, 24, 24, 29,
            21, 16, 16, 21,
            13, 8, 8, 13,
            5, 0, 0, 5)
    )
    
  rbind(wave, positions, legnd)
    
  })
    
  
    
    
  p<-reactive({
      
    S<-input$space
    H<-100+input$space
    
    for(i in seq_along(dat()$lab)){
        assign(dat()$lab[i], dat()$value[i])
    }
    
    if(!is.null(input$hot))
    
    ggplot(data()) +
      ggforce::geom_diagonal_wide(data=data()[1:48,], aes(x, y, group = group, fill = col, alpha=alpha), radius = 0) +
      ggforce::geom_shape(data=data()[49:120,], aes(x, y, group=group, fill=col, alpha=alpha)) +
      #ggforce::geom_shape(data=data[105:120,], aes(x, y, group=group, fill=col, alpha=alpha)) +
      scale_alpha_discrete(range = c(1, 0.1, 0.2)) +
      scale_fill_manual(values = c(input$col1, input$col2)) +
      theme_bw() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            #plot.title = element_text(size=14, face="bold"),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()) +
      annotate("text", x=c(10, 90, 170, 250, 10, 90, 170, 250), 
                       y=c(rep(H+10, 4), rep(H+5, 4)), 
                       label=c("Portfolio", "Ph1b", "Ph2", "Ph3", hot_to_r(input$hot)[5,-1]), fontface=2) +
      
      annotate("text", x=c(30, 30, 110, 110, 110, 110, 195, 195, 195, 195, 275, 275, 275, 275), 
                       y=c(H-A1/2, A2/2, H-B1/2, H-B1-B2/2, B4+B3/2, B4/2, 
                           H-C1/2, H-C1-C2/2, H-(C1+C2+S)-C3/2+1, H-(C1+C2+S+C3)-C4/2-1,
                           H-D1/2, H-D1-D2/2, H-(D1+D2+S)-D3/2+2, H-(D1+D2+S+D3)-D4/2-2), 
                           label=paste0(c(na.omit(unlist(hot_to_r(input$hot)[-5,2:5]))), "%")) +
      
      annotate("segment", x=60, xend=300, y=input$bar, yend=input$bar) +
      
      annotate("text", x=c(310, 310), y=c(input$bar+10, input$bar-10), label=c("Positive \nresults", "Negative \nresults"), fontface=2) +
      
      annotate("text", x=c(244, 245, 244, 243), y=seq(2.5, 26.5, 8), label=c("True negative", "False negative", "False positive", "True positive"), fontface=2) 
    
  })
  
  output$plot<-renderPlot(p())
  
}

shinyApp(ui, server)






