library(reshape2)
library(ggforce)
library(tidyverse)
library(rhandsontable)
library(shiny)

ui <- fluidPage(
    titlePanel("Sankey using ggforce"),
    br(),
    fluidRow(
      column(width=1),
      column(width=9, rHandsontableOutput("hot"))),
    br(),
    fluidRow(
      column(width=1),
      column(width=7, plotOutput("plot")))
)

server <- function(input, output, session) {
  output$hot <- renderRHandsontable(rhandsontable(data.frame(X=c("TP","FP","FN","TN"),
                                                             Portfolio=c(200,800,NA,NA),
                                                             Phase1b = c(180,40,20,760),
                                                             Phase2 = c(162,2,18,38),
                                                             Phase3 = c(145.8,0.05,16.2,1.95), 
                                                             stringsAsFactors = F, row.names = 1:4), useTypes = FALSE))
  #output$tb<-renderTable(hot_to_r(input$hot))
  
  
  p<-reactive({
    sample_data <- hot_to_r(input$hot)
    
    sample_data[,2:5]<-sample_data[,2:5]/10
    
    dat<-gather(sample_data, key="phase", value="value", Portfolio, Phase1b, Phase2, Phase3) %>%
      mutate(lab=paste0(rep(LETTERS[1:4], each=4), rep(1:4, 4))) %>%
      filter(!is.na(value))
    
    for(i in seq_along(dat$lab)){
      assign(dat$lab[i], dat$value[i])
    }
    
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
            B4, 120-(B1+B2), A2, 120-B1,
            120-A1, B4, 120-B1, B3+B4, 
            120-B1, 120-B1, 120, 120,
            
            120-(B1+B2), 120-(C1+C2)-20-(C3+C4), 120-(B1+B2)+C4, 120-(C1+C2)-20-C3,
            120-(B1+B2)+C4, 120-(C1+C2), 120-B1, 120-C1,
            120-B1, 120-(C1+C2)-20-C3, 120-C1, 120-(C1+C2)-20,
            120-C1, 120-C1, 120, 120,
            
            120-(C1+C2), 120-(D1+D2)-20-(D3+D4), 120-(C1+C2)+D4, 120-(D1+D2)-20-D3,
            120-(C1+C2)+D4, 120-(D1+D2), 120-C1, 120-D1,
            120-C1, 120-(D1+D2)-20-D3, 120-D1, 120-(D1+D2)-20,
            120-D1, 120-D1, 120, 120 
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
            120-A1, 120-A1, 120, 120, #portfolio TP
            
            0, 0, B4, B4, #ph1b TN
            B4, B4, B3+B4, B3+B4, #ph1b FN
            120-(B1+B2), 120-(B1+B2), 120-B1, 120-B1, #ph1b FP 
            120-B1, 120-B1, 120, 120, #ph1b TP
            
            120-(C1+C2)-20-(C3+C4), 120-(C1+C2)-20-(C3+C4), 120-(C1+C2)-20-C3, 120-(C1+C2)-20-C3, #ph2 TN
            120-(C1+C2)-20-C3, 120-(C1+C2)-20-C3, 120-(C1+C2)-20, 120-(C1+C2)-20, #ph2 FN
            120-(C1+C2), 120-(C1+C2), 120-C1, 120-C1, #ph2 FP
            120-C1, 120-C1, 120, 120, #ph2 TP
            
            120-(D1+D2)-20-(D3+D4), 120-(D1+D2)-20-(D3+D4), 120-(D1+D2)-20-D3, 120-(D1+D2)-20-D3, #ph3 TN
            120-(D1+D2)-20-D3, 120-(D1+D2)-20-D3, 120-(D1+D2)-20, 120-(D1+D2)-20, #ph3 FN
            120-(D1+D2), 120-(D1+D2), 120-D1, 120-D1, #ph3 FP
            120-D1, 120-D1, 120, 120) #ph3 TP
    )
    
    data<-rbind(wave, positions)
    
    
    ggplot(data) +
      ggforce::geom_diagonal_wide(data=data[1:48,], aes(x, y, group = group, fill = col, alpha=alpha), radius = 0) +
      ggforce::geom_shape(data=data[49:104,], aes(x, y, group=group, fill=col, alpha=alpha)) +
      scale_alpha_discrete(range = c(1, 0.2, 0.1)) +
      scale_fill_manual(values = c("dodgerblue3", "darkorchid3")) +
      theme_bw() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()) +
      annotate("text", x=c(20, 20, 100, 100, 100, 100, 180, 180, 180, 180, 260, 260, 260, 260, 
                           10, 90, 170, 250), 
               y=c(120-A1/2, A2/2, 120-B1/2, 120-B1-B2/2, B4+B3/2, B4/2, 
                   120-C1/2, 120-C1-C2/2, 120-(C1+C2+20)-C3/2, 120-(C1+C2+20+C3)-C4/2,
                   120-D1/2, 120-D1-D2/2, 120-(D1+D2+20)-D3/2+1, 120-(D1+D2+20+D3)-D4/2-1,
                   rep(125, 4)), 
               label=c(10*na.omit(unlist(sample_data[, 2:5])), "Portfolio", "Ph1b", "Ph2", "Ph3"))
    
  })
  
  output$plot<-renderPlot(p())
  
}

shinyApp(ui, server)




# sample_data<-data.frame(X=c("TP", "FP", "FN", "TN"),
#                         Portforlio=c(200, 800, NA, NA),
#                         Phase1b=c(180, 40, 20, 760),
#                         Phase2=c(162, 2, 18, 38),
#                         Phase3=c(145.8, 0.05, 16.2, 1.95))
# 
# sample_data<-data.frame(X=c("TP", "FP", "FN", "TN"),
#                         Portforlio=c(200, 800, NA, NA),
#                         Phase1b=c(160, 100, 40, 700),
#                         Phase2=c(120, 50, 40, 50),
#                         Phase3=c(100, 35, 20, 15))




