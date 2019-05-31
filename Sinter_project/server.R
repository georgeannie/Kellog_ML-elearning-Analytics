options(shiny.reactlog=TRUE)
#access reactive only with X()
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(plotly)
library(reshape2)
library(lubridate)
library(gridExtra)
library(cowplot)
library(DT)
source("sinter_function.R")

sinter= read_rename_sinter()

shinyServer(function(input, output) {
    var=reactive({
        input$radio_dep_choice
    })
    
    var2=reactive({
      input$radio_dep_choice2
    })
    
    var3=reactive({
       input$plant
    })
    
   output$correlation_matrix = renderPlot({
      var3 = var3()
      
      sinter_list = switch(var3, 
             "1" = sinter[[1]],
             "2" = sinter[[2]],
             "3" = sinter[[3]],
             "4" = sinter[[4]]
      )
      
       dep_sinter=sinter_list[, names(sinter_list) %in% c('Overall Efficiency', 
                                             "Internal Return Fines", 
                                             "External Return Fine", 
                                             "Shatter Index")]
       print(head(dep_sinter))
       dep_sinter[is.na(dep_sinter)]=0  
       corr_dep = cor(dep_sinter)
       
       p=ggcorrplot(corr_dep,  col=c("red", "navyblue", "gray"), hc.order = TRUE,
                      type = "lower", lab=TRUE, legend.title = "", outline.color = "grey",
                      show.diag = TRUE) +
            ggtitle("Correlation of Potential Dependent Variables") +
            
            theme(plot.title = element_text(hjust = 0.5, colour = "black", size=18, face='bold', 
                                            vjust=0.5))
       p
   })

   output$scatter_matrix = renderPlot({
      var3 = var3()
      
      sinter_list = switch(var3, 
                           "1" = sinter[[1]],
                           "2" = sinter[[2]],
                           "3" = sinter[[3]],
                           "4" = sinter[[4]]
      )
      
       dep_sinter=sinter_list[, names(sinter_list) %in% c("Overall Efficiency", 
                                                          "Internal Return Fines", 
                                                "External Return Fine", 
                                                "Shatter Index")]
       dep_sinter=dep_sinter%>% gather(key, value, -"Overall Efficiency") 
       dep_sinter=dep_sinter[complete.cases(dep_sinter),]
       
       p1=ggplot(dep_sinter, aes(x=value, y=`Overall Efficiency`))+
           geom_point(col="navy")+
           geom_smooth(method = "lm", col="red", se=FALSE) +
           facet_wrap( ~ key, scales = "free") + 
           theme_minimal() +
           ylab("Overall Efficiency") +
           xlab("") +
           ggtitle("Scatter plot of Potential Dependent Variables") +
           theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = 0.3),
                 strip.text.x =  element_text(size=12),
                 axis.ticks = element_blank(),
                 panel.background = element_rect(fill = "white", colour = "grey50"),
                 panel.grid = element_blank(),
                 axis.title.y=element_text(size=12, face="bold")
           )
       
       
       p1
   })
   
   output$histogram = renderPlot({
      var3 = var3()
      
      sinter_list = switch(var3, 
                           "1" = sinter[[1]],
                           "2" = sinter[[2]],
                           "3" = sinter[[3]],
                           "4" = sinter[[4]]
      )
      
       dep_sinter=sinter_list[, names(sinter_list) %in% c("Internal Return Fines", 
                                                "External Return Fine", 
                                                "Shatter Index")]
        
       dep_sinter=dep_sinter%>% gather(key, value)
       print(head(dep_sinter)) 
       p=ggplot(dep_sinter, aes(x=value))+
           geom_histogram(color="white", fill="navyblue", position="identity")+
           facet_wrap( ~ key, scales = "free") + 
           theme_minimal() +
           xlab("") +
           ylab("Frequency") +
           ggtitle("Histogram of Potential Dependent Variables") +
           theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = 0.3),
                 strip.text.x =  element_text(size=12),
                 axis.ticks = element_blank(),
                 panel.background = element_rect(fill = "white", colour = "grey50"),
                 panel.grid = element_blank(),
                 axis.title.y=element_text(size=12, face="bold")
                )
       p    
   })
   
   output$summary = renderDataTable({
     dep_sinter=sinter[, names(sinter) %in% c("% Internal Return Fines", 
                                              "% External Return Fines", 
                                              "Shatter Index")]
     
     dep_sd = apply(dep_sinter, 2, sd, na.rm=TRUE)
     dep_mean = apply(dep_sinter, 2, mean, na.rm=TRUE)
     dep_25 = apply(dep_sinter, 2, quantile, .25, na.rm=TRUE)
     dep_75 = apply(dep_sinter, 2, quantile, .75, na.rm=TRUE)
     dep_median = apply(dep_sinter, 2, median,  na.rm=TRUE)
     dep_min = apply(dep_sinter, 2, min, na.rm=TRUE)
     dep_max = apply(dep_sinter, 2, max, na.rm=TRUE)
     
     dep_summary=data.table("Variables" = names(dep_sinter),
                            "Standard Deviation" = dep_sd,
                            'Mean' = dep_mean,
                            'Median' = dep_median,
                            "Minimum Value" = dep_min,
                            "Maximum Value" = dep_max,
                            "25th Percentile" =  dep_25,
                            "75th Percentile" =  dep_75)
     
     datatable(dep_summary, options = list(dom = 't'))   
   })
   
   output$data = renderDataTable({
      dep_sinter=sinter[, names(sinter) %in% c("Timestamp", "% Sinter", "% Internal Return Fines", 
                                               "% External Return Fines", 
                                               "Shatter Index")]
      dep_sinter   
   })
   
}) 