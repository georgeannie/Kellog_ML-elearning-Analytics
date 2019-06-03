options(shiny.reactlog=TRUE)
#access reactive only with X()
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(corrplot)
library(plotly)
library(reshape2)
library(lubridate)
library(data.table)
library(DT)
library(GGally)
source("sinter_function.R")

sinter= read_rename_sinter()

shinyServer(function(input, output, session) {
    var=reactive({
        input$radio_dep_choice
    })
    
    var2=reactive({
      input$radio_dep_choice2
    })
    
    var3=reactive({
       input$plant
    })
    
    var4=reactive({
       input$plant_tab2
    })
    
    var5=reactive({
       input$plant_tab3
    })
    
    var7=reactive({
       input$plant_tab4
    })
    
   output$correlation_matrix = renderPlot({
      var4 = var4()
      
      sinter_list = switch(var4, 
             "1" = sinter[[1]],
             "2" = sinter[[2]],
             "3" = sinter[[3]],
             "4" = sinter[[4]]
      )
      
       dep_sinter=sinter_list[, names(sinter_list) %in% c('Overall Efficiency', 
                                             "%Internal Return Fine", 
                                             "%External Return Fine", 
                                             "Shatter Index")]
       dep_sinter[is.na(dep_sinter)]=0  
       corr_dep = cor(dep_sinter)
       
       p=ggcorrplot(corr_dep,  col=c("red", "blue", "gray"), hc.order = TRUE,
                      type = "lower", lab=TRUE, legend.title = "", outline.color = "grey",
                      show.diag = TRUE) +
            ggtitle("Correlation of Potential Dependent Variables") +
            
            theme(plot.title = element_text(hjust = 0.5, colour = "black", size=18, face='bold', 
                                            vjust=0.5))
       p
   })

   output$scatter_matrix = renderPlot({
      var4 = var4()
      
      sinter_list = switch(var4, 
                           "1" = sinter[[1]],
                           "2" = sinter[[2]],
                           "3" = sinter[[3]],
                           "4" = sinter[[4]]
      )
      
       dep_sinter=sinter_list[, names(sinter_list) %in% c("Overall Efficiency", 
                                                          "%Internal Return Fine", 
                                                "%External Return Fine", 
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
      
       dep_sinter=sinter_list[, names(sinter_list) %in% c("Overall Efficiency", 
                                                          "%Internal Return Fine", 
                                                "%External Return Fine", 
                                                "Shatter Index")]
        
       dep_sinter=dep_sinter%>% gather(key, value)
       
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
      var5 = var5()
      
      sinter_list = switch(var5, 
                           "1" = sinter[[1]],
                           "2" = sinter[[2]],
                           "3" = sinter[[3]],
                           "4" = sinter[[4]]
      )
   
      dep_sinter=sinter_list[, names(sinter_list) %in% c("Overall Efficiency", 
                                                         "%Internal Return Fine", 
                                                         "%External Return Fine", 
                                                         "Shatter Index")]
      dep_summary=summary_table(dep_sinter)
      DT::datatable(dep_summary)   
   })
   
   var6=function(){ 
      
   sinter_new = list()
   for (i in 1:length(sinter)){
      sinter_new[[i]] = sinter[[i]][, names(sinter[[i]]) %in% c("Overall Efficiency", 
                                                                   "%Internal Return Fine", 
                                                                   "%External Return Fine", 
                                                                   "Shatter Index")]
         }
   return(sinter_new)
   }
   
   output$data_no = renderDataTable({
      sinter_new=var6()
      dep_no = rbind(t(apply(sinter_new[[1]], 2, function(x) (sum(!is.na(x))))),
                     t(apply(sinter_new[[2]], 2, function(x) (sum(!is.na(x))))),
                     t(apply(sinter_new[[3]], 2, function(x) (sum(!is.na(x))))),
                     t(apply(sinter_new[[4]], 2, function(x) (sum(!is.na(x))))))
      
      rownames(dep_no) = make.names(c("Plant 1", "Plant 2", "Plant 3", "Plant 4"))
      datatable(dep_no,  options = list(dom = 't'))
   })
   
   output$data_mean = renderDataTable({
      sinter_new=var6()
      dep_mean = rbind(t(round(apply(sinter_new[[1]], 2, mean, na.rm=TRUE), 2)),
                      t(round(apply(sinter_new[[2]], 2, mean, na.rm=TRUE), 2)),
                      t(round(apply(sinter_new[[3]], 2, mean, na.rm=TRUE), 2)),
                      t(round(apply(sinter_new[[4]], 2, mean, na.rm=TRUE), 2)))
      rownames(dep_mean) = make.names(c("Plant 1", "Plant 2", "Plant 3", "Plant 4"))
      
      datatable(dep_mean,  options = list(dom = 't'))
   })
   
   output$data_sd = renderDataTable({
      sinter_new=var6()
      dep_sd = rbind(t(round(apply(sinter_new[[1]], 2, sd, na.rm=TRUE), 2)),
                     t(round(apply(sinter_new[[2]], 2, sd, na.rm=TRUE), 2)),
                     t(round(apply(sinter_new[[3]], 2, sd, na.rm=TRUE), 2)),
                     t(round(apply(sinter_new[[4]], 2, sd, na.rm=TRUE), 2)))
      rownames(dep_sd) = make.names(c("Plant 1", "Plant 2", "Plant 3", "Plant 4"))
      datatable(dep_sd,  options = list(dom = 't'))
   })
      
   output$data_na = renderDataTable({
      sinter_new=var6()
      dep_na = rbind(t(apply(sinter_new[[1]], 2, function(x) sum(is.na(x)))),
                     t(apply(sinter_new[[2]], 2, function(x) sum(is.na(x)))),
                     t(apply(sinter_new[[3]], 2, function(x) sum(is.na(x)))),
                     t(apply(sinter_new[[4]], 2, function(x) sum(is.na(x)))))
      rownames(dep_na) = make.names(c("Plant 1", "Plant 2", "Plant 3", "Plant 4"))
      
      datatable(dep_na,  options = list(dom = 't'))
   })
  
   output$data_plant = renderDataTable({
      var7 = var7()
      
      sinter_list = switch(var7, 
                           "1" = sinter[[1]],
                           "2" = sinter[[2]],
                           "3" = sinter[[3]],
                           "4" = sinter[[4]]
      )
      
      datatable(sinter_list)   
   })
   
   output$data_feed = renderDataTable({
      sinter=feed_data()
      dep_summary=summary_table(sinter)
      datatable(dep_summary, options = list(dom = 't'))   
   })
   
   output$ignition_hood = renderDataTable({
      sinter=ignition_data()   
      dep_summary=summary_table(sinter)
      datatable(dep_summary, options = list(dom = 't'))   
   })
   
   output$sinter_bed = renderDataTable({
      sinter=sinter_bed()
      dep_summary=summary_table(sinter)
      datatable(dep_summary, options = list(dom = 't'))   
   })
   
   
   output$stack = renderDataTable({
      sinter=stack()
      dep_summary=summary_table(sinter)
      datatable(dep_summary, options = list(dom = 't'))   
   })
   
   output$esp = renderDataTable({
      sinter=esp()
      dep_summary=summary_table(sinter)
      datatable(dep_summary, options = list(dom = 't'))   
   })
   
   
   output$cooler = renderDataTable({
      sinter=cooler()
      dep_summary=summary_table(sinter)
      datatable(dep_summary, options = list(dom = 't'))   
   })
   
   feed_choices=c("Select All", names(feed_data()))
   ignition_choices=c("Select All", names(ignition_data()))
   stack_choices =c("Select All", names(stack()))
   sinter_choices = c("Select All", names(sinter_bed()))
   esp_choices = c("Select All", names(esp()))
   cooler_choices = c("Select All", names(cooler()))

   # 
   observe({
      if("Select All" %in% input$in_feed){
         selected_choices1=feed_choices[-1] # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_feed", selected = selected_choices1)
      } else
         selected_choices1=input$in_feed # update the select input with choice selected by user
         
   })
    
   observe({
      if("Select All" %in% input$in_ignition){
         selected_choices2=ignition_choices[-1] # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_ignition", selected = selected_choices2)
       } else
         selected_choices2=input$in_ignition # update the select input with choice selected by user
     
   })

   observe({
      if("Select All" %in% input$in_sinter_bed){
         selected_choices3=sinter_choices[-1] # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_sinter_bed", selected = selected_choices3)
      } else
         selected_choices3=input$in_sinter_bed # update the select input with choice selected by user
      
   })

   observe({
      if("Select All" %in% input$in_stack){
         selected_choices4=names(stack()) # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_stack", selected = selected_choices4)
      }else
         selected_choices4=input$in_stack # update the select input with choice selected by user
   })
   # 
   observe({

      if("Select All" %in% input$in_esp){
         selected_choices5=names(esp()) # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_esp", selected = selected_choices5)
      } else
         selected_choices5=input$in_esp # update the select input with choice selected by user
   })
   # 
   observe({
      if("Select All" %in% input$in_cooler){
         selected_choices6=names(cooler()) # choose all the choices _except_ "Select All"
         updateSelectInput(session, "in_cooler", selected = selected_choices6)
      }
      else
         selected_choices6=input$in_cooler # update the select input with choice selected by user
   })

   var_feed_sel=reactive({
      input$in_feed

   })
   
   var_ignition_sel=reactive({
      input$in_ignition
      
   })
   
   var_sinter_sel=reactive({
      input$in_sinter_bed
      
   })
   
   var_stack_sel=reactive({
      input$in_stack
      
   })

   var_esp_sel=reactive({
      input$in_esp
      
   })
   
   var_cooler_sel=reactive({
      input$in_cooler
      
   })
   correlation =reactive({
      sinter=read_rename_final_data()
      var_feed_sel = var_feed_sel()
      if("Select All" %in% var_feed_sel)
         var_feed=feed_choices[-1] # choose all the choices _except_ "Select All"
      else
          var_feed = var_feed_sel
      
      var_ignition_sel = var_ignition_sel()
      if("Select All" %in% var_ignition_sel)
         var_ignition=ignition_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_ignition = var_ignition_sel
      

      var_sinter_sel = var_sinter_sel()
      if("Select All" %in% var_sinter_sel)
         var_sinter=sinter_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_sinter = var_sinter_sel
      
      var_stack_sel = var_stack_sel()
      if("Select All" %in% var_stack_sel)
         var_stack=stack_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_stack = var_stack_sel
      
      var_esp_sel = var_esp_sel()
      if("Select All" %in% var_esp_sel)
         var_esp=esp_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_esp = var_esp_sel
      
      var_cooler_sel = var_cooler_sel()
      if("Select All" %in% var_cooler_sel)
         var_cooler=cooler_choices[-1] # choose all the choices _except_ "Select All"
      else
         var_cooler = var_cooler_sel
      
      new_dataset = cbind(sinter[,var_feed], 
                          sinter[,var_ignition],
                          sinter[,var_stack],
                          sinter[,var_sinter],
                          sinter[,var_esp], 
                          sinter[,var_cooler])
         new_dataset = new_dataset[complete.cases(new_dataset),]
         cor(new_dataset, 
             use = "pairwise.complete.obs", method = "pearson")
   })
   output$corrPlot = renderPlot({
      val=correlation()
      #if(is.null(val)) return(NULL)
      
      val[is.na(val)] = 0
      args <- list(val)
      do.call(corrplot, c(args, method="color",  
                          type="upper",
                           tl.srt =45, #Text label color and rotation,
                          addCoef.col = "black", # Add coefficient of correlation
                          diag=FALSE ))
      
   })
   
   output$warning <- renderUI({
      val <- correlation()
      if(is.null(val)) {
         tags$i("Waiting for data input...")
      } else {
         isNA <- is.na(val)
         if(sum(isNA)) {
            tags$div(
               tags$h4("Warning: The following pairs in calculated correlation have been converted to zero because they produced NAs!"),
               helpText("Consider using an approriate NA Action to exclude missing data"),
               renderTable(expand.grid(attr(val, "dimnames"))[isNA,]))
         }
      }
   })
   
}) 