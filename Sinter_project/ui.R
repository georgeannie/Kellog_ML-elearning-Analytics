library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
source("sinter_function.R")

sinter= read_rename_sinter()

#PLOTS
#Correlation plot for dependent variables                                
corr_plot_dep= plotOutput('correlation_dependent', height="400px", width = 'auto')

#Histogram dependent
hist_dep  =   plotOutput('hist_dependent', width = "auto", height="350px")

#Scatter plot - dependent vs kpi
scatter_dep =plotOutput('scatter_dependent', width = "auto", height="350px")

#radio button potential dependent
#1. choice of dependent var
radio_dep = radioButtons("radio_dep_choice", "Potential Dependent Variables Plots", 
                          choices = list("Histogram" = 1, 
                                         "Correlation matrix" =2,
                                         "Summary" = 3, 
                                         "Data" =4),
                          selected = 1)

#Tab layouts
#1. NavBar Menu= Dependent variables
#a. Stationary layout - change panel based on choice
dependent_layout_row1 = fluidRow( 
    column(width=2, 
           wellPanel(radio_dep)
    ),
    column(width=10, 
             conditionalPanel(
                 condition="input.radio_dep_choice == 1", 
                 fluidRow(selectInput("plant", "Select Plant:", 
                                           choices=c('Plant 1' =1, 
                                                     'Plant 2' =2, 
                                                     'Plant 3' = 3,
                                                     'Plant 4'  = 4)),
                                            selected=choices[[1]]),
                 fluidRow(
                   box(title="", 
                     solidHeader = TRUE, width="auto", 
                     plotOutput("histogram", height="350") )
                 )),
             conditionalPanel(
                 condition=("input.radio_dep_choice == 2"), 
                 fluidRow(selectInput("plant", "Select Plant:", 
                                      choices=c('Plant 1' =1, 
                                                'Plant 2' =2, 
                                                'Plant 3' = 3,
                                                'Plant 4'  = 4)),
                          selected=choices[[1]]),
                 fluidRow(
                     box(title="", 
                         solidHeader = TRUE, width="auto",
                         plotOutput("scatter_matrix", height="380"))
                     ),
                 fluidRow(
                    box(title="", 
                     solidHeader = TRUE, width="auto",
                     plotOutput("correlation_matrix", width = "auto", height="400"))
                )),
             conditionalPanel(
               condition=("input.radio_dep_choice == 3"),
                 dataTableOutput("summary")
                 ),

             conditionalPanel(
                condition=("input.radio_dep_choice== 4"),
                dataTableOutput("data")
              )
    )
)
   
model_layout = fluidRow()


#List the tabs
tab_dependent = tabPanel(id="taba", "Exploratory Analysis",
                         dependent_layout_row1)

tab_model = tabPanel(id="tabb", "Model and Recommendation",
                     model_layout)


#Change background color of header
title_color=tags$head(tags$style(HTML('
            /* logo */
                .skin-black .main-header .logo {
                    background-color: #white;},                  ## #4B0082;},
            /* logo when hovered */
                .skin-black .main-header .logo:hover {
                    background-color: #4B0082;}
                            ')))

#Increase height of header
header_height=tags$li(class = "dropdown",
                      tags$style(".main-header {max-height: 580px}"),
                      tags$style(".main-header .logo {height: 70px}")
)

#Add logo to header
title_logo=span(column(1, tags$img(src='kellogg_logo.jpg', height='40', width='260', border='0', 
                                   style='margin-left:-10px; padding:0px 0px; margin-top:10px; 
                                   display:block; font-size:0')), 
                column(8, class="title-box",
                       tags$h2(class="primary-title", style='margin-top:20px; margin-left:470px; 
                       color:#4B0082; 
                               font-size:8', 
                               "SINTER PLANT ANALYSIS")
                )
)


#DASHBOARDING
shinyUI(dashboardPage(skin="black", 
                      dashboardHeader(header_height, title = title_logo,
                                      titleWidth='100%'),
                      
                      dashboardSidebar(disable = TRUE),
                      
                      dashboardBody(
                          title_color,
                          navbarPage(tags$h4("Sinter plant Viz", 
                                style='margin-top:2px; margin-left:10px; color:black; 
                                      font-size:10; font-weight:bold'),     
                              tab_dependent,
                              tab_model
                          ) 
                      )
))
