library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

sinter = read.csv("syntheticData_20190520.csv",
                  stringsAsFactors = FALSE, header = TRUE)

#Remove underscores and change column names to title format for display
names(sinter)=str_to_title(gsub("_", " ", names(sinter)))


#Correlation plot for dependent variables                                
corr_plot=box(title=tags$a(class="primary-title", 
                           style='margin-left:15px; font-type:bold', 
                           "Correlation plot of potential dependent variables", "and Outcome"), width = NULL, 
              background = "black", solidHeader = TRUE, 
              plotOutput('correlation_dependent'))


#tab1 - dependent variables layout
dependent_layout = fluidRow( 
    column(width=5,
           corr_plot
    )
    #,
    # column(width=3.5,
    #     fluidRow(
    #        irt_layout,
    #        ert_layout
    #     )
    # ),
    # column(width=3.5,
    #     fluidRow(
    #         ti_layout,
    #         si_layout
    #     )
    # )
)

#List only required independent variables
ind_var=names(sinter)[c(2:3, 11:19)]

#choice of independent var
radio_but_indep_var = radioButtons("radio_choice", "", 
                                   choices = ind_var,
                                   selected = ind_var[1])

#choice of plots
plot_type=selectInput("select_plot", "Type of Plot", 
                      choices = c("Correlation Plot/Histogram", "Scatter Plot (numerical variables)",
                                  "Box Plot (categorical variables)",
                                  "Box plot"))

#tab2 - independent variables layout            
independent_layout = fluidRow( 
    column(width=3, 
           box(title="Independent Variables", width = NULL,
               column(12,
                      radio_but_indep_var
               ))),
    column(width=7,
           fluidRow(
               plot_type    
           ),
           fluidRow(
               conditionalPanel(
                   condition="input.select_plot == 'Correlation Plot/Histogram'",
                   column(4, plotOutput("hist")),
                   column(8, plotOutput("corr_ind"))
                   
               ), 
               
               conditionalPanel(
                   condition="!input.select_plot == 'Correlation Plot/Histogram'",
                   plotOutput("plot_choice")
               ) 
           )
    )
)

model_layout = fluidRow()


#List the tabs
tab_dependent = tabPanel("Potential Dependent Variables",
                         dependent_layout)

tab_independent = tabPanel("Potential InDependent Variables",
                           independent_layout)

tab_model = tabPanel("Model and Recommendation",
                     model_layout)


#Change background color of header
title_color=tags$head(tags$style(HTML('
            /* logo */
                .skin-black .main-header .logo {
                    background-color: #4B0082;},
            /* logo when hovered */
                .skin-black .main-header .logo:hover {
                    background-color: #4B0082;}
                            ')))

#Increase height of header
header_height=tags$li(class = "dropdown",
                      tags$style(".main-header {max-height: 100px}"),
                      tags$style(".main-header .logo {height: 100px}")
)

#Add logo to header
title_logo=span(column(1, tags$img(src='logo.jpg', height='100', width='250', border='0', 
                                   style='margin-left:-2px; padding:0; margin:0; display:block; font-size:0')), 
                column(8, class="title-box",
                       tags$h2(class="primary-title", style='margin-top:50px; margin-left:5px; color:white', 
                               "SINTER PLANT PROJECT")
                )
)


#DASHBOARDING
shinyUI(dashboardPage(skin="black", 
                      dashboardHeader(header_height, title = title_logo,
                                      titleWidth='100%'),
                      
                      dashboardSidebar(disable = TRUE),
                      
                      dashboardBody(
                          title_color,
                          tabsetPanel(
                              tab_dependent,
                              tab_independent,
                              tab_model
                          ) 
                      )
))
