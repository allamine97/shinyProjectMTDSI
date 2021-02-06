library(shiny)
library(shinythemes)
library(shinyWidgets)
library(rmarkdown)
library(knitr)
library(pander)
ui <- fluidPage(setBackgroundColor("ghostwhite"),theme = shinytheme("cerulean"),navbarPage(title = "Data Analysis", windowTitle ="Data Analysis", id="tabactive", 
                            
                            tabPanel("Analyse Descriptive",icon = icon("table"),titlePanel("Analyse descriptive"),
                                     column(3,
                                            wellPanel(
                                              fileInput("datafile", "Importer le fichier",placeholder = "choisissez un fichier", accept = "csv"),
                                              selectInput(inputId = "var", label = "separateur", c("point virgule" = ";",
                                                                                                   "point"=".",
                                                                                                   "virgule" = ",",
                                                                                                   "deux points" = ":")),
                                            ),
                                           # br(),
                                            wellPanel(
                                              radioButtons("dist", "Description:",
                                                           c("Summary" = "summary",
                                                             "Head" = "head"
                                                             ), selected = "head"),
                                              radioButtons("format", "Download report:", c("PDF", "HTML", "Word"),
                                                           inline = TRUE
                                              ),
                                              downloadButton("downloadReport")
                                            )
                                     ),
                                     
                                     column(9,
                                            verbatimTextOutput("strfile"),
                                            dataTableOutput("table")
                                     ),
                                     
                            ),                                                                                             
                            tabPanel("Regression Liniaire",icon = icon("line-chart"),
                                     titlePanel("Regression liniaire"),
                                       column(3,wellPanel(
                                       selectInput(inputId = "variable1", label = "variable 1",choices =""),
                                       selectInput(inputId = "variable2", label = "variable 2",choices =""),
                                       #actionButton(inputId = "update", label = "Visualiser")
                                       actionBttn(
                                         inputId = "update",
                                         label = "Visualiser", 
                                         style = "jelly",
                                         color = "primary"
                                       )
                                       ),wellPanel(
                                         radioButtons("format1", "Download report:", c("HTML", "PDF", "Word"),
                                                      inline = TRUE
                                         ),
                                         downloadButton("downloadReport1")
                                       )
                                       
                                       ),
                                     column(9,
                                            HTML('<h4>Sommaire de la regression</h4>'),
                                            verbatimTextOutput("summary"),
                                            uiOutput("results"),
                                            br(),
                                            plotOutput("plot")
                                     )
                                     
                                     ), 
                            tabPanel("Analyse Multivariee",icon = icon("bar-chart-o"),
                                     titlePanel("Analyse Multivariee"),
                                     column(3,
                                            wellPanel(
                                              radioButtons("dis", "Choisir:",
                                                           c("ACP" = "cpa",
                                                             "AFC" = "afc",
                                                             "ACM"="acm"
                                                           ), selected = "cpa"),
                                              actionBttn(
                                                inputId = "am",
                                                label = "Visualiser", 
                                                style = "jelly",
                                                color = "primary"
                                              )
                                            ),wellPanel(
                                              radioButtons("format2", "Download report:", c("HTML", "PDF", "Word"),
                                                           inline = TRUE
                                              ),
                                              downloadButton("downloadReport2")
                                            )
                                       
                                     ),
                                     column(9, fluidRow(
                                       column(12,  plotOutput("plot1")),
                                       column(12,  plotOutput("plot2")),
                                       column(12,  plotOutput("plot3")),
                                       column(12,  plotOutput("plot4"))
                                     ) )
                                     ),
                            tabPanel("Model",icon = icon("list-alt")) 
                            
))

