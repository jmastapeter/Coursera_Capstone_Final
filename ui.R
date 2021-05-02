#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that engages user and allows display of tables, plots, and text
shinyUI(fluidPage(

    # Application title
    titlePanel("Next Word Predictions"),

    # Sidebar for the input of relevant text files
    sidebarLayout(
        sidebarPanel(
            fileInput('file1','Select the Text File of Interest', multiple = TRUE,
                                      accept <- c('text/csv','text/comma-separated-values,text/plain','.csv')),
                        tags$hr(),
            fileInput('file2','Select the Text File of Interest', multiple = TRUE,
                      accept <- c('text/csv','text/comma-separated-values,text/plain','.csv')),
            tags$hr(),
            fileInput('file3','Select the Text File of Interest', multiple = TRUE,
                      accept <- c('text/csv','text/comma-separated-values,text/plain','.csv')),
            tags$hr(),
            
            selectInput("graph", "Choose Plot to View:",
                        choices = 
                            c("Text 1 Single Words", "Text 1 Word Pairs", "Text 1 Word Triples",
                              "Text 2 Single Words", "Text 2 Word Pairs", "Text 2 Word Triples",
                              "Text 3 Single Words", "Text 3 Word Pairs", "Text 3 Word Triples",
                              "Complete Text Sample Single Words", "Complete Text Sample Word Pairs", "Complete Text Sample Word Triples")),
            selectInput("table", "Choose Source Test for Prediction based on Triples:",
                        choices = c("Complete Data", "Text 1", "Text 2", "Text 3")),
            selectInput("table2", "Choose Source Test for Prediction based on Pairs:",
                        choices = c("Complete Data", "Text 1", "Text 2", "Text 3")),
            textInput("text", "Input Single Word or Pair for Prediction")
            

        ),

        # Tabular Selection for user to view either pre-processing, data summary, selected plot, or next work predictions.
        mainPanel(
            tabsetPanel(
                tabPanel("Pre-Processing", HTML(
                    paste(
                        h3("Use following code to pre-process data:"), '<br/>',
                        h4("file <- readLines('file.txt')"),
                        h4("file_smpl <-sample(file, length(file)*0.01)"), 
                        h4("write.table(file_smpl, 'file_smpl.tx')"), '<br/>',
                        h5('Adjust sample size as necessary for exceptionally large files')
                    )
                )),
                tabPanel("Data Summary", tableOutput("smmry_tbl")),
                tabPanel("Selected Plot", plotOutput("selected_graph")),
                tabPanel("Predict Next Word", verbatimTextOutput("text"))
        ))
    ))
)
