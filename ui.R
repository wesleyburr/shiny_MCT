library(shiny)
shinyUI(fluidPage(
  titlePanel("Analyze the Quality of your Multiple-Choice Test"),
  sidebarLayout(
    sidebarPanel(
      helpText("Instructions: Upload the .csv file of the Scantron results you received from the
               printshop. File must be in the following format: Row 1 = column names, Row 2 = 
               answer key, remaining rows each represent one student's responses. Column 1 = ID 
               (e.g., student number), Column 2 = DEPT (e.g., MATH), Column 3 = COURSE (e.g., 1051). 
               For example, if you had an exam with 25 students and 40 questions, the .csv should
               have 27 rows and 43 columns."),
      br(),
      fileInput("file", "Upload your .csv", multiple = FALSE, 
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")), 
      br(),
      helpText("Once you've uploaded your .csv file, you can select one of the tabs from
               the menu to see diagnostics on the test."),
      br(),
      span(textOutput("invalid_key"), style = "color:red"),
      br(),
      uiOutput("examinfo")
    ),
    mainPanel(
      uiOutput("tb"),
      uiOutput("downloadb1")
      
    )
    
  )
))