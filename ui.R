
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    textInput("treat1","Treatment 1:",value="Treatment 1"),
    textInput("treat2","Treatment 2:", value="Treatment 2"),
    textInput("var1","Variable 1:", value= "A"),
    textInput("var2","Variable 2:",value= "B")
  
  )
)

body <- dashboardBody(
  fluidRow(
    box(title = "Data Entry", width=8,
        column(width=4,textOutput("tr1"), rHandsontableOutput("table1")),
        column(width=4,textOutput("tr2"),rHandsontableOutput("table2"))
    ),
    box(title = "Plotting means", width = 4,
        plotOutput("plot1"),
        radioButtons("errortype", "Error bars:", choices=c("95% Confidence interval"="ci","Standard error (SE)"="se","Standard deviation (S)"="sd"))#add CI
    )
  ),
  fluidRow(
    box(title="Summary Statistics", width = 8,
        dataTableOutput("summary_table")
    ),
    infoBoxOutput("significance",width=4)
  )
)

# Put them together into a dashboardPage
dashboardPage(skin = "red",
  dashboardHeader(title = "beans"),
  sidebar,
  body
)