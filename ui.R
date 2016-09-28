
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Example",tabName="example",icon=icon("stats",lib="glyphicon")),
    menuItem("test",tabName="test"),br(),
    textInput("treat1","Treatment 1:",value="Treatment 1"),
    textInput("treat2","Treatment 2:", value="Treatment 2"),
    textInput("var1","Variable 1:", value= "A"),
    textInput("var2","Variable 2:",value= "B")
  
  ) 
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="example",
      fluidRow(
        box(title = "Data Entry", width=6,solidHeader = TRUE,status="primary",
            p("Enter data here, then press 'Run data' to generate summary statistics and plot. 
              To replicate these analyses in R, use the 'Download Data' and 'Get Code' buttons to see how it was done."),
            column(width=5,textOutput("tr1"), rHandsontableOutput("table1"), downloadButton("downloadData","Download Data")),
            column(width=5,textOutput("tr2"),rHandsontableOutput("table2"),actionButton("getdata","Run Data"))
        ),
        tabBox(id = "plots",width = 4,
            tabPanel("Plotting means",plotOutput("plot1"),
              radioButtons("errortype", "Error bars:", 
                           choices=c("95% Confidence interval"="ci","Standard error (SE)"="se","Standard deviation (S)"="sd")),
              downloadButton("downloadplotr","Get code")),
            tabPanel("Histogram",plotOutput("histo"),
              checkboxInput("showmean","Show means", value=FALSE))
        )
      ),
      fluidRow(
        box(title="Data Summary", width = 6,status="primary",
            rHandsontableOutput("summary_table")
        ),
        infoBoxOutput("significance",width=4)
      )
    ),
    tabItem(tabName="test",
            h2("this is a test")
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(skin = "red",
  dashboardHeader(title = "beans"),
  sidebar,
  body
)