

library(shinydashboard)
<<<<<<< Updated upstream
=======
library(rhandsontable)
##need to figure out how to have conditional messages if there is not enough data yet to fill blank boxes
>>>>>>> Stashed changes

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Example",tabName="example",icon=icon("stats",lib="glyphicon")),
    menuItem("test",tabName="test"),br(),
    textInput("treat1","Treatment 1:",value="Treatment 1"),
    textInput("treat2","Treatment 2:", value="Treatment 2"),
    textInput("var1","Variable A:", value= "A"),
    textInput("var2","Variable B:",value= "B")
  
  ) 
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="example",
      fluidRow(
<<<<<<< Updated upstream
        box(title="Using this app",width=3,status="primary",
            p("This app demonstrates the comparison of 2 treatment groups, for which the proportion of 'Variable A' between the treatments is the measure of interest."),
            p("For the 2 treatments and variables, use the left-hand side to rename what you are measuring then enter your data in the tables provided."),br(),
            p("Pressing 'Run Data' will calculate the proportion of variable A and its' sumamry statistics, along with a plot showing mean values. Use inputs to change error values."),br(),
            p("In the bottom right corner, a one-way ANOVA will be run comparing your two treatment groups."),br(),
            actionButton("getdata","Run Data"),br(),
            p("To replicate these analyses in R, download the data and code to produce the same outputs"),br(),
            downloadButton("downloadData","Download Data"),
            actionButton("opencode","See R code",icon=icon("calculator",lib="font-awesome"),onclick="window.open('https://raw.githubusercontent.com/collnell/beans/master/beans_DIY','_blank')")),
=======
        box(title = "Comparing and plotting means",width=3,solidHeader=TRUE,status="primary",collapsible = TRUE,
            p("Use the text boxes on the left to name the treatments and variables. Enter data in the respective tables."),br(),
            p("Pressing 'Run Data' initiates a one-way ANOVA test on the data entered in the tables. 
              This test evaluates whether the variable of interest differs between the two treatment groups"),br(),
            p("Summary statistics will also be calculated and plotted. Use the controls to select which variables are plotted."),br(),
            p("Note: data must be entered for both treatments in order for the outputs to work."),br(),
            actionButton("getdata","Run Data"),br(),br(),
            p("To replicate these analyses in R, use download the data and code that produces these outputs."),
            downloadButton("downloadData","Download Data"),
            downloadButton("downloadplotr","Get code")
        ),
>>>>>>> Stashed changes
        box(title = "Data Entry", width=5,solidHeader = TRUE,status="primary",
            column(width=5,textOutput("tr1"), rHandsontableOutput("table1")),
            column(width=5,textOutput("tr2"),rHandsontableOutput("table2"))
        ),
        tabBox(id = "plots",width = 4,
            tabPanel("Plotting means",plotOutput("plot1"),
              radioButtons("errortype", "Error bars:", 
<<<<<<< Updated upstream
                           choices=c("95% Confidence interval"="ci","Standard error (SE)"="se","Standard deviation (S)"="sd"))),
=======
                           choices=c("95% Confidence interval"="ci","Standard error (SE)"="se","Standard deviation (S)"="sd")),
              htmlOutput("selectUI")
              ),
>>>>>>> Stashed changes
            tabPanel("Histogram",plotOutput("histo"),
              checkboxInput("showmean","Show means", value=FALSE),
              htmlOutput("selectUI"))
        )
      ),
      fluidRow(
        box(title="Data Summary", width = 6,status="primary",
<<<<<<< Updated upstream
=======
            textOutput("vartext"),
>>>>>>> Stashed changes
            rHandsontableOutput("summary_table"), br(),br()
        ),
        box(title="One-way ANOVA", width=6, status="primary",
            verbatimTextOutput("anovatable"))
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
