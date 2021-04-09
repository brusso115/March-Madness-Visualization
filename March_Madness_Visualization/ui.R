
# Define UI for application that draws a histogram

navbarPage("",
           #theme = bslib::bs_theme(bootswatch = "darkly"),
           header = tagList(
             useShinydashboard()
           ),

  tabPanel(HTML("<a>Home</a>"),
           HTML("<h1 id=DataSummary>Data Summary</h1>"),
           br(),
           
    verbatimTextOutput("Home"),
    sidebarLayout(
      sidebarPanel(
        varSelectInput(inputId="TeamStat",
                       label="Game Statistic",
                       data=RegSeasonData[c(-1,-2,-3,-17,-56)])

      ),
      mainPanel(
          plotOutput("boxPlot")
      )
    ),
    # sidebarLayout(
    #   sidebarPanel(
    #     varSelectInput(inputId="TourneyTeamStat",
    #                    label="Regular Season Team Statistic",
    #                    data=RegSeasonTT[c(-1,-2,-3,-17)])
    #     
    #   ),
    #   mainPanel(
    #     plotOutput("violinPlot"),
    #     br()
    #   )
    # ),
    # fluidRow(infoBoxOutput("ROneAvg"),infoBoxOutput("RTwoAvg"),
    #          infoBoxOutput("SweetSixteen"),infoBoxOutput("EliteEight"),
    #          infoBoxOutput("FinalFour"),infoBoxOutput("NationalChampionship"),infoBoxOutput("Champion")),
    br(),
    HTML("<h1 id=AdjustedStats>Adjusted Stats</h1>"),
    br(),
    splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("scatter"), dataTableOutput("OETable")),
    br(),
    splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("scatterAdjusted"), dataTableOutput("AdjustedOETable")),
    br(),
    sidebarLayout(
      sidebarPanel(
        varSelectInput(inputId="PairedStat",
                       label="Regular Season Team Statistic",
                       data=RegSeasonDataGrouped[c(-1,-2,-3,-4,-56)])
      ),
      mainPanel(
        plotlyOutput("AdjVsNonAdj")
      )
    ),
    
    
    # sidebarLayout(
    #   sidebarPanel(
    #     varSelectInput(inputId="TeamAdjustedStat",
    #                    label="Game Statistic",
    #                    data=RegSeasonDataAdjusted[c(-1,-2,-3,-4,-5,-6)])
    #   ),
    #   mainPanel(
    #     plotOutput("boxPlotAdjusted")
    #   )
    # ),
    
    HTML("<h1 id=SuccessfulTT>Traits of Successful Tournament Teams</h1>"),
    br(),
    sidebarLayout(
      sidebarPanel(
        varSelectInput(inputId="TourneyTeamStat",
                       label="Regular Season Team Statistic",
                       data=RegSeasonTT[c(-1,-2,-3,-17)])

      ),
      mainPanel(
        plotOutput("violinPlot"),
        br()
      )
    ),
    fluidRow(infoBoxOutput("ROneAvg"),infoBoxOutput("RTwoAvg"),
             infoBoxOutput("SweetSixteen"),infoBoxOutput("EliteEight"),
             infoBoxOutput("FinalFour"),infoBoxOutput("NationalChampionship"),infoBoxOutput("Champion")),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId="Season",
                       label="Season",
                       choices=c(2010:2019)
        )
      ),
      mainPanel(
        dataTableOutput("mainDF")
      )
    ),
    
    HTML("<h1 id=MakingPredictions>Making Predictions</h1>"),
    br(),

    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId="Team1",
                       label="Team 1",
                       choices=team_averages$TeamName),
        selectizeInput(inputId="Team2",
                       label="Team 2",
                       choices=team_averages$TeamName)
      ),
      mainPanel(
        htmlOutput("Predictions")
      )
    ),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  ),
  tabPanel(HTML("<a href='#DataSummary'>Data Summary</a>")),
  tabPanel(HTML("<a href='#AdjustedStats'>Adjusted Stats</a>")),
  tabPanel(HTML("<a href='#SuccessfulTT'>Traits of Successful Tournament Teams</a>")),
  tabPanel(HTML("<a href='#MakingPredictions'>Making Predicitions</a>"))
)

# ui <- shinyUI(fluidPage(
#   
#   navbarPage("",
#              tabPanel(HTML("<a href='#plot3'>Home</a> "), 
#                       verbatimTextOutput("Home"),
#                       tableOutput("cars"),
#                       plotOutput('plot'),
#                       plotOutput('plot2'),
#                       plotOutput('plot3')),
#              tabPanel(HTML("<a href='#cars'>Test1</a> ")), 
#              tabPanel(HTML("<a href='#plot'>Test2</a> ")),
#              tabPanel(HTML("<a href='#plot2'>Test3</a> ")),
#              tabPanel(HTML("<a href='#plot3'>Test4</a> "))
#              
#   )))