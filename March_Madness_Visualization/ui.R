
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

    HTML("<h1 id=SOS>Strength of Schedule</h1>"),
    splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("barChartSOS"),plotlyOutput("barChartConfStrength")),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId="TeamName",
                       label="Team Name",
                       choices=SOS$TeamName,
                    selected = c(1)
                    ),
                      
      ),
      mainPanel(
        infoBoxOutput("SOSRanking"),
        infoBoxOutput("ConfRanking"),
        infoBoxOutput("Conf")
      )
    ),
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
    HTML("<h1 id=HCA>Home Court Advantage</h1>"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId="SeasonHCA",
                       label="Season",
                       choices=c(HCA$Season),
                    selected = c(1)
                       
        ),
        varSelectInput(inputId="GameLocStat",
                       label="Game Statistic",
                       data=HCA[c(-1,-2, -3)]
        )
      ),
      mainPanel(
        plotlyOutput("HCABarChart"),
      )
    ),
    sidebarLayout(
      sidebarPanel(
        varSelectInput(inputId="GameLocStatLineChart",
                       label="Game Statistic",
                       data=HCA[c(-1,-2, -3)]
        )
      ),
      mainPanel(
        plotlyOutput("HCALineChart"),
      )
    ),
    HTML("<h1 id=SuccessfulTT>Traits of Successful Tournament Teams</h1>"),
    br(),
    sidebarLayout(
      sidebarPanel(
        varSelectInput(inputId="TourneyTeamStat",
                       label="Regular Season Team Statistic",
                       data=RegSeasonTTGrouped[c(-1,-2,-3,-4,-5,-17)])

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
    fluidRow(column(3,
          selectizeInput(inputId="Team1",
                         label="Team 1",
                         choices=team_averages$TeamName
                         )
      
    ),
    column(6,
      br(),
      br(),
      br(),
      br(),
      htmlOutput("Predictions")
    ),
    column(3,
        selectizeInput(inputId="Team2",
                       label="Team 2",
                       choices=team_averages$TeamName)
    )
    ),
    br(),
    br(),
    br(),
    br(),

  ),
  tabPanel(HTML("<a href='#DataSummary'>Data Summary</a>")),
  tabPanel(HTML("<a href='#SOS'>Strength of Schedule</a>")),
  tabPanel(HTML("<a href='#AdjustedStats'>Adjusted Stats</a>")),
  tabPanel(HTML("<a href='#HCA'>Home Court Advantage</a>")),
  tabPanel(HTML("<a href='#SuccessfulTT'>Traits of Successful Tournament Teams</a>")),
  tabPanel(HTML("<a href='#MakingPredictions'>Making Predicitions</a>"))
)
