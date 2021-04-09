#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output) {
  #thematic::thematic_shiny()


  output$boxPlot <- renderPlot({

    RegSeasonData %>% ggplot(RegSeasonData, mapping = aes(x=!!input$TeamStat,fill=Outcome)) + geom_boxplot() +
      ggtitle(paste("Winning Team vs Losing Team Distribution of", input$TeamStat,sep=' '))

  })
  
  output$violinPlot <- renderPlot({
      ggplot(RegSeasonTTGrouped,mapping = aes(x=as.factor(NumberOfTournamentWins),y=!!input$TourneyTeamStat,fill=as.factor(NumberOfTournamentWins))) + 
      geom_violin() + stat_summary(fun=mean,geom="point",shape=1,color='red') + ggtitle(paste("Distribution of ", " Seperated by Round in Tournament Where Team Lost",sep=as.character(input$TourneyTeamStat))) +
      xlab("Tournament Round") + ylab(paste("Regular Season", input$TourneyTeamStat,sep=' ')) +
      scale_x_discrete(labels=c("Lost In Round of 64","Lost In Round of 32","Lost In Sweet 16","Lost In Elite 8","Lost In Final Four","Lost In National Championship","Won National Championship")) +
      theme(axis.text.x = element_text(angle = 45,hjust = 1)) + scale_fill_discrete(name="",labels=c("Lost In Round of 64","Lost In Round of 32","Lost In Sweet 16","Lost In Elite 8",
                                                                                                                    "Lost In Final Four","Lost In National Championship","Won National Championship"))
  })
  
  output$ROneAvg <- renderInfoBox({
    infoBox(
      HTML(paste("Round of 64 Average",as.character(input$TourneyTeamStat),sep="<br/>")), round(mean(RegSeasonTTGrouped[RegSeasonTTGrouped$NumberOfTournamentWins==0,as.character(input$TourneyTeamStat)]),2),
      color = "yellow", fill = TRUE
    )
  })
  output$RTwoAvg <- renderInfoBox({
    infoBox(
      HTML(paste("Round of 32 Average",as.character(input$TourneyTeamStat),sep="<br/>")), round(mean(RegSeasonTTGrouped[RegSeasonTTGrouped$NumberOfTournamentWins==1,as.character(input$TourneyTeamStat)]),2),
      color = "yellow", fill = TRUE
    )
  })
  
  output$SweetSixteen <- renderInfoBox({
    infoBox(
      HTML(paste("Sweet 16 Average",as.character(input$TourneyTeamStat),sep="<br/>")), round(mean(RegSeasonTTGrouped[RegSeasonTTGrouped$NumberOfTournamentWins==2,as.character(input$TourneyTeamStat)]),2),
      color = "yellow", fill = TRUE
    )
  })
  output$EliteEight <- renderInfoBox({
    infoBox(
      HTML(paste("Elite 8 Average",as.character(input$TourneyTeamStat),sep="<br/>")), round(mean(RegSeasonTTGrouped[RegSeasonTTGrouped$NumberOfTournamentWins==3,as.character(input$TourneyTeamStat)]),2),
      color = "yellow", fill = TRUE
    )
  })
  
  output$FinalFour <- renderInfoBox({
    infoBox(
      HTML(paste("Final Four Average",as.character(input$TourneyTeamStat),sep="<br/>")), round(mean(RegSeasonTTGrouped[RegSeasonTTGrouped$NumberOfTournamentWins==4,as.character(input$TourneyTeamStat)]),2),
      color = "yellow", fill = TRUE
    )
  })
  output$NationalChampionship <- renderInfoBox({
    infoBox(
      HTML(paste("National Championship","Average",sep="<br/>")), round(mean(RegSeasonTTGrouped[RegSeasonTTGrouped$NumberOfTournamentWins==5,as.character(input$TourneyTeamStat)]),2),
      color = "yellow", fill = TRUE
    )
  })
  output$Champion <- renderInfoBox({
    infoBox(
      HTML(paste("Champion Average",as.character(input$TourneyTeamStat),sep="<br/>")), round(mean(RegSeasonTTGrouped[RegSeasonTTGrouped$NumberOfTournamentWins==6,as.character(input$TourneyTeamStat)]),2),
      color = "blue", fill = TRUE
    )
  })

  output$scatter <- renderPlotly({
    RegSeasonDataMainGrouped_Sub = RegSeasonDataMainGrouped %>% filter(.,Season=='2020')
    sub <- subset(RegSeasonDataMainGrouped_Sub,TeamName=='S Dakota St')
    p <- ggplot(RegSeasonDataMainGrouped_Sub, aes(x=OE,y=DE,color=TeamName)) + geom_point() + ggtitle("Offensive Efficiency vs Devensive Efficiency (2020 Regular Season)") + theme(legend.title = element_blank()) + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$scatterAdjusted <- renderPlotly({
    RegSeasonDataMainGrouped_Sub = RegSeasonDataMainGrouped %>% filter(.,Season=='2020')
    #RegSeasonDataWithAdjustedStats_Sub = RegSeasonDataWithAdjustedStats %>% filter(.,Season=='2020') %>% group_by(.,Season,TeamName,TeamID) %>% summarise(.,AvgAdjustedSeasonOE=mean(Adjusted_OE),AvgAdjustedSeasonDE=mean(Adjusted_DE))
    sub <- subset(RegSeasonDataMainGrouped_Sub,TeamName=='S Dakota St')
    p2 <- ggplot(RegSeasonDataMainGrouped_Sub, aes(x=Adjusted_OE,y=Adjusted_DE,color=TeamName)) + geom_point() + ggtitle("Adjusted Offensive Efficiency vs Adjusted Devensive Efficiency (2020 Regular Season)") + theme(legend.title = element_blank()) + theme(legend.position = "none")
    ggplotly(p2)
  })
  
  output$OETable <- renderDataTable({
    
    RSSorted = RegSeasonDataMainGrouped %>% filter(.,Season=='2020') %>% arrange(.,desc(OE)) %>% select(.,Season,TeamName,OE) %>% mutate(OE=round(OE,2))
    DT::datatable(RSSorted,options = list(lengthChange=FALSE, pageLength=10, searching=FALSE), rownames=FALSE)
  })
  
  output$AdjustedOETable <- renderDataTable({
    RSSorted = RegSeasonDataMainGrouped %>% filter(.,Season=='2020') %>% arrange(.,desc(Adjusted_OE)) %>% select(.,Season,TeamName,Adjusted_OE) %>% mutate(Adjusted_OE=round(Adjusted_OE,2))
    DT::datatable(RSSorted,options = list(lengthChange=FALSE, pageLength=10, searching=FALSE), rownames=FALSE)
  })
  
  output$AdjVsNonAdj <- renderPlotly({
    RegSeasonDataGroupedAll_Sub = RegSeasonDataGroupedAll %>% filter(.,Season=='2020')
    p3 <- RegSeasonDataGroupedAll_Sub %>% ggplot(RegSeasonDataGroupedAll_Sub, mapping = aes(x=!!input$PairedStat, y=!!as.name(paste0('Adjusted_',input$PairedStat)),color=TeamName)) + geom_point() +
      geom_abline(slope=1)+ theme(legend.title = element_blank()) + theme(legend.position = "none") + ggtitle(paste(paste(as.character(input$PairedStat),paste0('Adjusted_',input$PairedStat),sep=' vs '),'(2020 Regular Season)'))
    ggplotly(p3)
  })
  
<<<<<<< HEAD
=======
  # output$boxPlotAdjusted <- renderPlot({
  #   
  #   RegSeasonDataAdjusted %>% ggplot(RegSeasonDataAdjusted, mapping = aes(x=!!input$TeamAdjustedStat,fill=Outcome)) + geom_boxplot() +
  #     ggtitle(paste("Winning Team vs Losing Team Distribution of", input$TeamAdjustedStat,sep=' '))
  #   
  # })
  
  output$mainDF <- renderDataTable({
    RegSeasonTT_Sub = RegSeasonTTGrouped %>% select(.,Season,TeamID,NumberOfTournamentWins)
    RegSeasonDataMainGrouped_Joined = inner_join(RegSeasonDataMainGrouped,RegSeasonTT_Sub,by=c("Season","TeamID"))
    RegSeasonDataMainGrouped_Joined %>% select(.,Season,TeamName,Adjusted_OE,Adjusted_DE,Adjusted_EM,NumberOfTournamentWins) %>% mutate(.,Adjusted_OE=round(Adjusted_OE,2),Adjusted_DE=round(Adjusted_DE,2),Adjusted_EM=round(Adjusted_EM,2)) %>% filter(.,Season==as.name(input$Season))
  })
  
  output$Predictions <- renderUI({
    team_1 = team_averages %>% filter(.,TeamName==input$Team1)
    team_2 = team_averages %>% filter(.,TeamName==input$Team2)
    
    ExpectedOE_team1 = season_averages[,'Adjusted_OE'] + (team_1[,'Adjusted_OE'] - season_averages[,'Adjusted_OE']) + (team_2[,'Adjusted_DE'] - season_averages[,'Adjusted_OE'])
    
    ExpectedOE_team2 = season_averages[,'Adjusted_OE'] + (team_2[,'Adjusted_OE'] - season_averages[,'Adjusted_OE']) + (team_1[,'Adjusted_DE'] - season_averages[,'Adjusted_OE'])
    
    ExpectedTempo = season_averages[,'Adjusted_Tempo'] + (team_1[,'Adjusted_Tempo'] - season_averages[,'Adjusted_Tempo']) + (team_2[,'Adjusted_Tempo'] - season_averages[,'Adjusted_Tempo'])
    
    Team1Score = ExpectedOE_team1/100 * ExpectedTempo 
    Team2Score = ExpectedOE_team2/100 * ExpectedTempo
    Team1Name = team_1[,'TeamName']
    Team2Name = team_2[,'TeamName']
    Team1Name <- data.frame(lapply(Team1Name, as.character), stringsAsFactors=FALSE)
    Team2Name <- data.frame(lapply(Team2Name, as.character), stringsAsFactors=FALSE)
    
    div(style="display: flex; justify-content:space-around",
      div(paste0(Team1Name, ':'), round(Team1Score),style="font-weight: bold; font-size:30px"),
      div(
        div(paste('Line: ',Team1Name),paste0(ifelse(Team2Score-Team1Score>0,'+',''),round(Team2Score-Team1Score))),
        div('O/U:', round(Team1Score + Team2Score)),
      style="margin-top: 30px"
      ),
      div(paste0(Team2Name, ':'),round(Team2Score),style="font-weight: bold; font-size:30px")
    )
    
  })
  
>>>>>>> new git repository
  output$boxPlotAdjusted <- renderPlot({
    
    RegSeasonDataAdjusted %>% ggplot(RegSeasonDataAdjusted, mapping = aes(x=!!input$TeamAdjustedStat,fill=Outcome)) + geom_boxplot() +
      ggtitle(paste("Winning Team vs Losing Team Distribution of", input$TeamAdjustedStat,sep=' '))
    
  })
  
  output$mainDF <- renderDataTable({
    RegSeasonTT_Sub = RegSeasonTTGrouped %>% select(.,Season,TeamID,NumberOfTournamentWins)
    RegSeasonDataMainGrouped_Joined = inner_join(RegSeasonDataMainGrouped,RegSeasonTT_Sub,by=c("Season","TeamID"))
    RegSeasonDataMainGrouped_Joined %>% select(.,Season,TeamName,Adjusted_OE,Adjusted_DE,Adjusted_EM,NumberOfTournamentWins) %>% mutate(.,Adjusted_OE=round(Adjusted_OE,2),Adjusted_DE=round(Adjusted_DE,2),Adjusted_EM=round(Adjusted_EM,2)) %>% filter(.,Season==as.name(input$Season))
  })
  
  output$Predictions <- renderUI({
    team_1 = team_averages %>% filter(.,TeamName==input$Team1)
    team_2 = team_averages %>% filter(.,TeamName==input$Team2)
    
    ExpectedOE_team1 = season_averages[,'Adjusted_OE'] + (team_1[,'Adjusted_OE'] - season_averages[,'Adjusted_OE']) + (team_2[,'Adjusted_DE'] - season_averages[,'Adjusted_OE'])
    
    ExpectedOE_team2 = season_averages[,'Adjusted_OE'] + (team_2[,'Adjusted_OE'] - season_averages[,'Adjusted_OE']) + (team_1[,'Adjusted_DE'] - season_averages[,'Adjusted_OE'])
    
    ExpectedTempo = season_averages[,'Adjusted_Tempo'] + (team_1[,'Adjusted_Tempo'] - season_averages[,'Adjusted_Tempo']) + (team_2[,'Adjusted_Tempo'] - season_averages[,'Adjusted_Tempo'])
    
    Team1Score = ExpectedOE_team1/100 * ExpectedTempo 
    Team2Score = ExpectedOE_team2/100 * ExpectedTempo
    Team1Name = team_1[,'TeamName']
    Team2Name = team_2[,'TeamName']
    Team1Name <- data.frame(lapply(Team1Name, as.character), stringsAsFactors=FALSE)
    Team2Name <- data.frame(lapply(Team2Name, as.character), stringsAsFactors=FALSE)
    
    div(style="display: flex; justify-content:space-around",
      div(paste0(Team1Name, ':'), round(Team1Score),style="font-weight: bold; font-size:30px"),
      div(
        div(paste('Line: ',Team1Name),paste0(ifelse(Team2Score-Team1Score>0,'+',''),round(Team2Score-Team1Score))),
        div('O/U:', round(Team1Score + Team2Score)),
      style="margin-top: 30px"
      ),
      div(paste0(Team2Name, ':'),round(Team2Score),style="font-weight: bold; font-size:30px")
    )
    
  })
  
<<<<<<< HEAD


=======
>>>>>>> new git repository
})

# server <- function(input, output) {
#   output$cars <- renderPrint({summary(mtcars)})
#   output$plot <- renderPlot({plot(mtcars$mpg)})
#   output$plot2 <- renderPlot({plot(mtcars$cyl)})
#   output$plot3 <- renderPlot({plot(mtcars$hp)})
# }

