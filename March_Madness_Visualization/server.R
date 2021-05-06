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
  
  output$barChartSOS <- renderPlotly({
    SOS = SOS %>% filter(Season == '2021')
    SOS = head(SOS, n=25)
    barChart <- ggplot(SOS) + geom_bar(aes(x=reorder(TeamName, -SOS), y=SOS, fill=TeamName),stat="Identity") + 
      theme(axis.text.x = element_text(angle = 45,hjust = 1)) + theme(legend.title = element_blank()) + 
      theme(legend.position = "none") + ggtitle('Team Strength of Schedule') + xlab('Teams')
    ggplotly(barChart)
  })
  
  output$barChartConfStrength <- renderPlotly({
    SOS = SOS %>% filter(Season == '2021')
    SOS_Grouped = SOS %>% group_by(Season,ConfAbbrev) %>% summarise(ConfStrength = mean(Adjusted_EM_Conf)) %>% arrange(desc(Season),desc(ConfStrength))
    barChart <- ggplot(SOS_Grouped) + geom_bar(aes(x=reorder(ConfAbbrev, -ConfStrength), y=ConfStrength, fill=ConfAbbrev),stat="Identity") + 
      theme(axis.text.x = element_text(angle = 45,hjust = 1)) + theme(legend.title = element_blank()) + 
      theme(legend.position = "none") + ggtitle('Strength of Conference') + xlab('Conferences')
    ggplotly(barChart)
  })
  
  output$SOSRanking <- renderInfoBox({
    SOS = SOS %>% filter(Season=='2021')
    infoBox(
      "SOS Ranking", which(SOS$TeamName == as.character(input$TeamName)),color = "yellow", fill = TRUE
    )
  })
  
  output$ConfRanking <- renderInfoBox({
    SOS = SOS %>% filter(Season=='2021')
    SOS_Grouped = SOS %>% group_by(Season,ConfAbbrev) %>% summarise(ConfStrength = mean(Adjusted_EM_Conf)) %>% arrange(desc(Season),desc(ConfStrength))
    infoBox(
      "Conf Ranking", which(SOS_Grouped$ConfAbbrev == SOS[SOS$TeamName==as.character(input$TeamName),'ConfAbbrev']),color = "yellow", fill = TRUE
    )
  })
  
  output$Conf <- renderInfoBox({
    SOS = SOS %>% filter(Season=='2021')
    infoBox(
      "Conference", SOS[SOS$TeamName == as.character(input$TeamName),'ConfAbbrev'],color = "yellow", fill = TRUE
    )
  })
  
  output$HCABarChart <- renderPlotly({
    test_HCA = HCA %>% filter(Season==as.character(input$SeasonHCA))
    HCABarChart <- ggplot(test_HCA) + geom_bar(aes(x=GameLoc,y=!!input$GameLocStat,fill=GameLoc),stat = "Identity") + 
      ggtitle(paste('Home/Away Court Advantage for', sub('_LocDiff', '', as.character(input$GameLocStat))))
    ggplotly(HCABarChart)
  })

  output$HCALineChart <- renderPlotly({
    HCALineChart <- ggplot(HCA) + geom_line(aes(x=Season, y=!!input$GameLocStatLineChart, color=GameLoc)) +
      ggtitle(paste('Season Trend in Location Advantage for', sub('_LocDiff', '', as.character(input$GameLocStat))))
    ggplotly(HCALineChart)
  })
  
  output$violinPlot <- renderPlot({
      ggplot(RegSeasonTTGrouped,mapping = aes(x=as.factor(NumberOfTournamentWins),y=!!input$TourneyTeamStat,fill=as.factor(NumberOfTournamentWins))) + 
      geom_violin() + stat_summary(fun=mean,geom="point",shape=16,color='black') + ggtitle(paste("Distribution of ", " Seperated by Round in Tournament Where Team Lost",sep=as.character(input$TourneyTeamStat))) +
      xlab("Tournament Round") + ylab(paste("Regular Season", input$TourneyTeamStat,sep=' ')) +
      scale_x_discrete(labels=c("Lost In Round of 64","Lost In Round of 32","Lost In Sweet 16","Lost In Elite 8","Lost In Final Four","Lost In National Championship","Won National Championship")) +
      theme(axis.text.x = element_text(angle = 45,hjust = 1)) + scale_fill_discrete(name="",labels=c("Lost In Round of 64","Lost In Round of 32","Lost In Sweet 16","Lost In Elite 8",                                                                                                                "Lost In Final Four","Lost In National Championship","Won National Championship"))
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
    RegSeasonDataGroupedAll_Sub = RegSeasonDataGroupedAll %>% filter(.,Season=='2020')
    p <- ggplot(RegSeasonDataGroupedAll_Sub, aes(x=Offensive_Efficiency,y=Defensive_Efficiency,color=TeamName)) + geom_point() + ggtitle("Offensive Efficiency vs Devensive Efficiency (2020 Regular Season)") + theme(legend.title = element_blank()) + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$scatterAdjusted <- renderPlotly({
    RegSeasonDataGroupedAll_Sub = RegSeasonDataGroupedAll %>% filter(.,Season=='2020')
    p2 <- ggplot(RegSeasonDataGroupedAll_Sub, aes(x=Adjusted_Offensive_Efficiency,y=Adjusted_Defensive_Efficiency,color=TeamName)) + geom_point() + ggtitle("Adjusted Offensive Efficiency vs Adjusted Devensive Efficiency (2020 Regular Season)") + theme(legend.title = element_blank()) + theme(legend.position = "none")
    ggplotly(p2)
  })
  
  output$OETable <- renderDataTable({
    
    RSSorted = RegSeasonDataGroupedAll %>% filter(.,Season=='2020') %>% arrange(.,desc(Offensive_Efficiency)) %>% select(.,Season,TeamName,Offensive_Efficiency) %>% mutate(Offensive_Efficiency=round(Offensive_Efficiency,2))
    DT::datatable(RSSorted, caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black; font-size:200% ;','Highest Offensive Efficiencies - 2020 Season'), options = list(lengthChange=FALSE, pageLength=10, searching=FALSE), rownames=FALSE)
  })
  
  output$AdjustedOETable <- renderDataTable({
    RSSorted = RegSeasonDataGroupedAll %>% filter(.,Season=='2020') %>% arrange(.,desc(Adjusted_Offensive_Efficiency)) %>% select(.,Season,TeamName,Adjusted_Offensive_Efficiency) %>% mutate(Adjusted_Offensive_Efficiency=round(Adjusted_Offensive_Efficiency,2))
    DT::datatable(RSSorted, caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black; font-size:200% ;','Highest Adjusted Offensive Efficiencies - 2020 Season'),options = list(lengthChange=FALSE, pageLength=10, searching=FALSE), rownames=FALSE)
  })
  
  output$AdjVsNonAdj <- renderPlotly({
    RegSeasonDataGroupedAll_Sub = RegSeasonDataGroupedAll %>% filter(.,Season=='2020')
    p3 <- RegSeasonDataGroupedAll_Sub %>% ggplot(RegSeasonDataGroupedAll_Sub, mapping = aes(x=!!input$PairedStat, y=!!as.name(paste0('Adjusted_',input$PairedStat)),color=TeamName)) + geom_point() +
      geom_abline(slope=1)+ theme(legend.title = element_blank()) + theme(legend.position = "none") + ggtitle(paste(paste(as.character(input$PairedStat),paste0('Adjusted_',input$PairedStat),sep=' vs '),'(2020 Regular Season)'))
    ggplotly(p3)
  })

  output$mainDF <- renderDataTable({
    RegSeasonTT_Sub = RegSeasonTTGrouped %>% select(.,Season,TeamName,Adjusted_Offensive_Efficiency,Adjusted_Defensive_Efficiency,Adjusted_Efficiency_Margin,NumberOfTournamentWins)
    trans <- c('Loss In Round of 64','Loss In Round of 32','Loss In Sweet 16','Loss In Elite 8','Loss Final Four','Loss In National Championship','National Championship Winner')
    names(trans) <- c(0,1,2,3,4,5,6)
    RegSeasonTT_Sub$NumberOfTournamentWins <- trans[as.character(RegSeasonTT_Sub$NumberOfTournamentWins)]
    RegSeasonTT_Sub = RegSeasonTT_Sub %>% mutate(.,Adjusted_Offensive_Efficiency=round(Adjusted_Offensive_Efficiency,2),Adjusted_Defensive_Efficiency=round(Adjusted_Defensive_Efficiency,2),Adjusted_Efficiency_Margin=round(Adjusted_Efficiency_Margin,2)) %>%
      rename(Adj_Off_Eff=Adjusted_Offensive_Efficiency,Adj_Def_Eff=Adjusted_Defensive_Efficiency,Adj_Eff_Margin=Adjusted_Efficiency_Margin,Tournament_Result=NumberOfTournamentWins) %>% filter(.,Season==as.name(input$Season))
    DT::datatable(RegSeasonTT_Sub, caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black; font-size:200% ;','Regular Season Offense/Defense Rankings w/ Tournament Result'),options = list(lengthChange=FALSE, pageLength=10, searching=FALSE), rownames=FALSE)
    
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
      # div(
      #   div(paste('Line: ',Team1Name),paste0(ifelse(Team2Score-Team1Score>0,'+',''),round(Team2Score-Team1Score))),
      #   div('O/U:', round(Team1Score + Team2Score)),
      # style="margin-top: 30px"
      # ),
      div(paste0(Team2Name, ':'),round(Team2Score),style="font-weight: bold; font-size:30px")
    )
  })
})

