
# Only works if Variable_graph is from Process_TIMER_output.R and has structure: year, region, value, unit
PlotGraph_ScenarioComparison <- function(Scenario_i1, Scenario_i2, Variable_graph, Region_graph, StartYear, EndYear, Rundir, Project, TIMERGeneration)
{ library(tidyverse)
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(scales)

  #Scenario_i1 <- NoPolicy
  #Scenario_i2 <- NPi_EffExistCoalPlantsi
  #Variable_graph <- "FinalEnergy"
  #Region_graph <- "CHN"
  #StartYear = 2010
  #ndYear = 2030
  
  #Rundir=paste("C:/Users/markr/OneDrive/IMAGE", sep="")
  #Rundir=paste("C:/Users/Roelf003/OneDrive/IMAGE", sep="")
  #Project=paste("CD_LINKSupdate")
  #TIMERGeneration = "TIMER_2015"
  #RDir = "6_R"
  
  if (as.character(Variable_graph) %in% names(Scenario_i1))
  {  #tmp_NoPolicy <- filter(NoPolicyi[[as.character(Variables_in_sceny[i,3])]], region==as.character(Variables_in_sceny[i,2]), year >= StartYear, year <= EndYear) %>% select(year, region, value, unit)
    scen1_data <- filter(Scenario_i1[[as.character(Variable_graph)]], region==Region_graph, year >= StartYear, year <= EndYear) %>%
      mutate(scenario=deparse(substitute(Scenario1))) %>%
      select(year, region, value, unit, scenario)
    scen2_data <- filter(Scenario_i2[[as.character(Variable_graph)]], region==Region_graph, year >= StartYear, year <= EndYear) %>%
      mutate(scenario=deparse(substitute(Scenario2))) %>%
      select(year, region, value, unit, scenario)
    
    scen_data <- rbind(scen1_data, scen2_data)
    change <- inner_join(scen1_data,scen2_data, by=c('year')) %>% mutate(change=value.y-value.x) %>%
      select(year, region.x, change, unit.x) %>%
      rename(region=region.x, unit=unit.x)
    
    g<- ggplot() + 
      geom_line(data=scen_data, aes(x=year, y=value, colour=scenario)) + 
      scale_colour_discrete(labels=c(deparse(substitute(Scenario1)), deparse(substitute(Scenario2)))) +
      ylim(0,NA) + 
      ylab(unique(scen_data$unit)[1]) +
      ggtitle(Variable_graph) +
      theme_bw()
    plot(g) 
    
    PlotGraph_ScenarioComparison <- change
  } # if
  else # as.character(Read_CD_LINKS[i,3]) %in% names(NoPolicy))
  {  change <- 0
  } # else
    
 
  
} # function PlotGraph_ScenarioComparison


