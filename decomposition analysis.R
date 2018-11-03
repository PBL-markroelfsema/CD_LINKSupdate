library(tidyverse)
source('TIMER_output/functions/Settings.R')
source('TIMER_output/functions/Import_TIMER_output.R')
source('TIMER_output/functions/Process_TIMER_output.R')
source('decomposition_functions.R')

Rundir=paste("~/disks/y/ontwapps/Timer/Users/Mark", sep="")
Project=paste("CD_LINKSupdate")
TIMERGeneration = 'TIMER_2015'
TIMER_folder = paste(Rundir, Project, "2_TIMER/outputlib", TIMERGeneration, Project, sep="/")

NoPolicy <- ImportTimerScenario('NoPolicy_update','NoPolicy_update', Rundir, Project, TIMERGeneration)
NoPolicyi <- ProcessTimerScenario(NoPolicy, Rundir, Project)

CAFETargets_trucks <- ImportTimerScenario('NPi_update_CAFEStandards_trucks','NoPolicy_update', Rundir, Project, TIMERGeneration)
CAFETargets_trucksi <- ProcessTimerScenario(CAFETargets_trucks, Rundir, Project)

select_regions = c("CAN", "USA","MEX", "BRA","EU","TUR", "UKR", "RUS","INDIA","KOR", "CHN","INDO", "JAP","World")

# Determine total CO2eq reductions
ReductionFromPolicies1a <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, select_regions, 'Total', "EMISCO2EQ", "CAFE Standard trucks (total)")
ReductionFromPolicies1b <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, select_regions, 'Transport', "EMISCO2EQ", "CAFE Standard trucks (Transport)")
ReductionFromPolicies1c <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, select_regions, 'Industry', "EMISCO2EQ", "CAFE Standard trucks (Industry)")
ReductionFromPolicies1d <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, select_regions, 'Buildings', "EMISCO2EQ", "CAFE Standard trucks (Buildings)")
ReductionFromPolicies1e <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, select_regions, 'Energy supply', "EMISCO2EQ", "CAFE Standard trucks (Energy Supply)")

ReductionFromPolicies <- bind_rows(ReductionFromPolicies1a, ReductionFromPolicies1b) %>%
  bind_rows(ReductionFromPolicies1c) %>%
  bind_rows(ReductionFromPolicies1d) %>%
  bind_rows(ReductionFromPolicies1e)

#format(ReductionFromPolicies$value, digits=2, nsmall=2, Scientific=FALSE)
ReductionFromPolicies[,'value']=round(ReductionFromPolicies[,'value'],2)
write.table(ReductionFromPolicies,file="data/Reductions.csv", sep=",", row.names=FALSE)

# make table with reductions per policy
ReductionFromPolicies <- CalcReductions(2030, NoPolicyi, CAFETargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Cafe target")
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, BiofuelTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Biofuel target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, BuildingTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Building target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, CarbonTaxesi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Carbon Tax"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, ElectricVehiclesTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Electric vehicle share"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, FGasTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "F-gas target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, OilImportTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Oil import target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, OilMethaneTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Oil Methane target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, PowerPlantStandardsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Power plant standard"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, RenTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Renewable target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, AFOLUPolicyi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "AFOLU policies"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, NPii, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Current policies"))

# Determine total CO reductions
ReductionFromPoliciesCO2_1a <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "ENEMISCO2", "CAFE Standard trucks (total)")
ReductionFromPoliciesCO2_1b <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Transport', "ENEMISCO2","CAFE Standard trucks (Transport)")
ReductionFromPoliciesCO2_1c <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Industry', "ENEMISCO2","CAFE Standard trucks (Industry)")
ReductionFromPoliciesCO2_1d <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Buildings', "ENEMISCO2","CAFE Standard trucks (Buildings)")
ReductionFromPoliciesCO2_1e <- CalcReductions(2030, NoPolicyi, CAFETargets_trucksi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Energy Supply', "ENEMISCO2", "CAFE Standard trucks (Energy Supply)")

ReductionFromPoliciesCO2 <- bind_rows(ReductionFromPoliciesCO2_1a, ReductionFromPoliciesCO2_1b) %>%
  bind_rows(ReductionFromPoliciesCO2_1c) %>%
  bind_rows(ReductionFromPoliciesCO2_1d) %>%
  bind_rows(ReductionFromPoliciesCO2_1e)

#format(ReductionFromPolicies$value, digits=2, nsmall=2, Scientific=FALSE)
ReductionFromPoliciesCO2[,'value']=round(ReductionFromPoliciesCO2[,'value'],2)
write.table(ReductionFromPolicies,file="data/Reductions.csv", sep=",", row.names=FALSE)




#####################################

BuildingTargets <- ImportTimerScenario('Building_standards_targets','NoPolicy')
BuildingTargetsi <- ProcessTimerScenario(BuildingTargets)

CarbonTaxes <- ImportTimerScenario('Carbon_taxes','NoPolicy')
CarbonTaxesi <- ProcessTimerScenario(CarbonTaxes)

ElectricVehiclesTargets <- ImportTimerScenario('Electric_vehicle_targets','NoPolicy')
ElectricVehiclesTargetsi <- ProcessTimerScenario(ElectricVehiclesTargets)

FGasTargets <- ImportTimerScenario('F-gas_targets','NoPolicy')
FGasTargetsi <- ProcessTimerScenario(FGasTargets)

OilImportTargets <- ImportTimerScenario('Oil_import','NoPolicy')
OilImportTargetsi <- ProcessTimerScenario(OilImportTargets)

OilMethaneTargets <- ImportTimerScenario('Oil_methane','NoPolicy')
OilMethaneTargetsi <- ProcessTimerScenario(OilMethaneTargets)

PowerPlantStandards <- ImportTimerScenario('Power_plant_standards_targets','NoPolicy')
PowerPlantStandardsi <- ProcessTimerScenario(PowerPlantStandards)

RenTargets <- ImportTimerScenario('Ren_targets','NoPolicy')
RenTargetsi <- ProcessTimerScenario(RenTargets)

AFOLUPolicy <- ImportTimerScenario('NoPolicy', 'NPi')
AFOLUPolicyi <- ProcessTimerScenario(AFOLUPolicy)

NPi <- ImportTimerScenario('NPi_update','NPi')
NPii <- ProcessTimerScenario(NPi)

INDCi <- ImportTimerScenario('INDCi','NPi')
INDCii <- ProcessTimerScenario(INDCi)

select_regions = c("CAN", "USA","MEX", "BRA","EU","TUR", "UKR", "RUS","INDIA","KOR", "CHN","INDO", "JAP","World")

# make table with reductions per policy
ReductionFromPolicies <- CalcReductions(2030, NoPolicyi, CAFETargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Cafe target")
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, BiofuelTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Biofuel target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, BuildingTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Building target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, CarbonTaxesi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Carbon Tax"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, ElectricVehiclesTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Electric vehicle share"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, FGasTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "F-gas target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, OilImportTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Oil import target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, OilMethaneTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Oil Methane target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, PowerPlantStandardsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Power plant standard"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, RenTargetsi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Renewable target"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, AFOLUPolicyi, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "AFOLU policies"))
ReductionFromPolicies <- bind_rows(ReductionFromPolicies, CalcReductions(2030, NoPolicyi, NPii, c("USA","BRA","EU","RUS","INDIA","CHN","JAP","World"), 'Total', "Current policies"))

# make table with reductions per capita per policy
# unit: GHG emissions MtCO2eq, population in millions
POP_2030 <- filter(NoPolicy$POP, year==2030)
POP_2030 <- select(POP_2030, region, value)
ReductionFromPolicies_pc <- left_join(ReductionFromPolicies, POP_2030, by=c("region"))
ReductionFromPolicies_pc <- mutate(ReductionFromPolicies_pc, value=value.x/value.y)
ReductionFromPolicies_pc <- select(ReductionFromPolicies_pc, policy, region, main_sector, GHG_Category, value)

# make table with reductions per GDP per policy
GDP_2030 <- filter(NoPolicy$GDP_MER, year==2030)
GDP_2030 <- select(GDP_2030, region, value)
ReductionFromPolicies_GDP <- left_join(ReductionFromPolicies, GDP_2030, by=c("region"))
ReductionFromPolicies_GDP <- mutate(ReductionFromPolicies_GDP, value=value.x/value.y)
ReductionFromPolicies_GDP <- select(ReductionFromPolicies_GDP, policy, region, main_sector, GHG_Category, value)

# attempt 1
source('Settings.R')
# plot decomposition per policy type 
data_plot <- filter(ReductionFromPolicies, main_sector=="Total", region=="World", GHG_Category=="EMISCO2EQ")
# calculate sum of policies
x2 <- filter(data_plot, policy != "Current policies")
x3 <- x2 %>% group_by(region, main_sector, GHG_Category) %>% summarise(value=sum(value))
x3 <- data.table(x3)
x3 <- mutate(x3, policy="Sum of policies")
x3 <- select(x3, policy, everything())
data_plot <- bind_rows(data_plot,x3)
# calculate overlap
x2 <- filter(data_plot, policy == "Current policies" | policy == "Sum of policies")
#x2[policy=="Sum of policies"]$value <- -1*x2[policy=="Sum of policies"]$value
x2[x2[,"policy",]=="Sum of policies", "value"] <- -1*x2[x2[,"policy",]=="Sum of policies", "value"] 
x3 <- x2 %>% group_by(region, main_sector, GHG_Category) %>% summarise(value=sum(value))
x3 <- data.table(x3)
x3 <- mutate(x3, policy="Overlap")
x3 <- select(x3, policy, everything())
data_plot <- bind_rows(data_plot,x3)
data_plot <- arrange(data_plot, value)
data_plot$policy <- factor(data_plot$policy, levels = data_plot$policy[order(data_plot$value)])
ggplot(data=data_plot) + geom_bar(mapping=aes(x=policy, y=value), stat="identity") + theme(axis.text.x=element_text(angle=90, hjust=1))

# attempt 2
source('Settings.R')
# plot decomposition per policy type 
data_plot <- filter(ReductionFromPolicies, main_sector=="Total", region=="World", GHG_Category=="EMISCO2EQ")
# calculate sum of policies
x2 <- filter(data_plot, policy != "Current policies")
x3 <- x2 %>% group_by(region, main_sector, GHG_Category) %>% summarise(value=sum(value))
x3 <- data.table(x3)
x3 <- mutate(x3, policy="Sum of policies")
x3 <- select(x3, policy, everything())
data_plot <- bind_rows(data_plot,x3)
# calculate overlap
x2 <- filter(data_plot, policy == "Current policies" | policy == "Sum of policies")
#x2[policy=="Sum of policies"]$value <- -1*x2[policy=="Sum of policies"]$value
x2[x2[,"policy",]=="Sum of policies", "value"] <- -1*x2[x2[,"policy",]=="Sum of policies", "value"] 
x3 <- x2 %>% group_by(region, main_sector, GHG_Category) %>% summarise(value=sum(value))
x3 <- data.table(x3)
x3 <- mutate(x3, policy="Overlap")
x3 <- select(x3, policy, everything())
data_plot <- bind_rows(data_plot,x3)
data_plot <- arrange(data_plot, value)
data_plot$policy <- factor(data_plot$policy, levels = data_plot$policy[order(data_plot$value)])
#discriminate between policy reductions and total reductions
data_plot <- mutate(data_plot, aggregation="policies")
data_plot[data_plot[,"policy"]=="Current policies", "aggregation"]<-"total"
data_plot <- filter(data_plot, policy!="Sum of policies")
ggplot(data=data_plot) + geom_bar(mapping=aes(x=aggregation, y=value, fill=policy), stat="identity") 
#+      theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_fill_brewer(palette="Pastel1") + theme_bw()

# attempt 3, also include large countries
source('Settings.R')
# plot decomposition per policy type 
data_plot <- filter(ReductionFromPolicies, main_sector=="Total", region %in% c("World", "USA", "CHN", "EU", "IND", "BRA",  "RUS", "JPN"), GHG_Category=="EMISCO2EQ")
# calculate sum of policies
x2 <- filter(data_plot, policy != "Current policies")
x3 <- x2 %>% group_by(region, main_sector, GHG_Category) %>% summarise(value=sum(value))
x3 <- data.table(x3)
x3 <- mutate(x3, policy="Sum of policies")
x3 <- select(x3, policy, everything())
data_plot <- bind_rows(data_plot,x3)
# calculate overlap
x2 <- filter(data_plot, policy == "Current policies" | policy == "Sum of policies")
#x2[policy=="Sum of policies"]$value <- -1*x2[policy=="Sum of policies"]$value
x2[x2[,"policy",]=="Sum of policies", "value"] <- -1*x2[x2[,"policy",]=="Sum of policies", "value"] 
x3 <- x2 %>% group_by(region, main_sector, GHG_Category) %>% summarise(value=sum(value))
x3 <- data.table(x3)
x3 <- mutate(x3, policy="Overlap")
x3 <- select(x3, policy, everything())
data_plot <- bind_rows(data_plot,x3)
data_plot <- arrange(data_plot, value)
data_plot$policy <- factor(data_plot$policy, levels = unique(data_plot$policy[order(data_plot$value)]))
#discriminate between policy reductions and total reductions
data_plot <- mutate(data_plot, aggregation="policies")
data_plot[data_plot[,"policy"]=="Current policies", "aggregation"]<-"total"
data_plot <- filter(data_plot, policy!="Sum of policies", policy!="Sum of policies")
ggplot(data=data_plot) + geom_bar(mapping=aes(x=region, y=value, fill=policy), stat="identity") 
#+      theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_fill_brewer(palette="Pastel1") + theme_bw()

# make list with largest impact policies
impact_list_GHG <- filter(ReductionFromPolicies, policy!="Current policies", region!="World", GHG_Category=="EMISCO2EQ")
impact_list_GHG <- arrange(impact_list_GHG, desc(value))
order.scores<-order(impact_list_GHG$value)
impact_list_GHG$rank[order.scores] <- nrow(impact_list_GHG):1
impact_list_GHG <- select(impact_list_GHG, rank, everything())
impact_list_GHG <- filter(impact_list_GHG, rank<=20)
impact_list_GHG$value <- round(impact_list_GHG$value,2)
write.table(impact_list_GHG, file="graphs/table_impact_list.txt", sep=";", quote=FALSE, row.names=FALSE)

# make list with largest reductions per capita
impact_list_pc_GHG <- filter(ReductionFromPolicies_pc, policy!="Current policies", region!="World", GHG_Category=="EMISCO2EQ")
impact_list_pc_GHG <- arrange(impact_list_pc_GHG, desc(value))
order.scores<-order(impact_list_pc_GHG$value)
impact_list_pc_GHG$rank[order.scores] <- nrow(impact_list_pc_GHG):1
impact_list_pc_GHG <- select(impact_list_pc_GHG, rank, everything())
impact_list_pc_GHG <- filter(impact_list_pc_GHG, rank<=20)
impact_list_pc_GHG$value <- round(impact_list_pc_GHG$value,2)
write.table(impact_list_pc_GHG, file="graphs/table_impact_list_pc.txt", sep=";", quote=FALSE, row.names=FALSE)

impact_list_pc_CO2 <- filter(ReductionFromPolicies_pc, policy!="Current policies", region!="World", GHG_Category=="ENEMISCO2" | GHG_Category=="INDEMISCO2")
impact_list_pc_CO2 <- arrange(impact_list_pc_CO2, desc(value))
order.scores<-order(impact_list_pc_CO2$value)
impact_list_pc_CO2$rank[order.scores] <- nrow(impact_list_pc_CO2):1
impact_list_pc_CO2 <- select(impact_list_pc_CO2, rank, everything())

# make list with largest reductions per GDP
impact_list_GDP_GHG <- filter(ReductionFromPolicies_GDP, policy!="Current policies", region!="World", GHG_Category=="EMISCO2EQ")
impact_list_GDP_GHG <- arrange(impact_list_GDP_GHG, desc(value))
order.scores<-order(impact_list_GDP_GHG$value)
impact_list_GDP_GHG$rank[order.scores] <- nrow(impact_list_GDP_GHG):1
impact_list_GDP_GHG <- select(impact_list_GDP_GHG, rank, everything())
impact_list_GDP_GHG <- filter(impact_list_GDP_GHG, rank<=20)
impact_list_GDP_GHG$value <- round(10^6*impact_list_GDP_GHG$value,2)
write.table(impact_list_GDP_GHG, file="graphs/table_impact_list_pc.txt", sep=";", quote=FALSE, row.names=FALSE)

impact_list_GDP_CO2 <- filter(ReductionFromPolicies_GDP, policy!="Current policies", region!="World", GHG_Category=="ENEMISCO2" | GHG_Category=="INDEMISCO2")
impact_list_GDP_CO2 <- arrange(impact_list_GDP_CO2, desc(value))
order.scores<-order(impact_list_GDP_CO2$value)
impact_list_GDP_CO2$rank[order.scores] <- nrow(impact_list_GDP_CO2):1
impact_list_GDP_CO2 <- select(impact_list_GDP_CO2, rank, everything())

#plot 15 largest impact policies
rm(impact_plot)
#impact_plot <- filter(ReductionFromPolicies, policy!="Current policies", region!="World", GHG_Category=="EMISCO2EQ", value > 20)
impact_plot <- impact_list_GHG
impact_plot$region <- factor(impact_plot$region, regions28_EU, ordered=TRUE)
impact_plot <- arrange(impact_plot, region, desc(value))
impact_plot <- mutate(impact_plot, order=row_number())
ggplot(data=impact_plot) +
  geom_bar(mapping=aes(x=order, y=value), stat="identity")+
  facet_wrap( ~ region, ncol=4, strip.position = "bottom", scales = "free_x") +
  scale_x_continuous(
       breaks = impact_plot$order,
       labels = impact_plot$policy) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("MtCO2eq")

#plot 15 largest impact per capita policies
rm(impact_plot_pc)
#impact_plot_pc <- filter(ReductionFromPolicies_pc, policy!="Current policies", region!="World", GHG_Category=="EMISCO2EQ", value > 0.07)
impact_plot_pc <- impact_list_pc_GHG
impact_plot_pc$region <- factor(impact_plot_pc$region, regions28_EU, ordered=TRUE)
impact_plot_pc <- arrange(impact_plot_pc, region, desc(value))
impact_plot_pc <- mutate(impact_plot_pc, order=row_number())
ggplot(data=impact_plot_pc) +
  geom_bar(mapping=aes(x=order, y=value), stat="identity")+
  facet_wrap( ~ region, ncol=4, strip.position = "bottom", scales = "free_x") +
  scale_x_continuous(
    breaks = impact_plot_pc$order,
    labels = impact_plot_pc$policy) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("tCO2eq/capita")

#plot 15 largest impact per GDP policies
rm(impact_plot_GDP)
#impact_plot_GDP <- filter(ReductionFromPolicies_GDP, policy!="Current policies", region!="World", GHG_Category=="EMISCO2EQ")
impact_plot_GDP <- impact_list_GDP_GHG
impact_plot_GDP$region <- factor(impact_plot_GDP$region, regions28_EU, ordered=TRUE)
impact_plot_GDP <- arrange(impact_plot_GDP, region, desc(value))
impact_plot_GDP <- mutate(impact_plot_GDP, order=row_number())
ggplot(data=impact_plot_GDP) +
  geom_bar(mapping=aes(x=order, y=value), stat="identity")+
  facet_wrap( ~ region, ncol=4, strip.position = "bottom", scales = "free_x") +
  scale_x_continuous(
    breaks = impact_plot_GDP$order,
    labels = impact_plot_GDP$policy) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("ktCO2eq/GDP(US$(2005)")
