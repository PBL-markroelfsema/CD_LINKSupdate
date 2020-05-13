source('functions/ReadFactSheetData.R')
source('CD_LINKS/Settings_indicators.R')

CO2_negative <- filter(all, VARIABLE=="Emissions|CO2")
CO2_negative <- gather(CO2_negative, '1990', '1995', '2000', '2005', '2010', '2015', '2020', '2025', '2030', '2035', '2040', 
                                     '2045', '2050', '2055', '2060', '2065', '2070', '2075', '2080', '2085', '2090', '2095', '2100', '2110', 
                                     key="YEAR", value="EMISSIONS")
CO2_negative <- filter(CO2_negative, SCENARIO %in% Scenarios_factsheet)
CO2_negative$EMISSIONS <- CO2_negative$EMISSIONS*10^-3

CO2_negative$SCENARIO[CO2_negative$SCENARIO=='NoPolicy']        <- 'No policy'
CO2_negative$SCENARIO[CO2_negative$SCENARIO=='NPi']             <- 'National policies'
CO2_negative$SCENARIO[CO2_negative$SCENARIO=='INDCi']           <- 'NDC'
CO2_negative$SCENARIO[CO2_negative$SCENARIO=='NPi2020_400_V3']  <- '1.5C'
CO2_negative$SCENARIO[CO2_negative$SCENARIO=='NPi2020_1000_V3'] <- '2C'
CO2_negative$SCENARIO[CO2_negative$SCENARIO=='INDC2030i_1000_V3']  <- '2C delay'

CO2_negative <- filter(CO2_negative, YEAR>=2010, YEAR<=2100)

# remove NA
CO2_negative <- CO2_negative %>% drop_na()
CO2_negative$EMISSIONS[CO2_negative$EMISSIONS>0] <- 0
# 5 year time interval
CO2_negative$EMISSIONS <- CO2_negative$EMISSIONS*5
# calculate cumulative negative emissions
CO2_negative <- CO2_negative %>% group_by(SCENARIO, REGION) %>% mutate(CUM_EMISSIONS=cumsum(EMISSIONS))
CO2_negative$SCENARIO <- factor(CO2_negative$SCENARIO, levels=Scenarios_fig)

# save data
NE_PBL <- filter(CO2_negative, SCENARIO %in% Scenarios_fig, YEAR==2100)
NE_PBL$EMISSIONS <- NULL
NE_PBL <- spread(NE_PBL, key=YEAR, value=CUM_EMISSIONS)
write.table(NE_PBL, file="indicator_negative_emissions.csv", sep=';', row.names=FALSE)

fig_negative_emissions <- ggplot(data=filter(CO2_negative, SCENARIO %in% Scenarios_fig, YEAR==2100, REGION=="World")) +
                          geom_point(aes(x=SCENARIO, y=CUM_EMISSIONS, color=MODEL), size=5) +
                          labs(title="Cumulative negative emissions", subtitle="World") +
                          labs(x = "Scenario", y = "GtCO2eq") +
                          theme(axis.text.x = element_text(angle=90)) +
                          theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
                          scale_color_manual(values=rhg_cols)
plot(fig_negative_emissions)