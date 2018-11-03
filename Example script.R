# ----
# Data reading
rm(list=ls())
library(ggplot2)
library(reshape2)
library(data.table)
library(tidyr)

#Set the directory of R-script
setwd("~/disks/y/ontwapps/Timer/Users/David/Pojects/project20/6_R/R-project20")

source('mym2r.R') #Script to read MyM format in R, can also write to MyM format
source('pbl_colors.r') #PBL colours for R, naming fully consistent with PBL colour scheme (See pebbles)

REG28  = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF","WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN","SEAS","INDO","JAP","OCE","RSAS","RSAF","dummy","World")
TECH = c("PV","CSP","WON","WOFF","Hydro","oren","Nuc","dummy","CCoal","COil","CGas","CBio","IGCC","OGCC","NGCC","BioCC","CoalCS","OilCS","GasCS","BioCS","CHPCoal","CHPOil","CHPGas","CHPBio","CHPCoalCS","CHPOilCS","CHPGasCS","CHPBioCS")

df = read.mym2r.nice(mym.folder='../../2_timer/outputlib/TIMER_2015/project20/PB1/T2RT', scen.econ='', filename='ElecProdSpec', collist=list(REG28, TECH), namecols=c('region','tech'))
df$value = df$value * 2.77778e-10 #to PWh

# ----
# Region summing
temp=data.table(df)
temp <- spread(temp, region, value)
temp = temp %>% mutate(Africa=WAF+EAF+SAF+RSAF)
temp = temp %>% mutate(Europe=WEU+CEU+TUR)
temp = temp %>% mutate(India=INDIA+RSAS)
temp = temp %>% mutate(SAM=MEX+RCAM+BRA+RSAM)
temp = temp %>% mutate(MENA=ME+NAF)
temp = temp %>% mutate(NAM=CAN+USA)
temp = temp %>% mutate(POECD=JAP+OCE)
temp = temp %>% mutate(Russia=RUS+UKR+STAN)
temp = temp %>% mutate(Rasia=INDO+KOR+SEAS)
temp = temp %>% mutate(China=CHN)
temp = gather(temp, region, value, c(Africa,Europe,India,SAM,MENA,NAM,POECD,Russia,Rasia,China))
df.reg <- temp[, !colnames(temp) %in% REG28]

# ----
# Tech summing
temp=data.table(df.reg)
temp <- spread(temp, tech, value)
temp = temp %>% mutate(Coal=CCoal+IGCC+CoalCS+CHPCoal+CHPCoalCS)
temp = temp %>% mutate(Oil=COil+OGCC+OilCS+CHPOil+CHPOilCS)
temp = temp %>% mutate(Gas=CGas+NGCC+GasCS+CHPGas+CHPGasCS)
temp = temp %>% mutate(Bio=CBio+BioCC)
temp = temp %>% mutate(Nuclear=Nuc)
temp = temp %>% mutate(Wind=WON+WOFF+oren)
temp = temp %>% mutate(Hyd=Hydro)
temp = gather(temp, tech, value, c(Coal,Gas,Oil,Nuclear,Bio,PV,CSP,Wind,Hyd))
df.regtech <- temp[, !colnames(temp) %in% TECH]

# ----
df.regtech = subset(df.regtech, subset=year==2015|year==2030|year==2050)
df.regtech$year = as.character(df.regtech$year)
df.regtech$tech<-factor(df.regtech$tech,levels=c("PV","CSP","Wind","Bio","Nuclear","Hyd","Gas","Oil","Coal"))

# ----
# Plotting

G1 =  ggplot(data=df.regtech, 
            aes(x=year, y=value, fill=tech, width=0.8)) +
  facet_grid(.~region, scale="free_x", space = "free_x",margins = FALSE) +
  geom_bar(stat="identity") +
  # geom_text(data=Data.reg, aes(x=varname, y=value, label=lab),vjust=-0.5) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(legend.position="bottom") +
  # theme(legend.text=element_text(size=11)) +
  guides(fill=guide_legend(title=NULL,ncol=8)) +
  ylab( expression(paste("Electricity production (PWh/yr)"))) +
  xlab("") +
  ylim(0,15) +
  theme(panel.background = element_rect(fill = 'white', colour = 'gray')) +
  theme(text = element_text(size=20)) +
  geom_hline(yintercept=0,colour='gray30')+
  theme(strip.background = element_rect(fill="white")) + 
  theme(axis.line = element_line(colour = "grey")) +
  scale_fill_manual(breaks=c("PV","CSP","Wind","Bio","Nuclear","Hyd","Gas","Oil","Coal"),
                    labels=c("PV","CSP","Wind","Bio","Nuclear","Hyd","Gas","Oil","Coal"),
                    values = c(rood1_1,hemelblauw1_1,geel10_1,donkergeel1_1,groen1_1,violet5_2,donkerblauw1_1,zwart3_3,zwart3_2,zwart3_1))
plot(G1)


outfile <- sprintf("Fig 01 ElecProdSpec.png")
png(height=500, width=750, paste("../output/",outfile, sep=""))
plot(G1)
dev.off()
