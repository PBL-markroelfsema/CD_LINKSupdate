library(gridExtra)
source('functions/Settings.R')
source('functions/General Functions.R')
source('functions/Import_TIMER_output.R')
source('functions/Process_TIMER_output.R')
source('functions/pbl_colors.R')
source('CD_LINKS/Settings_indicators.R')

ProjectDir = "~/disks/y/ontwapps/Timer/Users/Mathijs/Projects/CD-LINKS"
Project = "CD_LINKS"
R_dir = paste(ProjectDir, Project, "R-scripts/TIMER_output", sep="/")
setwd(R_dir)
getwd()

# Source scripts (after setting working directory)
source('functions/Settings.R')
source('functions/General Functions.R')
source('functions/Import_TIMER_output.R')
source('functions/Process_TIMER_output.R')
source('functions/pbl_colors.R')
source('CD_LINKS/Settings_indicators.R')

# Read no policy scenario
NoPolicy <- ImportTimerScenario('NoPolicy_update','NoPolicy_update')
NoPolicy_ind <- ProcessTimerScenario(NoPolicy)
# Make file with variable names from list
var_names_bl <- as.data.frame(names(NoPolicy))
colnames(var_names_bl) <- c("variable")
var_names_ind <- as.data.frame(names(NoPolicy_ind))
colnames(var_names_ind) <- c("variable")
var_names <- rbind(var_names_bl,var_names_ind)
colnames(var_names) <- c("variable")
write.table(var_names, "CD_LINKS/var_names.csv", sep=";", row.names=FALSE)
           
# Read in other scenarios
NPi <- ImportTimerScenario('NPi','NPi')
#NPi_update <- ImportTimerScenario('NPi_update','NPi_update')
INDCi <- ImportTimerScenario('INDCi','INDCi')
NPi2020_1000 <- ImportTimerScenario('NPi2020_1000','NPi2020_1000')
NPi2020_400<- ImportTimerScenario('NPi2020_400','NPi2020_400')
INDCi2030_1000<- ImportTimerScenario('INDC2030i_1000','INDC2030i_1000')

NPi_ind <- ProcessTimerScenario(NPi)
#NPi_update_ind <- ProcessTimerScenario(NPi_update)
INDCi_ind <- ProcessTimerScenario(INDCi)
NPi2020_1000_ind <- ProcessTimerScenario(NPi2020_1000)
NPi2020_400_ind <- ProcessTimerScenario(NPi2020_400)
INDCi2030_1000_ind <- ProcessTimerScenario(INDCi2030_1000)


