library(tidyverse)
CalcReductions <- function(select_year, Scenario_i_1, Scenario_i_2, select_regions, select_main_sector, select_ghg_category, policy)
{ tmp1 <- Scenario_i_1$EMISCO2EQ
  tmp2 <- Scenario_i_2$EMISCO2EQ
  #tmp1 <- Scenario_i_1[[select_ghg_category]]
  #tmp2 <- Scenario_i_2[[select_ghg_category]]
  tmp1 <- filter(tmp1, year==select_year, region %in% select_regions, main_sector==select_main_sector, GHG_Category==select_ghg_category) %>% select(year, region, main_sector, value)
  tmp2 <- filter(tmp2, year==select_year, region %in% select_regions, main_sector==select_main_sector, GHG_Category==select_ghg_category) %>% select(year, region, main_sector, value)
  ReductionFromPolicies <- inner_join(tmp1, tmp2, by=c('year', 'region', 'main_sector'))
  ReductionFromPolicies <- mutate(ReductionFromPolicies, value = value.x-value.y)
  ReductionFromPolicies <- mutate(ReductionFromPolicies, policy=policy)
  ungroup(ReductionFromPolicies)
  ReductionFromPolicies <- select(ReductionFromPolicies, policy, year, region, main_sector, value)
  ReductionFromPolicies <- arrange(ReductionFromPolicies, policy, year, region, main_sector)
}

wait <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}