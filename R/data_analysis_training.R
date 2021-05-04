
# load butteR
library(tidyverse)
library(readxl)
library(butteR)
library(srvyr)

dap <- read_csv(file = "inputs/analysis_dap_MC_ev_MT.csv")

df_dap

df_data <- read_xlsx("inputs/BRIDGE Endline Survey - GM2.xlsx")
df_data


df_data$crop_production_kgs %>% 
  sort()


df_data$crop_production_kgs <- as.numeric(df_data$crop_production_kgs)



# select variables from dap tha are in dataset

variables_to_analyse <- dap$variable[dap$variable %in%  colnames(df_data)]


df_svy <-  as_survey(df_data)
df_svy

# back to data frame
df_svy$variables

overall_analysis <- butteR::survey_collapse(df_svy, 
                                            vars_to_analyze = variables_to_analyse)

overall_analysis

# data cleaning due to unclean data

df_svy$variables

# disaggregatiob by population status
by_population <- butteR::survey_collapse(df_svy, 
                                           vars_to_analyze = variables_to_analyse, disag = "participant_category"
                                           )


# combined analysis output
overall_analysis %>% 
  mutate(
    analysis_level = "overall"
  )

by_population <- by_population %>% 
  mutate(
    analysis_level = "population"
  )  
combined_analysis <-  bind_rows(by_population, overall_analysis)

# trick to combine disaggregation fatser
res <- list()

res$overall_analysis <- butteR::survey_collapse(df_svy, 
                                            vars_to_analyze = variables_to_analyse)
# disaggregation by population status

res$by_population <- butteR::survey_collapse(df_svy, 
                                         vars_to_analyze = variables_to_analyse, disag = "participant_category")


combined_analysis_with_list <- bind_rows(res)





