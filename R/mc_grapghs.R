library(tidyverse)


# Land Production and Productivity

# respondents utilizing tillage services by phases of intervention
df_farmers_use_tillage <- tibble(response= c("Yes", "Yes"), 
                                 perc_respondents  = c(27/(27+293)*100,
                                                       461/(461+56)*100),
                                                       
                                phase = c("Baseline", "Endline")
)

round(df_farmers_use_tillage$perc_respondents, 0)

df_farmers_use_tillage %>% 
  ggplot(aes(fill = phase, x= fct_reorder(response, perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity", width = 1) +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  ylim(0, 100) +
  theme_bw() +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .4)) +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text = element_blank(),
        axis.title.x = element_blank(),
                axis.ticks = element_blank())+
  labs(y = "Percentage  of respondents")+
  labs(title= "Proportion of respondents using tillage service", 
       subtitle = "n baseline: 320, n endline:517", x= element_blank(),
       y = element_blank())


# land ownership status
df_land_own_status <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                       "Endline", "Baseline", "Endline", "Baseline", "Endline"),
                             perc_respondents = c(49, 44, 23, 18, 12, 7, 0, 31, 16, 0),
                             status = c("Owned", "Owned", "Rented", "Rented", "Borrowed",
                                       "Borrowed", "Given for free", "Given for free", "Other", "Other")
                             
                                 )

df_land_own_status %>% 
  arrange(perc_respondents) %>%
  mutate(status = factor(status, levels=c("Other", "Given for free", "Borrowed", "Rented", "Owned"))) %>% 
  
  ggplot(aes(fill = phase, x= status,
             y = perc_respondents)) +
  
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Proportion of respondents by land ownership status",
       subtitle = "n baseline: 644, n endline:738") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.y = element_text(angle = 0, hjust=1),
        axis.title = element_blank())+
  labs(x = "Percentage  of respondents") +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 50, by = 5)) 



# respondents planting crops outside the settlement
df_plant_out <- tibble(response= c("Yes", "Yes"), 
                       perc_respondents  = c(74, 76), 
                       phases = c("Baseline", "Endline")
)

df_plant_out %>% 
  ggplot(aes(fill=phases, x= fct_reorder(response, -perc_respondents), y = perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = paste(" Proportion of respondents planting crops outside the settlement",
                     subtittle = "n baseline: 321, n endline:492")) +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  ylim(0, 80) +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9))+
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())+
  labs(title= "Proportion of respondents planting crops outside the settlement", 
       subtitle = "n baseline: 321, n endline:492", x= element_blank(),
       y = element_blank()) 

# distance to farmland

df_time_to_farm <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                    "Endline", "Baseline", "Endline"),
                          perc_respondents = c(52, 58, 31, 29, 14, 12, 3, 1),
                          time_farm = c("< 30 mins", "< 30 mins", "30 mins to 1 hr", "30 mins to 1 hr", 
                                        "1 hr to 3 hrs",  "1 hr to 3 hrs", "> 3 hrs", "> 3 hrs" )
                          
)

df_time_to_farm %>% 
  ggplot(aes(fill = phase, x= fct_reorder(time_farm, -perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  ylim(0, 60) +
  theme_bw() +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9))+
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(y = "Percentage  of respondents")+
  labs(title= "Proportion of respondents by distance taken to reach farmland",
       subtitle = "n baseline: 230, n endline:527", x= element_blank(),
       y = element_blank()) 

# ways of acquiring land
df_land_acquisition <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                        "Endline", "Baseline", "Endline"),
                              perc_respondents = c(2, 9, 19, 26, 47, 24, 32, 41),
                              method = c("Given by OPM", "Given by OPM", "Informal agreement with host farmer", 
                                         "Informal agreement with host farmer", "Rented land from host community", 
                                         "Rented land from host community", "Other", "Other")
                              
                            )
df_land_acquisition %>% 
  arrange(perc_respondents) %>%
  mutate(method = factor(method, levels=c("Rented land from host community", 
                                          "Informal agreement with host farmer", 
                                          "Given by OPM", "Other"))) %>% 
  
  ggplot(aes(fill = phase, x= method,
             y = perc_respondents)) +

  geom_bar(position = "dodge", stat = "identity") +
  labs(title = paste("Proportion of respondensts by method of acquiring land for planting outside the settlement",
                     subtitle = "n baseline: 232, n endline:527")) +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  ylim(0, 50)+
  theme_bw()+
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9))+
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(y = "Percentage  of respondents")+
  labs(ggtitle= "Proportion of respondensts by method of acquiring land for planting outside the settlement",
       subtitle = "n baseline: 232, n endline:527", x= element_blank(),
       y = element_blank())


# cost of tillage per acre
df_tillage_cost <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                    "Endline", "Baseline", "Endline", "Baseline", "Endline", "Baseline", 
                                    "Endline"),
                          perc_respondents = c(6, 10, 13, 11, 16, 11, 16, 18, 17, 17, 37, 33),
                          category = c(">120,000 UGX", ">120,000 UGX", "90,001-120,000 UGX", "90,001-120,000 UGX", 
                                       "60,001-90,000 UGX", "60,001-90,000 UGX", "30,001-60,000 UGX", "30,001-60,000 UGX", 
                                       "10,001-30,000 UGX", "10,001-30,000 UGX", "<10,000 UGX", "<10,000 UGX")
                          
)
df_tillage_cost %>% 
  arrange(perc_respondents) %>%
  mutate(category = factor(category, levels=c(">120,000 UGX", "90,001-120,000 UGX", "60,001-90,000 UGX", "30,001-60,000 UGX",
                                              "10,001-30,000 UGX", "<10,000 UGX"))) %>% 
  
  ggplot(aes(fill = phase, x= category,
             y = perc_respondents)) +

  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Proportion of respondents by cost of tilling land per acre",
       subtitle = "n baseline: 232, n endline:527") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.y = element_text(angle = 0, hjust=1),
        axis.title = element_blank())+
  labs(x = "Percentage  of respondents") +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 40, by = 5)) 


# sources of financing tillage payments
df_tillage_finance_source <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                              "Endline", "Baseline", "Endline", "Baseline", "Endline", 
                                              "Baseline", "Endline", "Baseline",
                                              "Endline", "Baseline", "Endline", "Baseline", "Endline",
                                              "Baseline", "Endline"),
                                    perc_respondents = c(35, 26, 20, 10, 7, 15, 11, 13, 6, 9, 8, 7, 4, 5, 4, 3, 2, 6, 3, 6),
                                    finance_source = c("Sale of crops", "Sale of crops", "Sale of food aid", "Sale of food aid", "Casual labour",
                                                       "Casual labour", "Sale livestock/products", "Sale livestock/products", "Loans/borowing money",
                                                       "Loans/borowing money", "Others", "Others", "Petty trade/commerce", "Petty trade/commerce",
                                                       "Sale of hh assets/NFIs", "Sale of hh assets/NFIs", "Savings", "Savings", "Others", "Others")
                                   )

df_tillage_finance_source %>% 
  arrange(perc_respondents) %>%
  mutate(finance_source = factor(finance_source, levels= c(     "Others", "Savings", "Petty trade/commerce", "Sale of hh assets/NFIs",
                                                                "Loans/borowing money", "Casual labour", "Sale livestock/products", 
                                                                "Sale of food aid","Sale of crops" ))) %>% 
  
  ggplot(aes(fill = phase, x= finance_source,
             y = perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Proportion of respondents by source of financing tillage payments",
       subtitle = "n baseline: 204, n endline:462") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.y = element_text(angle = 0, hjust=1),
        axis.title = element_blank())+
  coord_flip()+
  labs(y = "Percentage  of respondents") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 40, by = 5))
  
# Sources of information about farming practices and inputs

df_farming_information_source <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                                  "Endline", "Baseline", "Endline", "Baseline", "Endline", 
                                                  "Baseline", "Endline", "Baseline",
                                                  "Endline", "Baseline", "Endline"),
                                        perc_respondents = c(27, 30, 21, 15, 10, 20, 14, 13, 11, 7, 8, 9, 5, 5, 2, 1),
                                        information_source = c("NGOs", "NGOs", "Friends", "Friends", "Agro-dealer", "Agro-dealer",
                                                               "Community group", "Community group", "Family", "Family", "Radio",
                                                               "Radio", "Sub-county agric officer", "Sub-county agric officer", 
                                                               "Other", "Other")
                                                                          )


  df_farming_information_source %>% 
  arrange(perc_respondents) %>%
  mutate(information_source = factor(information_source, levels=c("Other", "Sub-county agric officer", "Radio","Agro-dealer",
                                                                  "Family", "Community group", "Friends","NGOs" ))) %>%

  ggplot(aes(fill = phase, x= information_source,
             y = perc_respondents)) +  
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Proportion of respondents by source of information about farming practices and inputs",
       subtitle = "n baseline: 320, n endline:527") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.y = element_text(angle = 0, hjust=1),
        axis.title = element_blank())+
    labs(y = "Percentage  of respondents") +
  coord_flip()+
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 40, by = 5))


# farmers benefiting from discounted rates of inputs and equipment
df_inputs_discount_beneficiaries <- tibble(gender = c("Male", "Female", "Male", "Female"),
                                           perc_respondents = c(1219/(1219+1616)*100, 1616/(1219+1616)*100,
                                                                704/(704+577)*100, 577/(704+577)*100),
                                           status = c("Refugee", "Refugee", "Host community", 
                                                      "Host community"))

round(df_inputs_discount_beneficiaries$perc_respondents, 0)

df_inputs_discount_beneficiaries %>% 
  ggplot(aes(fill = gender, x= fct_reorder(status, -perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  ylim(0, 60) +
  theme_bw() +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9)) +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 0,hjust= 1),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank())+
  labs(title = "Gender distribution of respondents who benefited from discounted rates of inputs and equipment",
       subtitle = "Female(n): 2320, Male(n):1923", x= element_blank(),
       y = element_blank()) 


# type of seed purchase
df_type_seed_purchased <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline", "Endline"),
                                 perc_respondents = c(44, 17, 49, 74, 6, 9),
                                 status = c("Local seeds", "Local seeds", "Improved seeds", 
                                            "Improved seeds", "Both", "Both")
                              )


df_type_seed_purchased %>% 
  ggplot(aes(fill = phase, x= fct_reorder(status, -perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Proportion of respondents by type of seeds purchased",
       subtitle = "n baseline: 279, n endline:489",  y = element_blank()) +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  ylim(0, 80) +
  theme_bw() +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9)) +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 45,hjust= 1),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


# main source of cost share 
df_cost_share_sources <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                          "Endline", "Baseline", "Endline", "Baseline", "Endline", 
                                          "Baseline", "Endline", "Baseline", "Endline", "Baseline", 
                                          "Endline", "Baseline", "Endline", "Baseline",
                                          "Endline"),
                                perc_respondents = c(31, 30, 27, 14, 11, 16, 7, 13, 4, 8, 3, 6, 6, 8, 3, 4,
                                                     6, 1, 3, 1),
                                cost_share_source = c("Sale of crops", "Sale of crops", "Sale of food aid", "Sale of food aid", "Casual labour",
                                                      "Casual labour", "Sale of livestock/products", "Sale of livestock/products", "Loans/borowing money",
                                                      "Loans/borowing money", "Savings", "Savings", "Others", "Others", "Petty trade and commerce", 
                                                      "Petty trade and commerce", "Cash from Aid organizations", "Cash from Aid organizations", "Remittances", "Remittances")
                                                    )
df_cost_share_sources %>% 
  arrange(perc_respondents) %>%
  mutate(cost_share_source = factor(cost_share_source, levels=c("Others", "Remittances", "Petty trade and commerce", "Savings", "Loans/borowing money",
                                                                "Cash from Aid organizations", "Sale of livestock/products", "Casual labour",
                                                                "Sale of food aid", "Sale of crops"   
                                                                  ))) %>%
  
  ggplot(aes(fill = phase, x= cost_share_source,
             y = perc_respondents)) +

  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Proportion of respondents by main cost share sources for the seeds purchased",
       subtitle = "n baseline: 194, n endline:492") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 0, hjust=1),
        axis.title = element_blank())+
  labs(y = "Percentage  of respondents") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 40, by = 5)) +
coord_flip()                 
  


# Respondents who used vouchers to access discounted inputs 
df_voucher_access <- tibble(phase = c("Baseline","Endline"),
                            perc_respondents = c(66, 73),
                            voucher_access = c("Yes", "Yes")
)

df_voucher_access%>% 
  ggplot(aes(fill = phase, x= fct_reorder(voucher_access, -perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  ylim(0, 80) +
  theme_bw() +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9)) +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 0,hjust= 1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(title = "Proportion of respondents using paper or E-vouchers to access seeds",
       subtitle = "n baseline: 282, n endline:484", x= element_blank(),
       y = element_blank()) 


# Farmers who accessed agricultural extension services 
df_extension_service_access <- tibble(phase = c("Baseline", "Endline"),
                                      perc_respondents = c(67, 88),
                                      extension_service_access = c("Yes", "Yes")
)

df_extension_service_access %>% 
  ggplot(aes(fill = phase, x= fct_reorder(extension_service_access, -perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  ylim(0, 100) +
  theme_bw() +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9)) +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 0,hjust= 1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())+
  labs(title = "Proportion of farmers accessing agricultural extension services",
       subtitle = "n baseline: 321, n endline:526", x= element_blank(),
       y = element_blank())

# extension agricultural services accessed by respondents 
df_extension_service_type <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline",
                                              "Baseline", "Endline", "Baseline", "Endline",
                                              "Baseline", "Endline", "Baseline", "Endline",
                                              "Baseline", "Endline", "Baseline", "Endline"),
                                    perc_respondents = c(32, 26, 15, 14, 18, 13, 0, 6, 14, 16, 6, 5, 14, 14, 1, 4),
                                    extension_service_type = c("Improved farming practices", "Improved farming practices", 
                                                               "Post-harvest handling training", "Post-harvest handling training",
                                                               "Market information", "Market information", "Better livestock control",
                                                               "Better livestock control", "Improved crop varieties", "Improved crop varieties",
                                                               "Improve waste management", "Improve waste management", 
                                                               "Control of weeds/pests/diseases", "Control of weeds/pests/diseases",
                                                               "Climate Smart Agriculture", "Climate Smart Agriculture")
                                      )

df_extension_service_type %>% 
  arrange(perc_respondents) %>%
  mutate(extension_service_type = factor(extension_service_type, levels=c("Better livestock control", "Climate Smart Agriculture", 
                                                                          "Improve waste management", "Control of weeds/pests/diseases", 
                                                                          "Improved crop varieties", "Post-harvest handling training",
                                                                          "Market information", "Improved farming practices" 
                                                                          
                                                                           ))) %>%
  
  ggplot(aes(fill = phase, x= extension_service_type,
             y = perc_respondents)) +

  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Proportion of respondents by type of extension service accessed",
       subtitle = "n baseline: 216, n endline:465") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 0, hjust=1),
        axis.title = element_blank())+
  labs(y = "Percentage  of respondents") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 40, by = 5)) +
  coord_flip()


# Innovation centers
# services offered at innovation centers 
df_ICs <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline",
                           "Baseline", "Endline", "Baseline", "Endline",
                           "Baseline", "Endline", "Baseline", "Endline",
                           "Baseline", "Endline", "Baseline", "Endline",
                           "Baseline", "Endline", "Baseline", "Endline",
                           "Baseline", "Endline"),
                 perc_respondents = c(10, 19, 6, 18, 11, 17, 6, 9, 22, 10, 2, 7, 0, 
                                      12, 9, 0, 6, 7, 10, 0, 17, 1),
                 innovation_center_services = c("Farming as a business", "Farming as a business", 
                                                "Agricultural demonstrations", "Agricultural demonstrations",
                                                "Interlocking brick trainings", "Interlocking brick trainings",
                                                "Online learning", "Online learning", "Tailoring", "Tailoring",
                                                "Business start-up", "Business start-up", "Technology/digital literacy trainig",
                                                "Technology/digital literacy trainig", "Tree seedling demonstration",
                                                "Tree seedling demonstration", "Online job linkages", "Online job linkages",
                                                "Tree planting", "Tree planting", "Others", "Others")
                                    )

df_ICs %>% 
  arrange(perc_respondents) %>%
  mutate(innovation_center_services = factor(innovation_center_services, levels=c("Others", "Technology/digital literacy trainig", 
                                                                                  "Business start-up", "Online job linkages",
                                                                                  "Online learning", "Agricultural demonstrations",
                                                                                  "Tree seedling demonstration", "Tree planting",
                                                                                  "Farming as a business", "Interlocking brick trainings",
                                                                                  "Tailoring"
                                                                                   
                                                                                   ))) %>%
  
  ggplot(aes(fill = phase, x= innovation_center_services,
             y = perc_respondents)) +

  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.y = element_text(angle = 0, hjust=1),
        axis.title = element_blank())+
  labs(title= "Most mentioned services offered at the innovation centers by respondents", 
       subtitle = "n baseline: 165, n endline:314", x= element_blank(), y = "Percentage  of respondents") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 25, by = 5)) +
  coord_flip()

# Awareness about existing innovation center services among targeted groups

df_ICs_awareness <- tibble(phase = c("Baseline", "Endline"),
                           perc_respondents = c(14, 73),
                           ICs_awareness = c("Yes", "Yes")
)

df_ICs_awareness %>% 
  ggplot(aes(fill = phase, x= fct_reorder(ICs_awareness, -perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9)) +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank())+
  labs(title= "Proportion of respondents aware of the existence of the innovation centers", 
       subtitle = "n baseline: 443, n endline:214", x= element_blank(),
       y = element_blank()) 


# Frequency of visits to the ICs by target group

df_ICs_visit_freq <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                      "Endline", "Baseline", "Endline", "Baseline", "Endline"),
                            perc_respondents = c(16, 20, 29, 20, 0, 8, 1, 29, 3, 22),
                            ICs_visit_freq = c("Daily", "Daily", "Weekly", "Weekly", "Bi-monthly", 
                                               "Bi-monthly", "Monthly", "Monthly", "Others", "Others")
                     )

df_ICs_visit_freq %>% 
  ggplot(aes(fill = ICs_visit_freq, x= phase, perc_respondents), 
             y= perc_respondents)+
  
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef", "#ffffff", "#e3dab8", "#e3dab8"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.y = element_text(angle = 0, hjust=1),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks = element_blank())+
  labs(title= "ICs visit frequency distribution by target group", 
       subtitle = "n baseline: 441, n endline:214", x= element_blank(),
       y = "Percentage  of respondents") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 40, by = 5))
  

# Gender Integration
# Household member inputs in productive decisions


df_hh_decision_making <- tibble( phase= c("Baseline", "Endline", "Baseline", "Endline"),
                                 perc_respondents = c(22, 27, 23, 26),
                                 gender = c("Male", "Male", "Female", "Female"),
)

df_hh_decision_making %>% 
  ggplot(aes(fill = phase, x= fct_reorder(gender, -perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9)) +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 0,hjust= 1),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title= "Proportion of respondents by women and men reporting in productive decisions' inputs", 
       subtitle = "n baseline: 652, n endline:738", x= element_blank(),
       y = element_blank()) 


# decision making roles in the hh about resources

df_resources_dec_making <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                            "Endline", "Baseline", "Endline"),
                                  perc_respondents = c(23, 23, 58, 60, 17, 16, 2, 1),
                                  resources_dec_making = c("Husband", "Husband", "Husband and wife", "Husband and wife", "Wife", 
                                                           "Wife", "Other", "Other")
)

df_resources_dec_making %>% 
  ggplot(aes(fill = phase, x= fct_reorder(resources_dec_making, -perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw()+
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9)) +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 45,hjust= 1),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())+
  labs(title= "Proportion of respondents by decision making roles in the household about resource acces, 
       utilization and ownership", subtitle = "n baseline: 652, n endline:738", x= element_blank(),
       y = element_blank())


# level of women's access to agricultural extension services
df_women_access <- tibble(phase = c("Baseline", "Endline"),
                          perc_respondents = c(80, 89),
                          women_access = c("Yes", "Yes")
)

df_women_access %>% 
  ggplot(aes(fill = phase, x= fct_reorder(women_access, -perc_respondents), y= perc_respondents)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw() +
  geom_text(aes(label = scales::percent(perc_respondents/100, 
                                        accuracy = 1, trim = TRUE), vjust = -.5),
            position = position_dodge2(width = .9)) +
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(angle = 0,hjust= 1),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(title= "Proportion of women accessing agricultural extension services", 
       subtitle = "n baseline: 652, n endline:738", x= element_blank(),
       y = element_blank())


# Factors limiting women's access to agricultural services
df_women_limit_agric_services <- tibble(phase = c("Baseline", "Endline", "Baseline", "Endline", "Baseline",
                                                  "Endline", "Baseline", "Endline", "Baseline", "Endline",
                                                  "Baseline", "Endline" ),
                                        perc_respondents = c(25, 14, 27, 14, 18, 11, 15, 29, 9, 23, 6, 9),
                                        women_limit_agric_services = c("Lack of information", "Lack of information", 
                                                                       "Illiteracy", "Illiteracy", "Lack of time", 
                                                                       "Lack of time", "Too much work at home", "Too much work at home",
                                                                       "Lack of consent from their husbands",
                                                                       "Lack of consent from their husbands", "Other", "Other")
                                             )

df_women_limit_agric_services %>% 
  arrange(perc_respondents) %>%
  mutate(women_limit_agric_services = factor(women_limit_agric_services, levels=c("Illiteracy", "Lack of information",
                                                                                  "Lack of time", "Too much work at home",
                                                                                  "Lack of consent from their husbands",
                                                                                  "Other"))) %>%
  
  ggplot(aes(fill = phase, x= women_limit_agric_services,
             y = perc_respondents)) +

geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.y = element_text(angle = 0, hjust=1),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks = element_blank())+
  labs(title= "Proportion of respondents by main factors limiting women's access to agricultural services", 
       subtitle = "n baseline: 132, n endline: 89", x= element_blank(),
       y = "Percentage  of respondents") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 40, by = 5)) +
  coord_flip()

# Farmers trained in agronomy management and land preparation 
# advanced ggplot_not completed!
df_agronomy_management <- tibble(settlement = c("Yumbe", "Yumbe","Yumbe","Yumbe",
                                                "Obongi", "Obongi", "Obongi", "Obongi",
                                                "Madi Okollo", "Madi Okollo","Madi Okollo","Madi Okollo"),
                                 status = c("Host community", "Host community", "Refugee", "Refugee",
                                            "Host community", "Host community", "Refugee", "Refugee",
                                            "Host community", "Host community", "Refugee", "Refugee"),
                                 gender = c("Male", "Female", "Male", "Female",
                                            "Male", "Female", "Male", "Female",
                                            "Male", "Female", "Male", "Female"),
                                 perc_respondents = c(456/(456+465)*100, 465/(456+465)*100,
                                                      452/(452+580)*100, 580/(452+580)*100,
                                                      163/(163+181)*100, 181/(163+181)*100,
                                                      441/(441+820)*100, 820/(441+820)*100,
                                                      433/(433+411)*100, 411/(433+411)*100,
                                                      810/(810+1329)*100, 1329/(810+1329)*100)
                                 
)

ggplot(df_agronomy_management, aes(x=status,  y=perc_respondents))+
  geom_col(aes(fill = gender), position = "dodge") +
  facet_grid(cols = vars(settlement)) +
  scale_fill_manual(values = c("#303434", "#00aeef"))+
  theme_bw()+
  theme(text= element_text(size= 12), legend.title= element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        axis.text.y = element_text(angle = 0, hjust=1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank())+
  labs(title= "Gender distribution of trained farmers by community type in the three locations", 
       subtitle = "n baseline: 320, n endline:527", x= element_blank(),
       y = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 80, by = 10))







