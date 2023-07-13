# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS") # nolint
ipak(pkg)
##########################################################################################################################################################

data <- read.csv("/Users/yj.noh/Desktop/crm_effect_result.csv", fileEncoding = "cp949")
colSums(is.na(data))
head(data)
#str(data)

# var <-  c('group_name', "cluster", "crm_group", "method_list")
# data[,var]<- lapply(data[,var], factor)

data$business_day <- as.Date(data$business_day)


# crm 1 분석
crm1 <- data %>%  
filter((group_name == 'crm1_reward0' | group_name == 'crm1_reward1' | group_name == 'no_crm') & (business_day == '2023-04-28' | business_day == '2023-05-05'))

table(crm1$group_name)
table(crm1$business_day)
#str(crm1)

# crm1[c("group_name", "business_day", "total_dlvry", "total_hour","total_fee", "single_dlvry","single_hour", "single_fee", "stod_dlvry", "stod_hour", "stod_fee","method_list", "ai_accept")]  %>% 
# tbl_strata(
#     strata = group_name,~.x  %>% 
#     tbl_summary(
#         by = business_day,
#         type = all_continuous() ~ "continuous2",
#               statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
#               missing_text = "(Missing value)", 
#               digits = list(all_continuous() ~ c(1, 2), 
#                             all_categorical() ~ c(0, 1))) %>%
#   add_overall() %>%
#   add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
#   bold_labels()
# )
# ##########################################################################################################################################################
# crm 2 분석
crm2 <- data %>%  
filter((group_name == 'crm2_reward0' | group_name == 'crm2_reward1' | group_name == 'no_crm') & (business_day == '2023-06-14' | business_day == '2023-06-21'))

table(crm2$group_name)
table(crm2$business_day,crm2$group_name)

# crm2[c("group_name", "business_day", "total_dlvry", "total_hour","total_fee", "single_dlvry","single_hour", "single_fee", "stod_dlvry", "stod_hour", "stod_fee","method_list", "ai_accept")]  %>% 
# tbl_strata(
#     strata = group_name,~.x  %>% 
#     tbl_summary(
#         by = business_day,
#         type = all_continuous() ~ "continuous2",
#               statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
#               missing_text = "(Missing value)", 
#               digits = list(all_continuous() ~ c(1, 2), 
#                             all_categorical() ~ c(0, 1))) %>%
#   add_overall() %>%
#   add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
#   bold_labels()
# )
##########################################################################################################################################################
# crm 3 분석
crm3_A <- data %>%  
filter((  group_name == 'crm3_reward0_A' | 
          group_name == 'crm3_reward1_A' | 
          group_name == 'no_crm') & (business_day == '2023-06-24' | business_day == '2023-06-25' |business_day == '2023-07-01' | business_day == '2023-07-02'))


crm3_B <- data %>%  
  filter((  group_name == 'crm3_reward0_B' | 
            group_name == 'crm3_reward1_B' | 
            group_name == 'no_crm') & (business_day == '2023-06-24' | business_day == '2023-06-25' |business_day == '2023-07-01' | business_day == '2023-07-02'))


crm3_C <- data %>%  
  filter((  group_name == 'crm3_reward0_C' | 
            group_name == 'crm3_reward1_C' | 
            group_name == 'no_crm') & (business_day == '2023-06-24' | business_day == '2023-06-25' |business_day == '2023-07-01' | business_day == '2023-07-02'))


# crm3[c("group_name", "business_day", "total_dlvry", "total_hour","total_fee", "single_dlvry","single_hour", "single_fee", "stod_dlvry", "stod_hour", "stod_fee","method_list", "ai_accept")]  %>% 
# tbl_strata(
#     strata = group_name,~.x  %>% 
#     tbl_summary(
#         by = business_day,
#         type = all_continuous() ~ "continuous2",
#               statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
#               missing_text = "(Missing value)", 
#               digits = list(all_continuous() ~ c(1, 2), 
#                             all_categorical() ~ c(0, 1))) %>%
#   add_overall() %>%
#   add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
#   bold_labels()
# )
