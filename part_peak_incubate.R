# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS", "MatchIt", "viridis") # nolint
ipak(pkg)
##########################################################################################################################################################
data <- read.csv("/Users/yj.noh/Desktop/part_peak_incubate.csv", fileEncoding = "cp949")
A3 <- read.csv("/Users/yj.noh/Desktop/A3.csv", fileEncoding = "utf-8")
A4 <- read.csv("/Users/yj.noh/Desktop/A4.csv", fileEncoding = "utf-8")
B3 <- read.csv("/Users/yj.noh/Desktop/B3.csv", fileEncoding = "utf-8")
B4 <- read.csv("/Users/yj.noh/Desktop/B4.csv", fileEncoding = "utf-8")

dim(A3)
dim(A4)
dim(B3)
dim(B4)

data <- data  %>% filter(group_final %in% c("A_3", "A_4", "B_3", "B_4"))
n_distinct(data$rider_id) #21051

str(data)

A3 <- left_join(A3, data[c("rider_id", "avg_week_dlvry_cnt", "avg_week_day_cnt", "dlvry_cnt_group", "day_cnt_group", "group_final")], by = c("target_rider_id" = "rider_id"))
A4 <- left_join(A4, data[c("rider_id", "avg_week_dlvry_cnt", "avg_week_day_cnt", "dlvry_cnt_group", "day_cnt_group", "group_final")], by = c("target_rider_id" = "rider_id"))
B3 <- left_join(B3, data[c("rider_id", "avg_week_dlvry_cnt", "avg_week_day_cnt",  "dlvry_cnt_group", "day_cnt_group","group_final")], by = c("target_rider_id" = "rider_id"))
B4 <- left_join(B4, data[c("rider_id", "avg_week_dlvry_cnt", "avg_week_day_cnt", "dlvry_cnt_group", "day_cnt_group", "group_final")], by = c("target_rider_id" = "rider_id"))

B3 <- B3[c("target_rider_id", "agency_name", "crm_cnt", "crm_success_pay","avg_week_dlvry_cnt", "avg_week_day_cnt","dlvry_cnt_group", "day_cnt_group", "group_final")]
B4 <- B4[c("target_rider_id", "agency_name", "crm_cnt", "crm_success_pay","avg_week_dlvry_cnt", "avg_week_day_cnt", "dlvry_cnt_group", "day_cnt_group","group_final")]
A3 <- A3[c("target_rider_id", "agency_name", "dlvry_cnt", "crm_cnt", "crm_success_pay", "avg_week_dlvry_cnt", "avg_week_day_cnt", "dlvry_cnt_group", "day_cnt_group", "group_final")]
A4 <- A4[c("target_rider_id", "agency_name", "dlvry_cnt", "crm_cnt", "crm_success_pay", "avg_week_dlvry_cnt", "avg_week_day_cnt", "dlvry_cnt_group", "day_cnt_group","group_final")]


# 데이터 합치기
df_A <- rbind(A3, A4)
df_B <- rbind(B3, B4) 

n_distinct(df_A$target_rider_id) #10538
n_distinct(df_B$target_rider_id) #10513

# 달성여부 
df_A <- df_A  %>% mutate(is_goal = ifelse(crm_success_pay != 0, 1, 0))
df_B <- df_B  %>% mutate(is_goal = ifelse(crm_success_pay != 0, 1, 0))


# 반응여부 
df_A <- df_A %>% mutate(is_react = case_when (crm_cnt == 0  ~ 0,
                                                        crm_cnt > 0 & is_goal == 1 ~ 2,
                                                        crm_cnt > 0 & is_goal == 0 ~ 1))

df_B <- df_B  %>% mutate(is_react = case_when (crm_cnt == 0  ~ 0,
                                                        crm_cnt > 0 & is_goal == 1 ~ 2,
                                                        crm_cnt > 0 & is_goal == 0 ~ 1))

table(df_A$is_react)
table(df_B$is_react)


# A 그룹 달성자vs 미달성자
df_A[c("agency_name", "crm_cnt", "dlvry_cnt", "avg_week_dlvry_cnt", "avg_week_day_cnt",  "group_final", "is_react")] %>% 
tbl_strata(
  strata = group_final,
  ~.x  %>% 
tbl_summary(
   by = is_react,
   type = list(
   avg_week_dlvry_cnt ~ "continuous2",
   avg_week_day_cnt ~ "continuous2",
   crm_cnt ~ "continuous2",
   dlvry_cnt ~ "continuous2"
  ),  
   statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
   missing_text = "(Missing value)", 
   digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
 ) %>%
 add_overall() %>%
 add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
 bold_labels()
)

# B 그룹 달성자vs 미달성자
df_B[c("agency_name", "crm_cnt",  "avg_week_dlvry_cnt", "avg_week_day_cnt",  "group_final", "is_react")] %>% 
tbl_strata(
  strata = group_final,
  ~.x  %>% 
tbl_summary(
   by = is_react,
   type = list(
   avg_week_dlvry_cnt ~ "continuous2",
   avg_week_day_cnt ~ "continuous2",
   crm_cnt ~ "continuous2"
  ),  
   statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
   missing_text = "(Missing value)", 
   digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
 ) %>%
 add_overall() %>%
 add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
 bold_labels()
)

write.csv(df_A, "/Users/yj.noh/Desktop/df_A.csv", fileEncoding = "cp949", row.names = FALSE)
write.csv(df_B, "/Users/yj.noh/Desktop/df_B.csv", fileEncoding = "cp949", row.names = FALSE)
