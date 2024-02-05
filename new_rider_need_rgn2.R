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
head(data)

# A/B 그룹 통계량
data[c("avg_week_dlvry_cnt", "avg_week_day_cnt", "group_name", "group_final")] %>% 
tbl_strata(
  strata = group_name,
  ~.x  %>% 
tbl_summary(
   by = group_final,
   type = list(
   avg_week_dlvry_cnt ~ "continuous2",
   avg_week_day_cnt ~ "continuous2"
  ),  
   statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
   missing_text = "(Missing value)", 
   digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
 ) %>%
 add_overall() %>%
 add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
 bold_labels()
)


# 세부그룹 분리 _A 
group_A = data  %>% filter(group_name == 'A')

group_A[c( "avg_week_day_cnt", "group_final")] %>% 
tbl_summary (
  by = group_final,
   type = list(
   avg_week_day_cnt ~ "continuous2"
  ),  
   statistic = all_continuous() ~ c("{mean} ({sd})", "{median}" ,  "{min}, {max}"),
   missing_text = "(Missing value)", 
   digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
 ) %>%
 add_overall() %>%
 add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
 bold_labels()

# 세부그룹 분리 _B 
group_B = data  %>% filter(group_name == 'B')

group_B[c( "avg_week_dlvry_cnt", "group_final")] %>% 
tbl_summary (
  by = group_final,
   type = list(
   avg_week_dlvry_cnt ~ "continuous2"
  ),  
   statistic = all_continuous() ~ c("{mean} ({sd})", "{median}" ,  "{min}, {max}"),
   missing_text = "(Missing value)", 
   digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
 ) %>%
 add_overall() %>%
 add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
 bold_labels()


#str(data)
#data <- data %>% rename("rgn1_nm" = "rgn1_nm::filter")

table(data$rgn1_nm)
data<- data %>% mutate(is_restrict = ifelse(baemin1_single_limited_time > 0, 1, 0)) 

region = unique(data$rgn1_nm)
region

make_df <- function(data, region) {
  for (i in region) {
    assign(paste0("df_", i), data %>% filter(rgn1_nm == i), envir = .GlobalEnv)
  }
}

make_df(data,region)

#data [c("rgn1_nm", "rgn2_nm", "r_value", "new_rider_per", "old_rider_per", "r_value_new", "r_value_old")] %>% 
#tbl_strata (
#  strata = rgn2_nm,
#  ~.x  %>% 
#tbl_summary(
#    by = is_restrict,
#   type = list(
#    ord_cnt ~ "continuous2"
#   ),  
#    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
#    missing_text = "(Missing value)", 
#    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
#  ) %>%
#  add_overall() %>%
#  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
#  bold_labels()
#)

