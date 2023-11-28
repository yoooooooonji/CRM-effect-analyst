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
getwd()
# 전체 
df <- read.csv("/Users/yj.noh/Desktop/app_push_data.csv", fileEncoding = "utf-8")
head(df)
dim(df) #52785

rr <- read.csv("/Users/yj.noh/Desktop/risk_ratio.csv", fileEncoding = "utf-8")
head(rr)
dim(rr) #17595

# data join 
data <- left_join(df, rr[c("brms_rider_id", "cluster", "is_churn", "risk_ratio")], by = c("mem_no" = "brms_rider_id"))
head(data)
dim(data) #52785

data$time <- as.Date(data$time, format = '%Y.%m.%d')
data$first_logon <- as.POSIXlt(data$first_logon, format = '%Y.%m.%d %H:%M')
data$first_dlvry <- as.POSIXlt(data$first_dlvry, format = '%Y.%m.%d %H:%M')

head(data)
colSums(is.na(data))

df <- data %>% filter(time == '2023-11-24')
dim(df) #17595
head(df$crm_group)

# 실험군 중, 첫 운행시작이 오전 10시 30분 이전 삭제하기 
test_rm <- df  %>% filter(crm_group == "test"  & first_dlvry < '2023-11-24 10:30')
control_rm <- df  %>% filter(crm_group == "control"  & first_dlvry < '2023-11-24 10:30')

dim(test_rm) #27명 
dim(control_rm) #30명 

df <- df  %>% filter(!mem_no %in% test_rm$mem_no & !mem_no %in% control_rm$mem_no)
dim(df) #17568 -> 17538


# is_logon : 오전 10시30분 이후 로그온
# is_dlvry : 오전 10시 30분 이후 배달

df <- df  %>% mutate(is_logon = ifelse(first_logon >= '2023-11-24 10:30', 1, 0))
df <- df  %>% mutate(is_dlvry = ifelse(first_dlvry >= '2023-11-24 10:30', 1, 0))

write.csv(df, "app_push_filtered.csv", fileEncoding= "utf-8")
colSums(is.na(df))
df$is_logon[is.na(df$is_logon)] <- 0
df$is_dlvry[is.na(df$is_dlvry)] <- 0

df[c("crm_group", "ord_cnt", "is_logon", "is_dlvry", "cluster", "is_churn", "risk_ratio")] %>% 
tbl_summary(
    by = crm_group,
   type = list(
    ord_cnt ~ "continuous2",
    risk_ratio ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()
