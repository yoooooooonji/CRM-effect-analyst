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

rr <- read.csv("CRM-effect-analyst-/app_push_filtered.csv", fileEncoding = "utf-8")
table(rr$crm_group)

data <- read.csv("/Users/yj.noh/Desktop/lms_data.csv", fileEncoding = "utf-8")
head(data)

# rr에서 risk_ratio, is_logon, is_dlvry  join
data <- left_join(data, rr[c(2,8,9,10,11,12)], by = "mem_no")
head(data)

che <- data  %>% filter(crm_group == "control" & is_logon == 1 & is_dlvry == 0)
n_distinct(che$mem_no) #729 

# test/control 나누기
data <- data  %>% mutate(lms_group = case_when (crm_group == "control" & is_logon == 1 & is_dlvry ==0~ "control",
                                                crm_group == "test" ~ "test",
                                                TRUE ~ "no"))

df <- data  %>% filter(lms_group %in% c("control", "test"))
n_distinct(df$mem_no) #1529 (test : 729, control : 800)
head(df)

df <- subset(df, select = -c(crm_group))
df <- df  %>% mutate(is_ok = ifelse(cnt_day >= 15, 1, 0))
dim(df)
head(df)



# "cnt_1", "cnt_2", "cnt_3", "cnt_day", "is_ok"
# "cluster",  "is_churn", "risk_ratio"

df[c("lms_group","cnt_1", "cnt_2", "cnt_3", "cnt_day", "is_ok")] %>% 
tbl_summary(
    by = lms_group,
   type = list(
    cnt_1 ~ "continuous2",
    cnt_2 ~ "continuous2", 
    cnt_3 ~ "continuous2", 
    cnt_day~ "continuous2"
    #risk_ratio ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()



ok_df = df  %>% filter(is_ok == 1)
table(ok_df$lms_group)


ok_df[c("cluster",  "is_churn", "risk_ratio", "lms_group","cnt_1", "cnt_2", "cnt_3", "cnt_day", "is_ok")] %>% 
tbl_summary(
    by = lms_group,
   type = list(
    cnt_1 ~ "continuous2",
    cnt_2 ~ "continuous2", 
    cnt_3 ~ "continuous2", 
    cnt_day~ "continuous2",
    risk_ratio ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()


data <- read_excel("/Users/yj.noh/Desktop/total_set.xlsx")
head(data)

data[c("cnt_ord", "sum_fee", "not_period", "group")] %>% 
tbl_summary(
    by = group,
   type = list(
    cnt_ord ~ "continuous2",
    sum_fee ~ "continuous2",
    not_period ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()


##########################################################################################################################################################
# 12/6 LMS 분석
rider = read_excel("/Users/yj.noh/Desktop/total_set.xlsx")

table(rider$group) #5222, 5184
dim(rider)

dlvry = read_excel("/Users/yj.noh/Desktop/lms_data.xlsx")
head(dlvry)

data = left_join(rider[c("brms_rider_id", "cnt_ord", "sum_fee", "not_period", "group")], dlvry, by = "brms_rider_id")
dim(data)
table(data$group)

colSums(is.na(data)) # 9762, 10242 na 

data$first_logon <- as.POSIXlt(data$first_logon, format = '%Y-%m-%d %H:%M')
data$first_dlvry <- as.POSIXlt(data$first_dlvry, format = '%Y-%m-%d %H:%M')

data <- data %>% mutate(is_logon = ifelse(first_logon >= '2023-12-06 11:00', 1, 0),
                        is_dlvry = ifelse(first_dlvry >= '2023-12-06 11:00', 1,0))


data$is_logon[is.na(data$is_logon)] <- 0
data$is_dlvry[is.na(data$is_dlvry)] <- 0
data$ord_cnt[is.na(data$ord_cnt)] <- 0
data$not_period <- as.numeric(data$not_period)

summary(data$not_period)

# 28~56 -> 28~35, 36~42, 43~49, 50~56 
data <- data %>% mutate(churn_group = case_when(
                            not_period <= 35 ~ "4",
                            not_period <= 42 ~ "5",
                            not_period <= 49 ~ "6",
                            TRUE ~ "7"
                        ))

table(data$churn_group)


data [c("group", "is_logon", "is_dlvry", "ord_cnt", "churn_group")] %>% 
tbl_strata (
  strata = churn_group,
  ~.x  %>% 
tbl_summary(
    by = group,
   type = list(
    ord_cnt ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()
)
