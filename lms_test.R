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
