# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "MatchIt", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS") # nolint
ipak(pkg)
##########################################################################################################################################################

#data <- read.csv("/Users/yj.noh/Desktop/psm_data.csv", fileEncoding = "cp949")
data <- read_excel("/Users/yj.noh/Desktop/crm_824.xlsx")
head(data)


data$lunch_cnt_past[is.na(data$lunch_cnt_past)] <- 0
data$off_cnt_past[is.na(data$off_cnt_past)] <- 0
data$dinner_cnt_past[is.na(data$dinner_cnt_past)] <- 0

data$lunch_cnt_824[is.na(data$lunch_cnt_824)] <- 0
data$off_cnt_824[is.na(data$off_cnt_824)] <- 0
data$dinner_cnt_824[is.na(data$dinner_cnt_824)] <- 0

# 달성 vs 미달성 
data$crm_group <- factor(data$crm_group, levels = c ("icrm_824_lunch", "icrm_824_off", "icrm_824_dinner"))

data[c("crm_group","lunch_cnt_past","lunch_cnt_824", "off_cnt_past", "off_cnt_824", "dinner_cnt_past","dinner_cnt_824", "달성여부")]  %>% 
tbl_strata(
    strata = crm_group,~.x  %>% 
    tbl_summary(
        by = 달성여부,
        type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
              missing_text = "(Missing value)", 
              digits = list(all_continuous() ~ c(1, 2), 
                            all_categorical() ~ c(0, 1))) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()
)


# 