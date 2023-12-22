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
data <- read.csv("/Users/yj.noh/Desktop/data.csv", fileEncoding = "utf-8")

head(data)


data[c("crm_group", "total_dlvry_cnt")] %>% 
tbl_summary(
    by = crm_group,
   type = list(
    total_dlvry_cnt ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()

today <- read_excel("/Users/yj.noh/Downloads/이탈위험군_대상자_추출_2023_12_15.xlsx") 
yest <- read_excel("/Users/yj.noh/Downloads/이탈위험군_대상자_추출_2023_12_16.xlsx") 


