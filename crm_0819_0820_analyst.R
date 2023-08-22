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
data <- read_excel("/Users/yj.noh/Desktop/psm_data.xls")
head(data)

# time_1st 변경
data <- data  %>% mutate(time_1st = case_when (data$time_1st == "11_13" ~ "lunch", 
                                              data$time_1st == "17_20" ~ "dinner",
                                              TRUE ~ "etc"))
table(data$time_1st)

# PSM 매칭
# crm group 1 , null - 0
data <- data  %>% mutate(is_crm = ifelse(crm_group %in% c("A", "B", "C", "D") ,1, 0))
table(data$is_crm) # 0 : 29526, 1: 13325

# matching
data_f <- data[c("수행건수", "실제수행기간","time_1st", "day_of_week_1st", "주요수행method", "is_crm")]

# mod_1st : 1순위수행시각, 1순위평일/주말 매칭 
mod_1st <- matchit(is_crm ~ time_1st + day_of_week_1st, data = data_f, ratio = 2, method = "nearest", distance = "logit", replace = TRUE, caliper= .00001)

dta_1st <- match.data(mod_1st)
dim(dta_1st)
table(dta_1st$is_crm) 

tbl_summary(dta_1st[c("수행건수", "실제수행기간","time_1st", "day_of_week_1st", "주요수행method", "is_crm")], by = is_crm)  %>% add_p()


# mod_2nd : 수행건수, 실제수행기간  
mod_2nd <- matchit(is_crm ~ 수행건수 + 실제수행기간, data = data_f, ratio = 2,  method = "nearest", distance = "logit",  caliper= .001)

dta_2nd <- match.data(mod_2nd)
dim(dta_2nd)
table(dta_2nd$is_crm) #0: 10463, 1 : 5565

tbl_summary(dta_2nd[c("수행건수", "실제수행기간","time_1st", "day_of_week_1st", "주요수행method", "is_crm")], by = is_crm)  %>% add_p()


# mod_3rd : 전체 
mod_3rd <- matchit(is_crm ~ ., data = data_f, ratio = 2,  method = "nearest", distance = "logit", caliper = .001)

dta_3rd <- match.data(mod_3rd)
dim(dta_3rd)
table(dta_3rd$is_crm) #0: 10463, 1 : 5565

tbl_summary(dta_3rd[c("수행건수", "실제수행기간","time_1st", "day_of_week_1st", "주요수행method", "is_crm")], by = is_crm)  %>% add_p()


tbl_summary(data_f[c("수행건수", "실제수행기간","time_1st", "day_of_week_1st", "주요수행method", "is_crm")], by = is_crm)  %>% add_p()

