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
data <- read_excel("/Users/yj.noh/Desktop/new_rider_need_rgn2.xlsx")
head(data)
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

