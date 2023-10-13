# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "MatchIt", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS", "pROC", "Epi") # nolint
ipak(pkg)
##########################################################################################################################################################

data <- read_excel("/Users/yj.noh/Desktop/crm_rain.xlsx")

df <- subset(data, select = -c(mem_no, first_time, cnt_bmart_913, cnt_bmart_920))

colSums(is.na(df))
df <- df [c("crm_group", "cnt_913", "cnt_920", "cnt_dinner_913", "cnt_dinner_920", "cnt_single_913", "cnt_single_920", "cnt_common_913", "cnt_common_920", "is_react", "is_check", "region")]
df <- df  %>% mutate(is_react = case_when (is_react == 0 ~ "기운행자",
                                            is_react == 1 ~ "미운행자",
                                            is_react == 2 ~ "무반응"),
                    is_check = case_when (is_check == 0 ~ "미달성",
                                         is_check == 1 ~ "달성"))

df$is_react <- factor(df$is_react, levels = c("기운행자", "미운행자", "무반응"))
df$is_check <- factor(df$is_check, level = c("달성", "미달성"))

df_A <- df  %>% filter(crm_group == "A")
df_B <- df  %>% filter(crm_group == "B")
df_C <- df  %>% filter(crm_group == "C")
df_D <- df  %>% filter(crm_group == "D")


df_A  %>% tbl_strata(
  strata = is_react,~.x  %>% 
    tbl_summary(
      by = is_check,
      type = all_continuous() ~ "continuous2",
      statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
      missing_text = "(Missing value)", 
      digits = list(all_continuous() ~ c(1, 2), 
                    all_categorical() ~ c(0, 1))) %>%
    add_overall() %>%
    add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
    bold_labels()
)

result <- df %>%
  group_by(crm_group, is_react, is_check, region) %>%
  summarise(count = n()) %>%
  arrange(crm_group, is_react, is_check, desc(count)) %>%
  group_by(crm_group, is_react, is_check) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3) %>%
  ungroup() #%>%
  #mutate(percentage = count / sum(count))

# 결과 출력
print(result)
