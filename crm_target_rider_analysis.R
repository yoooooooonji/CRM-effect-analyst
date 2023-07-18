# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS") # nolint
ipak(pkg)
####################################################################################################################################################################

#dlvry_raw <-read.table("/Users/yj.noh/Desktop/dlvry_raw_data.tsv")

dlvry_raw <- read_excel("/Users/yj.noh/Desktop/dlvry_raw_data.xlsx")

crm_A <- read_excel("/Users/yj.noh/Desktop/crm_rider_list.xlsx", sheet=2)
crm_B <- read_excel("/Users/yj.noh/Desktop/crm_rider_list.xlsx", sheet=3)
crm_C <- read_excel("/Users/yj.noh/Desktop/crm_rider_list.xlsx", sheet=4)

dlvry_raw <- dlvry_raw  %>% arrange(rider_no, consign_date)

dlvry_raw$business_day <- as.Date(dlvry_raw$business_day)
dlvry_raw$consign_date <- as.POSIXct(dlvry_raw$consign_date, format = "%Y-%m-%dT%H:%M:%S")

# 시간대 그룹 나누기
dlvry_raw <- dlvry_raw %>% 
  mutate(time_1 = ifelse(hour(consign_date) <11, 1,0),
        time_2 = ifelse(hour(consign_date) >= 11 & hour(consign_date) < 14,1,0),
        time_3 = ifelse(hour(consign_date) >= 14 & hour(consign_date) < 17,1,0),
        time_4 = ifelse(hour(consign_date) >= 17 & hour(consign_date) < 20,1,0),
        time_5 = ifelse(hour(consign_date)>=20,1,0))


  # mutate(time_group = case_when(
  #   hour(consign_date) < 11 ~ "time1",
  #   hour(consign_date) >= 11 & hour(consign_date) < 14 ~ "time2",
  #   hour(consign_date) >= 14 & hour(consign_date) < 17 ~ "time3",
  #   hour(consign_date) >= 17 & hour(consign_date) < 20 ~ "time4",
  #   TRUE ~ "time5"))

dlvry_raw <- dlvry_raw  %>% 
mutate(crm_A = ifelse(rider_no %in% crm_A$rider_id, 1,0), 
        crm_B = ifelse(rider_no %in% crm_B$rider_id ,1, 0), 
        crm_C = ifelse(rider_no %in% crm_C$rider_id,1,0))

dlvry_raw <- dlvry_raw  %>% 
mutate(crm_X = ifelse(crm_A == 0 & crm_B == 0 & crm_C == 0, 1, 0))

#  배차 간 시간 차이 계산 
dlvry_raw <- dlvry_raw  %>% 
group_by (rider_no) %>% 
mutate(time_diff = as.numeric(difftime(lead(consign_date), consign_date, units = "secs"))/60) #min 

summary(dlvry_raw$time_diff)

# 최초 운행/ 마지막 운행 
dlvry_raw <- dlvry_raw  %>% 
group_by(rider_no) %>% 
mutate(min_consign = min(consign_date),
        max_consign = max(consign_date))

dlvry_raw <- dlvry_raw  %>% 
mutate(day_diff = as.numeric(difftime(max_consign , min_consign, units = "mins"))/60) #hour 

# 처리건수 
test <- dlvry_raw %>%
  group_by(rider_no,crm_A, crm_B, crm_C, crm_X, time_1, time_2, time_3, time_4, time_5) %>%
  summarise(count = n_distinct(dlvry_id))

test <- test  %>% 
mutate(time_1 = ifelse(time_1 > 0, count, time_1),
      time_2 = ifelse(time_2 >0, count, time_2),
      time_3 = ifelse(time_3 >0, count, time_3),
      time_4 = ifelse(time_4 >0, count, time_4),
      time_5 = ifelse(time_5 >0, count, time_5))

test <- test  %>% 
group_by(rider_no, crm_A, crm_B, crm_C, crm_X) %>% 
mutate(time_1 = sum(time_1),
       time_2 = sum(time_2),
       time_3 = sum(time_3),
       time_4 = sum(time_4),
       time_5 = sum(time_5))

test <- test[!duplicated(test[,c("rider_no","crm_A","crm_B","crm_C","crm_X","time_1","time_2","time_3","time_4","time_5")]),]
n_distinct(test$rider_no)
dim(test)


dlvry_df <- left_join(subset(dlvry_raw, select = -c(time_1,time_2, time_3, time_4, time_5)), test, by = c("rider_no" = "rider_no", "crm_A" = "crm_A","crm_B" = "crm_B","crm_C" = "crm_C","crm_X" = "crm_X"))


## 내가 보고 싶은것
# crm 별로 평균 배차간격, 평균 운행시간, time별로 주문처리건수 

dlvry_df <- dlvry_df  %>% 
group_by(rider_no) %>% 
mutate(avg_consign = mean(time_diff, na.rm = TRUE),
       auto_sum = sum(auto_allocated),
       single_type = sum(delivery_carry_type == "SINGLE"),
       count = (time_1 + time_2 + time_3 + time_4 + time_5))


data = subset(dlvry_df, select = -c(business_day, consign_date, rgn1_nm, rgn2_nm, ord_no, dlvry_id, auto_allocated, delivery_carry_type, time_diff, min_consign, max_consign))


data <- data[!duplicated(data[,c("rider_no","crm_A","crm_B","crm_C","crm_X","time_1","time_2","time_3","time_4","time_5", "day_diff","count","avg_consign","auto_sum","single_type")]),]

data$avg_consign[is.na(data$avg_consign)] <- 0

data <- data %>% 
  mutate(group_name = case_when (crm_A == 1 & crm_C == 1 ~"A&C",
                                 crm_B == 1 & crm_C == 1 ~"B&C",
                                 crm_A == 1 & crm_B == 1 ~"A&B",
                                 crm_A == 1 ~ "A",
                                 crm_B == 1 ~ "B",
                                 crm_C == 1 ~ "C",
                                 crm_X == 1 ~ "X"))

data_filter <- subset(data, select = -c(rider_no, crm_A, crm_B, crm_C,crm_X, avg_consign))


data_filter <- data_filter %>% 
  mutate(n_time1 = ifelse(time_1 > 0, 1, 0),
         n_time2 = ifelse(time_2 > 0, 1, 0),
         n_time3 = ifelse(time_3 > 0, 1, 0),
         n_time4 = ifelse(time_4 > 0, 1, 0),
         n_time5 = ifelse(time_5 > 0, 1, 0))

