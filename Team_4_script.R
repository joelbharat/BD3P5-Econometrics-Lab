# Read Libraries
library(dplyr)
library(readr)
library(lubridate) # Working with Dates
library(reshape)
library(reshape2)# Reshaping the data
library(tidyr)
library(tidyverse)
library(data.table)
library(zoo) ## datetime
library(bizdays) # business day
library(ggeasy) # for easy ggplot editing
library(harrypotter) # for palettes
library(tsibble)
library(timeDate)
library(readxl)
library(tsibbledata)
library(magrittr)
library(fpp3)
library(feasts)
library(latex2exp)
library(slider)
library(fUnitRoots)
library(urca)
library(forecast)
library("writexl")
library(nnfor)
## Visualizations
library(ggplot2)
library(plotly)
library(MASS)

# Neural Network Model (NNAR)

# Converting segment 1 raw data into averaged figures
raw_train %>% dplyr::filter(segment==1) %>%  dplyr::select (-c(branch_id,zone,state, segment))-> train_s1
aggregate(train_s1$no_of_applicants, list(dmy(train_s1$application_date)), FUN=mean) -> train_s1_consol
colnames(train_s1_consol) <- c("date", "no_of_applicants")
train_s1_consol %>% mutate(date = as_date(date)) %>% as_tsibble(index = date) -> train_ts1


# Converting segment 2 raw data into averaged figures
raw_train %>% dplyr::filter(segment==2) %>%  dplyr::select (-c(branch_id,zone, segment))-> train_s2
aggregate(train_s2$no_of_applicants, list(dmy(train_s2$application_date)), FUN=mean) -> train_s2_consol
colnames(train_s2_consol) <- c("date", "no_of_applicants")
train_s2_consol %>% mutate(date = as_date(date)) %>% as_tsibble(index = date) -> train_ts2

train_ts1 <- tsibble::fill_gaps(train_ts1,no_of_applicants=0L)
dcmp_1 <- train_ts1 %>%
  model(stl = STL(no_of_applicants))

dcmp_2 <- train_ts2 %>%
  model(stl = STL(no_of_applicants))

seg_1_ts1 <- ts(components(dcmp_1)$season_adjust, start=c(2017,04,01), end=c(2019,06,05),frequency = 365)
seg_2_ts2 <- ts(components(dcmp_2)$season_adjust, start =c(2017,04,01), end =c(2019,06,23),frequency=365)

ts_seg1 <- nnetar(y=seg_1_ts1, order = c(1,0,1))
plot(forecast(ts_seg1))
pr_data1 = data.frame(forecast(ts_seg1,h=30))
ts_seg2 <- nnetar(y=seg_2_ts2, order = c(1,0,1))
plot(forecast(ts_seg2))
pr_data2 = data.frame(forecast(ts_seg2,h=30))

df_s <- data.frame(id = c(1:60),
                   no_of_applicants = c(abs(pr_data1$Point.Forecast), abs(pr_data2$Point.Forecast)))

write_csv(df_s, "C:\\MSc (BDA)\\BD3P5 Econometrics lab test\\sjcbusinessforecasting\\sample_submission.csv")
