######################################
### Title: 03——analysis
### Date: 01/26/2024
### Author: Shuai Zhu
### Description: 
######################################

library(dplyr)
library(stringr)
library(openxlsx)
library(chron)
library(lubridate)

### setting working directory
working_directory <-  'P:\\BERD\\toggl_timetrack'
setwd(working_directory)

### read members' time tracking data
df <- read.xlsx('./DataProcessed/members timetrack.xlsx')%>%as_tibble(.)

df_log <- df%>%group_by(Week.number,id)%>%summarise(`logged(minutes)`=sum(`Duration(minutes)`))%>%arrange(Week.number,id)%>%
  mutate(`hours`=`logged(minutes)`/60)


### function for plotting distribution
plot_dist <- function(data, week){
  df <- data%>%filter(Week.number==week)
  hist(df$hours)
}

plot_dist(df_log, 44)


for(i in 1:length(duration)){
  tryCatch(hour(duration[i]),error=function(e){message(i)})
  
}
