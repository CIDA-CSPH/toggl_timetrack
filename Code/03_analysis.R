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
df <- df%>%select(-c(Start.date,Start.time,End.date,End.time))

### hours logged
df_log <- df%>%group_by(Week.number,id)%>%summarise(`logged(minutes)`=sum(`Duration(minutes)`))%>%arrange(Week.number,id)%>%
  mutate(`hours`=`logged(minutes)`/60)

### hours on email or service
df <- df%>%mutate(Email=case_when(str_detect(Project,regex("email|emails", ignore_case = TRUE))~T,
                                  str_detect(Tags,regex("email|emails", ignore_case = TRUE))~T,
                                  str_detect(Description, regex("email|emails", ignore_case = TRUE))~T,
                                  .default = F),
                  Service=case_when(str_detect(Project,regex("service|services", ignore_case = TRUE))~T,
                                    str_detect(Tags,regex("service|services", ignore_case = TRUE))~T,
                                    str_detect(Description, regex("service|services", ignore_case = TRUE))~T,
                                    .default = F))

df_email <- df%>%filter(Email==T)%>%group_by(Week.number,id)%>%
  summarise(`email(minutes)`=sum(`Duration(minutes)`))%>%arrange(Week.number,id)%>%
  mutate(`hours`=`email(minutes)`/60)

df_service <- df%>%filter(Service==T)%>%group_by(Week.number,id)%>%
  summarise(`service(minutes)`=sum(`Duration(minutes)`))%>%arrange(Week.number,id)%>%
  mutate(`hours`=`service(minutes)`/60)


### function for plotting distribution
plot_dist <- function(data, week){
  df <- data%>%filter(Week.number==week)
  hist(df$hours,breaks=10,xlab='hours',main = paste('Distribution of week',week,sep = ' '))
}




