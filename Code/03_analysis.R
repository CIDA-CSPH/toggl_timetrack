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
df <- read.xlsx('./DataProcessed/members timetrack classify.xlsx')%>%as_tibble(.)
df <- df%>%select(-c(Start.date,Start.time,End.date,End.time))

### hours logged
df_log <- df%>%group_by(Week.number,id)%>%summarise(`logged(minutes)`=sum(`Duration(minutes)`))%>%arrange(Week.number,id)%>%
  mutate(`hours`=`logged(minutes)`/60)

### hours on email or service

df_email <- df%>%filter(Email==T)%>%filter(Tags=='Email'|Tags=='email'|Tags=='Emails')%>%group_by(Week.number,id)%>%
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

### project - admin - email - service
df_project <- df%>%filter(Email==TRUE|Admin==TRUE)%>%filter(Tags!='Email'&Tags!='email'&Tags!='Emails')%>%filter(Tags!='Admin'&Tags!='admin')%>%
  group_by(Project)%>%summarise(`Duration(minutes)` = sum(`Duration(minutes)`))%>%mutate(hours=`Duration(minutes)`/60)


