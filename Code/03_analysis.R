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
#df_project <- df%>%

### tags
df_tags <- read.xlsx('./DataProcessed/members timetrack tags.xlsx')%>%as_tibble()%>%group_by(Week.number,str_to_title(str_to_lower(`Primary.tags`)))%>%
  summarise(`Duration(hours)` = round(sum(`Duration.of.tag(mintues)`)/60,2) )%>%
  arrange(desc(`Duration(hours)`))

colnames(df_tags) <- c('Week','Primary.tags', 'Duration(hours)')
kable1 <- function(data,weeknumber){
  df <- data%>%filter(Week==weeknumber)
  df <- df%>%mutate(rate=`Duration(hours)`/sum(`Duration(hours)`))
  knitr::kable(df%>%slice(1:10))
}
### teaching
df%>%filter(Tags=='Teaching')%>%group_by(id,Week.number)


