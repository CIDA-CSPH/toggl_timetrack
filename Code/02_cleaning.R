######################################
### Title: 02_cleaning
### Date: 01/24/2024
### Author: Shuai Zhu
### Description: clean time tracking data
######################################

library(dplyr)
library(openxlsx)

### setting working directory
working_directory <-  'P:\\BERD\\toggl_timetrack'
setwd(working_directory)

### read data
filenames <- list.files("P:\\BERD\\toggl_timetrack\\DataProcessed/members tracking data", pattern="*.xlsx", full.names=F)
filepath <- paste("P:\\BERD\\toggl_timetrack\\DataProcessed/members tracking data", filenames,sep = '/')

read_xlsx <- function(filepath){
  df <- read.xlsx(filepath)
  name <- str_match(filepath,'([a-zA-Z]*\\s*[a-zA-Z]*).xlsx')[,2]
  df[,'User'] <- name
  return(df)
}

data_obj <- filepath%>%lapply(read_xlsx)
library(data.table)
df <- rbindlist(data_obj, use.names = TRUE , fill=T)%>%tibble()
df <- df%>%select(User, Email, Project, Task, Description, Billable, Start.date, Start.time, End.date, End.time, Duration, Tags)
write.xlsx(df,paste('P:\\BERD\\toggl_timetrack\\DataProcessed','members timetrack.xlsx',sep = '/'))



