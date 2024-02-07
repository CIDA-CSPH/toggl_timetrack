######################################
### Title: 02_cleaning
### Date: 01/24/2024
### Author: Shuai Zhu
### Description: clean time tracking data
######################################

library(dplyr)
library(stringr)
library(openxlsx)
library(tidyr)
### setting working directory
working_directory <-  'P:\\BERD\\toggl_timetrack'
setwd(working_directory)
### billable yes
read.xlsx('./DataProcessed/members tracking data/shannon murphy.xlsx')%>%as_tibble(.)%>%filter(Billable=='Yes')%>%
  write.xlsx('./DataProcessed/members tracking data/shannon murphy.xlsx')

### read data
filenames <- list.files("P:\\BERD\\toggl_timetrack\\DataProcessed/members tracking data", pattern="*.xlsx", full.names=F)
filepath <- paste("P:\\BERD\\toggl_timetrack\\DataProcessed/members tracking data", filenames,sep = '/')

read_xlsx <- function(filepath){
  df <- read.xlsx(filepath)
  df[df==""] <- NA
  df <- df%>%filter(!if_all(everything(), is.na))
  name <- str_match(filepath,'([a-zA-Z]*\\s*[a-zA-Z]*).xlsx')[,2]
  df[,'User'] <- tools::toTitleCase(name)
  return(df)
}

data_obj <- filepath%>%lapply(read_xlsx)
library(data.table)
df <- rbindlist(data_obj, use.names = TRUE , fill=T)%>%tibble()
df <- df%>%select(User, Project, Task, Description, Billable, Start.date, Start.time, End.date, End.time, Duration, Tags)

### read member id table
df_id <- read.xlsx('./DataRaw/membersid.xlsx')%>%tibble()%>%
  mutate(full.name=paste(`First.Name`,`Last.Name`,sep=' '))%>%
  select(id, position_type, full.name)
### merge id and user name
df_merged <- merge(df, df_id, by.x = 'User', by.y = 'full.name', all.x = T)%>%as_tibble(.)
df_merged[df_merged$User=='Sandell',]$id <- 44
### add leadership columns
df_merged$`Leadership` <- str_detect(df_merged$User, 'Theresa|Nichole|Mary Sammel|Katerina')
### drop user name
df_merged <- df_merged%>%select(-c('User'))%>%select(id,everything())

### add FTE column
df_merged <- df_merged%>%mutate(`FTE` = case_when(`position_type`=='student'|`id`==7 ~ 0.5,
                                    `id`==46 ~ 0.6,
                                    `id`==13 ~ 0.65,
                                    .default = 1))
### format date column
df_merged <- df_merged%>%mutate(Start.date = case_when(str_detect(`Start.date`,'/') ~ as.Date(`Start.date`,'%m/%d/%Y'),
                                                       str_detect(`Start.date`,'-') ~ as.Date(`Start.date`,'%Y-%m-%d')),
                                End.date = case_when(str_detect(`End.date`,'/') ~ as.Date(`End.date`,'%m/%d/%Y'),
                                                     str_detect(`End.date`,'-') ~ as.Date(`End.date`,'%Y-%m-%d')))

colnames(df_merged)[12] <- 'Position.type'
df$`Week.number` <- format(df_merged$Start.date,"%V")



### calculate working hours
df <- df_merged

df <- df%>%
  mutate(Start.date = as.Date(Start.date, origin="1899-12-30"),
         End.date = as.Date(End.date, origin="1899-12-30"))%>%
  filter(Start.date>'2023-11-01'&Start.date<'2024-12-15')

res <- times(df$Duration) 
df$`Duration(minutes)` <- (hours(res)*60 + minutes(res))/df$FTE
df<- unique( df )
write.xlsx(df, './DataProcessed/members timetrack.xlsx')

