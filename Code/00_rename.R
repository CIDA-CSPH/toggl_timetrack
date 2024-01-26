######################################
### Title: 00_rename
### Date: 01/22/2024
### Author: Shuai Zhu
### Description: rename toggle time tracing file of CIDA members by their name
######################################

library(tidyverse)
library(openxlsx)

### setting working directory
working_directory <-  'P:\\BERD\\toggl_timetrack'
setwd(working_directory)

### read data
filenames <- list.files("./DataRaw/Toggl_time_2023", pattern="*.csv", full.names=F)

Sierra_filepath<- paste('./DataRaw/Toggl_time_2023/TogglTrack_Chart_Detailed_table',c(".csv"," 2.csv"," 3.csv"," 4.csv"," 5.csv"," 6.csv"),sep = '')
df_Sierra <- Sierra_filepath%>%lapply(read_csv) %>% 
  bind_rows
write.xlsx(df_Sierra,'./DataProcessed/Sierra.xlsx')
nonuser_filepath <- c('McNair_Toggl_Track_summary_report_2023-11-01_2023-12-14.csv','Selbert_Nov2023_timetracking.csv' 
                      ,'Toggl Time Entries C Hochheimer.csv')
write.xlsx(read.csv("./DataRaw/Toggl_time_2023/McNair_Toggl_Track_summary_report_2023-11-01_2023-12-14.csv"),'./DataProcessed/McNair.xlsx')
write.xlsx(read.csv("./DataRaw/Toggl_time_2023/Selbert_Nov2023_timetracking.csv"),'./DataProcessed/Selbert.xlsx')
write.xlsx(read.csv("./DataRaw/Toggl_time_2023/Toggl Time Entries C Hochheimer.csv"),'./DataProcessed/Hochheimer.xlsx')

filenames <- filenames[!filenames %in% str_replace_all(Sierra_filepath, './DataRaw/Toggl_time_2023/', '')]
filenames <- filenames[!filenames %in% nonuser_filepath ]


for(i in 1:length(filenames)){
  df <- read.csv(paste('./DataRaw/Toggl_time_2023', filenames[i],sep='/'))%>%tibble()
  if ('User' %in% colnames(df)){
    name <- df$User[1]
  }else{
    print(filenames[i])
  }
  
  if (str_detect(name,'@')){
    name = paste(str_match(name,'([A-Za-z]+).([A-Za-z]+)@')[1,2],
                 str_match(name,'([A-Za-z]+).([A-Za-z]+)@')[1,3]
                 )
  }
  
  to_file <- paste('./DataProcessed/',name,'.xlsx',sep='')
  if(file.exists(to_file)){
    write.xlsx(df,  paste('./DataProcessed/',name,'2.xlsx',sep=''))
  }else{
    write.xlsx(df,to_file)
  }
}

### staff data

staff_filenames <- list.files("./DataRaw/Toggl_time_2023/staff", full.names=T)

for (i in 1:length(staff_filenames)){
  if (str_detect(staff_filenames[i],'.csv')){
    df <- read.csv(staff_filenames[i])%>%tibble()
  }else if (str_detect(staff_filenames[i],'.xlsx')){
    df <- read.xlsx(staff_filenames[i])%>%tibble()
  }
  name <- df$User[1]
  write.xlsx(df,paste('./DataProcessed/',name,'.xlsx',sep=''))
}
