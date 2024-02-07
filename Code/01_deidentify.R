######################################
### Title: 01_deidentify
### Date: 01/24/2024
### Author: Shuai Zhu
### Description: Deidentify cida members' data for analysis
######################################

library(tidyverse)
library(openxlsx)

### setting working directory
working_directory <-  'P:\\BERD\\toggl_timetrack'
setwd(working_directory)

### read members name
filenames <- list.files("./DataProcessed/members tracking data", pattern="*.xlsx", full.names=F)


membersname <- sample(str_match(filenames,'([a-zA-Z]*\\s*[a-zA-Z]*).xlsx')[,2])

df <- str_split(membersname,' ',simplify = T)%>%as_tibble(.)
colnames(df) <- c('First.Name', 'Last.Name')
df$First.Name <- tools::toTitleCase(df$First.Name)
df$Last.Name <- tools::toTitleCase(df$Last.Name)
df$id <- c(1:47)


### add position type columns

df_personel <- read.xlsx('./DataRaw/PERSONEL ROSTER for CIDA and B&I-NEC.xlsx',startRow =2)%>%as_tibble(.)%>%
  select(c('Last.Name','First.Name',"Job.Title" , "Business.Title"))

df_merged <- merge(df,df_personel, by = c('First.Name', 'Last.Name'), all.x = T)%>%
  distinct(`First.Name`,`Last.Name`,.keep_all = T)

position_type <- function(position){
  if (is.na(position)){
    return(NA)
  }
  return(
  case_when(str_detect(position, 'Research Assistant')~'Student',
            str_detect(position, 'Instructor|Sr Professional Research Asst')~'Masters',
            str_detect(position, 'Research Associate')~'PhD',
            str_detect(position, 'Professor|Director-Faculty')~'ProfPhD',
            str_detect(position, 'Finance/Acctg Prgm Mgr')~'Luan',
            str_detect(position, 'Business Services Professional')~'Becca',
            str_detect(position, 'Business Services Asst Dir')~'Theresa',
            .default =NA)
            
  )

}

df_merged$position_type <- sapply(df_merged$Job.Title, position_type)
df_merged[is.na(df_merged$position_type),]
#write.xlsx(df_merged, './DataRaw/membersid.xlsx')
read.xlsx('./DataRaw/membersid.xlsx')%>%View()

