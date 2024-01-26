######################################
### Title: 01_deidentify
### Date: 01/24/2024
### Author: Shuai Zhu
### Description: Deidentify cida members' data for analysis
######################################

library(tidyverse)
library(openxlsx)

### read members name
filenames <- list.files("P:\\BERD\\toggl_timetrack\\DataProcessed", pattern="*.xlsx", full.names=F)


membersname <- sample(str_match(filenames,'([a-zA-Z]*\\s*[a-zA-Z]*).xlsx')[,2])
df_deidentify <- tibble(membersname)
df_deidentify['id']=c(1:46)
write.xlsx(df_deidentify,"P:\\BERD\\toggl_timetrack\\DataRaw/memberid.xlsx")
