######################################
### Title: 02_cleaning
### Date: 01/24/2024
### Author: Shuai Zhu
### Description: clean time tracking data
######################################
library(tidyverse)
library(openxlsx)
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
# write.xlsx(df, './DataProcessed/members timetrack.xlsx')

## add a service, admin and email variable.
df <- read.xlsx("./DataProcessed/members timetrack.xlsx")%>%as_tibble(.)
df_project_email <- read.xlsx("./DataProcessed/project_emials_highlighted1.xlsx")
df_project_admin <- read.xlsx("./DataProcessed/project_tags_highlighted1.xlsx")
df_project_email <- df_project_email%>%select(Project)%>%mutate(Email = T)
df_project_admin <- df_project_admin%>%select(Project)%>%mutate(Admin = T)
df <- merge(df,df_project_email,by = 'Project',all.x=T)%>%as.tibble(.)
df <- merge(df,df_project_admin,by = 'Project',all.x=T)%>%as.tibble(.)
df <- df%>%mutate(Email = case_when(Email==T&Tags=='Email'~T,
                              Email==T&Tags=='Emails'~T,
                              Email==T&Tags=='email'~T,
                              .default = F))
df <- df%>%mutate(Admin = case_when(Email==T&Tags=='Admin'~T,
                              .default = F))
df <- df%>%replace_na(list("Email"=F,'Admin'=F))
df <- df%>%mutate(Service=case_when(str_detect(Project,regex("service", ignore_case = TRUE))~T,
                    str_detect(Tags,regex("service", ignore_case = TRUE))~T,
                    str_detect(Description, regex("service", ignore_case = TRUE))~T,
                    .default = F))



df$Tags <- df$Tags%>%str_replace('–','-')


df$Serviceby <-  str_match(df%>%select(Tags)%>%unlist()%>%as.vector(.), regex('service\\s?-?\\s?(\\w+)',ignore_case = T))[,2]

df$Serviceby <- df$Serviceby%>%str_replace('DEPT','BIOS')


df%>%filter(Service==T)%>%view()
write.xlsx(df,'./DataProcessed/members timetrack classify.xlsx')


## convert wide to long

df <- read.xlsx("./DataProcessed/members timetrack classify.xlsx")%>%as_tibble(.)
# df%>%filter(is.na(Tags))%>%group_by(Project)%>%summarise(Count = n(), `Duration(minutes)`=sum(`Duration(minutes)`))%>%
#  write.xlsx('./DataProcessed/Project with empty tags.xlsx')
# df%>%filter(is.na(Tags)&is.na(Project))%>%write.xlsx('./DataProcessed/Rows without project and tags.xlsx')

### detect primary and secondary tag

df <- df%>%select(-c(Task,Billable, Start.date, Start.time, End.date, End.time,Email, Admin, Service ))


df <- df%>%mutate(`Secondary tags` = case_when(str_detect(df$Tags,regex('admin', ignore_case=T))~'Admin',
                                         .default = NA))

df$`Primary tags` <- df$Tags%>%str_replace(regex('admin(\\s\\((.+)?\\))?', ignore =T),'')%>%str_replace('^, ','')


df$`Primary tags`<- df$`Primary tags`%>%str_replace(regex('meeting(\\s\\((.+)?\\))', ignore =T),'Long meeting')


primary_tags <- str_split(df$`Primary tags`,', ', simplify = T)
primary_tags <- primary_tags%>%as_tibble()
primary_tags[primary_tags==""]<-NA
colnames(primary_tags) <- paste("primary",1:4,sep='')
colnames(df)[2] <- 'Userid'
df$`Recordsid` <- 1:nrow(df)
df%>%select(Recordsid,everything())

df_separate <- cbind(df,primary_tags)%>%as_tibble()%>%select(-c(`Primary tags`))
df_separate <- df_separate%>%mutate(primary1=case_when(is.na(primary1)~`Secondary tags`,
                                        .default = primary1))

df_separate <- df_separate%>%pivot_longer(cols = starts_with("primary"),
                           names_to = NULL,
                           values_to ="Primary tags",
                           values_drop_na = T
  
)

df_separate <- df_separate %>%group_by(Recordsid)%>%
  mutate(`Duration on one tag(mintues)`=case_when(`Primary tags`==regex('Meeting',ignore_case=T)~min(60,`Duration(minutes)`),
                                                  `Primary tags`=='Long Meeting'~min(60,`Duration(minutes)`),
                                                  `Primary tags`=='Email'|`Primary tags`=='email'~`Duration(minutes)`/3 ))


