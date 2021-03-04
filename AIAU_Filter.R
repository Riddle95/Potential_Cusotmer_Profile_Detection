library(dplyr)
library(stringr)
View(df)
unique(df$jobTitle)
df_new <- df%>%
  select(description,jobTitle,jobTitle2,fullName)
df_new

#replace investor & entrepreneur 
#find founders


#Filter people with Startup in jobTitle
df_new_jobTitle_start <- df_new %>%
  filter(str_detect(str_to_lower(jobTitle),"star"))
#Filter people with Startup in jobTitle2
df_new_jobTitle_start2 <- df_new %>%
  filter(str_detect(str_to_lower(jobTitle2),"star"))
#Filter people with Startup in description
df_new_description_start <- df_new %>%
  filter(str_detect(str_to_lower(description),"star"))
#merge the dataset
filter_startup <- rbind(df_new_jobTitle_start,df_new_jobTitle_start2,df_new_description_start)
#remove the duplication 
filter_startup <- filter_startup[!duplicated(filter_startup$fullName),]

#Filter people with Venture
df_new_jobTitle_venture <- df_new %>%
  filter(str_detect(str_to_lower(jobTitle),"venture"))
#Filter people with Venture in jobTitle2
df_new_jobTitle2_venture <- df_new %>%
  filter(str_detect(str_to_lower(jobTitle2),"venture"))
#Filter people with Venture in description
df_new_description_venture <- df_new %>%
  filter(str_detect(str_to_lower(description),"venture"))
#Combine the venture list
filter_venture <- rbind(df_new_jobTitle_venture,df_new_jobTitle_venture,df_new_description_venture)
filter_venture<- filter_venture[!duplicated(filter_venture$fullName),]
View(filter_venture)

#Filter people with investor in jobTitle
df_new_jobTitle_investor <- df_new %>%
  filter(str_detect(str_to_lower(jobTitle),"investor"))
#Filter people with investor in jobTitle2
df_new_jobTitle2_investor <- df_new %>%
  filter(str_detect(str_to_lower(jobTitle2),"investor"))
#Filter people with investor in description
df_new_description_investor <- df_new %>%
  filter(str_detect(str_to_lower(description),"investor"))
#Combine the investor list
filter_investor <- rbind(df_new_jobTitle_investor,df_new_jobTitle2_investor,df_new_description_investor)
filter_investor<- filter_investor[!duplicated(filter_investor$fullName),]
View(filter_investor)

#Filter people with entrepreneur in jobTitle
df_new_jobTitle_entrepreneur <- df_new %>%
  filter(str_detect(str_to_lower(jobTitle),"entrepreneur"))
#Filter people with entrepreneur in jobTitle2
df_new_jobTitle2_entrepreneur <- df_new %>%
  filter(str_detect(str_to_lower(jobTitle2),"entrepreneur"))
#Filter people with entrepreneur in description
df_new_description_entrepreneur <- df_new %>%
  filter(str_detect(str_to_lower(description),"entrepreneur"))
#Combine the entrepreneur list
filter_entrepreneur <- rbind(df_new_jobTitle_entrepreneur,df_new_jobTitle2_entrepreneur,df_new_description_entrepreneur)
filter_entrepreneur <- filter_entrepreneur[!duplicated(filter_entrepreneur$fullName),]
View(filter_entrepreneur)

##Combine the final list
filter_final <- rbind(filter_startup,filter_venture,filter_investor,filter_entrepreneur)
filter_final <- filter_final[!duplicated(filter_final$fullName),]
View(filter_final)

#export csv file
write.csv(filter_final,"Desktop\\filter.csv", row.names = FALSE)
