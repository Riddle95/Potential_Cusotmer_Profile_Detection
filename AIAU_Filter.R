library(dplyr)
library(stringr) #package to deal with string
library(tm) #text mining package
#Google_raw: final_google_combined_raw ; 957 observations
#create a df_test subset for manipulating the dataset while keeping the original version 
df_test <- Google_raw

#Part.1. Pre-processing
#location: transform the current version into (1) city, (2) state, (3) country
#fullName: matching & testing

#Option 1. Dummy Method
#Generate "City" column
#City: Sydney
Sydney <- ifelse(grepl("Syd|Rand|Man|Spit|King|Manl|Longue|Baran|Chatsw|Lindfield|Castle|Umina|Newport|Cove|Dundas|Leichhardt",df_test$location),"Sydney",NA)
#City: Millers Point
Millers_Point <- ifelse(grepl("Mill",df_test$location),"Millers Point",NA)
#City: Cherrybrook
Cherrybrook <- ifelse(grepl("Cher",df_test$location),"Cherrybrooky",NA)
#City: Rooty Hill
Rooty_Hill <- ifelse(grepl("Roo",df_test$location),"Rooty Hill",NA)
#City: Yagoona
Yagoona <- ifelse(grepl("Yagoo",df_test$location),"Yagoona",NA)
#City: Hornsby
Hornsby <- ifelse(grepl("Hor",df_test$location),"Hornsby",NA)
#City: Baulkham Hills
Baulkham_Hills <- ifelse(grepl("Baul",df_test$location),"Baulkham Hills",NA)
#City: Drummoyne
Drummoyne <- ifelse(grepl("Drum",df_test$location),"Drummoyne",NA)
#City: Parkwood
Parkwood <- ifelse(grepl("Parkw",df_test$location),"Parkwood",NA)
#City: Seaforth
Seaforth <- ifelse(grepl("Seaf",df_test$location),"Seaforth",NA)
#City: Carlingford
Carlingford <- ifelse(grepl("Carlin",df_test$location),"Carlingford",NA)
#City: Blaxland
Blaxland <- ifelse(grepl("Blax",df_test$location),"Blaxland",NA)
#City: Canberra
Canberra <- ifelse(grepl("Canber",df_test$location),"Canberra",NA)
#City: Melbourne
Melbourne <- ifelse(grepl("Melb|Tho|Abbot|Northcote|Koondrook|Caulfield",df_test$location),"Melbourne",NA)
#City: Yarraville
Yarraville <- ifelse(grepl("Yarr",df_test$location),"Yarraville",NA)
#City: Brisbane
Brisbane <- ifelse(grepl("Bris",df_test$location),"Brisbane",NA)
#City: Gold Coast
Gold_Coast <- ifelse(grepl("Gold",df_test$location),"Gold Coast",NA)
#City: Adelaide
Adelaide <- ifelse(grepl("Adel",df_test$location),"Adelaide",NA)
#City: Perth
Perth <- ifelse(grepl("Pert",df_test$location),"Perth",NA)
#City: Hobart
Hobart <- ifelse(grepl("Hoba",df_test$location),"Hobart",NA)
#City: Darwin
Darwin <- ifelse(grepl("Darw",df_test$location),"Darwin",NA)
#City: Thomastown
Parramatta <- ifelse(grepl("Parramat",df_test$location),"Parramatta,",NA)
#City: Epping
Epping <- ifelse(grepl("Epp",df_test$location),"Epping,",NA)

#Combine all datasets
checking <- cbind(df_test,Sydney,Millers_Point,Cherrybrook,Rooty_Hill,Yagoona,Hornsby,Baulkham_Hills,Drummoyne,Parkwood,Seaforth,Carlingford,Blaxland,Canberra,Melbourne,Yarraville,Brisbane,Gold_Coast,Adelaide,Perth,Hobart,Darwin,Parramatta,Epping)
#Create the standalone States column
checking <- checking %>% 
  mutate(City = coalesce(Sydney,Millers_Point,Cherrybrook,Rooty_Hill,Yagoona,Hornsby,Baulkham_Hills,Drummoyne,Parkwood,Seaforth,Carlingford,Blaxland,Canberra,Melbourne,Yarraville,Brisbane,Gold_Coast,Adelaide,Perth,Hobart,Darwin,Parramatta,Epping))
#remove unnecessary columns
checking <- checking %>% 
  subset(select = -c(Sydney,Millers_Point,Cherrybrook,Rooty_Hill,Yagoona,Hornsby,Baulkham_Hills,Drummoyne,Parkwood,Seaforth,Carlingford,Blaxland,Canberra,Melbourne,Yarraville,Brisbane,Gold_Coast,Adelaide,Perth,Hobart,Darwin,Parramatta,Epping))

#Generate "State" variable
#State: New South Wales
New_South_Wales <- ifelse(grepl("Syd|Mil|Cher|Root|Yago|Horn|Baul|Drum|Park|Seafor|Blax|New",df_test$location),"New South Wales",NA)
#State: Victoria
Victoria <- ifelse(grepl("Mel|Yar|Ben|Vic",df_test$location),"Victoria",NA)
#State: Australian Capital Territory
Australian_Capital_Territory <- ifelse(grepl("Can",df_test$location),"Australian Capital Territory",NA)
#State: Queensland
Queensland <- ifelse(grepl("Bri|Gol",df_test$location),"Queensland",NA)
#State: South Australia
South_Australia <- ifelse(grepl("Adelai",df_test$location),"South Australia",NA)
#State: Western Australia
Western_Australia <- ifelse(grepl("Per",df_test$location),"Western Australia",NA)
#State: Tasmania
Tasmania <- ifelse(grepl("Hob",df_test$location),"Tasmania",NA)
#State: Northern Territory
Northen_Territory <- ifelse(grepl("Dar",df_test$location),"Northern Territory",NA)
#Combine all datasets
checking <- cbind(checking,New_South_Wales,Victoria,Australian_Capital_Territory,Queensland,South_Australia,Western_Australia,Tasmania,Northen_Territory)
#Create the standalone States column
checking <- checking %>% 
  mutate(States = coalesce(New_South_Wales,Victoria,Australian_Capital_Territory,Queensland,South_Australia,Western_Australia,Tasmania,Northen_Territory))
#remove unnecessary columns
checking <- checking %>% 
  subset(select = -c(New_South_Wales,Victoria,Australian_Capital_Territory,Queensland,South_Australia,Western_Australia,Tasmania,Northen_Territory))

#Generate "Country" variable
#Country: Australia
Country_1 <- ifelse(grepl("New|Vic|Au|Que|Wes|Tas|Nor|Aus",checking$States),"Australia",NA)
Country_2 <- ifelse(grepl("Aus",checking$location),"Australia",NA)
checking <- cbind(checking,Country_1,Country_2)
checking <- checking %>% 
  mutate(Country = coalesce(Country_1,Country_2))

#firstName,lastName,fullName: transformation
#firstName
#transform all into lower case, dealing with case sensitive
firstName <- tolower(checking$firstName)
#remove all non-letters characters
firstName <- gsub(pattern = "[^[:alpha:]]+",replacement = " ",firstName)
#remove contents inside the brackets
checking$firstName <- str_replace(firstName, " \\s*\\([^\\)]+\\)", "")
#remove unnecessary white space
checking$firstName <- stripWhitespace(firstName)

#lastName
#transform all into lower case, dealing with case sensitive
lastName <- tolower(checking$lastName)
#remove all non-letters characters
lastName <- gsub(pattern = "[^[:alpha:]]+",replacement = " ",lastName)
#remove contents inside the brackets
checking$lastName <- str_replace(lastName, " \\s*\\([^\\)]+\\)", "")
#remove unnecessary white space
checking$lastName <- stripWhitespace(lastName)
#new fullName
checking$fullName <- paste(checking$firstName, checking$lastName, sep = " ")

#Part.2. Identify people with certain conditions

#Filter people with Startup in jobTitle
Google_jobTitle_start <- checking %>%
  filter(str_detect(str_to_lower(jobTitle),"star"))
#Filter people with Startup in jobTitle2
Google_jobTitle_start2 <- checking %>%
  filter(str_detect(str_to_lower(jobTitle2),"star"))
#Filter people with Startup in description
Google_description_start <- checking %>%
  filter(str_detect(str_to_lower(description),"star"))
#merge the dataset
filter_startup <- rbind(Google_jobTitle_start,Google_jobTitle_start2,Google_description_start)
#remove the duplication 
filter_startup <- filter_startup[!duplicated(filter_startup$fullName),]

#Filter people with Venture
Google_jobTitle_venture <- checking %>%
  filter(str_detect(str_to_lower(jobTitle),"venture"))
#Filter people with Venture in jobTitle2
Google_jobTitle2_venture <- checking %>%
  filter(str_detect(str_to_lower(jobTitle2),"venture"))
#Filter people with Venture in description
Google_description_venture <- checking %>%
  filter(str_detect(str_to_lower(description),"venture"))
#Combine the venture list
filter_venture <- rbind(Google_jobTitle_venture,Google_jobTitle2_venture,Google_description_venture)
filter_venture<- filter_venture[!duplicated(filter_venture$fullName),]

#Filter people with investor in jobTitle
Google_jobTitle_investor <- checking %>%
  filter(str_detect(str_to_lower(jobTitle),"investor"))
#Filter people with investor in jobTitle2
Google_jobTitle2_investor <- checking %>%
  filter(str_detect(str_to_lower(jobTitle2),"investor"))
#Filter people with investor in description
Google_description_investor <- checking %>%
  filter(str_detect(str_to_lower(description),"investor"))
#Combine the investor list
filter_investor <- rbind(Google_jobTitle_investor,Google_jobTitle2_investor,Google_description_investor)
filter_investor<- filter_investor[!duplicated(filter_investor$fullName),]

#Filter people with entrepreneur in jobTitle
Google_jobTitle_entrepreneur <- checking %>%
  filter(str_detect(str_to_lower(jobTitle),"entrepreneur"))
#Filter people with entrepreneur in jobTitle2
Google_jobTitle2_entrepreneur <- checking %>%
  filter(str_detect(str_to_lower(jobTitle2),"entrepreneur"))
#Filter people with entrepreneur in description
Google_description_entrepreneur <- checking %>%
  filter(str_detect(str_to_lower(description),"entrepreneur"))
#Combine the entrepreneur list
filter_entrepreneur <- rbind(Google_jobTitle_entrepreneur,Google_jobTitle2_entrepreneur,Google_description_entrepreneur)
filter_entrepreneur <- filter_entrepreneur[!duplicated(filter_entrepreneur$fullName),]

#Filter people with founder in jobTitle
Google_jobTitle_founder <- checking %>%
  filter(str_detect(str_to_lower(jobTitle),"founder"))
#Filter people with founder in jobTitle2
Google_jobTitle2_founder <- checking %>%
  filter(str_detect(str_to_lower(jobTitle2),"founder"))
#Filter people with founder in description
Google_description_founder <- checking %>%
  filter(str_detect(str_to_lower(description),"founder"))
#Combine the founder list
filter_founder <- rbind(Google_jobTitle_founder,Google_jobTitle2_founder,Google_description_founder)
filter_founder <- filter_founder[!duplicated(filter_founder$fullName),]

#Filter people with business development in jobTitle
Google_jobTitle_bd <- checking %>%
  filter(str_detect(str_to_lower(jobTitle),"business"))
#Filter people with entrepreneur in jobTitle2
Google_jobTitle2_bd <- checking %>%
  filter(str_detect(str_to_lower(jobTitle2),"business"))
#Filter people with entrepreneur in description
Google_description_bd <- checking %>%
  filter(str_detect(str_to_lower(description),"business"))
#Combine the entrepreneur list
filter_bd <- rbind(Google_jobTitle_bd,Google_jobTitle2_bd,Google_description_bd)
filter_bd <- filter_bd[!duplicated(filter_bd$fullName),]

##Combine the final list
filter_final <- rbind(filter_startup,filter_venture,filter_investor,filter_entrepreneur,filter_founder,filter_bd)
filter_final <- filter_final[!duplicated(filter_final$fullName),]

#Scanning people working at Google with the "present" in the jobDateRange & jobDateRange2
filter_final_1 <- filter_final %>%
  filter(str_detect(str_to_lower(jobDateRange),"prese")|str_detect(str_to_lower(jobDateRange2),"prese"))
df_filter <- filter_final_1 %>%
  subset(company=="Google"|company2=="Google")

#removing the data with c('intern','Recruit','Stud','Desig' key word ; 
#1: people with intern or data title
#0: people without intern or data title
abc <- ifelse(grepl("Talent|Recruit|Stud|Engine|Desig|HR|Human|People|Staffi|Marketing|Intern|Financ|research|UX",df_filter$jobTitle),1,0)
testing <- cbind(abc,df_filter)
#create the new subset that removes the people with intern or data title 
df_filter <- subset(testing,abc == 0)
select(df_filter,-abc)
df_filter = subset(df_filter, select = -c(abc)) %>%
  data.frame()
View(df_filter)

#final version: 52 observations
#export csv file
write.csv(df_filter,"Desktop\\Google.filter.final.csv", row.names = FALSE)
