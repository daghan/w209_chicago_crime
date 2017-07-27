library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
library(scales)
library(lubridate)

setwd("/Users/daghanaltas/Hacking/Berkeley/W209/Project/data/")

#crimes <- read.csv("./Crimes_-_2001_to_present.csv",fileEncoding="latin1")
crimes <- read.csv("./FinalCrime.csv",fileEncoding="latin1")
# save as a dataframe
#save(crimes, file = "./crimes_with_zip.Rda")
load("./crimes_with_zip.Rda")

# convert dates 
crimes$Date <- mdy_hms(crimes$Date)
#crimes$Date <- mdy_hm(crimes$Date)

# filter for 2016 only
#crimes <- subset(crimes, year(crimes$Date) > 2011)
crimes <- subset(crimes, year(crimes$Date) == 2016)

#fix types
crimes$Primary.Type <- as.factor(crimes$Primary.Type)
#crimes$Description <- as.factor(crimes$Description)
crimes$Location.Description <- as.factor(crimes$Location.Description)
crimes$Zip <- as.factor(crimes$Zip)

# note for self: clean-up NON - CRIMINAL, OTHER NARCOTIC VIOLATION


# tc stands for temporary crime date
tc <- data.frame( #ID = crimes$ID,
                  #Case.Number = crimes$Case.Number,
                  Date = as.Date(crimes$Date),
                  Primary.Type = crimes$Primary.Type,
                  #Location.Description = as.factor(crimes$Location.Description),
                  Zip = crimes$Zip
                )
tc$Occurance <- 1
sum.crimes.by.date.zip <- as.data.frame(tc  %>% group_by(Date,Zip,Primary.Type) %>% summarise(Sum.Occurance = sum(Occurance))) %>% spread(Primary.Type, Sum.Occurance, fill = 0)
sum.crimes.by.date.zip$Total.Crimes <- apply(sum.crimes.by.date.zip[,3:dim(sum.crimes.by.date.zip)[2]],1,sum)
#head(sum.crimes.by.date.zip,100)



alley.lights <- read.csv("./311_Service_Requests_-_Alley_Lights_Out.csv",fileEncoding="latin1")

# Only keep the completed services
alley.lights <- alley.lights[!grepl("- Dup" , alley.lights$Status),]
alley.lights <- alley.lights[grepl("Completed" , alley.lights$Status),]

# fix the dates
alley.lights$Creation.Date <- mdy(alley.lights$Creation.Date)
alley.lights$Completion.Date <- mdy(alley.lights$Completion.Date)
# for each completed service, how many dates were a given light was out?
alley.lights$Broken.Days <- alley.lights$Completion.Date - alley.lights$Creation.Date

#limit the dates to 2016 for now
alley.lights <- subset(alley.lights, year(alley.lights$Creation.Date) == 2016)

# temporary dataframe for manipulation
t <- data.frame( Creation.Date = alley.lights$Creation.Date,
                 Completion.Date =  alley.lights$Completion.Date,
                 Broken.Days = as.numeric(alley.lights$Broken.Days)+1, 
                 Service.Request.Number = alley.lights$Service.Request.Number,
                 ZIP.Code = as.factor(alley.lights$ZIP.Code),
                 Community.Area = as.factor(alley.lights$Community.Area))

# mega hacks, not even sure where to begin describing
t$temp <- sapply(t$Broken.Days, function(x) seq(1,x)-1)
t$temp <- sapply(t$temp, unlist)
ttt <- separate_rows(t, temp, sep = ",", convert = TRUE)
ttt$temp <- gsub("^c\\(","",ttt$temp)
ttt$temp <- gsub("\\)","",ttt$temp)
ttt$temp <- as.numeric(ttt$temp)
ttt$Broken.Date <- ttt$Creation.Date + ttt$temp


#for each day in each zip code, aggregate the count of total broken alley lights                  
broken.alley.lights.by.ZIP<- as.data.frame(ttt %>% count(Broken.Date, ZIP.Code))
colnames(broken.alley.lights.by.ZIP) <- c("Date", "Zip","Broken.Alley.Lights")

# merge it with the per_day, per_zip crime data
merged_data = merge(x = sum.crimes.by.date.zip, y = broken.alley.lights.by.ZIP, 
                    by = c("Date","Zip"), all.x = T, all.y = T)
merged_data[is.na(merged_data)] <- 0

# write it out to a CVS file
write.csv(merged_data, file="./broken_alley_lights_merged_with_crime_2016.csv")

#sort(cor(merged_data[,3:dim(merged_data)[2]])[,32],decreasing = T)



# adding street lights too
street.lights <- crimes <- read.csv("./311_Service_Requests_-_Street_Lights_-_One_Out.csv",fileEncoding="latin1")
save(street.lights, file = "./street_lights.Rda")

street.lights <- street.lights[!grepl("- Dup" , street.lights$Status),]
street.lights <- street.lights[grepl("Completed" , street.lights$Status),]

# fix the dates
street.lights$Creation.Date <- mdy(street.lights$Creation.Date)
street.lights$Completion.Date <- mdy(street.lights$Completion.Date)

# for each completed service, how many dates were a given light was out?
street.lights$Broken.Days <- street.lights$Completion.Date - street.lights$Creation.Date

#limit the dates to 2016 for now
street.lights <- subset(street.lights, year(street.lights$Creation.Date) == 2016)

# temporary dataframe for manipulation
t <- data.frame( Creation.Date = street.lights$Creation.Date,
                 Completion.Date =  street.lights$Completion.Date,
                 Broken.Days = as.numeric(street.lights$Broken.Days)+1, 
                 Service.Request.Number = street.lights$Service.Request.Number,
                 ZIP.Code = as.factor(street.lights$ZIP.Code),
                 Community.Area = as.factor(street.lights$Community.Area))

# mega hacks, not even sure where to begin describing
t$temp <- sapply(t$Broken.Days, function(x) seq(1,x)-1)
t$temp <- sapply(t$temp, unlist)
ttt <- separate_rows(t, temp, sep = ",", convert = TRUE)
ttt$temp <- gsub("^c\\(","",ttt$temp)
ttt$temp <- gsub("\\)","",ttt$temp)
ttt$temp <- as.numeric(ttt$temp)
ttt$Broken.Date <- ttt$Creation.Date + ttt$temp


#for each day in each zip code, aggregate the count of total broken alley lights                  
broken.street.lights.by.ZIP<- as.data.frame(ttt %>% count(Broken.Date, ZIP.Code))
colnames(broken.street.lights.by.ZIP) <- c("Date", "Zip","Broken.Street.Lights")


merged_data2 = merge(x = merged_data, y = broken.street.lights.by.ZIP, 
                     by = c("Date","Zip"), all.x = T, all.y = T)
merged_data2[is.na(merged_data2)] <- 0

write.csv(merged_data2, file="./broken_alley_and_street_lights_merged_with_crime_2016.csv")


