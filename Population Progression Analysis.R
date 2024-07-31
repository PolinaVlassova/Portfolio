# We used Global Education data to analyze population progression
# below are cleaning codes for each of the 5 selected datasets 

# 1st dataset "Official entrance age to pre-primary education" 
#install dplyr package
install.packages("tidyverse")
install.packages("reshape2")
library(dplyr)
library(tidyverse)
library(reshape2)

# set a directory
setwd("/Users/polinavlasova/Downloads")
getwd()
df1 <- read.csv("official-entrance-age-to-pre-primary-education.csv",sep=",")

# let's identify countries with data less then 45 years
gg <- df1 %>%   group_by(Entity) %>%   summarise(diff_year = max(Year) - min(Year)) 

# erase irrelevant countries
filt <- gg[gg$diff_year >45, ]  
clean_df1 <- df1[which(df1$Entity %in% filt$Entity ),] 

# 2nd dataset "Primary school completion rate" 
df2 <- read.csv("primary-completion-rate.csv") 

# let's identify countries with data less then 45 years
gg <- df2 %>%   group_by(Entity) %>%   summarise(diff_year = max(Year) - min(Year)) 


# erase irrelevant countries
filt <- gg[gg$diff_year >45, ]  
clean_df2 <- df2[which(df2$Entity %in% filt$Entity ),] 

# 3rd "Duration of pre-primary education" 
df3 <- read.csv2("duration-of-pre-primary-education.csv", sep=",") 
df4 <- read.csv2("official-entrance-age-to-pre-primary-education.csv", sep=",") 

# let's identify countries with data less then 45 years
gg <- df3 %>%   group_by(Entity) %>%   summarise(diff_year = max(Year) - min(Year)) 

# erase irrelevant countries
filt <- gg[gg$diff_year >45, ]  
clean_df3 <- df3[which(df3$Entity %in% filt$Entity ),]  

# 5th dataset "World population level of education"
df4 = read.csv("world-population-level-education.csv")

# erase irrelevant countries
clean_df4 <- subset(df4, Year > 1965 & Year < 2025)

# let's identify max and min available year per each country

# 1st dataset "Official entrance age to pre-primary education"
entrance_age <- clean_df1 %>%
  group_by(Entity) %>%
  summarise(MaxYear = max(Year), Entrance_age_to_pre_primary_new = max(Official.entrance.age.to.pre.primary.education..years.),
            MinYear = min(Year), Entrance_age_to_pre_primary_old = min(Official.entrance.age.to.pre.primary.education..years.))

# 2nd dataset "Primary school completion rate" 
completion_rate <- clean_df2 %>%
  group_by(Entity) %>%
  summarise(MaxYear = max(Year), Primary_completion_rate_new = max(Primary.completion.rate..total....of.relevant.age.group.),
            MinYear = min(Year), Primary_completion_rate_old = min(Primary.completion.rate..total....of.relevant.age.group.))

# 3rd  dataset "Duration of pre-primary education"
primary_duration <- clean_df3 %>%
  group_by(Entity) %>%
  summarise(MaxYear = max(Year), Duration_pre_primary_new = max(Theoretical.duration.of.pre.primary.education..years.),
            MinYear = min(Year), Duration_pre_primary_old = min(Theoretical.duration.of.pre.primary.education..years.))

# 4th dataset "World population level of education" - population with upper secondary
population_upper_secondary <- clean_df4 %>%
  group_by(Entity) %>%
  summarise(MaxYear = max(Year), Pop_upper_secondary_new = max(Population...Education..Upper.Secondary),
            MinYear = min(Year), Pop_upper_secondary_old = min(Population...Education..Upper.Secondary))

# 5th dataset "World population level of education"- population with no education
population_no_education <- clean_df4 %>%
  group_by(Entity) %>%
  summarise(MaxYear = max(Year), Pop_no_education_new = max(Population...Education..No.Education),
            MinYear = min(Year), Pop_no_education_old = min(Population...Education..No.Education))

# 6th dataset "World population level of education"- population primary education
population_primary_education <- clean_df4 %>%
  group_by(Entity) %>%
  summarise(MaxYear = max(Year), Pop_primary_education_new = max(Population...Education..Primary),
            MinYear = min(Year), Pop_primary_education_old = min(Population...Education..Primary))

# merge all datasets together
merged_data <- merge(entrance_age, completion_rate, by = c("Entity"))
merged_data <- merge(merged_data, primary_duration, by = c("Entity"))
merged_data <- merge(merged_data, population_upper_secondary, by = c("Entity"))
merged_data <- merge(merged_data, population_no_education, by = c("Entity"))
merged_data <- merge(merged_data, population_primary_education, by = c("Entity"))

#descriptive statistics and inferences...


# As the first step we need to find outliers and not-suitable data 

# We gonna define maximal and minimal year that will be maximally properly for our research. In our opinion we define the 
# range equal to 5. We think 5 years different will not be significant for our data and research overall. 

max_out <- unique(which(merged_data[,c(which(colnames(merged_data) == "MaxYear.x"),which(colnames(merged_data) == "MaxYear.y"))] < 2015) %%70)

min_out <- unique(which(merged_data[,c(which(colnames(merged_data) == "MinYear.x"),which(colnames(merged_data) == "MinYear.y"))] > 1975) %% 70)

out <- c(max_out,min_out)


!(1:70 %in% out)

new_data <-  merged_data[!(1:70 %in% out),]


# we should calculate change that happened in every country for 50 years.

OLD <- which(grepl("old",colnames(new_data)))
NEW <- which(grepl("new", colnames(new_data)))

change_data <- new_data[,NEW] - new_data[,OLD]

summary(change_data$Entrance_age_to_pre_primary_new)

#descriptive statistics and inferences...


# As the first step we need to find outliers and not-suitable data 

# We gonna define maximal and minimal year that will be maximally properly for our research. In our opinion we define the 
# range equal to 5. We think 5 years different will not be significant for our data and research overall. 

max_out <- unique(which(merged_data[,c(which(colnames(merged_data) == "MaxYear.x"),which(colnames(merged_data) == "MaxYear.y"))] < 2015) %%70)

min_out <- unique(which(merged_data[,c(which(colnames(merged_data) == "MinYear.x"),which(colnames(merged_data) == "MinYear.y"))] > 1975) %% 70)

out <- c(max_out,min_out)


!(1:70 %in% out)

new_data <-  merged_data[!(1:70 %in% out),]


# we should calculate change that happened in every country for 50 years.

OLD <- which(grepl("old",colnames(new_data)))
NEW <- which(grepl("new", colnames(new_data)))

change_data <- new_data[,NEW] - new_data[,OLD]

summary(change_data)



chosen_countries = c("Norway", "Denmark", "Qatar")

df_main <- merged_data[which(merged_data$Entity %in% chosen_countries),]
df_main



vis <- df_main %>%
select("Pop_primary_education_new", "Entity", "Pop_primary_education_old")


re_vis <- melt(vis, id.vars = c("Entity"))

ggplot(re_vis, aes(x = Entity, y = value/1000, fill = variable, color=variable, alpha=variable)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("lightblue", "pink"))+ 
  scale_colour_manual(values=c("lightblue4", "red"))+ 
  coord_flip()


################################################################################################

vis_1 <- df_main %>%
  select("Pop_no_education_new", "Entity", "Pop_no_education_old")


re_vis_1 <- melt(vis_1, id.vars = c("Entity"))


ggplot(re_vis_1, aes(x = Entity, y = value/1000, fill = variable, color=variable, alpha=variable)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("lightblue", "pink"))+ 
  scale_colour_manual(values=c("lightblue4", "red"))+ 
  coord_flip()


################################################################################################

vis_2 <- df_main %>%
  select("Pop_upper_secondary_new", "Entity", "Pop_upper_secondary_old")


re_vis_2 <- melt(vis_2, id.vars = c("Entity"))

ggplot(re_vis_2, aes(x = Entity, y = value/1000, fill = variable, color=variable, alpha=variable)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("lightblue", "pink"))+ 
  scale_colour_manual(values=c("lightblue4", "red"))+ 
  coord_flip()


#######################################################################################


chosen_countries_mid = c("Costa Rica", "Poland", "Portugal")

df_main_mid <- merged_data[which(merged_data$Entity %in% chosen_countries_mid),]
df_main_mid



vis_21 <- df_main_mid %>%
  select("Pop_primary_education_new", "Entity", "Pop_primary_education_old")


re_vis_21 <- melt(vis_21, id.vars = c("Entity"))

ggplot(re_vis_21, aes(x = Entity, y = value/100000, fill = variable, color=variable, alpha=variable)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("lightblue", "pink"))+ 
  scale_colour_manual(values=c("lightblue4", "red"))+ 
  coord_flip()


################################################################################################

vis_22 <- df_main_mid %>%
  select("Pop_no_education_new", "Entity", "Pop_no_education_old")


re_vis_22 <- melt(vis_22, id.vars = c("Entity"))

ggplot(re_vis_22, aes(x = Entity, y = value/100000, fill = variable, color=variable, alpha=variable)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("lightblue", "pink"))+ 
  scale_colour_manual(values=c("lightblue4", "red"))+ 
  coord_flip()


################################################################################################

vis_23 <- df_main_mid %>%
  select("Pop_upper_secondary_new", "Entity", "Pop_upper_secondary_old")


re_vis_23 <- melt(vis_23, id.vars = c("Entity"))

ggplot(re_vis_23, aes(x = Entity, y = value/100000, fill = variable, color=variable, alpha=variable)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("lightblue", "pink"))+ 
  scale_colour_manual(values=c("lightblue4", "red"))+ 
  coord_flip()


#######################################################################################


chosen_countries_l <-  c("Argentina", "Italy", "South Korea")

df_main_l <- merged_data[which(merged_data$Entity %in% chosen_countries_l),]
df_main_l



vis_31 <- df_main_l %>%
  select("Pop_primary_education_new", "Entity", "Pop_primary_education_old")


re_vis_31 <- melt(vis_31, id.vars = c("Entity"))

ggplot(re_vis_31, aes(x = Entity, y = value/1000000, fill = variable, color=variable, alpha=variable)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("lightblue", "pink"))+ 
  scale_colour_manual(values=c("lightblue4", "red"))+ 
  coord_flip()


################################################################################################



vis_32 <- df_main_l %>%
  select("Pop_no_education_new", "Entity", "Pop_no_education_old")


re_vis_32 <- melt(vis_32, id.vars = c("Entity"))

ggplot(re_vis_32, aes(x = Entity, y = value/1000000, fill = variable, color=variable, alpha=variable)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("lightblue", "pink"))+ 
  scale_colour_manual(values=c("lightblue4", "red"))+ 
  coord_flip()


################################################################################################

vis_33 <- df_main_l %>%
  select("Pop_upper_secondary_new", "Entity", "Pop_upper_secondary_old")


re_vis_33 <- melt(vis_33, id.vars = c("Entity"))

ggplot(re_vis_33, aes(x = Entity, y = value/1000000, fill = variable, color=variable, alpha=variable)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("lightblue", "pink"))+ 
  scale_colour_manual(values=c("lightblue4", "red"))+ 
  coord_flip()

#######################################################################################

par(mfcol = c(1,1))

# visualisations  
avg_school_entrty_age_old = mean(merged_data$Entrance_age_to_pre_primary_old) 
avg_school_entrty_age_new = mean(merged_data$Entrance_age_to_pre_primary_new)  
data <- c(avg_school_entrty_age_old, avg_school_entrty_age_new)  
names <- c("School entrty age 1970", "School entrty age 2020")  
chosen_countries = c("United Arab Emirates", "South Korea", "Norway", "Ecuader", "Denmark", 
                     "Central African Republic", "India")  
micro_df = merged_data[merged_data$Entity == chosen_countries,] 
micro_df  

# Create a bar chart 
barplot(data, names.arg = c("Pre primary entry age 1970", "Pre primary entry age 2020"), 
        col = "skyblue", main = "Column Chart of pre primary entry age", ylim = c(0, 4))

avg_school_entrty_age_old = mean(merged_data$Duration_pre_primary_old) 
avg_school_entrty_age_new = mean(merged_data$Duration_pre_primary_new)  
data_1 <- c(avg_school_entrty_age_old, avg_school_entrty_age_new) 

barplot(data_1, names.arg = c("Duration pre primary 1970", "Duration pre primary 2020"), 
        col = "skyblue", main = "Column Chart of duration pre primary education", ylim = c(0, 4))

avg_school_entrty_age_old = mean(merged_data$Primary_completion_rate_old) 
avg_school_entrty_age_new = mean(merged_data$Primary_completion_rate_new)  
data_2 <- c(avg_school_entrty_age_old, avg_school_entrty_age_new) 

barplot(data_2, names.arg = c("Primary completion rate 1970", "Primary completion rate 2020"), 
        col = "skyblue", main = "Column Chart of primary completion rate", ylim = c(0,100))

