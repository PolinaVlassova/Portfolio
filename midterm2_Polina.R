# The Program for International Student Assessment (PISA) assessment, 
# which is coordinated by the OECD, is the most well known international 
# assessment of learning outcomes. 
# The first PISA study was carried out in 1997 and since then it was 
# repeated every three years.
# Your task is to analyze 2 datasets that contain the information
# about average score of 15-year-old students from mathematics and 
# reading across multiple countries in different years.
#
# You shall analyse these dataset:
# 1. pisa_math_score
#    columns - Entity (country name), 
#              Code (country code), 
#              Year (year of testing),
#              Mathematics (average score from math).
# 2. pisa_reading_score
#    columns - Entity (country name), 
#              Code (country code), 
#              Year (year of testing),
#              Reading (average score from reading)
#
# Load these datasets from database using provided credentials below:  
install.packages('RMySQL')
library(RMySQL)
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
our.conn = dbConnect(MySQL(), user="midterm_4st604", password="pass_4st604",
                     dbname="db_4st604midterm", host="db4free.net", port=3306)
dbListTables(our.conn)
math=dbGetQuery(our.conn,"SELECT * FROM pisa_math_score;")
reading= dbGetQuery(our.conn,"SELECT * FROM pisa_reading_score;")
# then answer following questions (you may use R of SQL):

# 1. What is range of years stored in these datasets? 
#    (A.k.a. There are data from which year to which year?)
dbGetQuery(our.conn,"SELECT MAX(Year) FROM pisa_math_score;")
dbGetQuery(our.conn,"SELECT MIN(Year) FROM pisa_math_score;")
dbGetQuery(our.conn,"SELECT MAX(Year) FROM pisa_reading_score;")
dbGetQuery(our.conn,"SELECT MIN(Year) FROM pisa_reading_score;")
# Both datasets are ranged from 2000 to 2015

# 2. Is there any relationship between math score and reading score? Quantify it and visualize on a graph.
merged_data=merge(math, reading, by=c("Entity","Code", "Year"))
cor(merged_data$Mathematics, merged_data$Reading)
plot(merged_data$Mathematics, merged_data$Reading, main="Correlation between scores")
# Yes, there is strong positive correlation between math and reading scores, which is equal to
# 0.943

#*choose one of questions 3 or 4 to solve
# 3.* Visualize evolution of average math score per year in a chart. Are the scores improving, stagnating, declining?
# 4.* Which country improved the most in mathematics between years 2000 to 2015?

math[math$Year>2000 & math$Year< 2016,]
gg <- math %>%   group_by(Entity) %>%   summarise(diff_score = max(Mathematics) - min(Mathematics)) 
max(gg$diff_score)
gg[gg$diff_score==95,]
# It is Peru, score of which has improved by 95 points over 15 years old period

# 5. Which country has the highest difference between its math score and reading score? And in which year this occured? 
#    HINT: You may spot it in the graph from task 2

math=dbGetQuery(our.conn,"SELECT * FROM pisa_math_score;")
reading= dbGetQuery(our.conn,"SELECT * FROM pisa_reading_score;")
merged_data$Score_Difference <- merged_data$Mathematics - merged_data$Reading
max_difference_country <- merged_data[which.max(merged_data$Score_Difference), "Entity"]
max_difference_value <- max(merged_data$Score_Difference)
max_difference_year <- merged_data[which.max(merged_data$Score_Difference), "Year"]
# Azerbaijan has the highest difference between math and reading score, which stands for 123 points
# and occured in 2006

# 6. In 2015: which country was best in mathematics? Which country was best in reading?
m_new=math[math$Year==2015,]
r_new=reading[reading$Year==2015,]
max(r_new$Reading)
max(m_new$Mathematics)
dbGetQuery(our.conn,"SELECT Entity FROM pisa_math_score WHERE Mathematics=564;")
dbGetQuery(our.conn,"SELECT Entity FROM pisa_reading_score WHERE Reading=535;")
# For 2015 Singapore scored the best in mathematics = 564 and reading= 535

# 7. In 2015: Compare your home country with the rest of the world and comment on it in 3-5 sentences.
m_new[m_new$Entity== "Kazakhstan",]
r_new[m_new$Entity== "Kazakhstan",]
mean(m_new$Mathematics)
mean(r_new$Reading)
# Kazakhstan's score in reading is 427 and in mathematics is 460 for 2015, whilst
# average world's score in reading is 459.4 and in mathematics is 459.6.  It shows that my country
# math score is aligned with world's standards and on the same level with average, but reading score is lower then
# the average for an entire world in the same year

# 8. Save your analysis as html and upload it to InSIS.


#NOTE, when merging tables together (or joining tables) together, you may need to use more than one column as your key
#example:
x=data.frame(first.name=c("John","John","Tom"),last.name=c("Smith","Black","Black"),age=c(22,19,25))
y=data.frame(first.name=c("John","John","Tom"),last.name=c("Smith","Black","Black"),height=c(190,171,173))
merge(x,y,by="first.name") #obviously wrong
merge(x,y,by=c("first.name","last.name")) #correct!