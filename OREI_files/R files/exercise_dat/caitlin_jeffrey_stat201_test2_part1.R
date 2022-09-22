##Caitlin Jeffrey, Stat 201 Test 2 6.25.20

##Part 1

#a) Set your working directory
setwd("C:/Users/caitl/OneDrive/Documents/BaseR/data/exercise_dat")

#b) Read the clean_test.csv file into your R working session and 
    #save it as a data frame called "part1"
part1<- read.csv("C:/Users/caitl/OneDrive/Documents/BaseR/data/exercise_dat/clean_test.csv", header=T)

#c) Convert the "date" variable so that it is correctly stored as an R date. Be sure to write over the 
    #existing "date" variable, instead of creating a new one.
part1$date<-as.Date(part1$date, format="%m/%d/%y") #change from character, then as.Date

#d) Print the first 6 observations of the data frame to your console
head(part1)

#e) Create a new variable called "month" which contains the month the homework assignment
    #was completed. For example a homework date of 4/2/2018 would be month 4 (April)
part1$month<-format(part1$date, "%m")

#f) Using base package R, draw a bar graph that shows the distribution of month
barplot(table(part1$month))

#g) Construct a confidence interval estimate of homework grade 3. Write a comment 
    #to interpret your results
library(dplyr)
homework3<-filter(part1, hw==3)
t.test(homework3$grade) #we can be 95% confidence that the true mean grade for 
    #homework 3 falls between 80.52517 and 83.82083

#h) Calculate the median hw2 grade.
homework2<-filter(part1, hw==2)
median(homework2$grade) #[1] 84.35


#i) Using dplyr functions, create a new variable called "avghw" - this is the average
    #total homework grade (average of the 5 homework grades per person). 
part1<-part1%>%
  group_by(id) %>%
  mutate(avghw=mean(grade))

#j) Using dplyr functions, create a data subset called "part1a" so each person is 
    #represented by 1 row (instead of each person represented by 5 rows) so you could 
    #conduct a hypothesis test to answer the question: "Is there a difference in average
    #total hw grade ("avghw") between those who scored => median on homework 2 and
    #those who scored < the median on homework 2"?
part1$hw1grade<-NA
part1$hw2grade<-NA
part1$hw3grade<-NA
part1$hw4grade<-NA
part1$hw5grade<-NA

part1$hw1grade<-ifelse(part1$hw == 1, part1$hw1grade<-part1$grade, NA)
part1$hw2grade<-ifelse(part1$hw == 2, part1$hw2grade<-part1$grade, NA)
part1$hw3grade<-ifelse(part1$hw == 3, part1$hw3grade<-part1$grade, NA)
part1$hw4grade<-ifelse(part1$hw == 4, part1$hw4grade<-part1$grade, NA)
part1$hw5grade<-ifelse(part1$hw == 5, part1$hw5grade<-part1$grade, NA)


part1a<-part1 %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(gradehw1=max(hw1grade, na.rm=TRUE)) %>%
  mutate(gradehw2=max(hw2grade, na.rm=TRUE)) %>%
  mutate(gradehw3=max(hw3grade, na.rm=TRUE)) %>%
  mutate(gradehw4=max(hw4grade, na.rm=TRUE)) %>%
  mutate(gradehw5=max(hw5grade, na.rm=TRUE))

part1a<-filter(part1a, hw==1)
part1a<-part1a %>%
  select(id, date, month, avghw, gradehw1, gradehw2, gradehw3, gradehw4, gradehw5)

#k) Now that you have the data subset created from question f, use it to actually answer 
    #the question "Is there a difference in average total hw grade ("avghw") between those 
    #who scored => median on homework 2 and those who scored < the median on homework 2"? 
    #Write a comment with your interpretation of the results.
median(part1a$gradehw2) #[1] 84.35
part1a$eqtoabovemed2<-ifelse(part1a$gradehw2 >= 84.35, 1, 0)
t.test(part1a$avghw~part1a$eqtoabovemed2)
#p-value = 0.001494, the difference in average hw score between those who scored => median 
#grade for hw2 and those who scored below median grade of hw2 is statisically significant

#l) Using the ggplot2 package, write a series of commands to produce this graph:
library(ggplot2)
part1 %>%
  ggplot(aes(x=grade, fill=hw))+
  geom_histogram()+
  labs(title="Grades by homework assignment",
       x="grade",
       y="",
       color="legend")+
  facet_wrap(~hw)+
  theme(legend.position = "right")

