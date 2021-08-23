##Caitlin Jeffrey, Stat 201 Test 2 6.25.20

##Part 2
#Use "messy_a_test.csv" and "messy_b_test.xls" data for this section. These data are not 
#clean (you do need to worry about missing values or typos). Combine and clean these data 
#as appropriate in order to conduct the below analyses

a<- read.csv("C:/Users/caitl/OneDrive/Documents/BaseR/data/exercise_dat/messy_a_test.csv", header=T)
library(readxl)
b<-read_excel("C:/Users/caitl/OneDrive/Documents/BaseR/data/exercise_dat/messy_b_test.xlsx")

#clean a
summary(a)
a$date[a$date == ""] <-NA
a$grade[a$grade == 9999999 | a$grade == 99999 | a$grade ==9999 |a$grade == 8888 | a$grade == 999] <-NA


#clean b
summary(b)
unique(b$ClassRoom)
b$ClassRoom[b$ClassRoom == "missing"] <-NA
b$ClassRoom[b$ClassRoom =="Rowel"] <-"Rowell"
b$ClassRoom[b$ClassRoom =="votey" | b$ClassRoom == "otey"] <-"Votey"
b$ClassRoom[b$ClassRoom == "perkin" | b$ClassRoom == "perkins" | b$ClassRoom == "Perkin"] <-"Perkins"

#merge data sets a and b
a$hw1grade<-NA
a$hw2grade<-NA
a$hw3grade<-NA
a$hw4grade<-NA
a$hw5grade<-NA
a$date1<-NA
a$date2<-NA
a$date3<-NA
a$date4<-NA
a$date5<-NA

a$date<-as.Date(a$date, format="%m/%d/%y")
a$hw1grade<-ifelse(a$hw == 1, a$hw1grade<-a$grade, NA)
a$hw2grade<-ifelse(a$hw == 2, a$hw2grade<-a$grade, NA)
a$hw3grade<-ifelse(a$hw == 3, a$hw3grade<-a$grade, NA)
a$hw4grade<-ifelse(a$hw == 4, a$hw4grade<-a$grade, NA)
a$hw5grade<-ifelse(a$hw == 5, a$hw5grade<-a$grade, NA)

a$date1<-as.Date(a$date1, format="%m/%d/%y")
a$date2<-as.Date(a$date2, format="%m/%d/%y")
a$date3<-as.Date(a$date3, format="%m/%d/%y")
a$date4<-as.Date(a$date4, format="%m/%d/%y")
a$date5<-as.Date(a$date5, format="%m/%d/%y")

a$date1<-ifelse(a$hw == 1, a$date1<-a$date, NA)
a$date2<-ifelse(a$hw == 2, a$date2<-a$date, NA)
a$date3<-ifelse(a$hw == 3, a$date3<-a$date, NA)
a$date4<-ifelse(a$hw == 4, a$date4<-a$date, NA)
a$date5<-ifelse(a$hw == 5, a$date5<-a$date, NA)

a$date1<-as.Date(a$date1, origin="1970-1-1")
a$date2<-as.Date(a$date2, origin="1970-1-1")
a$date3<-as.Date(a$date3, origin="1970-1-1")
a$date4<-as.Date(a$date4, origin="1970-1-1")
a$date5<-as.Date(a$date5, origin="1970-1-1")

clean_a<-a %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(gradehw1=max(hw1grade, na.rm=TRUE)) %>%
  mutate(gradehw2=max(hw2grade, na.rm=TRUE)) %>%
  mutate(gradehw3=max(hw3grade, na.rm=TRUE)) %>%
  mutate(gradehw4=max(hw4grade, na.rm=TRUE)) %>%
  mutate(gradehw5=max(hw5grade, na.rm=TRUE)) %>%
  mutate(hwdate1=max(date1, na.rm=TRUE)) %>%
  mutate(hwdate2=max(date2, na.rm=TRUE)) %>%
  mutate(hwdate3=max(date3, na.rm=TRUE)) %>%
  mutate(hwdate4=max(date4, na.rm=TRUE)) %>%
  mutate(hwdate5=max(date5, na.rm=TRUE))
    
    
hw1_a<-filter(clean_a, hw==1)
use_a<-hw1_a %>%
  select(id, gradehw1, gradehw2, gradehw3, gradehw4, gradehw5, hwdate1, hwdate2, hwdate3, hwdate4, hwdate5)

use_a$gradehw1[use_a$gradehw1 == -Inf] <-NA
use_a$gradehw2[use_a$gradehw2 == -Inf] <-NA
use_a$gradehw3[use_a$gradehw3 == -Inf] <-NA
use_a$gradehw4[use_a$gradehw4 == -Inf] <-NA
use_a$gradehw5[use_a$gradehw5 == -Inf] <-NA
colnames(b)[colnames(b)=="ID"]<-"id"
ab<-merge(use_a, b, by="id")

#a) Conduct a hypothesis test to answer the question: "Is there a difference in average
#homework 3 grades between classrooms Votey and Rowell?" Write a comment to interpret 
#your findings. 
votrow<-filter(ab, ClassRoom =="Votey" | ClassRoom == "Rowell")
t.test(votrow$gradehw3~votrow$ClassRoom, na.rm=T)

#p-value = 0.3361, the difference in hw3 score between classrooms Votey and Rowell is 
#is not statisically significant

#b) Conduct a hypothesis test to answer the question: "Is there a difference in 
#average homework 5 grade among the 3 classrooms?" Write a comment to interpret your 
#findings. 
summary(aov(ab$gradehw5~ab$ClassRoom))
#Pr(>F)=0.816, the difference in hw5 score by classroom is not statisically significant

#c) Conduct a hypothesis test to answer the question: "Is there a difference in 
#homework 1 and homework 2 grades? Write a comment to interpret your findings. 
t.test(ab$gradehw1, ab$gradehw2, paired=T, na.rm=T)

#p-value = 0.1899, there is no statistically significant difference between homework 
#1 and 2 grades

#d) What is the average number of days between homework 3 and homework 4?
ab$datediff34<-ab$hwdate4-ab$hwdate3
ab$datediff34[ab$datediff34 == -Inf | ab$datediff34 == Inf ] <-NA
unique(ab$datediff34) #[1] 21, NA- there are 21 days between hw 3 and 4 for all observations

#e) Create a new variable called pass1, where those who received >=85 on homework 1 are "pass" 
#and those who received <85 on homework 1 are "fail". Use this pass1 variable to conduct a 
#hypothesis test to answer the question "Is there a relationship between passing homework 1 and
#classroom?" Write a comment to interpret your findings. 

ab$pass1<-ifelse(ab$gradehw1 >= 85, "pass", "fail")
chisq.test(ab$pass1, ab$ClassRoom)
#p-value = 0.2175, there is no statistically significant relationship between passing hw1 and
#classroom





