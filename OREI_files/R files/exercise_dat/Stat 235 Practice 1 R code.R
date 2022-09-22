#######################Practice 1########################
#######################Question 2########################

#Entering the counts into a vector, and naming each element of the vector with the time of day
wreck_counts <- c(`midnight-4am`=4547, `4-8am`=4388, `8-noon`=4154, `noon-4pm`=5943, `4-8pm`=7329, `8-midnight`=7022)

#Total number of wrecks using the sum() function
N <- sum(wreck_counts)

########Part A###########

#Sample proportion of wrecks that occurred during pm hours, located in the 4th to 6th position
count_pm <- sum(wreck_counts[4:6])
(pm_prop <- count_pm/N)


##Getting the critical value of a 95% CI using a normal model
#qnorm(1-0.05/2) = 1.96
z_star <- qnorm(0.975)

###Calculating the CI interval###
#The fraction infront of the +-
(center <- (count_pm+z_star^2/2)/(N+z_star^2))

#The part infront of the standard error
cv_adj <- z_star/(N+z_star^2)

#SE equivalent
SE_adj <- sqrt(count_pm*(N-count_pm)/N+z_star^2/4)

###95% CI for pm proportion of wrecks###
c(center - cv_adj*SE_adj, center + cv_adj*SE_adj)

##Using the functions in R for it
library(DescTools)
BinomCI(x=count_pm, n=N, method="wilson")

#Base R: prop.test
prop.test(x=count_pm, n=N)



########Part B########
#Tests for a difference in proportions
#Sample proportions we want to compare
(p_4pm <- wreck_counts[5]/N)
(p_8pm <- wreck_counts[6]/N)

#Wald SE
(SE_wald <- sqrt((p_4pm+p_8pm - (p_4pm-p_8pm)^2)/N))

#Score SE: Assume pi_4pm = pi_8pm
(SE_score <- sqrt((p_4pm+p_8pm)/N))

###Tests stats
(z_diff <- (p_4pm-p_8pm)/c(SE_wald,SE_score))

##P-values using the normal distribution
2*pnorm(abs(z_diff),lower.tail=F)




########Part C########
#Sample proportions of the AM time slots
#AM time periods are in the first 3 slots in the count vector
(p_am <- wreck_counts[1:3]/N)


#If H0 is true, then the am proportions should all be the same.
#We will pool the am counts together and divide the proportion equally among the 3 groups
(count_am <- sum(wreck_counts[1:3])) #Pooling the wrecks in the AM together; made a LIST of 3 am proportions

#The proportion of wrecks in the am averaged across the 3 time periods; made a LIST of 3 expected, all same
(pi_0 <- 1/3*count_am/N)             

#Comparing the sample props (p_am) to the expected prop (pi_0) in sample props and expected props; made a little object
cbind(p_am,pi_0)

###Test statistics###
#Pearson chi-squared Test stat: sum of N*(p_i-pi_i)^2/pi_i; used lists from above
(Pear_am <- sum(N*(p_am-pi_0)^2/pi_0))

#LRT G test: 2*(n_i*log(p_i/pi_i))
#Keeping just the n_i we want
am_counts <- wreck_counts[1:3] #just the wrecks that occurred in the AM, but not combining them

#The test stats
(G_am <- 2*(sum(am_counts*log(p_am/pi_0)))) #used LISTS made of probabilities above

#There is only 1 parameter that needs to be estimated in the null hypothesis
#There are 3 proportions needed to be estimated in the alternative hypothesis
#df = 3-1=2
pchisq(c(Pear_am, G_am),df=2, lower.tail=F) #does for BOTH the pearson result and the g-test result; use chi-sq distribution








########Part C######## if N was right
#Sample proportions of the AM time slots
#AM time periods are in the first 3 slots in the count vector
wreck_counts <- c(`midnight-4am`=4574, `4-8am`=4388, `8-noon`=4154, `noon-4pm`=5943, `4-8pm`=7329, `8-midnight`=7022)
(p_am <- wreck_counts[1:3]/33410)


#If H0 is true, then the am proportions should all be the same.
#We will pool the am counts together and divide the proportion equally among the 3 groups
(count_am <- sum(wreck_counts[1:3])) #Pooling the wrecks in the AM together; made a LIST of 3 am proportions

#The proportion of wrecks in the am averaged across the 3 time periods; made a LIST of 3 expected, all same
(pi_0 <- 1/3*count_am/33410)             

#Comparing the sample props (p_am) to the expected prop (pi_0) in sample props and expected props; made a little object
cbind(p_am,pi_0)

###Test statistics###
#Pearson chi-squared Test stat: sum of N*(p_i-pi_i)^2/pi_i; used lists from above
(Pear_am <- sum(33410*(p_am-pi_0)^2/pi_0))

#LRT G test: 2*(n_i*log(p_i/pi_i))
#Keeping just the n_i we want
am_counts <- wreck_counts[1:3] #just the wrecks that occurred in the AM, but not combining them

#The test stats
(G_am <- 2*(sum(am_counts*log(p_am/pi_0)))) #used LISTS made of probabilities above

#There is only 1 parameter that needs to be estimated in the null hypothesis
#There are 3 proportions needed to be estimated in the alternative hypothesis
#df = 3-1=2
pchisq(c(Pear_am, G_am),df=2, lower.tail=F)
