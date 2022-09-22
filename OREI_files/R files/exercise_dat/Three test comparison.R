###################################################################
###################################################################
######R code for tests involving a single categorical variable#####
###################### Stat 235: 1.4 ##############################
###################################################################
library(tidyverse); library(DescTools)

#Entering the day of birth counts into a dataframe
birth_days <- data.frame(Day = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                         Counts = c(108, 131, 136, 135, 158, 217, 115))



########################################
######Testing if P(Birth on Weekend)=2/7
########################################
(n=sum(birth_days$Counts)) #Total sample size

#Combining Sat and Sun together into one category using filter() to only keep
#weekend days and summarize(sum()) to add up those days counts.
(y <- as.numeric(birth_days %>% filter(Day %in% c("Sat","Sun")) %>%
                     summarize(sum(Counts))))

(p_end = y/n) #Sample proportion

pi0 = 2/7 #Null hypothesis value

#############Wald and Score Tests##################
#Getting the Wald and score SE
(wald_se <- sqrt(p_end*(1-p_end)/n)) #Wald SE uses sample proportion
(score_se <- sqrt(pi0*(1-pi0)/n)) #Score SE uses null proportion

#We can get both tests stats simultaneously!
(z_end <- (p_end-pi0)/c(wald_se,score_se))

#p-values for wald and score tests: 2P(Z > |z|)
2*pnorm(abs(z_end), lower.tail = F)



#####Likelihood Ratio Test#####
#Find two probabilities of the sample data using a bin(n,pi0) and bin(n,p_end)
(L0 <- dbinom(y,n,pi0))   #Restricted Likelihood = max P(Sample | H0 is true)
(L1 <- dbinom(y,n,p_end)) #Unrestricted Likelihood = max P(Sample)

#Likelihood Ratio Test statistic
(Lambda = -2*log(L0/L1))

#P-value uses a chisquared distribution with 
#df = difference in estimated parameters between L1 and L0: 1 - 0 = 1
1-pchisq(Lambda,df=1)


###Functions in R for our tests###
#score test
?prop.test #Putting a ? infront of a function name pulls up the help menu

#The function below will perform a score test and score CI
prop.test(x=y, n=n, p=pi0, alternative = "two.sided", correct=F) 

#We can use the GTest function in the DescTools package to do our LRT
?GTest #Requires success and failure counts and probabilities

#n-y = failure count, 1-pi0 = failure probability
GTest(x = c(y, n-y), p=c(pi0, 1-pi0)) 




########################################################
########Confidence intervals for pi_end#################
########################################################

##################Wald: p +- z*(SE) ####################
(z_star <- qnorm(0.975)) #Critical Value
(Wald_CI <- c(p_end - z_star*wald_se, p_end + z_star*wald_se))

###########################Score CI##########################
###we need to find the range of pi0 that doesn't reject H0###
###########################Score CI##########################
####Using the formula in the slides:
score_center <- (y+z_star^2/2)/(n+z_star^2) #Middle point
score_crit <- z_star/(n+z_star^2) #Quantity before square root
score_CI_se <- sqrt(y*(n-y)/n+z_star^2/4) #Quantity inside sqrt

#Score confidence interval
(score_CI <- c(score_center - score_crit*score_CI_se,
               score_center + score_crit*score_CI_se))

#The BinomCI function in the DescTools package will calculate several diff CI
?BinomCI #wilson = score, wald = wald
BinomCI(x=y, n=n, method = "wilson")
BinomCI(x=y, n=n, method = "wald")


###############Score and LRT CI using the "search" method #################
pi1 = seq(0.2, 0.4, by=0.005)      #Range of values of pi0 to test
pi1 = seq(0.2, 0.4, by=0.0001) #Range of values of pi0 for picture

#Creating a vector to hold the p-values from our searches
score_pvals <- rep(-1,length(pi1)) #Use negative values as placeholders 
lrt_pvals <- rep(-1,length(pi1))   #since p-values can't be negative

for(i in 1:length(pi1)){
  score_pvals[i] <- prop.test(x=y, n=n, p=pi1[i], correct=F)$p.value #Score test p-value
  
  lrt_pvals[i] <- GTest(x = c(y, n-y), p=c(pi1[i], 1-pi1[i]))$p.value 
}

#Creating a data.frame that has the value of pi0, score p-value, and LRT p-value
(CI_results <- round(data.frame(pi1=pi1, score_pvals=score_pvals, lrt_pvals=lrt_pvals),4))

#LRT CI
CI_results %>% filter(between(lrt_pvals, 0.049, 0.051)) %>% select(-score_pvals)
#Between(x, lower, upper) returns TRUE if x is somewhere between lower and upper limits 
?between

#Score CI
CI_results %>% filter(between(score_pvals, 0.049, 0.051)) %>% select(-lrt_pvals)

#All three methods found the same score CI!


plot(pi1, score_pvals, data=CI_results, type="l", col="blue", xlim=c(.25, .40),
     main="P-values of a score test for pi0")
abline(h=0.05, col="red")

plot(pi1, lrt_pvals, data=CI_results, type="l", col="orange", xlim=c(0.25, 0.40),
     main="P-values of a LRT for pi0")
abline(h=0.05, col="red")
#####Plotting the Likelihood curve##### 
pi_ll = seq(0.27, 0.40, by=0.001)
lik_val <- dbinom(x=y, size = n, prob = pi_ll)

plot(pi_ll, lik_val, type = "l", xlab ="pi0", ylab="Likelihood", main = "Likelihood Ratio Confidence Interval")
abline(v=c(0.3033, 0.3616),col="red")













######################################################
######Testing if P(Friday)=P(Saturday)=P(Sunday)######
######################################################

#Combining Monday to Thursday together, then tacking on Friday - Sun counts
(n_i <- c(sum(birth_days$Counts[1:4]),birth_days$Counts[5:7]))

#Calculating the observed proportions
(p_obs <- n_i/n)

#Since HO assumes that the proportions should be the same for 
#Fri - Sun, we sum their observed proportions together and average it
#rep(pi0, 3) will repeated the null proportions 3 times, one for each day
(pi_i <- c(p_obs[1], rep(mean(p_obs[2:4]),3)))

#Pearson Chi2 Test statistic by hand
(Pearson_chi <- sum(n*(p_obs-pi_i)^2/pi_i))

#Pearson Chi2 test stat by function
(pear_test = chisq.test(x=n_i, p=pi_i))

#Gives us the wrong df :(
#4 props estimated under Ha, 2 prop estimated under H0 --> df = 2

#LRT stat by hand
(LRT_G = 2*sum(n_i*log(p_obs/pi_i)))

#LRT stat by function
(G_Test <- GTest(x=n_i, p = pi_i))

#P-value using correct df
pchisq(c(Pearson_chi, LRT_G), df=2, lower.tail=F)


###Getting the Pearson residuals for Pearson's test
pear_test$residuals



######################################################
###########Testing if all days are equal##############
######################################################
#Expected proportions if Ho is true
pi_i <- rep(1,7)/7

#Tests in R
chisq.test(x=birth_days$Counts, p=pi_i)
GTest(x=birth_days$Counts, p=pi_i)





