library(tidyverse); library(DescTools); library(vcd)
source("C:/Users/Jacob/OneDrive - University of Vermont/Stat 235/Course Material/2 Two Way Tables/In Class R Code/Ordinal Table Functions.R")
####################################################
############Flu: Mantel-Haenszel R##################
####################################################
#Creating a table for the flu results
(flu <- as.table(rbind(c(2,6,1),
                       c(2,2,4))))

#Naming the rows and columns
row.names(flu) <- c("treat_1","treat_2") ; colnames(flu) <- c("weak","med","strong") ; flu


####Using different choices of scores for row and columns.
##Simple choices
scores_row_simple <- 0:1 ; scores_col_simple <- 1:3

#Using the function provided in the R file in blackboard
MH.cor(tab = flu, row_scores = scores_row_simple, col_scores = scores_col_simple)



####Column scores using the middle antibody level
scores_col_mid <- c(0.40, 0.60, 1.2)

MH.cor(tab = flu, row_scores = scores_row_simple, col_scores = scores_col_mid)




#Scores using the average rank for rows and columns
scores_row_rank <- c(mean(1:350),mean(351:700))
scores_col_rank <- c(mean(1:280), mean(281:540), mean(541:700))


MH.cor(tab = flu, row_scores = scores_row_rank, col_scores = scores_col_rank)


##############################################################################################
######################## Flu: Goodman & Kruskal's Gamma ######################################
##############################################################################################
?GoodmanKruskalGamma
#The function only gives the "correlation" and confidence interval
GoodmanKruskalGamma(x=flu, conf.level = 0.95)

#My function that gives gamma, P, D, z, and p-value
GK.gamma(x_table=flu)

