library(tidyverse); library(DescTools); library(vcd)

####Function for MH r####
MH.cor = function(tab, row_scores, col_scores){
  
  tab_df <- data.frame(RS = rep(row_scores, ncol(tab)),
                       CS = rep(col_scores, each=nrow(tab)),
                       Counts = c(tab))
  
  MH_r <- cov.wt(x=cbind(tab_df$RS, tab_df$CS), wt=tab_df$Counts, method='ML', cor=T)$cor[1,2]
  
  n <- sum(tab)
  
  M2 <- (n-1)*MH_r^2
  p_val <- pchisq(M2, df=1, lower.tail=F)

  return(list(r = MH_r, M2 = M2, p_value = p_val))
}




###GK gamma function###
GK.gamma <- function(x_table){
  N <- sum(x_table)
  I <- nrow(x_table)
  J <- ncol(x_table)

  con_mat <- matrix(0,nrow=I, ncol=J)
  row.names(con_mat) <- row.names(x_table)
  colnames(con_mat) <- colnames(x_table)
  con_mat

  for (i in 1:(I-1)){
    for (j in 1:(J-1)){
      con_cells <- x_table[(i+1):I,(j+1):J]
      con_mat[i,j] <- x_table[i,j]* sum(con_cells) 
    }
  }
  con_mat

  dis_mat <- matrix(0,nrow=I, ncol=J)
  row.names(dis_mat) <- row.names(x_table)
  colnames(dis_mat) <- colnames(x_table)
  dis_mat

  for (i in 1:(I-1)){
    for (j in 2:J){
      dis_cells <- x_table[(i+1):I,1:(j-1)]
      dis_mat[i,j] <- x_table[i,j]* sum(dis_cells) 
    }
  }
  dis_mat
  
  (P_con <- sum(con_mat))
  (P_dis <- sum(dis_mat))
  
  gk_gamma <- (P_con-P_dis)/(P_con+P_dis)

  z_stat <- gk_gamma*sqrt((P_con+P_dis)/(N*(1-gk_gamma^2)))
  
  p_val <- pchisq(z_stat^2,df=1,lower.tail = F)
  
  return(list(Concordant_Pairs = P_con, Discordant_Pairs = P_dis, GK_Gamma = gk_gamma, z = z_stat, p.value = p_val))
}