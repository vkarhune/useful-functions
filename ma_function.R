
meta_analysis <- function(beta, se, hk=FALSE){

# FE
 w <- 1/se^2
 m <- sum(w*beta)/sum(w)
 sem <- 1/sqrt(sum(w))

 pf <- 2*{pnorm(abs(m/sem), lower.tail=F)}

# RE
 sumw <- sum(w)

 Q <- sum(w*beta^2) - ((sum(w*beta))^2)/(sumw)
 df <- length(beta) - 1

 t2 <- 0
 if(Q > df) t2 <- (Q-df) /  (sumw - (sum(w^2))/(sumw))

 ws <- 1/(se^2 + t2)
 sumws <- sum(ws)

 ms <- sum(ws*beta)/sumws
 sems <- 1/sqrt(sumws)
 
 pval <- 2*{pnorm(abs(ms/sems), lower.tail=F)}

# outlist <- list(beta_F = m, SE_F = sem, BETA_R = ms, SE_R = sems)
# slightly quicker to do the return within each if/else block

# Hartung-Knapp(-Sidik-Jonkman) 
 if(hk){
 SE_HK <- sqrt((sum(ws*(beta-ms)^2)/((df*sumws))))
 P_HK <- 2*(pt(abs(ms/SE_HK), df=df, lower.tail=F))
# outlist <- c(outlist, SE_HK=SE_HK, P_HK=P_HK)
 return(list(k=length(beta),
  BETA_F = m, SE_F = sem, P_F = pf, BETA_R = ms, SE_R = sems, P_R = pval,
  SE_HK=SE_HK, P_HK=P_HK, Q=Q))
 } else {
 return(list(k=length(beta),
  BETA_F = m, SE_F = sem, P_F = pf, BETA_R = ms, SE_R = sems, P_R = pval, Q=Q))
 }
}


