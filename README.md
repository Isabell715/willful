# willful
calculate utility under willful ignorance
In order to assign a variable to the function result, please run the following codes (taking Info Set 4 as example): 

utility:
UInfo4h_l <- as.numeric(capture.output(Info4h_l(parameter))[1]); cat("Utility under optimal strategy (Info Set 4):", UInfo4h_l, "\n")

utility difference:
DInfo4h_l <- as.numeric(capture.output(Info4h_l(parameter))[2]); cat("U(act) - U(no act) (Info Set 4):", DInfo4h_l, "\n")

for(i in 1:length(X_list)){
   for(j in 1:length(alpha_list)){
       X <- X_list[i]
       alpha <- alpha_list[j]
       parameter <- c(X, k, h, p, alpha)
  DInfo1h_l <- as.numeric(capture.output(Info1h_l(parameter))[2])
  matrixW[i,j] <- DInfo1h_l
   }
}
