#' Calculate utility w/o action in information set 4, that is, after receiving a good signal
#' @param pars vector with length of 6
#' @param X The benefit from acting
#' @param k The cost of inquiry
#' @param T0 The punishment when acting knowingly
#' @param p The probability of bad state
#' @param alpha The probability of correct inquiry
#' @param T1 The punishment of willful ignorance
#' @return The optimal utility in Info Set 4
#' @examples
#' X <- 5;
#' k <- 1;
#' T0 <- 10;
#' p <- 0.4;
#' alpha <- 0.8;
#' T1 <- 5;
#' parameter <- c(X, k, T0, p, alpha, T1);
#' Info4T_l(parameter);
#' UInfo4T_l <- as.numeric(capture.output(Info4T_l(parameter))[1]);
#' cat("Utility under optimal strategy (Info Set 4):", UInfo4T_l, "\n")
#' DInfo4T_l <- as.numeric(capture.output(Info4T_l(parameter))[2]);
#' cat("U(act) - U(no act) (Info Set 4):", DInfo4T_l, "\n")
#' @export
Info4T_l <- function(pars){
  Biga <- pars[1] - pars[2] - pars[3]
  Giga <- pars[1] - pars[2]
  Bign <- -pars[2]
  Gign <- -pars[2]
  p4 <- pars[4]*(1-pars[5])/(pars[4]*(1-pars[5])+(1-pars[4])*pars[5])
  EPa4 <- p4 * Biga + (1-p4) * Giga
  EPn4 <- p4 * Bign + (1-p4) * Gign
  UInfo4T_l <- max(EPa4, EPn4)
  DInfo4T_l <- EPa4 - EPn4
  cat(UInfo4T_l, "\n")
  cat(DInfo4T_l, "\n")
  cat("Expected utility from acting:", "\t =>", EPa4, "\n")
  cat("Expected utility from not acting:", "\t =>", EPn4, "\n")
  if (EPa4 >= EPn4){
    cat("The DM prefers to act when receiving good news", "\n")
  } else {
    cat("The DM prefers not to act when receiving good news", "\n")
  }

  return(p4)
  return(UInfo4T_l)
  return(DInfo4T_l)
}


#' Calculate utility w/o action in information set 3, that is, after receiving a bad signal
#' @param pars vector with length of 6
#' @param X The benefit from acting
#' @param k The cost of inquiry
#' @param T0 The punishment when acting knowingly
#' @param p The probability of bad state
#' @param alpha The probability of correct inquiry
#' @param T1 The punishment of willful ignorance
#' @return The optimal utility in Info Set 3
#' @examples
#' X <- 5;
#' k <- 1;
#' T0 <- 10;
#' p <- 0.4;
#' alpha <- 0.8;
#' T1 <- 5;
#' parameter <- c(X, k, T0, p, alpha, T1);
#' Info3T_l(parameter);
#' UInfo3T_l <- as.numeric(capture.output(Info3T_l(parameter))[1]);
#' cat("Utility under optimal strategy (Info Set 3):", UInfo3T_l, "\n")
#' DInfo3T_l <- as.numeric(capture.output(Info3T_l(parameter))[2]);
#' cat("U(act) - U(no act) (Info Set 3):", DInfo3T_l, "\n")
#' @export
Info3T_l <- function(pars){
  Biba <- pars[1] - pars[2] - pars[3]
  Giba <- pars[1] - pars[2]
  Bibn <- -pars[2]
  Gibn <- -pars[2]
  p3 <- pars[4] * pars[5]/(pars[4] * pars[5]+(1 - pars[4])*(1 - pars[5]))
  EPa3 <- p3 * Biba + (1-p3) * Giba
  EPn3 <- p3 * Bibn + (1-p3) * Gibn
  UInfo3T_l <- max(EPa3, EPn3)
  DInfo3T_l <- EPa3 - EPn3
  cat(UInfo3T_l, "\n")
  cat(DInfo3T_l, "\n")
  cat("Expected utility from acting:", "\t =>", EPa3, "\n")
  cat("Expected utility from not acting:", "\t =>", EPn3, "\n")
  if (EPa3 >= EPn3){
    cat("The DM prefers to act when receiving bad news", "\n")
  } else {
    cat("The DM prefers not to act when receiving bad news", "\n")
  }

  return(p3)
  return(UInfo3T_l)
  return(DInfo3T_l)
}

#' Calculate utility w/o action in information set 2, that is the DO does not inqurie
#' @param pars vector with length of 6
#' @param X The benefit from acting
#' @param k The cost of inquiry
#' @param T0 The punishment when acting knowingly
#' @param p The probability of bad state
#' @param alpha The probability of correct inquiry
#' @param T1 The punishment of willful ignorance
#' @return The optimal utility in Info Set 2
#' @examples
#' X <- 5;
#' k <- 1;
#' T0 <- 10;
#' p <- 0.4;
#' alpha <- 0.8;
#' T1 <- 5;
#' parameter <- c(X, k, T0, p, alpha, T1);
#' Info2T_l(parameter);
#' UInfo2T_l <- as.numeric(capture.output(Info2T_l(parameter))[1]);
#' cat("Utility under optimal strategy (Info Set 2):", UInfo2T_l, "\n")
#' DInfo2T_l <- as.numeric(capture.output(Info2T_l(parameter))[2]);
#' cat("U(act) - U(no act) (Info Set 2):", DInfo2T_l, "\n")
#' @export
Info2T_l <- function(pars){
  Bna <- pars[1] - pars[6]
  Gna <- pars[1]
  Bnn <- 0
  Gnn <- 0
  EPa2 <- pars[4] * Bna + (1 - pars[4]) * Gna
  EPn2 <- pars[4] * Bnn + (1 - pars[4]) * Gnn
  UInfo2T_l <- max(EPa2, EPn2)
  DInfo2T_l <- EPa2 - EPn2
  cat(UInfo2T_l, "\n")
  cat(DInfo2T_l, "\n")
  cat("Expected utility from acting:", "\t =>", EPa2, "\n")
  cat("Expected utility from not acting:", "\t =>", EPn2, "\n")
  if (EPa2 >= EPn2){
    cat("The DM prefers to act when not inquiring", "\n")
  } else {
    cat("The DM prefers not to act when not inquiring", "\n")
  }

  return(UInfo2T_l)
  return(DInfo2T_l)
}

#' Calculate the expected utility in information set 1
#' @param pars vector with length of 6
#' @param X The benefit from acting
#' @param k The cost of inquiry
#' @param T0 The punishment when acting knowingly
#' @param p The probability of bad state
#' @param alpha The probability of correct inquiry
#' @param T1 The punishment of willful ignorance
#' @return The optimal utility in Info Set 1
#' @examples
#' X <- 5;
#' k <- 1;
#' T0 <- 10;
#' p <- 0.4;
#' alpha <- 0.8;
#' T1 <- 5;
#' parameter <- c(X, k, T0, p, alpha, T1);
#' Info1T_l(parameter);
#' UInfo1T_l <- as.numeric(capture.output(Info1T_l(parameter))[1]);
#' cat("Utility under optimal strategy (Info Set 1):", UInfo1T_l, "\n")
#' DInfo1T_l <- as.numeric(capture.output(Info1T_l(parameter))[2]);
#' cat("U(act) - U(no act) (Info Set 1):", DInfo1T_l, "\n")
#'
#' @examples
#' X_list <- seq(0.00, 10, by=.01);
#' alpha_list <- seq(0.5, 1, by = .001);
#' X <- X_list;
#' k <- k_list;
#' T0 <- 10;
#' p <- 0.5;
#' alpha <- 0.8;
#' T1 <- 5;
#' matrixW = matrix(data=NA, nrow=length(X_list), ncol=length(k_list));
#' for(i in 1:length(X_list)){
#'   for(j in 1:length(k_list)){
#'     X <- X_list[i]
#'     k <- k_list[j]
#'     parameter <- c(X, k, T0, p, alpha, T1)
#'     DInfo1T_l <- as.numeric(capture.output(Info1T_l(parameter))[2])
#'     matrixW[i,j] <- DInfo1T_l
#'   }
#' }
#' @export
Info1T_l <- function(pars){
  UInfo4T_l <- as.numeric(capture.output(Info4T_l(parameter))[1])
  UInfo3T_l <- as.numeric(capture.output(Info3T_l(parameter))[1])
  UInfo2T_l <- as.numeric(capture.output(Info2T_l(parameter))[1])
  pg <- pars[4] * (1 - pars[5]) + (1 - pars[4]) * pars[5]
  pb <- 1 - pg
  EPinquire <- pg * UInfo4T_l + pb * UInfo3T_l
  EPnoinquire <- UInfo2T_l
  UInfo1T_l <- max(EPinquire, EPnoinquire)
  DInfo1T_l <- EPinquire - EPnoinquire
  cat(UInfo1T_l, "\n")
  cat(DInfo1T_l, "\n")
  cat("Expected utility from inquiring:", "\t =>", EPinquire, "\n")
  cat("Expected utility from not inquiring:", "\t =>", EPnoinquire, "\n")
  if (EPinquire >= EPnoinquire){
    cat("The DM prefers to inquire", "\n")
  } else {
    cat("The DM prefers not to inquire", "\n")
  }

  return(UInfo1T_l)
  return(DInfo1T_l)
}
