#' Calculate utility w/o action in information set 4, that is, after receiving a good signal
#' @param X The benefit from acting
#' @param k The cost of inquiry
#' @param h The negative utility from showing empathy
#' @param p The probability of bad state
#' @param alpha The probability of correct inquiry
#' @return The optimal utility in Info Set 4
#' @examples 
#' u1 <- Info4h_l(c(1, 2, 3, 0.4, 0.7));
#' @export
Info4h_s <- function(pars){
  Biga <- pars[1] - pars[2]
  Giga <- pars[1] - pars[2]
  Bign <- -pars[2]
  Gign <- -pars[2]
  p4 <- pars[4]*(1-pars[5])/(pars[4]*(1-pars[5])+(1-pars[4])*pars[5])
  EPa4 <- p4 * Biga + (1-p4) * Giga
  EPn4 <- p4 * Bign + (1-p4) * Gign
  UInfo4h_s <- max(EPa4, EPn4)
  DInfo4h_s <- EPa4 - EPn4
  cat(UInfo4h_s, "\n")
  cat(DInfo4h_s, "\n")
  cat("Expected utility from acting:", "\t =>", EPa4, "\n")
  cat("Expected utility from not acting:", "\t =>", EPn4, "\n")
  if (EPa4 >= EPn4){
    cat("The DM prefers to act when receiving good news", "\n")
  } else {
    cat("The DM prefers not to act when receiving good news", "\n")
  }
  
  return(p4)
  return(UInfo4h_s)
  return(DInfo4h_s)
}


#' Calculate utility w/o action in information set 3, that is, after receiving a bad signal
#' @param X The benefit from acting
#' @param k The cost of inquiry
#' @param h The negative utility from showing empathy
#' @param p The probability of bad state
#' @param alpha The probability of correct inquiry
#' @return The optimal utility in Info Set 3
#' @examples 
#' u1 <- Info3h_l(c(1, 2, 3, 0.4, 0.7));
#' @export
Info3h_s <- function(pars){
  Biba <- pars[1] - pars[2] - pars[3]
  Giba <- pars[1] - pars[2]
  Bibn <- -pars[2]
  Gibn <- -pars[2]
  p3 <- pars[4] * pars[5]/(pars[4] * pars[5]+(1 - pars[4])*(1 - pars[5]))
  EPa3 <- p3 * Biba + (1-p3) * Giba
  EPn3 <- p3 * Bibn + (1-p3) * Gibn
  UInfo3h_s <- max(EPa3, EPn3)
  DInfo3h_s <- EPa3 - EPn3
  cat(UInfo3h_s, "\n")
  cat(DInfo3h_s, "\n")
  cat("Expected utility from acting:", "\t =>", EPa3, "\n")
  cat("Expected utility from not acting:", "\t =>", EPn3, "\n")
  if (EPa3 >= EPn3){
    cat("The DM prefers to act when receiving bad news", "\n")
  } else {
    cat("The DM prefers not to act when receiving bad news", "\n")
  }
  
  return(p3)
  return(UInfo3h_s)
  return(DInfo3h_s)
}

#' Calculate utility w/o action in information set 2, that is, if the Dm does not inquire
#' @param X The benefit from acting
#' @param k The cost of inquiry
#' @param h The negative utility from showing empathy
#' @param p The probability of bad state
#' @param alpha The probability of correct inquiry
#' @return The optimal utility in Info Set 2
#' @examples 
#' u1 <- Info2h_l(c(1, 2, 3, 0.4, 0.7));
#' @export
Info2h_s <- function(pars){
  Bna <- pars[1] - pars[3]
  Gna <- pars[1]
  Bnn <- 0
  Gnn <- 0
  EPa2 <- pars[4] * Bna + (1 - pars[4]) * Gna
  EPn2 <- pars[4] * Bnn + (1 - pars[4]) * Gnn
  UInfo2h_s <- max(EPa2, EPn2)
  DInfo2h_s <- EPa2 - EPn2
  cat(UInfo2h_s, "\n")
  cat(DInfo2h_s, "\n")
  cat("Expected utility from acting:", "\t =>", EPa2, "\n")
  cat("Expected utility from not acting:", "\t =>", EPn2, "\n")
  if (EPa2 >= EPn2){
    cat("The DM prefers to act when not inquiring", "\n")
  } else {
    cat("The DM prefers not to act when not inquiring", "\n")
  }
  
  return(UInfo2h_s)
  return(DInfo2h_s)
}

#' Calculate the expected utility in information set 1
#' @param X The benefit from acting
#' @param k The cost of inquiry
#' @param h The negative utility from showing empathy
#' @param p The probability of bad state
#' @param alpha The probability of correct inquiry
#' @return The optimal utility in Info Set 1
#' @examples 
#' u1 <- Info1h_l(c(1, 2, 3, 0.4, 0.7));
#' @export
Info1h_s <- function(pars){
  UInfo4h_s <- as.numeric(capture.output(Info4h_s(parameter))[1])
  UInfo3h_s <- as.numeric(capture.output(Info3h_s(parameter))[1])
  UInfo2h_s <- as.numeric(capture.output(Info2h_s(parameter))[1])
  pg <- pars[4] * (1 - pars[5]) + (1 - pars[4]) * pars[5]
  pb <- 1 - pg
  EPinquire <- pg * UInfo4h_s + pb * UInfo3h_s
  EPnoinquire <- UInfo2h_s
  UInfo1h_s <- max(EPinquire, EPnoinquire)
  DInfo1h_s <- EPinquire - EPnoinquire
  cat(UInfo1h_s, "\n")
  cat(DInfo1h_s, "\n")
  cat("Expected utility from inquiring:", "\t =>", EPinquire, "\n")
  cat("Expected utility from not inquiring:", "\t =>", EPnoinquire, "\n")
  if (EPinquire >= EPnoinquire){
    cat("The DM prefers to inquire", "\n")
  } else {
    cat("The DM prefers not to inquire", "\n")
  }
  
  return(UInfo1h_s)
  return(DInfo1h_s)
}