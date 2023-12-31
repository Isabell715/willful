% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/punishment_short.R
\name{Info1T_s}
\alias{Info1T_s}
\title{Calculate the expected utility in information set 1}
\usage{
Info1T_s(pars)
}
\arguments{
\item{pars}{vector with length of 6}

\item{X}{The benefit from acting}

\item{k}{The cost of inquiry}

\item{T0}{The punishment when acting knowingly}

\item{p}{The probability of bad state}

\item{alpha}{The probability of correct inquiry}

\item{T1}{The punishment of willful ignorance}
}
\value{
The optimal utility in Info Set 1
}
\description{
Calculate the expected utility in information set 1
}
\examples{
X <- 5;
k <- 1;
T0 <- 10;
p <- 0.4;
alpha <- 0.8;
T1 <- 5;
parameter <- c(X, k, T0, p, alpha, T1);
Info1T_s(parameter);
UInfo1T_s <- as.numeric(capture.output(Info1T_s(parameter))[1]);
cat("Utility under optimal strategy (Info Set 1):", UInfo1T_s, "\n")
DInfo1T_s <- as.numeric(capture.output(Info1T_s(parameter))[2]);
cat("U(act) - U(no act) (Info Set 1):", DInfo1T_s, "\n")

X_list <- seq(0.00, 10, by=.01);
alpha_list <- seq(0.5, 1, by = .001);
X <- X_list;
k <- k_list;
T0 <- 10;
p <- 0.5;
alpha <- 0.8;
T1 <- 5;
matrixW = matrix(data=NA, nrow=length(X_list), ncol=length(k_list));
for(i in 1:length(X_list)){
  for(j in 1:length(k_list)){
    X <- X_list[i]
    k <- k_list[j]
    parameter <- c(X, k, T0, p, alpha, T1)
    DInfo1T_s <- as.numeric(capture.output(Info1T_s(parameter))[2])
    matrixW[i,j] <- DInfo1T_s
  }
}
}
