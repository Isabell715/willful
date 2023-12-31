% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empathy_long.R
\name{Info3h_l}
\alias{Info3h_l}
\title{Calculate utility w/o action in information set 3, that is, after receiving a bad signal}
\usage{
Info3h_l(pars)
}
\arguments{
\item{pars}{vector with length of 5}

\item{X}{The benefit from acting}

\item{k}{The cost of inquiry}

\item{h}{The negative utility from showing empathy}

\item{p}{The probability of bad state}

\item{alpha}{The probability of correct inquiry}
}
\value{
The optimal utility in Info Set 3
}
\description{
Calculate utility w/o action in information set 3, that is, after receiving a bad signal
}
\examples{
X <- 5;
k <- 1;
h <- 10;
p <- 0.4;
alpha <- 0.8;
parameter <- c(X, k, h, p, alpha);
Info3h_l(parameter);
UInfo3h_l <- as.numeric(capture.output(Info3h_l(parameter))[1]);
cat("Utility under optimal strategy (Info Set 3):", UInfo3h_l, "\n")
DInfo3h_l <- as.numeric(capture.output(Info3h_l(parameter))[2]);
cat("U(act) - U(no act) (Info Set 3):", DInfo3h_l, "\n")
}
