#' Simulation data generator to test diffsig
#'
#' @export
#' @param N number of samples, default 100
#' @param K number of signatures, default 5
#' @param truetau true value of Dirichlet precision parameter tau, default 100
#' @param C mutational signature matrix, COSMIC signatures recommended (LxK)
#' @param continuous number of continuous variables
#' @param binary number of binary variables
#' @param categorical number of categorical variables
#' @param cat_levels vector of number of levels for each categorical variables
#' @param L number of transitions to use, default 96
#' @param seed seed number for random number generate (set.seed)
#' @import fastDummies
#' @import dirmult
#' @importFrom("stats", "rmultinom", "rnbinom", "rnorm", "runif")
#' @return A list of simulated beta, mutational count (Y), and risk factor (X)
#'
simdat_gen <- function(N=100,K=5,truetau=100, C,
                       continuous=NA,binary=NA,categorical=NA, cat_levels=NA,
                       L=96, seed=sample(1:1e6,1)) {

  if (ncol(C)!=K) {
    stop("Number of signatures does not match specified K")
  }
  if (!is.na(continuous)) {
    if (continuous%%1!=0) {
      stop("continuous should be specified with an integer")
    }
  }
  if (!is.na(binary)) {
    if (binary%%1!=0) {
      stop("binary should be specified with an integer")
    }
  }
  if (!is.na(categorical)) {
    if (categorical%%1!=0) {
      stop("categorical should be specified with an integer")
    }

    if (length(cat_levels)!=categorical) {
      stop("Number of categorical variables and levels do not match (length(cat_levels)!=categorical)")
    }

    if(sum(cat_levels<=2)>0) {
      stop("Categorical levels should be greater than 2. Use binary if levels=2 is desired")
    }
  }

  set.seed(seed)

  M = sum(continuous,binary,cat_levels,-categorical, na.rm = T) + 1
  # Generate beta
  ## Associations with K signatures (beta: M x K)
  beta <- matrix(round(stats::runif(M*K, -2, 2),1),ncol=K)
  beta <- beta - rowMeans(beta) # centralize betas to 0

  # Generate risk factor
  ## Continuous risk factor
  X <- as.matrix(t(rep(1,N)))
  if (!is.na(continuous)) {
    for (i in 1:continuous) {
      X_cont <- stats::rnorm(N,0,10)
      X_cont <- as.vector(scale(X_cont))
      X <- rbind(X, X_cont)
    }
  }

  ## Binary risk factor
  if (!is.na(binary)) {
    for (i in 1:binary) {
      X <- rbind(X, sample(c(0,1),N, replace = T))
    }
  }

  ## Categorical risk factor
  if (!is.na(categorical)) {
    for (i in 1:categorical) {
      levels = cat_levels[i]
      x <- sample(1:levels, N, replace=T)
      x <- fastDummies::dummy_cols(x)[,-c(1:2)]
      colnames(x) = NULL
      X <- rbind(X, t(x))
    }
  }

  # Generate mutation count Y
  J <- stats::rnbinom(N, mu=300, size=2) # J - number of mutations for each sample
  J[J == 0] <- 1 # remove 0s - 0 represents no mutation, only include samples with mutations

  alpha <- t(beta) %*% X # alpha - K vector for each sample used to define its contribution
  softmax <- function(x) exp(x)/sum(exp(x)) # map to 0<=alpha<=1 & sum(alpha)=1 for each sample
  sm_alpha <- apply(alpha, 2, softmax)
  sm_alpha_b <- sm_alpha * truetau # truetau: precision parameter for dirichlet sampling

  phi <- sapply(seq_len(N), function(x) dirmult::rdirichlet(1,sm_alpha_b[,x])) # phi - the prob of each mutation type for each sample
  pi <- as.matrix(C) %*% phi # pi - phi mirrored in COSMIC space
  Y <- sapply(seq_len(N), function(x) stats::rmultinom(1, size=J[x], prob=pi[,x])) # Y - the observed counts

  return(list(truebeta=beta, Y=Y, X=X))
}
