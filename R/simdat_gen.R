#' Simulation data generator to test diffsig
#'
#' @export
#' @param N number of samples, default 100
#' @param M number of covariates, default 2 (includes intercept)
#' @param K number of signatures, default 5
#' @param L number of transitions to use, default 96
#' @param truetau true value of Dirichlet precision parameter tau, default 100
#' @param C mutational signature matrix, COSMIC signatures recommended (LxK)
#' @param continuous number of continuous variables
#' @param binary number of binary variables
#' @param categorical number of categorical variables
#' @param cat_levels vector of number of levels for each categorical variables
#' @param seed seed number for random number generate (set.seed)
#' @return A list of simulated beta, mutational count (Y), and risk factor (X)
#'
simdat_gen <- function(N=100,K=5,L=96,truetau=100, C,
                       continuous=NULL,binary=NULL,categorical=NULL, cat_levels=NULL,
                       seed=sample(1:1e6,1)) {
  require(dirmult)
  require(fastDummies)

  if (is.na(Xtype)) {
    stop("Must include at least 1 risk factor")
  }
  if (length(each)!=3) {
    stop("Must specify number of each risk factor")
  }
  if(nrow(C)!=L) {
    stop("Number of mutation transition does not match specified L")
  }
  if (ncol(C)!=K) {
    stop("Number of signatures does not match specified K")
  }
  if(!"categorical"%in% Xtypes & !is.null(cat_levels)) {
    stop("Must include categorical variable to specify categorical risk factor levels")
  }
  if(sum(cat_levels<=2)>0) {
    stop("Categorical levels should be greater than 2. Use binary if levels=2 is desired")
  }

  set.seed(seed)

  # Generate beta
  ## Associations with K signatures (beta: M x K)
  beta <- matrix(round(runif(M*K, -2, 2),1),ncol=K)
  beta <- beta - rowMeans(beta) # centralize betas to 0

  # Generate risk factor
  ## Continuous risk factor
  X <- as.matrix(t(rep(1,N)))
  if (continuous > 0) {
    for (i in 1:continuous) {
      X <- rbind(X, as.vector(scale(rnorm(N,0,10))))
    }
  }

  if (binary > 0) {
    for (i in 1:binary) {
      X <- rbind(X, sample(c(0,1),N, replace = T))
    }
  }

  if (categorical > 0) {
    for (i in 1:categorical) {
      for (j in cat_levels) {
        x <- sample(1:j, N, replace=T)
        x <- fastDummies::dummy_cols(x)[,-c(1:2)]
        colnames(x) = NULL
        X <- rbind(X, t(x))
      }
    }
  }

  # Generate mutation count Y
  J <- rnbinom(N, mu=300, size=100) # J - number of mutations for each sample
  J[J == 0] <- 1 # remove 0s - 0 represents no mutation, only include samples with mutations

  alpha <- t(beta) %*% X # alpha - K vector for each sample used to define its contribution
  softmax <- function(x) exp(x)/sum(exp(x)) # map to 0<=alpha<=1 & sum(alpha)=1 for each sample
  sm_alpha <- apply(alpha, 2, softmax)
  sm_alpha_b <- sm_alpha * tau # tau: precision parameter for dirichlet sampling

  phi <- sapply(seq_len(N), function(x) rdirichlet(1,sm_alpha_b[,x])) # phi - the prob of each mutation type for each sample
  pi <- as.matrix(C) %*% phi # pi - phi mirrored in COSMIC space
  Y <- sapply(seq_len(N), function(x) rmultinom(1, size=J[x], prob=pi[,x])) # Y - the observed counts

  return(list(beta=beta,Y=Y,X=X))
}
