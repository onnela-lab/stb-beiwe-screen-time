#' Fit longitudinal function-on-scalar regression
#' 
#' Fit longitudinal function-on-scalar regression using the proposed 3-step approach
#' proposed and implemented in Cui et al. (2021). Minor edits 
#' (non-parallel handling, message prining, other) introduced by 
#' Marcos Matabuena and Marta Karas. 
#'  
#' @param formula two-sided formula object in lmer() format, except that the response is a matrix  
#' @param data data frame containing variables in formula
#' @param family GLM family of the response
#' @param argvals_manual locations of observations on the functional domain
#' @param var whether to estimate variance. By default FALSE
#' @param analytic whether to use the analytic inference approach
#' @param parallel whether to run parallel computing
#' @param silent whether to show descriptions of each step
#' @param B_boot number of bootstrap repetitions in step 3 (bootstrap inference case)
#' @param knots_manual number of knots for penalized splines smoothing; if not specified,
#' defaults to min(round(L/4), 35)
#' @references
#' Erjia Cui, Andrew Leroux, Ekaterina Smirnova, Ciprian M. Crainiceanu (2021) 
#' Fast Univariate Inference for Longitudinal Functional Models, 
#' Journal of Computational and Graphical Statistics, 
#' DOI: 10.1080/10618600.2021.1950006
#' @export
#' @return a list containing estimated beta(s)

lfosr3s <- function(formula, data, family = "gaussian", argvals_manual = NULL, var = FALSE, 
                    parallel = FALSE, silent = FALSE, B_boot = 100, 
                    knots_manual = NULL, msg_prefix = ""){
  
  require(lme4) ## mixed models
  require(refund) ## fpca.face
  require(dplyr) ## organize lapply results
  require(progress) ## display progress bar
  require(mgcv) ## smoothing in step 2
  require(mvtnorm) ## joint CI
  require(parallel) ## mcapply
  
  if(family != "gaussian") analytic <- FALSE ## bootstrap inference for non-Gaussian family
  
  ## Organize the input
  model_formula <- as.character(formula)
  stopifnot(model_formula[1] == "~" & length(model_formula) == 3)
  
  
  ##########################################################################################
  ## Step 1
  ##########################################################################################
  if(silent == FALSE) print("Step 1: Massive Univariate Mixed Models")
  
  L <- ncol(data[,model_formula[2]]) ## number of observations on the functional domain
  if(is.null(argvals_manual)) argvals_manual <- 1:L
  
  ## function "unimm" fit univariate mixed model at location l
  unimm <- function(l){
    data$Yl <- unclass(data[,model_formula[2]][,l])
    out <- NULL
    try({
      if (family == "gaussian"){
        fit_uni <- suppressMessages(lmer(formula = as.formula(paste0("Yl ~ ", model_formula[3])), 
                                         data = data, control = lmerControl(optimizer = "bobyqa")))
      } else {
        fit_uni <- suppressMessages(glmer(formula = as.formula(paste0("Yl ~ ", model_formula[3])), 
                                          data = data, family = family, control = glmerControl(optimizer = "bobyqa")))
      }
      betaTilde <- lme4::fixef(fit_uni)
      out <- list(betaTilde = betaTilde)
    })
    # return(list(betaTilde = betaTilde, group = as.data.frame(VarCorr(fit_uni))[1,1]))
    return(out)
  }
  
  ## fit massive univariate mixed models
  # if(parallel == TRUE){
  #   massmm <- mclapply(argvals_manual, unimm, mc.cores = detectCores() - 1)
  # }else{
  # if(!(silent)) print(argvals_manual)
  argvals_manual_n <- length(argvals_manual)
  massmm = vector(mode = "list", length = argvals_manual_n)
  for(i in 1 : argvals_manual_n){ # i <- 13; argvals_manual_i <- argvals_manual[i]; l <- argvals_manual[i]
    print(paste0(msg_prefix, "massmm i = ", i, "/", argvals_manual_n))
    argvals_manual_i <- argvals_manual[i]
    massmm[[i]] = unimm(argvals_manual_i)
  }
  massmm_isnotnull <- (!sapply(massmm, is.null))
  # update argvals in case some were not fitted
  argvals <- argvals_manual[massmm_isnotnull]

  ## obtain betaTilde
  # betaTilde <- t(lapply(massmm, '[[', 1) %>% bind_rows())
  # note null ones disappear
  betaTilde0 <- t(lapply(massmm, '[[', 1) %>% bind_rows())
  betaTilde <- matrix(NA, nrow = nrow(betaTilde0), ncol = L)
  print("dim(betaTilde)")
  print(dim(betaTilde))
  print("dim(betaTilde0)")
  print(dim(betaTilde0))
  print("length(argvals)")
  print(length(argvals))
  betaTilde[, argvals] <- betaTilde0

  ##########################################################################################
  ## Step 2
  ##########################################################################################
  if(silent == FALSE) print("Step 2: Smoothing")
  
  if (is.null(knots_manual)){
    nknots <- min(round(L/4), 35) ## number of knots for penalized splines smoothing
  } else {
    nknots <- knots_manual
  }
  # message(paste0("number of knots for penalized splines smoothing = ", nknots))
  
  argvals_manual_ext = 1 : L 
  newd <- data.frame(argvals_manual_ext = argvals_manual_ext)
  betaHat <- t(apply(betaTilde, 1, function(x) predict(gam(x ~ s(argvals_manual_ext, bs = "cr", k = (nknots + 1)), method = "REML"), newdata = newd)))
  rownames(betaHat) <- rownames(betaTilde)
  colnames(betaHat) <- 1 : L
  # plot(exp(betaHat[1, ]))
  
  
  ##########################################################################################
  ## Step 3
  ##########################################################################################
  B_boot_eff <- NULL
  if(var == TRUE){
    
    ##########################################################################################
    ## Bootstrap Inference
    ##########################################################################################
    if(silent == FALSE) print("Step 3: Bootstrap Inference")
    
    # B <- 100
    betaHat_boot <- array(NA, dim = c(nrow(betaHat), ncol(betaHat), B_boot))
    # group <- massmm[[1]]$group
    group <- all.vars(terms(formula))[length( all.vars(terms(formula)))]
    ID.number <- unique(data[,group])
    # pb <- progress_bar$new(total = B_boot)
    for(boots in 1:B_boot){ # boots <- 30
      # pb$tick()
      sample.ind <- sample(1:length(ID.number), size = length(ID.number), replace = TRUE)
      dat.ind <- c()
      for(i in 1:length(ID.number)){
        dat.ind <- c(dat.ind, which(data[,group] == ID.number[sample.ind[i]])) ## subject-level bootstrap
      }
      set.seed(boots)
      try({
        fit_boot <- lfosr3s(formula = formula, data = data[dat.ind,], family = family, 
                            argvals_manual = argvals_manual,
                            var = FALSE, parallel = parallel, silent = TRUE,
                            knots_manual = knots_manual,
                            msg_prefix = paste0("[boot_idx = ", boots, "] "))
        betaHat_boot[,,boots] <- fit_boot$betaHat
      })
    }
    betaHat_boot_keep_idx <- apply(betaHat_boot, 3, function(mat) !any(is.na(mat)))
    betaHat_boot <- betaHat_boot[,,betaHat_boot_keep_idx]
    
    ## obtain bootstrap variance
    betaHat.var <- array(NA, dim = c(L,L,nrow(betaHat)))
    for(r in 1:nrow(betaHat)){
      betaHat.var[,,r] <- 1.2 * var(t(betaHat_boot[r,,])) ## account for within-subject correlation
    }
    
    ## obtain qn to construct joint CI using the fast approach
    qn <- rep(NA, length = nrow(betaHat))
    N <- length(unique(data[, group])) ## sample size in simulation-based approach
    for(i in 1:length(qn)){ # i <- 1
      try({
        est_bs <- t(betaHat_boot[i,,])
        fit_fpca <- fpca.face(est_bs)
        ## extract estimated eigenfunctions/eigenvalues
        phi <- fit_fpca$efunctions
        lambda <- fit_fpca$evalues
        K <- length(fit_fpca$evalues)
        ## simulate random coefficients 
        theta <- matrix(rnorm(N*K), nrow=N, ncol=K) # generate independent standard normals 
        if (K == 1){
          theta <- theta * sqrt(lambda) # scale to have appropriate variance
        } else {
          theta <- theta %*% diag(sqrt(lambda)) # scale to have appropriate variance
        }
        X_new <- theta %*% t(phi) # simulate new functions
        x_sample <- X_new + t(fit_fpca$mu %o% rep(1,N)) # add back in the mean function
        Sigma <- apply(x_sample, 2, var)
        x_mean <- colMeans(est_bs)
        un <- rep(NA, N) 
        for(j in 1:N){
          un[j] <- max(abs((x_sample[j,] - x_mean)/sqrt(Sigma)))
        }
        qn[i] <- quantile(un, 0.95)
      })
    }
    return(list(betaHat = betaHat, betaHat.var = betaHat.var, qn = qn, 
                B_boot_eff = sum(betaHat_boot_keep_idx)))
    
  } else{ # no var 
    return(list(betaHat = betaHat))
  }
  
}
