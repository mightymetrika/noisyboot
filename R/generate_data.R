#' Generate Data for ANOVA
#'
#' This function generates data for ANOVA (Analysis of Variance) with the option
#' for skewed distributions. It can generate data for multiple groups with
#' different means, standard deviations, and skewness parameters.
#'
#' @param M A numeric vector of means for each group.
#' @param S A numeric vector of standard deviations for each group.
#' @param Sk A numeric vector of skewness parameters for each group, or NULL for
#'           no skewness. If NULL, normal distributions will be used for all
#'           groups.
#' @param n A numeric vector of sample sizes for each group.
#'
#' @return A data frame with two columns:
#'   \item{x}{The generated data values}
#'   \item{grp}{The group labels (F1, F2, etc.)}
#'
#' @details All input vectors (M, S, and n) must have the same length,
#'          representing the number of groups. If Sk is provided, it must also
#'          have the same length. If Sk is NULL, normal distributions are used
#'          for all groups.
#'
#' @examples
#' # Generate data for 3 groups with no skewness
#' data1 <- generate_data_anova(M = c(0, 2, 4), S = c(1, 1, 1),
#'                              n = c(10, 10, 10))
#'
#' # Generate data for 2 groups with skewness
#' data2 <- generate_data_anova(M = c(0, 3), S = c(1, 2), Sk = c(0.5, -0.5),
#'                              n = c(15, 20))
#'
#' @export
generate_data_anova <- function(M, S, Sk = NULL, n) {
  # Check if all non-NULL input vectors have the same length
  lengths <- sapply(list(M, S, n), length)
  if (length(unique(lengths)) != 1) {
    stop("The 'M', 'S', and 'n' input vectors must have the same length.")
  }

  num_groups <- length(M)

  # If Sk is NULL, create a vector of zeros with the same length as other inputs
  if (is.null(Sk)) {
    Sk <- rep(0, num_groups)
  } else if (length(Sk) != num_groups) {
    stop("Sk must be NULL or have the same length as other input vectors.")
  }

  # Generate normal or skew normal data
  generate_data <- function(n, mean, sd, skew) {
    if (skew == 0) {
      stats::rnorm(n, mean, sd)
    } else {
      fGarch::rsnorm(n, mean, sd, xi = skew)
    }
  }

  # Generate data for each group
  Fg <- mapply(generate_data, n = n, mean = M, sd = S, skew = Sk, SIMPLIFY = FALSE)

  # Create the data frame
  df <- data.frame(
    x = unlist(Fg),
    grp = rep(paste0("F", seq_along(M)), n)
  )

  return(df)
}

#' Generate Correlated Data for Two-Group ANOVA
#'
#' This function generates correlated data for a two-group ANOVA (Analysis of
#' Variance) with the option for skewed distributions. It creates data for two
#' groups with specified means, standard deviations, skewness parameters, and
#' correlation.
#'
#' @param M A numeric vector of length 2 specifying the means for each group.
#' @param S A numeric vector of length 2 specifying the standard deviations for
#'          each group.
#' @param Sk A numeric vector of length 2 specifying the skewness parameters for
#'           each group, or NULL for no skewness. If NULL, normal distributions
#'           will be used.
#' @param n A numeric vector of length 2 specifying the sample sizes for each
#'          group.
#' @param correl A single numeric value specifying the correlation between the
#'               two groups.
#'
#' @return A data frame with two columns:
#'   \item{x}{The generated data values}
#'   \item{grp}{The group labels (F1 or F2)}
#'
#' @details All input vectors (M, S, and n) must have length 2, representing the
#'          two groups. If Sk is provided, it must also have length 2. If Sk is
#'          NULL, normal distributions are used for both groups. The function uses
#'          MASS::mvrnorm for normal distributions and sn::rmsn for skew-normal
#'          distributions.
#'
#' @examples
#' # Generate correlated data for 2 groups with no skewness
#' data1 <- generate_data_anova_cor(M = c(0, 2), S = c(1, 1), n = c(10, 10),
#'                                  correl = 0.5)
#'
#' @export
generate_data_anova_cor <- function(M, S, Sk = NULL, n, correl) {
  # Check if all non-NULL input vectors have length 2
  if (!all(sapply(list(M, S, n), length) == 2)) {
    stop("The 'M', 'S', and 'n' input vectors  must have length 2.")
  }

  if (length(correl) != 1) {
    stop("correl must be a single value.")
  }

  # If Sk is NULL, create a vector of zeros
  if (is.null(Sk)) {
    Sk <- c(0, 0)
  } else if (length(Sk) != 2) {
    stop("Sk must be NULL or have length 2.")
  }

  # Generate normal or skew normal data
  generate_data <- function(n, M, S, Sk, correl) {
    Sigma <- matrix(c(S[1]^2, S[1]*S[2]*correl, S[1]*S[2]*correl, S[2]^2), 2, 2)
    if (all(Sk == 0)) {
      # Use MASS::mvrnorm for normal distribution
      return(MASS::mvrnorm(sum(n), mu = M, Sigma = Sigma))
    } else {
      # Use sn::rmsn for skew normal distribution
      cpM <- list(mean=M, var.cov=Sigma, gamma1=Sk)
      dpM <- sn::cp2dp(cpM, family="SN")
      return(sn::rmsn(sum(n), dp=dpM))
    }
  }

  # Generate the data
  FP <- generate_data(n, M, S, Sk, correl)

  # Create the data frame in long format
  df <- data.frame(
    x = c(FP[1:n[1], 1], FP[1:n[2], 2]),
    grp = rep(c("F1", "F2"), times = n)
  )

  # Order the data frame by group
  df <- df[order(df$grp), ]
  rownames(df) <- NULL

  return(df)
}

#' Generate Data for Generalized Linear Models
#'
#' This function generates data for various types of Generalized Linear Models
#' (GLMs), including logistic, linear, and Poisson regression. It can generate
#' data for a single group or for two groups (main and control) with an optional
#' treatment effect.
#'
#' @param n_main Integer. Number of observations in the main group.
#' @param n_covariates Integer. Number of covariates to generate.
#' @param true_coef_main Numeric vector. True coefficients for the main group.
#' @param n_control Integer or NULL. Number of observations in the control group.
#'                  If NULL, no control group is generated.
#' @param true_coef_control Numeric vector or NULL. True coefficients for the
#'                          control group. Required if n_control is not NULL.
#' @param treatment_effect Numeric or NULL. The effect of treatment (difference
#'                         between groups).
#' @param model Character. The type of GLM: "logistic", "linear", or "poisson".
#' @param skewness_main Numeric vector or NULL. Skewness parameters for covariates
#'                      in the main group.
#' @param skewness_control Numeric vector or NULL. Skewness parameters for
#'                         covariates in the control group.
#' @param Sigma_main Matrix or NULL. Covariance matrix for covariates in the main
#'                   group.
#' @param Sigma_control Matrix or NULL. Covariance matrix for covariates in the
#'                      control group.
#'
#' @return A data frame containing:
#'   \item{y}{The generated outcome variable}
#'   \item{X1, X2, ...}{The generated covariates}
#'   \item{group}{Group indicator (1 for main, 0 for control)}
#'
#' @details
#' If n_covariates is 0, only an intercept term is included.
#' If Sigma is not provided, an identity matrix is used for the covariance
#' structure. If skewness is not provided, normal distribution is used for
#' generating covariates.
#'
#' @examples
#' # Generate data for logistic regression with one group
#' data_single <- generate_data_glm(
#'   n_main = 100,
#'   n_covariates = 3,
#'   true_coef_main = c(0.5, -0.3, 0.2),
#'   model = "logistic"
#' )
#'
#' # Generate data for linear regression with two groups and treatment effect
#' data_two_groups <- generate_data_glm(
#'   n_main = 100,
#'   n_control = 100,
#'   n_covariates = 2,
#'   true_coef_main = c(0.5, -0.3),
#'   true_coef_control = c(0.4, -0.2),
#'   treatment_effect = 0.5,
#'   model = "linear",
#'   skewness_main = NULL,
#'   skewness_control = NULL)
#'
#' @export
generate_data_glm <- function(n_main, n_covariates, true_coef_main,
                              n_control = NULL, true_coef_control = NULL,
                              treatment_effect = NULL,
                              model = c("logistic", "linear", "poisson"),
                              skewness_main = NULL, skewness_control = NULL,
                              Sigma_main = NULL, Sigma_control = NULL) {
  model <- match.arg(model)

  # Helper function to generate covariate data
  generate_covariate_data <- function(n, n_covariates, Sigma, skewness = NULL) {
    if (n_covariates == 0) return(matrix(1, nrow = n, ncol = 1))  # Intercept only

    if (is.null(Sigma)) Sigma <- diag(n_covariates)

    if (is.null(skewness)) {
      X <- MASS::mvrnorm(n, mu = rep(0, n_covariates), Sigma = Sigma)
    } else {
      cpM <- list(mean = rep(0, n_covariates), var.cov = Sigma, gamma1 = skewness)
      dpM <- sn::cp2dp(cpM, family = "SN")
      X <- sn::rmsn(n, dp = dpM)
    }

    return(X)
  }

  # Helper function to generate outcome variable
  generate_outcome <- function(X, true_coef, model, treatment_effect = NULL, group = NULL) {
    if(is.null(true_coef)){
      true_coef = c(0)
    }

    eta <- X %*% true_coef
    if (!is.null(treatment_effect) & !is.null(group)) {
      eta <- eta + group * treatment_effect
    }

    if (model == "logistic") {
      p <- stats::plogis(eta)
      y <- stats::rbinom(nrow(X), size = 1, prob = p)
    } else if (model == "linear") {
      y <- eta + stats::rnorm(nrow(X), mean = 0, sd = 1)
    } else if (model == "poisson") {
      mu <- exp(eta)
      y <- stats::rpois(nrow(X), lambda = mu)
    }

    return(y)
  }

  # Generate data for main group
  X_main <- generate_covariate_data(n_main, n_covariates, Sigma_main, skewness_main)
  y_main <- generate_outcome(X_main, true_coef_main, model, treatment_effect, group = 1)
  data_main <- data.frame(y = y_main, X_main)
  if(n_covariates == 0){
    colnames(data_main) <- c("y", paste0("X", 0))
  } else{
    colnames(data_main) <- c("y", paste0("X", 1:n_covariates))
  }
  data_main$group <- 1

  # Generate data for control group if specified
  if (!is.null(n_control)) {
    X_control <- generate_covariate_data(n_control, n_covariates, Sigma_control, skewness_control)
    y_control <- generate_outcome(X_control, true_coef_control, model, treatment_effect, group = 0)
    data_control <- data.frame(y = y_control, X_control)
    if(n_covariates == 0){
      colnames(data_control) <- c("y", paste0("X", 0))
    } else{
      colnames(data_control) <- c("y", paste0("X", 1:n_covariates))
    }
    data_control$group <- 0

    # Combine main and control data
    data <- rbind(data_main, data_control)
  } else {
    data <- data_main
  }

  return(data)
}
