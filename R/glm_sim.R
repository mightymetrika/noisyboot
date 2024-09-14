# glm_sim <- function(n_main, n_covariates, true_coef_main,
#                     n_control = NULL, true_coef_control = NULL,
#                     treatment_effect = NULL,
#                     model = c("logistic", "linear", "poisson"),
#                     skewness_main = NULL, skewness_control = NULL,
#                     Sigma_main = NULL, Sigma_control = NULL,
#                     boot_func = NULL, conf.level = 0.95, R = 1000,
#                     .sd, .xi, noise_trans = \(x)x, n_simulations = 100) {
#
#   get_result <- function() {
#     # Generate data
#     df <- generate_data_glm(n_main = n_main, n_covariates = n_covariates,
#                             true_coef_main = true_coef_main,
#                             n_control = n_control,
#                             true_coef_control = true_coef_control,
#                             treatment_effect = treatment_effect, model = model,
#                             skewness_main = skewness_main,
#                             skewness_control = skewness_control,
#                             Sigma_main = Sigma_main,
#                             Sigma_control = Sigma_control)
#
#
#     # Update formula to include group indicator
#     if (n_covariates > 0) {
#       formula <- stats::as.formula(paste("y ~ group +", paste(colnames(data)[-(1:2)], collapse = " + ")))
#     } else {
#       formula <- stats::as.formula("y ~ group")
#     }
#
#     # Set default boot_func if not provided
#     if (is.null(boot_func)) {
#       boot_func <- function(data, indices = NULL) {
#         if (is.null(indices)) {
#           sub_data <- data
#         } else {
#           sub_data <- data[indices, ]
#         }
#         fmod <- stats::lm(formula, sub_data)
#         if (model %in% c("logistic", "poisson")) {
#           glm_p_value <- summary(fmod)$coefficients["group", "Pr(>|z|)"]
#         } else if (model == "linear") {
#           glm_p_value <- summary(fmod)$coefficients["group", "Pr(>|t|)"]
#         }
#         return(stats::anova(model)$`F value`[1])
#       }
#     } else {
#       # Wrap the provided boot_func to handle both indexed and non-indexed calls
#       original_boot_func <- boot_func
#       boot_func <- function(data, indices = NULL) {
#         if (is.null(indices)) {
#           return(original_boot_func(data))
#         } else {
#           return(original_boot_func(data, indices))
#         }
#       }
#     }
#
#     # Fit the models
#     fit_glm <- tryCatch({
#       stats::glm(formula = formula, family = if (model == "logistic") stats::binomial() else if (model == "linear") stats::gaussian() else stats::poisson(), data = data)
#     }, error = function(e) {
#       warning("GLM model fitting failed: ", e$message)
#       return(NULL)
#     })
#
#     fit_boots <- tryCatch({
#       boot::boot(data, statistic = boot_func, R = R)
#     }, , error = function(e) {
#       warning("boot fitting failed: ", e$message)
#       return(NULL)
#     })
#
#     noise_result <- noisyboot(df, func = boot_func, R = R,
#                               .sd = .sd, .xi = .xi, func_noise = noise_trans)
#
#     fit_noise <- tryCatch({
#       noisyboot(df, func = boot_func, R = R,
#                 .sd = .sd, .xi = .xi, func_noise = noise_trans)
#     }, error = function(e) {
#       warning("noisyboot fitting failed: ", e$message)
#       return(NULL)
#     })
#
#     return(fit_noise)
#
#     tryCatch({
#       glm_coef <- fit_glm$coefficients["group"]
#       glm_se <- summary(fit_glm)$coefficients["group", "Std. Error"]
#       if (model %in% c("logistic", "poisson")) {
#         glm_p_value <- summary(fit_glm)$coefficients["group", "Pr(>|z|)"]
#       } else if (model == "linear") {
#         glm_p_value <- summary(fit_glm)$coefficients["group", "Pr(>|t|)"]
#       }
#
#
#       boot_p_value <- mean(fit_boots$t[, 1] >= fit_boots$t0)
#       noise_p_value <- mean(fit_noise$t[, 1] >= fit_noise$t0)
#
#       c(
#         glm_estimate = glm_coef,
#         glm_p_value = glm_p_value,
#         boot_estimate = ,
#         boot_p_value = ,
#         glm_se = glm_se
#       )
#     }, error = function(e) {
#       return(NULL)
#     })
#
#   }
#
#   # Run n_simulations
#   results <- replicate(n_simulations, get_result(), simplify = FALSE)
#
#   # Calculate proportions and success rates
#   proportions <- colMeans(do.call(rbind, lapply(results, \(x) x$results)), na.rm = TRUE)
#   success_rates <- colMeans(do.call(rbind, lapply(results, \(x) x$success)), na.rm = TRUE)
#
#   return(list(
#     results = as.data.frame(t(proportions)),
#     success = as.data.frame(t(success_rates))
#   ))
# }
