test_that("anova_sim works", {

  sim_results <- anova_sim(M = c(5, 6, 7), S = c(1, 2, 2), n = c(30, 30, 30),
                           .sd = 0.5, .xi = 0.2, n_simulations = 10, R = 20)
  expect_equal(length(sim_results), 2)
})

# test_taht("glm_sim works", {
#
#   data_two_groups <- generate_data_glm(
#     n_main = 100,
#     n_control = 100,
#     n_covariates = 2,
#     true_coef_main = c(0.5, -0.3),
#     true_coef_control = c(0.4, -0.2),
#     treatment_effect = 0.5,
#     model = "linear",
#     skewness_main = NULL,
#     skewness_control = NULL)
#
#   boot_func <- function(data, indices = NULL) {
#     if (is.null(indices)) {
#       sub_data <- data
#     } else {
#       sub_data <- data[indices, ]
#     }
#     model <- stats::lm(x ~ factor(grp), sub_data)
#     return(stats::anova(model)$`F value`[1])
#   }
#
#   nb_vals <- noisyboot(data_two_groups, func = boot_func, R = 20,
#                        .sd = 0.5, .xi = 1)
#
#
# })
