test_that("generate_data_anova works", {

  # generate ANOVA data
  grp_dat <- generate_data_anova(M = c(5,6,7),
                                 S = c(1,3,2),
                                 Sk = c(-1.5, 0, 1.5),
                                 n = c(6,6,6)) |>
    suppressWarnings()

  stats::anova(stats::lm(x ~ factor(grp), grp_dat))$`F value`[1]
  stats::anova(stats::lm(x ~ factor(grp), grp_dat))[["Pr(>F)"]][1]


  expect_equal(nrow(grp_dat), 18)
  expect_equal(length(unique(grp_dat$grp)), 3)

  # generate ANOVA data with no skew
  grp_dat <- generate_data_anova(M = c(5,6,7),
                                 S = c(1,3,2),
                                 Sk = NULL,
                                 n = c(6,6,6)) |>
    suppressWarnings()

  expect_equal(nrow(grp_dat), 18)
  expect_equal(length(unique(grp_dat$grp)), 3)
})


test_that("generate_data_anova_cor works", {

  # Test with skewed data
  grp_dat <- generate_data_anova_cor(M = c(5, 6), S = c(1, 1.2),
                                     Sk = c(0.9, 0.6), n = c(10, 12),
                                     correl = 0.7)

  expect_equal(nrow(grp_dat), 22)
  expect_equal(length(unique(grp_dat$grp)), 2)

  # Test with non-skewed data
  grp_dat2 <- generate_data_anova_cor(M = c(5, 20) , S = c(1, 1.2), Sk = c(0, 0), n = c(10, 12), correl = 0.7)

  expect_equal(nrow(grp_dat2), 22)
  expect_equal(length(unique(grp_dat2$grp)), 2)

  # Test with non-skewed data (using NULL skew)
  grp_dat3 <- generate_data_anova_cor(M = c(5, 20) , S = c(1, 1.2), n = c(10, 12), correl = 0.7)

  expect_equal(nrow(grp_dat3), 22)
  expect_equal(length(unique(grp_dat3$grp)), 2)
})

test_that("generate_data_glm works",{

  data_single <- generate_data_glm(
    n_main = 100,
    n_covariates = 3,
    true_coef_main = c(0.5, -0.3, 0.2),
    model = "logistic"
  )

  expect_equal(nrow(data_single), 100)
  expect_equal(ncol(data_single), 5)
  expect_equal(length(unique(data_single$group)), 1)

  # Example usage for two groups with treatment effect
  data_two_groups <- generate_data_glm(
    n_main = 100,
    n_control = 100,
    n_covariates = 2,
    true_coef_main = c(0.5, -0.3),
    true_coef_control = c(0.4, -0.2),
    treatment_effect = 0.5,
    model = "linear",
    skewness_main = NULL,
    skewness_control = NULL)

    expect_equal(nrow(data_two_groups), 200)
    expect_equal(ncol(data_two_groups), 4)
    expect_equal(length(unique(data_two_groups$group)), 2)

})
