anova_sim <- function(M, S, Sk = NULL, n, correl = NULL,
                      boot_func = NULL, conf.level = 0.95, R = 1000,
                      .sd, .xi, noise_trans = \(x)x, n_simulations = 100) {

  get_result <- function() {
    # Generate data
    if (is.null(correl)) {
      df <- generate_data_anova(M = M, S = S, Sk = Sk, n = n)
    } else {
      df <- generate_data_anova_cor(M = M, S = S, Sk = Sk, n = n, correl = correl)
    }

    # Set default boot_func if not provided
    if (is.null(boot_func)) {
      boot_func <- function(data, indices = NULL) {
        if (is.null(indices)) {
          sub_data <- data
        } else {
          sub_data <- data[indices, ]
        }
        model <- stats::lm(x ~ factor(grp), sub_data)
        return(stats::anova(model)$`F value`[1])
      }
    } else {
      # Wrap the provided boot_func to handle both indexed and non-indexed calls
      original_boot_func <- boot_func
      boot_func <- function(data, indices = NULL) {
        if (is.null(indices)) {
          return(original_boot_func(data))
        } else {
          return(original_boot_func(data, indices))
        }
      }
    }

    # Test p-value for each method
    anov <- tryCatch({
      stats::anova(stats::lm(x ~ factor(grp), df))$`Pr(>F)`[1] <= 1 - conf.level
    }, error = function(e) NA)

    kw <- tryCatch({
      stats::kruskal.test(x ~ factor(grp), df)$p.value <= 1 - conf.level
    }, error = function(e) NA)

    boots <- tryCatch({
      boot_result <- boot::boot(df, statistic = boot_func, R = R)
      p_value <- mean(boot_result$t[, 1] >= boot_result$t0)
      p_value <= 1 - conf.level
    }, error = function(e) NA)

    noise_result <- noisyboot(df, func = boot_func, R = R,
                              .sd = .sd, .xi = .xi, func_noise = noise_trans)

    noise <- tryCatch({
      noise_result <- noisyboot(df, func = boot_func, R = R,
                                .sd = .sd, .xi = .xi, func_noise = noise_trans)
      p_value <- mean(noise_result$t[, 1] >= noise_result$t0)
      p_value <= 1 - conf.level
    }, error = function(e) NA)

    # Returning a named list
    return(
      list(
        results = c(anov = anov, kw = kw, boots = boots, noise = noise),
        success = c(anov = !is.na(anov), kw = !is.na(kw),
                    boots = !is.na(boots), noise = !is.na(noise))
      )
    )
  }

  # Run n_simulations
  results <- replicate(n_simulations, get_result(), simplify = FALSE)

  # Calculate proportions and success rates
  proportions <- colMeans(do.call(rbind, lapply(results, \(x) x$results)), na.rm = TRUE)
  success_rates <- colMeans(do.call(rbind, lapply(results, \(x) x$success)), na.rm = TRUE)

  return(list(
    results = as.data.frame(t(proportions)),
    success = as.data.frame(t(success_rates))
  ))
}
