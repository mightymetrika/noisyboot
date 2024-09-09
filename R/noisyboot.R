#' Perform Bootstrap with Noise
#'
#' This function performs a bootstrap analysis with added noise. It allows for
#' skewed noise and custom noise transformations, making it useful for
#' sensitivity analyses and robust statistical inference.
#'
#' @param dat A data frame or vector containing the dataset to be bootstrapped.
#' @param func A function that computes the statistic of interest. Default is the mean.
#'   This function should take the dataset as its argument and return the statistic(s).
#' @param .sd Numeric. The standard deviation of the noise to be added. Default is 1.
#' @param .xi Numeric. The skewness parameter for the noise. If 0 or NULL, normal
#'   noise is used. Positive values of .xi skew the noise for samples towards the
#'   reference value while negative of .xi skew the noise away from the reference
#'   value . Default is 1.5. The reference value is the function defined in func
#'   applied to the original dataset.
#' @param func_noise A function to transform the noise before adding it to the data.
#'   Default is the identity function.
#' @param R Integer. The number of bootstrap replicates. Default is 500.
#' @param ... Additional arguments passed to boot::boot().
#'
#' @return An object of class "boot", as returned by boot::boot().
#'
#' @details
#' The function adds noise to each bootstrap sample before computing the statistic.
#' The noise is generated using a skew-normal distribution if .xi is non-zero,
#' otherwise it uses a normal distribution. The noise can be further transformed
#' using the func_noise parameter.
#'
#' @examples
#' # Basic usage with cars dataset
#' result <- noisyboot(cars, func = \(x)mean(x$speed), R = 1000)
#' print(result)
#'
#' # Using custom noise function
#' result_custom <- noisyboot(cars, func = \(x)mean(x$speed),
#'                            func_noise = \(x)sqrt(abs(x)), R = 1000)
#'
#' # With vector input
#' vec_data <- rnorm(100)
#' result_vec <- noisyboot(vec_data, func = mean, .xi = 0, R = 1000)
#'
#' @export
noisyboot <- function(dat, func = \(x)mean(x, na.rm = TRUE), .sd = 1, .xi = 1.5,
                      func_noise = \(x)x, R = 500, ...){

  # get the reference value by running func on the full dataset
  ref_val <- func(dat)

  # create function to pass to boot::boot()
  fnc <- function(d, i){

    # get rows i of data
    di <- if(is.data.frame(d)) d[i,] else d[i]

    # get noise for rows i
    n <- if(is.data.frame(di)) nrow(di) else length(di)
    noise <- rep(0, n)

    if (is.null(.xi) || .xi == 0){
      noise <- stats::rnorm(n, sd = .sd)
    } else {
      comp <- func(di) - ref_val
      noise[comp > 0] <- fGarch::rsnorm(sum(comp > 0), sd = .sd, xi = -.xi)
      noise[comp < 0] <- fGarch::rsnorm(sum(comp < 0), sd = .sd, xi = .xi)
      noise[comp == 0] <- stats::rnorm(sum(comp == 0), sd = .sd)
    }

    # apply func_noise to the noise
    transformed_noise <- func_noise(noise)

    # combine di and noise
    if (is.data.frame(di)) {
      din <- di
      din[, names(di)] <- din[, names(di)] + transformed_noise
    } else {
      din <- di + transformed_noise
    }

    # return function of din
    return(func(din))
  }

  # run bootstrap on data and function
  boot::boot(dat, fnc, R = R, ...)
}
