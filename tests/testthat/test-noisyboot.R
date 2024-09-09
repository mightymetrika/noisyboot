test_that("noisyboot works with cars", {

  # run noisyboot on cars and transform the noise using the func_noise parameter
  nb_cars <- noisyboot(cars, func = \(x)mean(x$speed), R = 1000,
                       .sd = 0.5, .xi = 1, func_noise = \(x)sqrt(abs(x)))

  expect_s3_class(nb_cars, "boot")

})

test_that("noisyboot works with random vector", {

  # generate random values
  vals <- rnorm(1000)

  # run noisyboot on random values
  nb_vals <- noisyboot(vals, func = \(x)mean(x), R = 1000,
                       .sd = 0.5, .xi = 1)

  expect_s3_class(nb_vals, "boot")
})
