context("Stats: descriptive stats for quantitative vars")
library(qacr)

xs <- c(5, 4, 100, 2, -10)
ys <- c(5, 4, 100, 2, -10, 5)

mpg_n_mean_sd_2 <- tibble(am = c(0, 0, 1, 1),
                            gear = c(3, 4, 4, 5),
                            n = c(15, 4, 8, 5),
                            mean = c(16.11, 21.05, 26.27, 21.38),
                            sd = c(3.37, 3.07, 5.41, 6.66)) %>%
  mutate_at(vars(am:gear), as_factor) %>%
  mutate_at(vars(n:sd), as.double)

mpg_min_max_median_2 <- tibble(am = c(0, 0, 1, 1),
                          gear = c(3, 4, 4, 5),
                          min = c(10.4, 17.8, 21, 15),
                          max = c(21.5, 24.4, 33.9, 30.4),
                          median = c(15.5, 21, 25.05, 19.7)) %>%
  mutate_at(vars(am:gear), as_factor) %>%
  mutate_at(vars(min:median), as.double)




test_that("simple user-defined statistics is working", {
  expect_equal(median(xs), 4)
  expect_equal(median(ys), 4.5)
  expect_equal(n(xs), 5)
  expect_equal(n(ys), 6)
})


test_that("auxilliary summary functions is working", {
  expect_equal(my_sum(mtcars, mpg, "median"), data.frame(median = 19.2))
  expect_equal(my_sum(mtcars, hp, "median"), data.frame(median = 123))

  expect_equal(my_sums(mtcars, mpg, c("n", "median")), data.frame(n = 32, median = 19.2))
  expect_equal(my_sums(mtcars, hp, c("sd", "median")) %>% round(2), data.frame(sd = 68.56, median = 123))
})



test_that("stats gives useful error message when given incorrect input", {
  expect_error(stats(mtcars %>% mutate(cyl = as.factor(cyl)),
                     cyl, c("min", "max", "median"),
                     na.rm = TRUE, digits = 0, am, gear),
               "is not numeric")
})



test_that("stats implements default values", {
  expect_equal(stats(mtcars, mpg, am, gear), mpg_n_mean_sd_2)
  expect_equal(stats(mtcars, mpg, digits = 2, am, gear), mpg_n_mean_sd_2)
  expect_equal(stats(mtcars, mpg, statistics = c("n", "mean", "sd"), am, gear), mpg_n_mean_sd_2)
})

test_that("stats outputs correct answer", {
  expect_equal(stats(mtcars, mpg, statistics = c("n", "mean", "sd"),
                     na.rm = TRUE, digits = 0, am, gear),
               mpg_n_mean_sd_2 %>% mutate_if(is.double, round, 0))
  expect_equal(stats(mtcars, mpg, statistics = c("min", "max", "median"),
                     na.rm = TRUE, digits = 2, am, gear),
               mpg_min_max_median_2)
  expect_equal(stats(mtcars, mpg, am, digits = 2,
                     statistics = c("n", "mean", "sd"),
                     na.rm = TRUE, gear),
               mpg_n_mean_sd_2)


  mpg_w_missing <- mpg_n_mean_sd_2
  mpg_w_missing[1, 4] <- NA
  mpg_w_missing[1, 5] <- NaN
  expect_equal(stats(mtcars %>% mutate(mpg = ifelse(mpg < 15, NA, mpg)),
                     mpg, statistics = c("n", "mean", "sd"),
                     na.rm = FALSE, digits = 2, am, gear),
               mpg_w_missing)

})

test_that("stats gives useful warnings for missing", {
  expect_warning(stats(mtcars %>% mutate(gear = ifelse(gear == 4, NA, gear)),
                     mpg, statistics = c("n", "mean", "sd"),
                     na.rm = FALSE, digits = 2, am, gear),
               "implicit NA")

})
