#' @title FiveThirtyEight Hate Crimes Dataset
#' @description This dataset contains the data that supports the article,
#' "Higher Rates of Hate Crimes Are Tied to Income Inequality", published by
#' FiveThirtyEight on 23 January, 2017, authored by Maimuna Majumder.
#' @format A data frame with 51 rows and 12 variables:
#' \describe{
#'   \item{\code{state}}{character State name}
#'   \item{\code{median_household_income}}{integer Median household income, 2016}
#'   \item{\code{share_unemployed_seasonal}}{double Share of the population that is unemployed}
#'   \item{\code{share_population_in_metro_areas}}{double Share of the population that lives in metropolitan areas, 2015}
#'   \item{\code{share_population_with_high_school_degree}}{double Share of adults 25 and older with a high-school degree, 2009}
#'   \item{\code{share_non_citizen}}{double Share of the population that are not U.S. citizens, 2015}
#'   \item{\code{share_white_poverty}}{double Share of white residents who are living in poverty, 2015}
#'   \item{\code{gini_index}}{double Gini Index, 2015}
#'   \item{\code{share_non_white}}{double Share of the population that is not white, 2015}
#'   \item{\code{share_voters_voted_trump}}{double Share of 2016 U.S. presidential voters who voted for Donald Trump}
#'   \item{\code{hate_crimes_per_100k_splc}}{double Hate crimes per 100,000 population, Southern Poverty Law Center, Nov. 9-18, 2016}
#'   \item{\code{avg_hatecrimes_per_100k_fbi}}{double Average annual hate crimes per 100,000 population, FBI, 2010-2015}
#'}
#' @docType data
#' @keywords datasets
#' @name hate_crimes
#' @usage hate_crimes
"hate_crimes"
