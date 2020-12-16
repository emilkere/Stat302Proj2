#' my_gapminder data
#'
#' Copy of gapminder data from gapminder package used for demo purposes in
#' Stat302 Project2.
#'
#' Excerpt of the Gapminder data on life expectancy, GDP per capita, and
#' population by country.
#' This data and documentation come from the \code{gapminder} package,
#' available as \code{\link[gapminder]{gapminder}}.
#'
#' @format The main data frame has 1704 rows and 6 variables:
#' \describe{
#'   \item{country}{factor with 142 levels}
#'   \item{continent}{factor with 5 levels}
#'   \item{year}{ranges from 1952 to 2007 in increments of 5 years}
#'   \item{lifeExp}{life expectancy at birth, in years}
#'   \item{pop}{population}
#'   \item{gdpPercap}{GDP per capita (US$, inflation-adjusted)}
#'   }
#'
#'
#' #' @source \url{http://www.gapminder.org/data/}
#' @importFrom tibble tibble
#' @examples
#' str(my_gapminder)
#' head(my_gapminder)
#' summary(my_gapminder)
#' table(my_gapminder$continent)
#' aggregate(lifeExp ~ continent, my_gapminder, median)
#' plot(lifeExp ~ year, my_gapminder, subset = country == "Cambodia", type = "b")
#' plot(lifeExp ~ gdpPercap, my_gapminder, subset = year == 2007, log = "x")
#'
#' if (require("dplyr")) {
#'   my_gapminder %>%
#'     filter(year == 2007) %>%
#'     group_by(continent) %>%
#'     summarise(lifeExp = median(lifeExp))
#'
#'   # how many unique countries does the data contain, by continent?
#'   my_gapminder %>%
#'     group_by(continent) %>%
#'     summarize(n_obs = n(), n_countries = n_distinct(country))
#'   #'
#'   # by continent, which country experienced the sharpest 5-year drop in
#'   # life expectancy and what was the drop?
#'   my_gapminder %>%
#'     group_by(continent, country) %>%
#'     select(country, year, continent, lifeExp) %>%
#'     mutate(le_delta = lifeExp - lag(lifeExp)) %>%
#'     summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>%
#'     filter(min_rank(worst_le_delta) < 2) %>%
#'     arrange(worst_le_delta)
#' }
"my_gapminder"
