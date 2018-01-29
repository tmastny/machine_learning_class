#+ warning = FALSE, message = FALSE
library(ISLR)
library(tidyverse)

#' ## 1, 2
as.tibble(Wage)
nrow(Wage)
ncol(Wage)

class(Wage$year)

#' ## 3
quantile(Wage$wage, probs = c(0,.9))

#' ## 4
table(Wage$maritl[Wage$wage >= 154.70360])

#' ## 5
Wage %>%
  group_by(education) %>%
  summarise(median_wage = median(wage))

#' ## 6
age_wage_slope_per_year <- function(data, year) {
  return(coef(lm(log(wage) ~ age, data = data[data$year == year,]))[2])
}
age_wage_slope_per_year(Wage, 2006)

summary(lm(log(wage) ~ age, data = Wage[Wage$year == 2006,]))