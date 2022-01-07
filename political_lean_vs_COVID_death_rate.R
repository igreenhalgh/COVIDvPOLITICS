# Packages ----
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, vroom, magrittr, car)

# Get county Presidential 2020 data ----
url = "https://dataverse.harvard.edu/api/access/datafile/4819117"

data <- vroom(url) %>%
  filter(year == 2020, office == "PRESIDENT") %>%
  select(fips = county_fips, party, candidatevotes, totalvotes)

rm(url)


# Calculate lean ----
data2 <-
  data %>% mutate(percentvotes = candidatevotes / totalvotes) %>% drop_na()

data3 <- data2 %>% pivot_wider(
  names_from = party,
  values_from = percentvotes,
  values_fn = sum,
  values_fill = 0) %>%
  group_by(fips) %>%
  summarise(dem = sum(DEMOCRAT), rep = sum(REPUBLICAN))

countylean <- data3 %>% transmute(fips, lean = dem - rep)

rm(data2, data3)


## Visualize county lean ----
ggplot(countylean) +
  aes(x = lean) +
  geom_histogram(bins = 30L,
                 fill = "#112446") +
  labs(
    x = "Republican to Democrat lean",
    y = "Number of Counties",
    title = "2020 Presidential Election",
    subtitle = "Lean by County"
  ) +
  theme_minimal()


# Get county COVID data ----
url <-
  "https://api.covidactnow.org/v2/counties.csv?apiKey=10c563b961284a51aca3a7b06c675998"

coviddata <- vroom(url)
coviddata %<>% mutate(deathrate = actuals.deaths / population)


coviddata2 <-
  coviddata %>% select(fips,
                       metrics.vaccinationsCompletedRatio,
                       deathrate) %>% drop_na()


# Calculate vaccination ratio ----
library(scales)
coviddata2$completevaxratio <-
  rescale(
    coviddata2$metrics.vaccinationsCompletedRatio,
    to = c(-1, 1),
    from = range(coviddata2$metrics.vaccinationsCompletedRatio)
  )
coviddata2 %<>% select(fips, completevaxratio, deathrate)
coviddata2$deathrate <- rescale(coviddata2$deathrate,
                                to = c(0, 1),
                                from = range(coviddata2$deathrate))


# Join tables ----
join <- inner_join(countylean, coviddata2)


car::scatterplot(y ~ x,
                 xlab = "Political Lean (R to D)",
                 ylab = "Complete Vaccination Ratio",
                 col = "black")


# Death rate visualization ----
x <- join$lean
y <- join$deathrate

ggplot(join) +
  aes(x = lean,
      y = deathrate) +
  geom_point(shape = "circle open",
             size = 2L,
             colour = "#112446") +
  geom_smooth(span = 0.74, method = "gam") +
  labs(
    x = "Political Lean (Republican to Democrat)",
    y = "COVID Death Rate",
    title = "County Death Rate by Political Lean",
    subtitle = paste0(
      "As of ",
      format(Sys.Date(), "%m/%d/%Y"),
      ", with 2020 General Election results for political lean"
    ),
    caption = "Sources: dataverse.harvard.edu for election results and covidactnow.org for COVID data"
  ) +
  ggtech::theme_tech(theme = "facebook") +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold"),
    axis.title.y = element_text(size = 13L),
    axis.title.x = element_text(size = 13L)
  )
