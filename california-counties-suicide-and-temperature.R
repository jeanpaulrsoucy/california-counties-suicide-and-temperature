### The Effect of Average Temperature on Suicide Rates in Five Urban California Counties, 1999–⁠2019: An Ecological Time Series Analysis ###
# Sierra Cheng, Rebecca Plouffe, Stephanie M. Nanos, Mavra Qamar, David N. Fisman, and Jean-Paul R. Soucy

# set random seed for reproducible results
set.seed(1613277343)

# load packages
library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(purrr) # data manipulation
library(ggplot2) # plot data
library(ggpubr) # themes for ggplot
library(cowplot) # join plots
library(mgcv) # GAM models
library(broom) # tidy model output

# create directories for outputs
dir.create("figs", showWarnings = FALSE)
dir.create("tabs", showWarnings = FALSE)

### LOAD DATA ###

# load mortality data

## load data
death <- read.table("data/cdc_mortality.txt",
                    header = TRUE,
                    sep = "\t",
                    nrows = 14616, # read up to footnotes
                    stringsAsFactors = FALSE)

## print counties by proportion of monthly suicide death counts suppressed
## (suppressed when < 10 suicide deaths in the month)
death %>%
  select(County, Deaths) %>%
  mutate(Suppressed = ifelse(Deaths == "Suppressed", 1, 0)) %>%
  select(-Deaths) %>%
  group_by(County) %>%
  summarize(Suppressed = mean(Suppressed) * 100, .groups = "drop") %>%
  arrange(Suppressed)

## list 5 counties with under 10% suppressed observations in order of FIPS code
counties <- c(
  "Los Angeles County, CA", # FIPS: 037
  "Orange County, CA", # FIPS: 059
  "Riverside County, CA", # FIPS: 065
  "San Bernardino County, CA", # FIPS: 071
  "San Diego County, CA" # FIPS: 073
)

## process mortality data and filter to the above 5 counties
death <- death %>%
  ## add county FIPS code for later joining
  mutate(county_code = substr(County.Code, start = 2, stop = 4)) %>%
  ## add integer variable for month and year
  mutate(
    year = as.integer(Year),
    month = rep(1:12, times = 58 * 21)
  ) %>%
  ## filter
  filter(County %in% c(counties)) %>%
  ## ensure data are ordered by FIPS code and date
  arrange(County.Code, year, month)

## set suppressed values as missing (NA) for now
death <- death %>%
  mutate(
    suppressed = ifelse(Deaths == "Suppressed", 1, 0), # label suppressed values
    deaths = as.integer(Deaths)
    )

# load climate data

## define desired years of data
years <- paste(1999:2019, collapse = "|")

## define desired counties FIPS
fips <- paste(c("037", "059", "065", "071", "073"), collapse = "|")

## load mean temperature
t_avg <- read.table("data/t_avg.txt",
                       header = FALSE,
                       sep = "\n",
                       stringsAsFactors = FALSE) %>%
  ## split columns
  separate(col = 1, into = c("id", month.abb), sep = "\\s+") %>%
  ### keep only relevant data
  filter(., grepl(paste(c("^04", "(", fips, ")", "02", "(", years, ")$"), collapse = ""), .$id))

## load min temperature
t_min <- read.table("data/t_min.txt",
                       header = FALSE,
                       sep = "\n",
                       stringsAsFactors = FALSE) %>%
  ## split columns
  separate(col = 1, into = c("id", month.abb), sep = "\\s+") %>%
  ### keep only relevant data
  filter(., grepl(paste(c("^04", "(", fips, ")", "28", "(", years, ")$"), collapse = ""), .$id))

## load max temperature
t_max <- read.table("data/t_max.txt",
                       header = FALSE,
                       sep = "\n",
                       stringsAsFactors = FALSE) %>%
  ## split columns
  separate(col = 1, into = c("id", month.abb), sep = "\\s+") %>%
  ### keep only relevant data
  filter(., grepl(paste(c("^04", "(", fips, ")", "27", "(", years, ")$"), collapse = ""), .$id))

# load population data

## load data
pop <- read.table("data/pop.txt",
                  header = FALSE,
                  sep = "\n",
                  stringsAsFactors = FALSE) %>%
  ## split into columns
  separate(col = 1, sep = c(4, 6, 8, 11, 13, 14, 15, 16, 18),
           into = c("year", "state", "state_code",
                    "county_code", "registry", "race",
                    "origin", "sex", "age",
                    "pop")) %>%
  mutate(year = as.integer(year)) %>%
  ## keep only total population estimate
  select(year, county_code, pop) %>%
  group_by(year, county_code) %>%
  summarize(pop = sum(as.integer(pop)), .groups = "drop")

# load unemployment data
unemployment <- read.csv("data/local_area_unemployment_statistics.csv", stringsAsFactors = FALSE) %>%
  ## keep only relevant counties
  filter(Area.Type == "County" & Area.Name %in% sub(", CA", "", counties)) %>%
  ## keep only seasonally unadjusted data (seasonally adjusted data are not generally available at the county level)
  filter(Seasonally.Adjusted..Y.N. == "N") %>%
  ## modify variables
  mutate(
    unemployment = Unemployment.Rate * 100, # unemployment rate as a percentage
    date = as.Date(Date, "%m/%d/%Y"),
    county = as.factor(sub(" County", "", Area.Name))
  ) %>%
  ## keep only relevant variables
  select(county, date, unemployment)
  
# combine and process data

## combine data and convert temperature to Celsius
dat <- bind_cols(death,
                 data.frame(
                   t_avg = (as.numeric(t(select(t_avg, -1))) - 32) * 5/9,
                   t_min = (as.numeric(t(select(t_min, -1))) - 32) * 5/9,
                   t_max = (as.numeric(t(select(t_max, -1))) - 32) * 5/9
                 )
) %>%
  left_join(
    pop,
    by = c("year", "county_code")
  )

## keep only necessary data
dat <- dat %>%
  select(County, county_code, year, month, deaths, pop, t_avg, t_min, t_max) %>%
  ## add proper date and time variable
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-")),
    time = rep(1:length(unique(date)), times = length(unique(County)))
    ) %>%
  ## create factor variables and create short county labels
  mutate(
    month_cat = as.factor(month),
    county_code = as.factor(county_code),
    county = as.factor(sub(" County, CA", "", County))
  )

## add unemployment rate data
dat <- dat %>%
  left_join(
    unemployment,
    by = c("county", "date")
  )

### DESCRIPTIVE PLOTS ###

# figure 1: time series of suicide and temperature by county
f1a <- ggplot(dat, aes(x = date, y = deaths)) +
  geom_line() +
  labs(x = "Date", y = "Monthly deaths by suicide") +
  facet_wrap(~county, scales = "free", ncol = 1) +
  theme_pubr()
f1b <- ggplot(dat, aes(x = date, y = t_avg)) +
  geom_line() +
  labs(x = "Date", y = "Monthly average temperature (°C)") +
  facet_wrap(~county, scales = "free", ncol = 1) +
  theme_pubr()
f1 <- plot_grid(f1a, f1b, labels = c("A", "B"))
f1
ggsave("figs/f1.png")

# figure 2: boxplots for suicide rates by month

## data for boxplot
boxplot <- dat %>%
  select(deaths, pop, month_cat) %>%
  drop_na %>%
  mutate(rate = deaths / pop * 100000)

## plot
f2 <- ggplot(boxplot, aes(x = month_cat, y = rate)) +
  geom_boxplot() +
  labs(x = "Month", y = "Monthly suicide rate per 100,000") +
  scale_x_discrete(labels = month.abb) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
f2
ggsave("figs/f2.png")

### PREPARE DATA FOR MODELLING ###

# drop NA values
dat_1 <- dat %>%
  drop_na(deaths)

### MODELLING ###

# baseline model - no temperature (Poisson)
m_pois <- gam(deaths ~ s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_1, family = poisson, method = "REML")

## model summary
summary(m_pois)

## model checking
gam.check(m_pois)

## ACF by county
a <- lapply(split(residuals(m_pois, "response"), dat_1$county), acf, plot = FALSE)
par(mfrow=c(2, 3))
lapply(names(a), function(x) plot(a[[x]], main = x))

# baseline model - no temperature (negative binomial)

## fit model
m_nb <- gam(deaths ~ s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_1, family = nb, method = "REML")

## model summary
summary(m_nb)

## model checking
gam.check(m_nb)

## ACF by county
a <- lapply(split(residuals(m_nb, "response"), dat_1$county), acf, plot = FALSE)
par(mfrow=c(2, 3))
lapply(names(a), function(x) plot(a[[x]], main = x))

# model: average temperature (Poisson)

## fit model
m_pois_t_avg <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_1, family = poisson, method = "REML")

## model summary
summary(m_pois_t_avg)

## model checking
gam.check(m_pois_t_avg)

## ACF by county
a <- lapply(split(residuals(m_pois_t_avg, "response"), dat_1$county), acf, plot = FALSE)
par(mfrow=c(2, 3))
lapply(names(a), function(x) plot(a[[x]], main = x))

# model: average temperature (negative binomial)

## fit model
m_nb_t_avg <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_1, family = nb, method = "REML")

## model summary
summary(m_nb_t_avg)

## model checking
gam.check(m_nb_t_avg)

## ACF by county
a <- lapply(split(residuals(m_nb_t_avg, "response"), dat_1$county), acf, plot = FALSE)
par(mfrow=c(2, 3))
lapply(names(a), function(x) plot(a[[x]], main = x))

# model: maximum temperature (Poisson)

## fit model
m_pois_t_max <- gam(deaths ~ t_max + s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_1, family = poisson, method = "REML")

## model summary
summary(m_pois_t_max)

## model checking
gam.check(m_pois_t_max)

## ACF by county
a <- lapply(split(residuals(m_pois_t_max, "response"), dat_1$county), acf, plot = FALSE)
par(mfrow=c(2, 3))
lapply(names(a), function(x) plot(a[[x]], main = x))

# model: maximum temperature (negative binomial)

## fit model
m_nb_t_max <- gam(deaths ~ t_max + s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_1, family = nb, method = "REML")

## model summary
summary(m_nb_t_max)

## model checking
gam.check(m_nb_t_max)

## ACF by county
a <- lapply(split(residuals(m_nb_t_max, "response"), dat_1$county), acf, plot = FALSE)
par(mfrow=c(2, 3))
lapply(names(a), function(x) plot(a[[x]], main = x))

# model: minimum temperature (Poisson)

## fit model
m_pois_t_min <- gam(deaths ~ t_min + s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_1, family = poisson, method = "REML")

## model summary
summary(m_pois_t_min)

## model checking
gam.check(m_pois_t_min)

## ACF by county
a <- lapply(split(residuals(m_pois_t_min, "response"), dat_1$county), acf, plot = FALSE)
par(mfrow=c(2, 3))
lapply(names(a), function(x) plot(a[[x]], main = x))

# model: minimum temperature (negative binomial)

## fit model
m_nb_t_min <- gam(deaths ~ t_min + s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_1, family = nb, method = "REML")

## model summary
summary(m_nb_t_min)

## model checking
gam.check(m_nb_t_min)

## ACF by county
a <- lapply(split(residuals(m_nb_t_min, "response"), dat_1$county), acf, plot = FALSE)
par(mfrow=c(2, 3))
lapply(names(a), function(x) plot(a[[x]], main = x))

### MODEL COMPARISON ###
AIC(m_pois, m_pois_t_avg, m_pois_t_max, m_pois_t_min, m_nb, m_nb_t_avg, m_nb_t_max, m_nb_t_min)

### PLOTS FOR PRIMARY MODEL ###

# figure 3: fitted values by county against observed values

## prepare data
pred <- predict(m_nb_t_avg, dat_1, type = "link", se.fit = TRUE)
pred <- data.frame(
  date = dat_1$date,
  county = dat_1$county,
  deaths = dat_1$deaths,
  fit = m_nb_t_avg$family$linkinv(pred$fit),
  upper = m_nb_t_avg$family$linkinv(pred$fit + (1.96 * pred$se.fit)),
  lower = m_nb_t_avg$family$linkinv(pred$fit - (1.96 * pred$se.fit))
)

## plot
f3 <- ggplot(pred, aes(x = date)) +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  geom_point(aes(y = deaths), shape = 16, alpha = 0.25) +
  labs(x = "Date", y = "Monthly deaths by suicide") +
  facet_wrap(~county, scales = "free") +
  theme_pubr()
f3
ggsave("figs/f3.png")

# supplementary figure 1: QQ plot of deviance residuals for 4 models
png("figs/sf1.png", height = 737, width = 615)
par(mfrow=c(2, 2))
qq.gam(m_nb, cex = 5, main = "Baseline (temperature excluded)")
qq.gam(m_nb_t_avg, cex = 5, main = "Average temperature")
qq.gam(m_nb_t_max, cex = 5, main = "Maximum temperature")
qq.gam(m_nb_t_min, cex = 5, main = "Minimum temperature")
dev.off()

# supplementary figure 2: ACF by county for primary model
a <- lapply(split(residuals(m_nb_t_avg, "response"), dat_1$county), acf, plot = FALSE)
png("figs/sf2.png", height = 737, width = 615)
par(mfrow=c(2, 3))
lapply(names(a), function(x) plot(a[[x]], main = x))
dev.off()

### SENSITIVITY ANALYSIS ###

# impute missing values for sensitivity analysis

## randomly assign suppressed values a number between 0 and 9 (with equal probability)
dat_2 <- dat %>%
  mutate(deaths = ifelse(is.na(deaths), sample(0:9, size = 1), deaths))

## fit models (negative binomial) with imputed data
m_nb_sens <- gam(deaths ~ s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_2, family = nb, method = "REML")
m_nb_t_avg_sens <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_2, family = nb, method = "REML")
m_nb_t_max_sens <- gam(deaths ~ t_max + s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_2, family = nb, method = "REML")
m_nb_t_min_sens <- gam(deaths ~ t_min + s(year, county_code, bs = "fs") + month_cat + offset(log(pop)), data = dat_2, family = nb, method = "REML")

### EXPORT DATA FOR TABLES ###

# table 1: annual suicide rate and annual average temperature by county

## create and write table to CSV
t1 <- bind_cols(
  # annual suicide rate
  dat %>%
    select(year, county, deaths, pop) %>%
    group_by(year, county, pop) %>%
    summarize(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
    mutate(rate = sprintf("%.2f", deaths / pop * 100000)) %>%
    select(year, rate, county) %>%
    pivot_wider(names_from = county, names_prefix = "rate_", values_from = rate),
  # annual average temperature
  dat %>%
    select(year, county, t_avg) %>%
    group_by(year, county) %>%
    summarize(t_avg = sprintf("%.1f", mean(t_avg)), .groups = "drop") %>%
    pivot_wider(names_from = county, names_prefix = "t_avg_", values_from = t_avg) %>%
    select(-1)
)
write.csv(t1, "tabs/t1.csv", row.names = FALSE)

# table 2: model results (negative binomial)

## function: grab estimates from model object
extract_estimates <- function(mod) {
  tidy(mod, parametric = TRUE, conf.int = TRUE) %>%
    slice(-1) %>%
    mutate(
      est_out = sprintf("%.4f", exp(estimate)),
      ci_out = paste0(sprintf("%.4f", exp(conf.low)), "–", sprintf("%.4f", exp(conf.high)))
    ) %>%
    select(term, est_out, ci_out) %>%
    bind_rows(data.frame(
      term = "AIC",
      est_out = sprintf("%.2f", mod$aic),
      ci_out = "")) %>%
    bind_rows(data.frame(
      term = "percent_dev_explained",
      est_out = paste0(((mod$null.deviance - mod$deviance) / mod$null.deviance) * 100, "%"),
      ci_out = ""))
}

## function: create model summary table
summary_table <- function(mod_list, col_names) {
  lapply(mod_list, extract_estimates) %>%
  reduce(full_join, by = "term") %>%
  # rename
  rename(
    setNames(1:9, col_names)
  ) %>%
  # reorder rows
  `[`(c(14:16, 1:13), )
}

## create table and write as CSV
t2 <- summary_table(
  list(m_nb, m_nb_t_avg, m_nb_t_max, m_nb_t_min),
  c("term",
    "est_out_m_nb",
    "ci_out_m_nb",
    "est_out_m_nb_t_avg",
    "ci_out_m_nb_t_avg",
    "est_out_m_nb_t_max",
    "ci_out_m_nb_t_max",
    "est_out_m_nb_t_min",
    "ci_out_m_nb_t_min"
  )
)
write.csv(t2, "tabs/t2.csv", row.names = FALSE)

# supplementary table 1: model results (Poisson)

## create table and write as CSV
st1 <- summary_table(
  list(m_pois, m_pois_t_avg, m_pois_t_max, m_pois_t_min),
  c("term", 
    "est_out_m_pois",
    "ci_out_m_pois",
    "est_out_m_pois_t_avg",
    "ci_out_m_pois_t_avg",
    "est_out_m_pois_t_max",
    "ci_out_m_pois_t_max",
    "est_out_m_pois_t_min",
    "ci_out_m_pois_t_min"
  )
)
write.csv(st1, "tabs/st1.csv", row.names = FALSE)

# supplementary table 2: model results (negative binomial, sensitivity analysis)

## create table and write as CSV
st2 <- summary_table(
  list(m_nb_sens, m_nb_t_avg_sens, m_nb_t_max_sens, m_nb_t_min_sens),
  c("term",
    "est_out_m_nb",
    "ci_out_m_nb",
    "est_out_m_nb_t_avg",
    "ci_out_m_nb_t_avg",
    "est_out_m_nb_t_max",
    "ci_out_m_nb_t_max",
    "est_out_m_nb_t_min",
    "ci_out_m_nb_t_min"
  )
)
write.csv(st2, "tabs/st2.csv", row.names = FALSE)

# sensitivity analysis: unemployment rate

## create variables for lags of unemployment rate up to 6 months
dat_1$unemployment_1 <- lag(dat_1$unemployment, 1)
dat_1$unemployment_2 <- lag(dat_1$unemployment, 2)
dat_1$unemployment_3 <- lag(dat_1$unemployment, 3)
dat_1$unemployment_4 <- lag(dat_1$unemployment, 4)
dat_1$unemployment_5 <- lag(dat_1$unemployment, 5)
dat_1$unemployment_6 <- lag(dat_1$unemployment, 6)

## fit models
m_nb_t_avg_unemployment_0 <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + unemployment + offset(log(pop)), data = dat_1, family = nb, method = "REML", na.action = "na.omit")
m_nb_t_avg_unemployment_1 <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + unemployment_1 + offset(log(pop)), data = dat_1, family = nb, method = "REML", na.action = "na.omit")
m_nb_t_avg_unemployment_2 <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + unemployment_2 + offset(log(pop)), data = dat_1, family = nb, method = "REML", na.action = "na.omit")
m_nb_t_avg_unemployment_3 <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + unemployment_3 + offset(log(pop)), data = dat_1, family = nb, method = "REML", na.action = "na.omit")
m_nb_t_avg_unemployment_4 <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + unemployment_4 + offset(log(pop)), data = dat_1, family = nb, method = "REML", na.action = "na.omit")
m_nb_t_avg_unemployment_5 <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + unemployment_5 + offset(log(pop)), data = dat_1, family = nb, method = "REML", na.action = "na.omit")
m_nb_t_avg_unemployment_6 <- gam(deaths ~ t_avg + s(year, county_code, bs = "fs") + month_cat + unemployment_6 + offset(log(pop)), data = dat_1, family = nb, method = "REML", na.action = "na.omit")

## summarize models
summary(m_nb_t_avg_unemployment_0)
summary(m_nb_t_avg_unemployment_1)
summary(m_nb_t_avg_unemployment_2)
summary(m_nb_t_avg_unemployment_3)
summary(m_nb_t_avg_unemployment_4)
summary(m_nb_t_avg_unemployment_5)
summary(m_nb_t_avg_unemployment_6)

## summarize values for t_avg coefficient
t_avg_unemployment <- c(
  exp(coef(m_nb_t_avg_unemployment_0)["t_avg"]),
  exp(coef(m_nb_t_avg_unemployment_1)["t_avg"]),
  exp(coef(m_nb_t_avg_unemployment_2)["t_avg"]),
  exp(coef(m_nb_t_avg_unemployment_3)["t_avg"]),
  exp(coef(m_nb_t_avg_unemployment_4)["t_avg"]),
  exp(coef(m_nb_t_avg_unemployment_5)["t_avg"]),
  exp(coef(m_nb_t_avg_unemployment_6)["t_avg"])
)
sprintf("%.4f", sort(t_avg_unemployment))
