library(datasets)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggpubr)

set.seed(1)
productionm <- rnorm(64)
lapsecanm <- rnorm(64)
growthm <- productionm - lapsecanm
dftx <- tibble(state = rep("Texas", 64),
               date = seq(mdy('01/01/2013'), mdy('04/01/2018'), by='1 month'),
               productionm, lapsecanm, growthm)

set.seed(2)
productionm <- rnorm(64)
lapsecanm <- rnorm(64)
growthm <- productionm - lapsecanm
dfca <- tibble(state = rep("California", 64),
               date = seq(mdy('01/01/2013'), mdy('04/01/2018'), by='1 month'),
               productionm, lapsecanm, growthm)

set.seed(3)
productionm <- rnorm(64)
lapsecanm <- rnorm(64)
growthm <- productionm - lapsecanm
dffl <- tibble(state = rep("Florida", 64),
               date = seq(mdy('01/01/2013'), mdy('04/01/2018'), by='1 month'),
               productionm, lapsecanm, growthm)

df <- rbind(dftx, dfca, dffl)

# Create companywide
df <- df %>% 
  group_by(date) %>%
  summarize(lapsecanm = sum(lapsecanm),
            productionm = sum(productionm),
            growthm = sum(growthm)) %>%
  mutate(state = rep("Countrywide", 64)) %>%
  bind_rows(., df) %>%
  select(state, date, 
         productionm, lapsecanm, growthm) %>%
  ungroup()

# Create YTD variables
df %>% group_by(year(date), state) %>%
  mutate(productionytd = cumsum(productionm),
         lapsecanytd = cumsum(lapsecanm),
         growthytd = cumsum(growthm)) %>%
  ungroup() %>%
  filter(date %in% c(ymd("2013-01-01"), ymd("2013-02-01")))

m_ytd_graphs <- function(data, statearg) {
  data <- data %>% filter(state==statearg)
  
  lapsecanchartm <- ggplot(data, aes(x=date)) + 
    geom_line(aes(y=lapsecanm)) +
    theme_few()
  lapsecanchartytd <- ggplot(data, aes(x=date)) + 
    geom_line(aes(y=lapsecanytd)) +
    theme_few()
  productionchartm <- ggplot(data = df, aes(x=date)) +
    geom_line(aes(y=productionm)) +
    theme_few()
  productionchartytd <- ggplot(data = df, aes(x=date)) +
    geom_line(aes(y=productionytd)) +
    theme_few()
  growthchartm <- ggplot(data = df, aes(x=date)) +
    geom_line(aes(y=growthm)) +
    theme_few()
  growthchartytd <- ggplot(data = df, aes(x=date)) +
    geom_line(aes(y=growthytd)) +
    theme_few()
  charts <- ggarrange(productionchartm, productionchartytd,
                      lapsecanchartm, lapsecanchartytd,
                      growthchartm, growthchartytd,
                      nrow=3, ncol=2)
  return(charts)
}
