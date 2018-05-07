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

lapsecanytd <- cumsum(lapsecanm)
productionytd <- cumsum(productionm)
growthm <- productionm - lapsecanm
growthytd <- cumsum(growthm)


set.seed(2)
lapsecanm <- rnorm(100)
lapsecanytd <- cumsum(lapsecanm)
productionm <- rnorm(100)
productionytd <- cumsum(productionm)
growthm <- productionm - lapsecanm
growthytd <- cumsum(growthm)
dffl <- tibble(state = rep("Florida", 100),
             time = seq(1:100), 
             lapsecanm, lapsecanytd,
             productionm, productionytd,
             growthm, growthytd)

set.seed(30)
lapsecanm <- rnorm(100)
lapsecanytd <- cumsum(lapsecanm)
productionm <- rnorm(100)
productionytd <- cumsum(productionm)
growthm <- productionm - lapsecanm
growthytd <- cumsum(growthm)
dfca <- tibble(state = rep("California", 100),
             time = seq(1:100), 
             lapsecanm, lapsecanytd,
             productionm, productionytd,
             growthm, growthytd)

df <- rbind(dftx, dffl, dfca)

df <- df %>% 
  group_by(time) %>%
  summarize(lapsecanm = sum(lapsecanm),
            lapsecanytd = sum(lapsecanytd),
            productionm = sum(productionm),
            productionytd = sum(productionytd),
            growthm = sum(growthm),
            growthytd = sum(growthytd)) %>%
  mutate(state = rep("Countrywide", 100)) %>%
  bind_rows(., df) %>%
  select(state, time, 
         productionm, productionytd, 
         lapsecanm, lapsecanytd,
         growthm, growthytd) %>%
  ungroup()

df %>% filter(time %in% c(1,2))

m_ytd_graphs <- function(data) {
  lapsecanchartm <- ggplot(data = df, aes(x=time)) + 
    geom_line(aes(y=lapsecanm)) +
    theme_few()
  lapsecanchartytd <- ggplot(data = df, aes(x=time)) + 
    geom_line(aes(y=lapsecanytd)) +
    theme_few()
  productionchartm <- ggplot(data = df, aes(x=time)) +
    geom_line(aes(y=productionm)) +
    theme_few()
  productionchartytd <- ggplot(data = df, aes(x=time)) +
    geom_line(aes(y=productionytd)) +
    theme_few()
  growthchartm <- ggplot(data = df, aes(x=time)) +
    geom_line(aes(y=growthm)) +
    theme_few()
  growthchartytd <- ggplot(data = df, aes(x=time)) +
    geom_line(aes(y=growthytd)) +
    theme_few()
  charts <- ggarrange(productionchartm, productionchartytd,
                      lapsecanchartm, lapsecanchartytd,
                      growthchartm, growthchartytd,
                      nrow=3, ncol=2)
}
