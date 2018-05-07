library(datasets)
library(tidyverse)
library(ggthemes)

set.seed(1)
lapsecanm <- rnorm(100)
lapsecanytd <- cumsum(lapsecanm)
productionm <- rnorm(100)
productionytd <- cumsum(productionm)
growthm <- productionm - lapsecanm
growthytd <- cumsum(growthm)
dftx <- tibble(state = rep("Texas", 100),
             time = seq(1:100), 
             lapsecanm, lapsecanytd,
             productionm, productionytd,
             growthm, growthytd)

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

df %>% group_by(time) %>%
  summarize(lapsecanm = sum(lapsecanm),
            lapsecanytd = sum(lapsecanytd),
            productionm = sum(productionm),
            productionytd = sum(productionytd),
            growthm = sum(growthm),
            growthytd = sum(growthytd)) %>%
  mutate(state = rep("Countrywide", 100)) %>%
  bind_rows(., df)

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

