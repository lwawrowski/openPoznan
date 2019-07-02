library(tidyverse)
library(lubridate)

graves <- readRDS("data/graves.rds")

graves <- graves %>% 
  distinct() %>%
  mutate_if(is.factor, as.character)

save(graves, file = "data/graves.RData")

# imiona, nazwiska
# data urodzenia
# data śmierci
# data pogrzebu

load("data/graves.RData")

theme_set(theme_classic())
theme_update(legend.position = "bottom")

graves <- graves %>%
  mutate(g_date_birth=ymd(g_date_birth), # zamiana na datę w formacie ymd z pakietu lubridate
         g_year_birth=year(g_date_birth), # wyodrębnienie roku
         g_date_death=ymd(g_date_death), 
         g_year_death=year(g_date_death),
         g_date_burial=ymd(g_date_burial), 
         g_year_burial=year(g_date_burial))

# sprawdzenie dat urodzenia

date_birth_n <- graves %>%
  group_by(g_date_birth) %>%
  count()

date_birth_n %>%
  arrange(desc(n)) %>%
  head(n=10)

graves_birth_clean <- graves %>%
  filter(!(g_date_birth %in% ymd(c("0001-01-01", "1900-01-01", "0001-09-22"))))

year_birth_n <- graves_birth_clean %>%
  group_by(g_year_birth) %>%
  count()

graves_birth_clean %>%
  filter(g_year_birth %in% c(1850:2000)) %>%
  ggplot(., aes(x=g_year_birth)) + geom_histogram(binwidth = 1)

# daty śmierci

date_death_n <- graves %>%
  group_by(g_date_death) %>%
  count()

graves_death_clean <- graves %>%
  filter(!(g_date_death %in% ymd("0001-01-01")))

graves_death_clean %>%
  filter(g_year_death %in% c(1850:2000)) %>%
  ggplot(., aes(x=g_year_death)) + geom_histogram(binwidth = 1)

# daty pogrzebu

date_burial_n <- graves %>%
  group_by(g_date_burial) %>%
  count()

graves_burial_clean <- graves %>%
  filter(!(g_date_burial %in% ymd("0001-01-01"))) 

graves_burial_clean %>%
  filter(g_year_burial %in% c(1850:2000)) %>%
  ggplot(., aes(x=g_year_burial)) + geom_histogram(binwidth = 1)

# porównanie daty urodzenia i śmierci

graves_birth_death <- graves %>%
  filter(!(g_date_birth %in% ymd(c("0001-01-01", "1900-01-01", "0001-09-22")))) %>%
  filter(!(g_date_death %in% ymd("0001-01-01"))) %>%
  mutate(age=g_year_death-g_year_birth)

graves_birth_death %>%
  filter(g_year_birth %in% c(1850:2000),
         g_year_death %in% c(1850:2000)) %>%
  ggplot(., aes(x=g_year_birth, y=g_year_death)) +
  geom_point()

birth_death_n <- graves_birth_death %>%
  group_by(g_year_birth, g_year_death) %>%
  count() %>%
  filter(g_year_birth > 1850, g_year_death > 1850) %>%
  mutate(n_group=cut(n, breaks = c(0,10,20,30,40,50,Inf)))

# wykres dla próby

birth_death_n_sample <- birth_death_n %>%
  group_by(n_group) %>%
  sample_n(10)

birth_death_n_sample %>%
  ggplot(., aes(x=g_year_birth, y=g_year_death)) +
  geom_point(aes(size=n_group))

# wiek

graves_birth_death %>%
  filter(age %in% c(0:120)) %>%
  ggplot(., aes(age)) + geom_histogram(binwidth = 1)

# data urodzenia, a wiek w momencie śmierci

death_age_n <- graves_birth_death %>%
  group_by(g_year_death, age) %>%
  count() %>%
  filter(age %in% c(0:120))

# porównanie daty śmierci i pogrzebu

graves_death_burial <- graves %>%
  filter(!(g_date_burial %in% ymd("0001-01-01"))) %>%
  filter(!(g_date_death %in% ymd("0001-01-01"))) %>%
  mutate(period=as.numeric(g_date_burial-g_date_death))

graves_death_burial %>%
  filter(period %in% c(0:30)) %>%
  ggplot(., aes(period)) + geom_histogram(binwidth = 1)

# braki w nazwisku

graves_surname_n <- graves %>%
  group_by(g_surname) %>%
  count()

# zbiór bez braków w datach

graves_clean <- graves %>%
  filter(!(g_date_burial %in% ymd("0001-01-01"))) %>%
  filter(!(g_date_death %in% ymd("0001-01-01"))) %>%
  filter(!(g_date_birth %in% ymd(c("0001-01-01", "1900-01-01", "0001-09-22")))) %>%
  mutate(age=g_year_death-g_year_birth,
         period=as.numeric(g_date_burial-g_date_death)) %>%
  filter(age %in% c(0:120), period %in% c(0:30), g_year_birth %in% c(1900:2000)) %>%
  mutate(g_year_birth_10=cut(g_year_birth, breaks = seq(1900, 2000, by=10), 
                             labels = seq(1910, 2000, length.out=10), include.lowest = T))

graves_clean %>%
  group_by(g_year_birth_10) %>%
  count()

graves_clean %>%
  ggplot(., aes(x=g_year_birth_10, y=age)) + geom_boxplot()

