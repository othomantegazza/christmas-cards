library(tidyverse)
library(magrittr)
library(cranlogs)
library(lubridate)


# Set time span of one year to now ----------------------------------------

to <- today()

from <- to %m-% months(12)

# Get data ----------------------------------------------------------------

dat_path <- "data/cran-downloads.Rdata"

if(!file.exists(dat_path)) {
  dat <- 
    installed.packages() %>%
    as_tibble() %$%
    Package %>%
    # cran_downloads(when = "last-month")
    cran_downloads(from = from,
                   to = to)
  
  save(dat, file = dat_path)
} else {
  load(dat_path)
}

dat %>% View()

# Tidy --------------------------------------------------------------------

# packages with 0 counts?
no_counts <- 
  dat %>%
  group_by(package) %>%
  summarise(counts = sum(count)) %>%
  filter(counts == 0) %$%
  package

dat <- 
  dat %>%
  filter(! package %in% no_counts) %>%
  as_tibble()

# Plot --------------------------------------------------------------------

dat_scaled <- 
  dat %>% 
  group_by(package) %>%
  filter(count > 0) %>%
  mutate(scaled_count = count %>%
           log() %>%
           scales::rescale(from = range(.),
                           to = c(0,1))) %T>%
  View()

p <- dat_scaled %>% 
  ggplot(aes(x = date,
             y = package,
             fill = scaled_count)) +
  geom_tile()

p + scale_fill_viridis_c()

png("cards/pack_heat.png",
    height = 2000,
    width = 700,
    res = 300)
p + 
  scale_fill_viridis_c(guide = FALSE) +
  theme_void()
dev.off()


# Subset ------------------------------------------------------------------

set.seed(1)

pack_sub <- 
  dat_scaled$package %>% 
  unique() %>%
  sample(100)

p_subset <- 
  dat_scaled %>%
  filter(package %in% pack_sub) %>% 
  ggplot(aes(x = date,
             y = package,
             fill = scaled_count)) +
  geom_tile()

p_subset + scale_fill_viridis_c()


png("cards/pack-heat-subset.png",
    height = 1000,
    width = 1200,
    res = 300)
p_subset + 
  scale_fill_viridis_c(guide = FALSE) +
  theme_void()
dev.off()


set.seed(25)
pack_sub <- 
  dat_scaled$package %>% 
  unique() %>%
  sample(100)

p_subset <- 
  dat_scaled %>%
  filter(package %in% pack_sub) %>% 
  ggplot(aes(x = date,
             y = package,
             fill = scaled_count)) +
  geom_tile()


png("cards/pack-heat-subset-inferno.png",
    height = 1000,
    width = 1200,
    res = 300)
p_subset + 
  scale_fill_viridis_c(option = "B", 
                       guide = FALSE) +
  theme_void()
dev.off()

