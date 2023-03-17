library(tidyverse)
library(fpp3)
library(seasonal)

us_employment

us_employment %>% filter(Title == 'Total Private') %>% 
  autoplot(Employed)

us_employment %>% filter(Title == 'Total Private') %>% 
  gg_lag(Employed, geom = 'point')

us_employment %>% filter(Title == 'Total Private') %>% 
  ACF(Employed) %>% autoplot()

aus_production %>% select(Quarter, Bricks)

aus_production %>% autoplot(Bricks)

aus_production %>% gg_lag(Bricks, geom = 'point')

aus_production %>% ACF(Bricks) %>% autoplot()

pelt

pelt %>% autoplot(Hare)

pelt %>% gg_lag(Hare, geom = 'point')

pelt %>% ACF(Hare) %>% autoplot()

dgoog <- gafa_stock %>% 
  filter(Symbol == 'GOOG' & year(Date) >= 2018) %>% 
  mutate(diff = difference(Close)) %>% 
  select(diff)

dgoog %>% autoplot(diff)

dgoog %>% gg_lag(diff, geom = 'point')

dgoog %>% ACF(diff, lag_max = 100) %>% autoplot()

aus_production

aus_production %>% autoplot(Gas)

aus_production %>% autoplot(log(Gas))

aus_production %>% autoplot(box_cox(Gas, 0))

lambda_optimal <- aus_production %>% features(Gas, features = guerrero)

aus_production %>% autoplot(box_cox(Gas, lambda_optimal$lambda_guerrero))

global_economy %>% filter(Country == 'China') %>% View()

global_economy %>% 
  filter(Country == 'Australia') %>% 
  autoplot(GDP)

global_economy %>% 
  filter(Country == 'China') %>% 
  autoplot(GDP)

global_economy %>% 
  filter(Country == 'Australia') %>% 
  autoplot(GDP/Population)

global_economy %>% 
  filter(Country == 'China') %>% 
  autoplot(GDP/Population)

canadian_gas

canadian_gas %>% autoplot(box_cox(Volume, 0.7))

canadian_gas %>% gg_season()

canadian_gas %>% gg_subseries()

canadian_gas %>% gg_lag(geom = 'point')

canadian_gas %>% ACF(Volume) %>% autoplot()

fit_STL <- canadian_gas %>% 
  model(STL(Volume)) %>% 
  components()

fit_STL

fit_STL %>% autoplot()

fit_STL %>% gg_season(season_year)

fit_STL %>% gg_season(trend)

canadian_gas %>% 
  autoplot(Volume) +
  autolayer(fit_STL, trend, col = 'red') +
  autolayer(fit_STL, season_adjust, col = 'blue')

fit_x11 <- canadian_gas %>% 
  model(x11 = X_13ARIMA_SEATS(Volume ~ x11())) %>% 
  components()

fit_x11 %>% autoplot()

fit_SEATS <- canadian_gas %>% 
  model(seats = X_13ARIMA_SEATS(Volume ~ seats())) %>% 
  components()

fit_SEATS %>% autoplot()
