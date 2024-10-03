library(tidymodels)
library(parsnip)

# Helper packages
library(readr)       # import danych
library(broom.mixed) # konwersja 
library(dotwhisker)  # wizualizacja
library(gggally)

install.packages("gggally")

urchins <-
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  mutate(food_regime = factor(food_regime, 
                              levels = c("Initial", "Low", "High")))
urchins |> is.na() |> as_tibble() |> summarise_all(sum)


urchins %>%
  ggplot(aes(
    x = initial_volume,
    y = width,
    col = food_regime,
    group = food_regime
  )) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  scale_color_viridis_d(option = "C", end = .9)
width ~ initial_volume * food_regime
linear_reg()

linear_reg() |> 
  set_engine("keras")

lm_mod <- 
  linear_reg() |> 
  set_engine("lm")

lm_fit <-  
  lm_mod |>
  fit(width ~ initial_volume * food_regime, data = urchins)
lm_fit  


print(lm_fit, digits = 5)

lm_fit$fit |> summary()

colnames(airquality) <- tolower(colnames(airquality))

air <-
  airquality |>
  as_tibble() |>
  na.omit() |> 
  select(-day) |> 
  mutate(month = factor(month)) 
air
ggpairs(air)
