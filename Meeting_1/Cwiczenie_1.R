library(tidymodels)
library(parsnip)

# Helper packages
library(readr)       # import danych
library(broom.mixed) # konwersja 
library(dotwhisker)  # wizualizacja
library(GGally)
library(ggplot2)


colnames(airquality) <- tolower(colnames(airquality))

air <- 
  airquality |> 
  as_tibble() |>   # konwersja na tibble dla wygodniejszej pracy z danymi
  na.omit() |>     # usunięcie brakujących danych
  select(-day) |>  # usunięcie kolumny day
  mutate(month = factor(month))  # przekształcenie zmiennej month na czynnik
air

ggpairs(air)

# budowa modelu regresji liniowej bez interakcji
lm_mod <- lm(ozone ~ solar.r + wind + temp + month, data = air)

# podsumowanie modelu
summary(lm_mod)

# wykonanie prognoz na podstawie modelu i wstawienie jej do tabeli air
predictions <- predict(lm_mod, newdata = air)
air <- air |>  mutate(predicted_ozone = predict(lm_mod, newdata = air))

# podgląd prognoz
head(predictions)

# wpływ miesiąca na poziom ozonu
summary(lm_mod)


# wykres 
ggplot(air,
       aes(ozone, predicted_ozone)) +      # returns a ggplot object 
  geom_jitter() +                         # same
  geom_smooth(method = lm, se = FALSE) +  # same                    
  labs(x = "Volume", y = "Width")         # etc
  