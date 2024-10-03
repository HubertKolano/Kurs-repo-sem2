library(tidymodels)
library(parsnip)

# Helper packages
library(readr)       # import danych
library(broom.mixed) # konwersja 
library(dotwhisker)  # wizualizacja
library(GGally)



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
model <- lm(ozone ~ solar.r + wind + temp + month, data = air)

# podsumowanie modelu
summary(model)

# wykonanie prognoz na podstawie modelu
predictions <- predict(model, newdata = air)

# podgląd prognoz
head(predictions)

# wpływ miesiąca na poziom ozonu
summary(model)$coefficients

