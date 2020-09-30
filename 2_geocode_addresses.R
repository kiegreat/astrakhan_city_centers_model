
library(tidyverse)
library(XML)
library(rjson)

# 1. Read files and join them together ----

f <- list.files(path = 'data/', pattern = '.csv', full.names = T) # f stands for files

df <- map_df(.x = f, .f = ~read_csv(.x)); rm(f)
glimpse(df)

df <- df %>% distinct(name, address) # Оставим уникальные объекты (некоторые могли попасть в несколько категорий)

# 2. Geocode addresses ----

api_key <- "insert your api-key here"

geocode <- function(i) {
  
  name <- df$name[i]
  address <- df$address[i]
  
  location <- str_replace_all(string = address, pattern = ',', replacement = '') %>% 
    str_replace_all('[:space:]', '+')
  
  url <- str_c('http://geocode-maps.yandex.ru/1.x/?apikey=', api_key, '&geocode=', location) %>% 
    URLencode()
  
  xml_data <- readLines(url) %>% 
    paste(sep = "\n", collapse="") %>% 
    xmlParse(asText = TRUE) %>% 
    xmlToList()
  
  pos <- xml_data$GeoObjectCollection$featureMember$GeoObject$Point$pos
  
  lon <- str_replace(string = pos, pattern = '[:space:].*', replacement = '') %>% as.numeric()
  lat <- str_replace(string = pos, pattern = '.*[:space:]', replacement = '') %>% as.numeric()
  
  result <- data.frame(name = name, address = address, lon = lon, lat = lat)
  return(result)
}

df_coords <- map_df(.x = c(1:nrow(df)), .f = possibly(geocode, otherwise = NULL))
saveRDS(df_coords, 'data/df_coords.rds')


