###  Script para reporte de estaciones meteorologicas
# https://github.com/jrodriguez88/visualiti
# Author: Rodriguez-Espinoza J.
# 2019


###cargar librerias

library(tidyverse)
library(lubridate)
library(data.table)


###read data
headers <- c("id", "date", "rain", "wvel", "wdir", "perm_e1", "cond_e1",
             "stemp1", "perm_e2", "cond_e2", "stemp2", "temp", "rhum", "srad")

nombres <- c("id", 
             "Fecha", 
             "Precipitacion", 
             "Velocidad del Viento", 
             "Direccion del Viento", 
             "Permitividad electrica S1",
             "Conductividad electrica S1", 
             "Temperatura del suelo S1",
             "Permitividad electrica S2",
             "Conductividad electrica S2", 
             "Temperatura del suelo S2",
             "Temperatura del aire",
             "Humedad relativa",
             "Radiacion solar"
             )

unidades <- c("id", "ymd_hms", "mm", "m/s", "grados 0N", "XXX", "dS", "Celcius", "XXX", "dS", "Celcius", "Celcius", "%", "W/m2")


#names_comp
#units <- c()
# rain  = 1 = 0.2mm
# wdir = grados respecto al N
# temp = celcius
# srad = w/m2

#length(headers)
path <- getwd()
path <- "D:/03_DEVELOPER/visualiti/script_datos/data/6"
file <- "TEST_20190712_cucurita.TXT"

files <- list.files(pattern = ".TXT|.txt")

read_data <- function(path, file, headers){
  
  
  dat1 <- read_lines(paste0(path, "/", file)) %>% 
    enframe(name = NULL , value = "test") %>%
    mutate(test =  str_replace_all(test, "-", "_")) %>%
    filter(str_detect(test, fixed("__"), negate = T)) %>%
    mutate(separators = str_count(test, "_"),
           test =  if_else(separators == length(headers), 
                           str_replace_all(test, fixed("_0_0_0."), "_0_."), test)) %>% #filter(separators != 13) %>% View()
    separate(test, headers, sep = fixed("_")) 
  
  
  ifelse(length(unique(dat1$separators)) != 1, 
         paste0("Existen lineas con diferente numero de separadores. ", paste(unique(dat1$separators), collapse = " & ")), 
         paste0("Correcto numero de separadores. ", unique(dat1$separators))) %>% message()
  
  
  dat2 <- dat1 %>%  dplyr::select(-separators) %>% 
    mutate_at(vars(rain:srad), as.numeric) %>%
    mutate(date = as_datetime(as.numeric(date)),
           rain = if_else(rain > 50, NA_real_, rain*0.2))
  
  return(dat2)
  
  
}

convert_to_ <- function(data, tiempo = c("days", "weeks", "months", "years")) {
  
  data %>% group_by(year = year(date), mes = month(date),
                    dia = day(date), hora = hour(date)) %>%
    summarise_at(vars(rain:srad), .funs = mean, na.rm = T)
  
  
  
}



data <- read_data(path, file, headers)

data_test <- files %>% map(~read_data(path, .x, headers))


list.files(path = path) %>% map(~read_data(path, .x, headers))


data_test %>% bind_rows() %>% distinct()


viento <- data %>% dplyr::select(date, wvel, wdir)
lluvia <- data %>% dplyr::select(date, rain)
aire <- data %>% dplyr::select(date, temp, rhum)
radiacion <- data %>% dplyr::select(date, srad)
suelo <- data %>% dplyr::select(date, contains("perm"), contains("stemp"), contains("cond"))


viento %>% group_by(mes = month(date), dia = day(date)) %>% #, hora = hour(date)) %>% 
  summarise_at(vars(wvel, wdir), .funs = mean, na.rm = T) %>% 
  ungroup() %>%
ggplot(aes(wdir, wvel)) + 
  coord_polar(theta = "x", start = -pi/45) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(0, 360, 60)) +
  facet_wrap(~mes)


plot_rain <- function(data)
  
  
data %>%
  ggplot(aes(date, rain)) +
  geom_line()

data %>%
  ggplot(aes(date, temp)) +
  geom_line()
  

library(plotly)
library(quantmod)
getSymbols(Symbols = c("AAPL", "MSFT"))


ds <- data.frame(Date = index(AAPL), AAPL[,6], MSFT[,6])




read_data(path, "TEST_20190719_cucurita.TXT", headers)




















  
