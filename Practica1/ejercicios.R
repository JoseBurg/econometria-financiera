library(tidyverse)
library(forecast)
library(tseries)
library(urca)
library(fpp3)


#source("./Practica1/functions.R")

path_ipc <- "ipc.xlsx"


ipc <- readxl::read_excel(path_ipc)


# Programación eficiente --------------------------------------------------
base_ipc <- ipc |> 
  mutate(
    across(
      -fecha, 
      list(
        vi   = \(x) (x/lag(x, 12) - 1) *100,
        log  = \(x) log(x),
        dif1 = \(x) c(NA, diff(x))
      )
    )
  ) |> na.omit()

base_ipc_longer <- base_ipc |> 
  tidyr::pivot_longer(
    cols = -fecha, 
    names_to = "variables", 
    values_to = "valor"
  )




# Visualización de las Series -----------


# Análisis de Raíz Unitaria -----------------------------------------------
ipc_ts <- base_ipc |> 
  select(fecha, matches("(log|dif1|vi)$")) |> 
  mutate(fecha = yearmonth(fecha)) |> 
  as_tsibble(index = fecha)

