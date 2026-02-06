library(tidyverse)
library(forecast)
library(tseries)
library(urca)
library(fpp3)


ipc <- readxl::read_excel("ipc.xlsx")


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


# Análisis de Raíz Unitaria -----------------------------------------------
ipc_ts <- base_ipc |> 
  select(fecha, contains("log")) |> 
  mutate(fecha = yearmonth(fecha)) |> 
  as_tsibble(index = fecha)