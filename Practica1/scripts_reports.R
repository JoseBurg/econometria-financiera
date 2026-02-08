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


ipc_ts <- base_ipc |> 
  select(fecha, contains("log")) |> 
  mutate(fecha = yearmonth(fecha)) |> 
  as_tsibble(index = fecha)




# Análisis de Raíz Unitaria -----------------------------------------------
ipc_ts <- base_ipc |> 
  select(fecha, contains("log")) |> 
  mutate(fecha = yearmonth(fecha)) |> 
  as_tsibble(index = fecha)



ts_ipc <- ts(
  ipc |> select(-fecha), 
  start = c(2006, 01), 
  frequency = 12) |> 
  as.list()

log_ts <- purrr::map(ts_ipc, log)         # Series en logaritmos

test_log <- purrr::map(log_ts, ~urca::ur.df(.x, type = "trend", lags = 4))

test_summary <- purrr::map(test_log, summary)


# estacionariedad(
#   test_summary$ipc@teststat[1,1] > test_summary$ipc@cval[1,2]
# )