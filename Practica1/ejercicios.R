library(tidyverse)
library(forecast)
library(tseries)
library(urca)
library(fpp3)

#path_general <- "./1. Materias/econometria-financiera"

path_ipc <- file.path(
 # path_general, 
  "ipc.xlsx")


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


# Análisis de Raíz Unitaria -----------------------------------------------
ipc_ts <- base_ipc |> 
  select(fecha, contains("log")) |> 
  mutate(fecha = yearmonth(fecha)) |> 
  as_tsibble(index = fecha)


ipc_ts |> 
  ACF(ipc_transporte_log, lag_max = 36) |> # función de autocorrelación
  autoplot() +
  theme_classic()

# ACF -------
ipc_ts |> 
  pivot_longer(
    cols = -fecha, 
    names_to = "ipc",
    values_to = "valor"
  ) |> 
  mutate(ipc = stringr::str_remove(ipc, "_log")) |> 
  group_by(ipc) |> 
  ACF(valor, lag_max = 24) |> 
  autoplot() +
  theme_classic() + 
  labs(title = "Logaritmos de IPC") +
  theme_conf()

# ACF -------
ipc_ts |> 
  pivot_longer(
    cols = -fecha, 
    names_to = "ipc",
    values_to = "valor"
  ) |> 
  mutate(ipc = stringr::str_remove(ipc, "_log")) |> 
  group_by(ipc) |> 
  ACF(valor, lag_max = 24, type = "partial") |> 
  autoplot() +
  theme_classic() + 
  labs(title = "Logaritmos de IPC") +
  theme_conf()





# Cuando los datos presentan una tendencia, las autocorrelaciones para pequeños rezagos tienden a ser grandes y positivas, ya que las observaciones cercanas en el tiempo también tienen valores cercanos. Por lo tanto, la ACF de una serie temporal con tendencia tiende a tener valores positivos que disminuyen lentamente a medida que aumentan los rezagos.  

# Cuando los datos son estacionales, las autocorrelaciones serán mayores para los rezagos estacionales (en múltiplos del período estacional) que para otros rezagos.

# Contraste de raíz unitaria ----------------------------------------------

ipc_ts |> 
  features(ipc_log, unitroot_kpss)

# La hipotesis nula es que los datos son estacionario,
# debido a que p-value es menor 0.05, se rechaza la 
# hipotesis nula, lo datos no son estacionario.

ipc_ts |> 
  mutate(ipc_log = difference(ipc_log)) |> 
  features(ipc_log, unitroot_kpss)

ts_ipc <- ts(
  ipc |> select(-fecha), 
  start = c(2006, 01), 
  frequency = 12) |> 
  as.list()

log_ts <- purrr::map(ts_ipc, log)         # Series en logaritmos

test_log <- purrr::map(log_ts, ~urca::ur.df(.x, type = "trend", lags = 4))

test_summary <- purrr::map(test_log, summary)

test_summary$ipc@teststat[1,2]
test_summary$ipc@cval[1,2]


log_diff <- purrr::map(log_ts, diff)     # Primera diferencia de los logaritmos

