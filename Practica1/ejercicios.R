library(tidyverse)
library(forecast)
library(tseries)
library(urca)
library(fpp3)

path_general <- "./Practica1"

path_ipc <- file.path(
 path_general, 
  "ipc.xlsx")

source("./Practica1/functions.R")

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


estacionariedad(
  test_summary$ipc@teststat[1,1] > test_summary$ipc@cval[1,2]
)

# Transformación para corregir la no estacionariedad ----------------------
# Se dice que una serie es integrada de ”orden” uno cuando al
# diferenciar la serie no estacionaria una vez, esta se vuelve
# estacionaria
log_diff <- purrr::map(log_ts, diff)     # Primera diferencia de los logaritmos

test_diff <- purrr::map(log_diff, ~urca::ur.df(.x, type = "trend", lags = 4))

test_summary_diff <- purrr::map(test_diff, summary)

estacionariedad(test_summary_diff$ipc@teststat[1,1] > test_summary_diff$ipc@cval[1,2]) 
estacionariedad(test_summary_diff$ipc_subyacente@teststat[1,1] > test_summary_diff$ipc_subyacente@cval[1,2]) 



# Estadísticos de la serie estacionaria -----------------------------------

purrr::map(log_diff, summary)



# Identificación del Modelo ARIMA -----------------------------------------
ipc_longer <- ipc_ts |> 
  select(fecha, contains("log")) |> 
  mutate(
    across(
      -fecha, 
      difference
    )
  ) |>  
  pivot_longer(
    cols = -fecha, 
    names_to = "ipc",
    values_to = "valor"
  ) |>
  drop_na() |> 
  mutate(ipc = stringr::str_remove(ipc, "_log"))

# Prestar atención al ipc subyacente, tienen saltos en diferentes periodos 
ipc_longer |>                                                               #ACF
  group_by(ipc) |> 
  ACF(valor, lag_max = 12) |> 
  autoplot() +
  theme_classic() + 
  labs(title = "Primera diferencia de logaritmo de los IPCs",
    subtitle = "ACF: Autocorrelación simple") +
  theme_conf()

# ACF parcial
ipc_longer |> 
  group_by(ipc) |> 
  PACF(valor, lag_max = 12, type = "partial") |> 
  autoplot() +
  theme_classic() + 
  labs(title = "Primera diferencia de logaritmo de los IPCs",
       subtitle = "PACF: Autocorrelación parcial") +
  theme_conf()

# Se sugiere un modelo ARMA con un orden de 1, ARMA(1,1)
ipc_fit <- ipc_longer |> 
  dplyr::filter(ipc == "ipc") |> 
  dplyr::select(-ipc)

mod_ar1 <- tseries::arma(
  ts(ipc_fit[2], 
     start = c(2007, 2),
     frequency = 12), 
  order = c(1,1)
)

summary(mod_ar1)
