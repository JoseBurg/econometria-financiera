# Test poco eficiente -----------------------------------------------------
ipc_log <- ipc |> 
  mutate(
    across(-fecha, log)
  )

ipc_diff <- ipc |> 
  mutate(
    across(
      -fecha, 
      \(x) c(NA, diff(x))
    )
  )

ipc_vi <- ipc |> 
  mutate(
    across(
      -fecha, 
      \(x) (x/lag(x, 12) - 1) *100)
  )



summary(ur.df(ipc_ts |> select(2), type = "trend", lags = 4))

acf(ipc_ts |> select(2))











# base_ipc_longer |>
#   mutate(fecha = yearmonth(fecha)) |> 
#   filter(variables == "ipc_vi") |>
#   select(-variables) |> 
#   as_tsibble(index = fecha) |> 
#   gg_tsdisplay(valor, plot_type = "partial")







# DRAFTS: -----------------------------------------------------------------
ipc_ts <- ts(ipc[,-1], start = c(2006, 1), frequency = 12)
ipc_ts_log <- ts(log(ipc[,-1]), start = c(2006, 1), frequency = 12)
ipc_ts_diff <- ts(diff(ipc[,-1]), start = c(2006, 1), frequency = 12)

ipc_long <- ipc |> 
  tidyr::pivot_longer(
    cols = -fecha,
    values_to = "value",
    names_to = "IPC"
  ) |> 
  mutate(IPC = stringr::str_replace())

plot_ts <- function(
    data_plot,
    variable = "IPC", 
    ylab_name = "Logaritmo"
){
  
  plot(data_plot,
       col = "steelblue",
       lwd = 2,
       ylab = ylab_name,
       xlab = "Periodo",
       main = glue::glue("{variable} de República Dominicana \n 2006 - 2025"),
       cex.main = 0.8)
}


# Gráficos en logaritmos -----------

par(mfrow = c(2,2))

plot_ts(
  log(ipc_ts[,1])
)

plot_ts(
  log(ipc_ts[,2]),
  variable = "IPC Subyacente"
)

plot_ts(
  log(ipc_ts[,3]),
  variable = "IPC Alimentos y Bebidas"
)

plot_ts(
  log(ipc_ts[,4]),
  variable = "IPC Transporte"
)


# Gráficos en primera diferencias -----------

par(mfrow = c(2,2))

plot_ts(
  diff(ipc_ts[,1]),
  ylab_name = "Primera diferencia"
)

plot_ts(
  diff(ipc_ts[,2]),
  ylab_name = "Primera diferencia",
  variable = "IPC Subyacente"
)

plot_ts(
  diff(ipc_ts[,3]),
  ylab_name = "Primera diferencia",
  variable = "IPC Alimentos y Bebidas"
)

plot_ts(
  diff(ipc_ts[,4]),
  ylab_name = "Primera diferencia",
  variable = "IPC Transporte"
)


# variables en variaciones interanuales porcentuales
ipc_vi <- ipc |> 
  mutate(
    across(-fecha, \(x){(x/lag(x, 12) - 1)*100 })
  )

ipc_ts_vi <- ts(na.omit(ipc_vi[,-1]) , start = c(2007, 1), frequency = 12)


plot_ts(
  ipc_ts_vi[,1],
  ylab_name = "Variación porcentual interanual"
)

plot_ts(
  ipc_ts_vi[,2],
  ylab_name = "Variación porcentual interanual",
  variable = "IPC Subyacente"
)

plot_ts(
  ipc_ts_vi[,3],
  ylab_name = "Variación porcentual interanual",
  variable = "IPC Alimentos y Bebidas"
)

plot_ts(
  ipc_ts_vi[,4],
  ylab_name = "Variación porcentual interanual",
  variable = "IPC Transporte"
)


