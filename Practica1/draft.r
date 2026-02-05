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




