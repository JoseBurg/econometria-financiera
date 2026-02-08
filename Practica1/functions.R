theme_conf <- function(){
  ggplot2::theme_light() +
    theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", 
                                         # family = 'serif', 
                                         size = 16),
      axis.title = ggplot2::element_text(size = 11, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      legend.text = ggplot2::element_text(size = 11),
      # text = ggplot2::element_text(family = 'serif')
    )
}

colores <- c("#00171f","#bdd5ea","#007ea7","#00a8e8")


plot_ejercicio1 <- function(
    data = base_ipc_longer, 
    var = "VI", 
    sub_title = "Variaciones interanuales porcentuales"
){
  
  varm <- stringr::str_to_lower(var)
  
  data |> 
    dplyr::filter(stringr::str_detect(variables, varm)) |> 
    dplyr::mutate(
      variables = stringr::str_remove(variables, paste0("_", varm)),
      variables = stringr::str_to_upper(variables),
      variables = stringr::str_replace(variables, "_", " ")
    ) |> 
    ggplot2::ggplot(ggplot2::aes(fecha, valor, color = variables)) +
    ggplot2::labs(
      title = "IPC de República Dominicana 2006-2025",
      subtitle = sub_title,
      x = "Periodo",
      y = var,
      caption = "Fuente: BCRD"
    )+
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_color_manual(values = colores)+
    ggplot2::facet_wrap(~variables) +
    theme_conf()
}


estacionariedad <- function(test){
  ifelse(
    test,
    "H0: Serie no estacionaria", 
    "H1: Serie estacionaria"
  )
}



