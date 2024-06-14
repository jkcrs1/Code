# Instalar y cargar las librerías necesarias
required_packages <- c("ggplot2", "gridExtra", "dplyr", "stringr", 
                       "paletteer","palette", "scales","lubridate",
                       "prophet","reshape2","e1071","tibble")
installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

lapply(required_packages, library, character.only = TRUE)

tam.muestra <- function(alfa, epsilon, s, N = Inf) {
  za2 <- qnorm(1 - alfa / 2)
  n <- if (N == Inf) (s * za2 / epsilon)^2 else N * ((za2 * s)^2) / ((N - 1) * epsilon^2 + (za2 * s)^2)
  return(ceiling(n))
}

reemplazar_nulos <- function(x) {
  ifelse(x %in% c( "", "NULL", "Null", "null"), NA, x)
}

reemplazar_por_mediana <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

paleta_colores <- function() {
  colores <- paletteer::paletteer_d("ggthemes::Classic_10_Light")
  return(colores)
}


grid_plot <- function(..., n = NULL) {
  plots <- list(...)
  
  # Determinar el número de gráficos
  n_plots <- length(plots)
  
  # Verificar que el número de gráficos esté en el rango permitido
  if (n_plots < 1 || n_plots > 10) {
    stop("El número de gráficos debe estar entre 1 y 10.")
  }
  
  # Determinar el tamaño de la grilla
  if (is.null(n)) {
    if (n_plots <= 2) {
      nrow <- 1
      ncol <- n_plots
    } else if (n_plots <= 4) {
      nrow <- 2
      ncol <- 2
    } else if (n_plots <= 6) {
      nrow <- 2
      ncol <- 3
    } else if (n_plots <= 9) {
      nrow <- 3
      ncol <- 3
    } else {
      nrow <- 2
      ncol <- 5
    }
  } else {
    nrow <- n[1]
    ncol <- n[2]
  }
  
  grid.arrange(grobs = plots, nrow = nrow, ncol = ncol)
}

grafico_cualitativo <- function(data, var_cual_x, tipo_grafico) {
  
  # Verificar que la variable cualitativa sea un factor
  if (!is.factor(data[[var_cual_x]])) {
    data[[var_cual_x]] <- as.factor(data[[var_cual_x]])
  }
  
  # Crear el gráfico base
  p <- ggplot(data, aes(x = !!sym(var_cual_x), fill = !!sym(var_cual_x))) +
    theme_minimal()
  
  # Añadir diferentes tipos de gráficos
  p <- switch(tipo_grafico,
              "torta" = {
                data_pie <- data %>%
                  count(!!sym(var_cual_x)) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = "", y = percentage, fill = !!sym(var_cual_x))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar("y") +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL, caption = paste("Este gráfico de torta muestra la proporción de cada categoría de", var_cual_x, ".")) +
                  theme_void()
              },
              "anillo" = {
                data_pie <- data %>%
                  count(!!sym(var_cual_x)) %>%
                  mutate(percentage = n / sum(n))
                ggplot(data_pie, aes(x = 2, y = percentage, fill = !!sym(var_cual_x))) +
                  geom_bar(stat = "identity", width = 1) +
                  coord_polar(theta = "y") +
                  xlim(0.5, 2.5) +
                  geom_text(aes(label = scales::percent(percentage)), position = position_stack(vjust = 0.5)) +
                  labs(title = paste("Proporción de categorías de", var_cual_x), x = NULL, y = NULL, caption = paste("Este gráfico de anillo muestra la proporción de cada categoría de", var_cual_x, ".")) +
                  theme_void() +
                  theme(legend.position = "none",
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
              },
              "barra" = ggplot(data, aes(x = !!sym(var_cual_x), fill = !!sym(var_cual_x))) +
                geom_bar() +
                geom_text(stat='count', aes(label=..count..), angle = 90,  vjust = 0.5) +
                labs(title = paste("Frecuencia de", var_cual_x), x = var_cual_x, y = "Frecuencia", caption = paste("Este gráfico de barras muestra la frecuencia de", var_cual_x, ".")),
              stop(paste(tipo_grafico, " Tipo de gráfico no soportado."))
  )
  
  # Aplicar tema minimalista y formato de etiquetas en eje y
  p <- p + theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  # Mostrar el gráfico
  print(p)
}





grafico_cuantitativo <- function(data, var_cuant_x, tipo_grafico) {
  # Verificar si la variable es numérica
  if (!is.numeric(data[[var_cuant_x]])) {
    stop("La variable cuantitativa seleccionada debe ser numérica.")
  }
  
  # Definir la variable x
  x <- sym(var_cuant_x)
  
  # Obtener la paleta de colores
  colores <- paleta_colores()
  
  # Calcular estadísticos para el boxplot
  stats <- data %>% 
    summarise(
      Min = min(!!x, na.rm = TRUE),
      Q1 = quantile(!!x, 0.25, na.rm = TRUE),
      Mediana = median(!!x, na.rm = TRUE),
      Q3 = quantile(!!x, 0.75, na.rm = TRUE),
      Max = max(!!x, na.rm = TRUE)
    )
  
  IQR <- stats$Q3 - stats$Q1
  bigote_superior <- min(stats$Max, stats$Q3 + 1.5 * IQR)
  bigote_inferior <- max(stats$Min, stats$Q1 - 1.5 * IQR)
  
  # Crear el gráfico base
  p <- ggplot(data, aes(x = factor(1), y = !!x)) +
    theme_minimal() +
    scale_fill_manual(values = colores) +
    theme(legend.position = "none")  # Eliminar la leyenda
  
  # Añadir diferentes tipos de gráficos
  p <- switch(tipo_grafico,
              "histograma" = ggplot(data, aes(x = !!x, fill = ..count..)) +
                geom_histogram(binwidth = 10) +
                geom_text(stat='count', aes(label=..count..), angle = 90, hjust = -0.1, vjust = 0.5) +
                scale_fill_gradientn(colors = colores) +
                labs(title = paste("Distribución de", var_cuant_x), x = var_cuant_x, y = "Frecuencia", caption = paste("Este histograma muestra la distribución de", var_cuant_x, ".")),
              "densidad" = ggplot(data, aes(x = !!x, fill = ..density..)) +
                geom_density(alpha = 0.5) +
                scale_fill_gradientn(colors = colores) +
                labs(title = paste("Densidad de", var_cuant_x), x = var_cuant_x, y = "Densidad", caption = paste("Este gráfico de densidad muestra la distribución de", var_cuant_x, ".")),
              "barra" = ggplot(data, aes(x = !!x, fill = !!x)) +
                geom_bar(stat = "count") +
                geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
                scale_fill_manual(values = colores) +
                labs(title = paste("Frecuencia de", var_cuant_x), x = var_cuant_x, y = "Frecuencia", caption = paste("Este gráfico de barras muestra la frecuencia de", var_cuant_x, ".")),
              "violin" = ggplot(data, aes(x = factor(1), y = !!x)) +
                geom_violin(aes(fill = factor(1))) +
                scale_fill_manual(values = colores) +
                labs(title = paste("Distribución de", var_cuant_x), x = NULL, y = var_cuant_x, caption = paste("Este gráfico de violín muestra la distribución de", var_cuant_x, ".")) +
                theme(legend.position = "none"),  # Eliminar la leyenda
              "boxplot" = {
               
                ggplot(data, aes(x = factor(1), y = !!x)) +
                  geom_boxplot(alpha = 0.5, outlier.shape = NA, aes(fill = factor(1))) + 
                  geom_jitter(shape = 16, position = position_jitter(0.2), size = 2, alpha = 0.7) +
                  scale_fill_manual(values = colores) +
                  scale_color_manual(values = colores) +
                  theme_minimal() +
                  theme(legend.position = "none") +  # Eliminar la leyenda
                  annotate("label", x = 1.3, y = stats$Min, label = paste("Min:", round(stats$Min, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = stats$Q1, label = paste("Q1:", round(stats$Q1, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = stats$Mediana, label = paste("Mediana:", round(stats$Mediana, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = stats$Q3, label = paste("Q3:", round(stats$Q3, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = bigote_superior, label = paste("Bigote Sup.:", round(bigote_superior, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = bigote_inferior, label = paste("Bigote Inf.:", round(bigote_inferior, 2)), hjust = 0, size = 3, fill = "grey90") +
                  annotate("label", x = 1.3, y = stats$Max, label = paste("Max:", round(stats$Max, 2)), hjust = 0, size = 3, fill = "grey90") +
                  labs(title = paste("Boxplot con puntos de", var_cuant_x), x = NULL, y = var_cuant_x, caption = paste("Este boxplot con puntos muestra la distribución de", var_cuant_x, "."))
              },
              stop(paste(tipo_grafico, " Tipo de gráfico no soportado."))
  )
  
  # Aplicar tema minimalista y formato de etiquetas en eje y
  p <- p + theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  # Mostrar el gráfico
  print(p)
}



grafico <- function(data, var, tipo_grafico) {
  # Verificar si la variable es cualitativa o cuantitativa
  if (is.factor(data[[var]]) || is.character(data[[var]])) {
    # Llamar a la función grafico_cualitativo
    grafico_cualitativo(data, var, tipo_grafico)
  } else if (is.numeric(data[[var]])) {
    # Llamar a la función grafico_cuantitativo
    grafico_cuantitativo(data, var, tipo_grafico)
  } else {
    stop("El tipo de variable no es soportado. Debe ser una variable cualitativa o cuantitativa.")
  }
}




grafico_dispersion <- function(data, var_x, var_y) {
  # Verificar si las variables son numéricas
  if (!is.numeric(data[[var_x]]) || !is.numeric(data[[var_y]])) {
    stop("Ambas variables seleccionadas deben ser numéricas.")
  }
  
  # Obtener la paleta de colores
  colores <- paleta_colores()
  
  # Calcular la covarianza y el coeficiente de correlación
  covarianza <- cov(data[[var_x]], data[[var_y]], use = "complete.obs")
  correlacion <- cor(data[[var_x]], data[[var_y]], use = "complete.obs")
  
  # Evaluación de la relación
  abs_correlacion <- abs(correlacion)
  fuerza <- if (abs_correlacion < 0.1) {
    "Correlación inexistente"
  } else if (abs_correlacion < 0.3) {
    "Correlación débil"
  } else if (abs_correlacion < 0.5) {
    "Correlación moderada"
  } else {
    "Correlación fuerte"
  }
  
  direccion <- if (correlacion > 0) {
    "relación positiva (directa)"
  } else if (correlacion < 0) {
    "relación negativa (inversa)"
  } else {
    "sin relación"
  }
  
  evaluacion <- paste(fuerza, "con", direccion)
  
  # Crear el gráfico de dispersión
  p <- ggplot(data, aes(x = !!sym(var_x), y = !!sym(var_y))) +
    geom_point(color = colores[1], alpha = 0.6) +
    geom_smooth(method = "lm", color = colores[2], se = FALSE, formula = y ~ x) + # Añadir línea de tendencia con fórmula explícita
    theme_minimal() +
    labs(title = paste("Gráfico de Dispersión de", var_x, "vs", var_y),
         x = var_x,
         y = var_y,
         caption = paste("Este gráfico de dispersión muestra la relación entre", var_x, "y", var_y, "\nLa relaciòn es:", evaluacion)) +
    scale_x_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
  
  # Obtener los límites actuales del gráfico
  xlim <- range(data[[var_x]], na.rm = TRUE)
  ylim <- range(data[[var_y]], na.rm = TRUE)
  
  # Añadir anotaciones de covarianza y correlación
  p <- p + annotate("text", x = xlim[2], y = ylim[2], 
                    label = paste("Covarianza:", round(covarianza, 6), "\nCorrelación:", round(correlacion, 6)),
                    hjust = 1, vjust = 1, size = 3, color = "black") 
  
  # Mostrar el gráfico
  print(p)
}
