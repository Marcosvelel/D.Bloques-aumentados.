#' Diseño de bloques aumentados
#'
#' @param data un conjunto de datos
#' @param block un factor de bloque
#' @param std_trt un tratamiento estándar
#' @param new_trt un tratamiento nuevo
#' @param response una variable de respuesta
#'
#' @return un data frame con lo valores de la suma de cuadrados, los grados de libertad, el estadistico F y el p. valor
#' @export
#' @examples
#' \dontrun{
#' # Ejemplo de uso de la función con los datos del ejemplo
#' data <- data.frame(block = rep(1:3, each = 4),
#'            std_trt = rep(c("A", "B"), times = 6),
#'            new_trt = rep(c("C", "D"), each = 6),
#'            response = c(1.23, 2.34, 3.45, 4.56, 5.67, 6.78, 7.89, 8.90, 9.01, 0.12, 1.23, 2.34))
#'bloquesaum(data, "block", "std_trt", "new_trt", "response")
#'}
#creamos la funcion
bloquesaum <- function(data, block, std_trt, new_trt, response) {
  # Crear fórmula para el modelo lineal
  formula <- as.formula(paste(response, "~", block, "+", std_trt, "+", new_trt))

  # Ajustar modelo lineal
  model <- lm(formula, data = data)
  # Calcular sumas de cuadrados y grados de libertad
  ss_total <- sum((data[[response]] - mean(data[[response]]))^2)
  ss_residual <- sum(resid(model)^2)
  ss_new_trt <- ss_total - ss_residual - anova(model)[[2]][1]
  df_new_trt <- length(unique(data[[new_trt]])) - 1

  # Realizar prueba F para el tratamiento nuevo
  fstat <- (ss_new_trt / df_new_trt) / (ss_residual / df.residual(model))
  pvalue <- pf(fstat, df_new_trt, df.residual(model), lower.tail = FALSE)

  #Crear tabla con resultados
  results <- data.frame(
    "Suma de cuadrados" = ss_new_trt,
    "Grados de libertad" = df_new_trt,
    "Estadístico F" = fstat,
    "Valor p" = pvalue
  )

  return(results)

}
