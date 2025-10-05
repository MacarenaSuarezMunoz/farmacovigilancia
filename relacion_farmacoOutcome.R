getwd()
setwd("C:/Users/macas/Desktop/one love/Graficos")


AE

# Asegurarte de que existe la fila 90
stopifnot(nrow(AE) >= 90)

# Guardar e imprimir la fila 90
fila_90 <- AE[90, , drop = FALSE]
print(fila_90)

# Eliminar la fila 90 del dataset (el resto queda igual)
AE_NS <- AE[-90, , drop = FALSE]

# Guardar en la misma carpeta (directorio de trabajo actual)
out_path <- file.path(getwd(), "AE_NS.csv")
write.csv(AE_NS, out_path, row.names = FALSE)

colnames(AE_NS)


# Frecuencia de valores en la columna usada para el fill: aeout
AE_NS |> dplyr::count(aeout, name = "freq") |> dplyr::arrange(dplyr::desc(freq))


library(dplyr); library(forcats); library(ggplot2)

tbl <- AE_NS %>%
  mutate(
    aerel_v   = fct_explicit_na(factor(aerel), na_level = "Desconocido"),
    aeout_lab = ifelse(is.na(aeout), "Desconocido",
                       iconv(as.character(aeout), from = "latin1", to = "UTF-8"))
  ) %>%
  count(aerel_v, aeout_lab, name = "n") %>%
  group_by(aerel_v) %>% mutate(total = sum(n)) %>% ungroup() %>%
  mutate(aerel_v = fct_reorder(aerel_v, total, .desc = TRUE))

# Paleta personalizable (clave: etiqueta -> color)
pal <- c("Recuperado" = "#1b9e77",
         "No recuperado" = "#d95f02",
         "En recuperación" = "#7570b3",
         "Desconocido" = "#666666")

relacion <-ggplot(tbl, aes(n, fct_rev(aerel_v), fill = aeout_lab)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_fill_manual(values = pal, breaks = names(pal)) +
  labs(title = "Eventos adversos NO SERIOS y su relación con el fármaco",
       x = "Cantidad de eventos", y = NULL, fill = NULL) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(), plot.title = element_text(face = "bold"))

relacion
ggsave(file.path(getwd(), "grafico_AE_NS.png"), plot = relacion, width = 8, height = 5, dpi = 300)
