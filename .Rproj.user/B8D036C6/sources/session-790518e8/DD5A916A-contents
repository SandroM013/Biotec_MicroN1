data_sac <- read_excel("Tablas.xlsx", sheet = "Saca")
data_glu <- read_excel("Tablas.xlsx", sheet = "Gluc")
data_ara <- read_excel("Tablas.xlsx", sheet = "Ara")
data_alm <- read_excel("Tablas.xlsx", sheet = "Alm")


graficaCurva <- function(df){
  df %>%
    ggplot(aes(x = Time)) +
    geom_smooth(aes(y = Biomasa, color = "Biomasa (g/L)"), size = 1, linetype = "dotted") +
    geom_point(aes(y = Biomasa, color = "Biomasa (g/L)"), size = 2) +
    geom_smooth(aes(y = Sustrato/10, color = "Sustrato (g/L)"), size = 1, linetype = "dashed") +  # Ajustar el Sustrato a escala
    geom_point(aes(y = Sustrato/10, color = "Sustrato (g/L)"), size = 2) +  # Ajustar el Sustrato a escala
    scale_y_continuous(
      name = "Biomasa (g/L)",
      sec.axis = sec_axis(~ .  * 10, name = "Sustrato (g/L)")
    ) +
    labs(x = "Tiempo (h)", color = "") +
    theme_classic() +
    theme(legend.position = "top") +
    scale_color_manual(values = c("Biomasa (g/L)" = "blue", "Sustrato (g/L)" = "orange")) -> graft
  
  return(graft)
}

graficaCurva(data_creAlm)
graficaCurva(data_creGlu)
graficaCurva(data_creSac)
graficaCurva(data_creAra)




data_creGlu <- data_glu %>%
  reframe(Time = Tiempo * 4,
          DOR1 = DO1 * D,
          DOR2 = DO2 * D,
          DOR3 = DO3 * D,
          Sustrato = Consumo/1000) %>%
  gather(key = "DOReal", value = "Abs", 2:4) %>%
  mutate(Biomasa = predict(curva_biomasa, newdata = data.frame(D.O.REAL = data_creGlu$Abs)))

data_creSac <- data_sac %>%
  reframe(Time = Tiempo * 4,
          DOR1 = DO1 * D,
          DOR2 = DO2 * D,
          DOR3 = DO3 * D,
          Sustrato = Consumo/1000) %>%
  gather(key = "DOReal", value = "Abs", 2:4) %>%
  mutate(Biomasa = predict(curva_biomasa, newdata = data.frame(D.O.REAL = data_creSac$Abs)))

data_creAra <- data_ara %>%
  reframe(Time = Tiempo * 4,
          DOR1 = DO1 * D,
          DOR2 = DO2 * D,
          DOR3 = DO3 * D,
          Sustrato = Consumo/1000) %>%
  gather(key = "DOReal", value = "Abs", 2:4) %>%
  mutate(Biomasa = predict(curva_biomasa, newdata = data.frame(D.O.REAL = data_creAra$Abs)))

data_creAlm <- data_alm %>%
  reframe(Time = Tiempo * 4,
          DOR1 = DO1 * D,
          DOR2 = DO2 * D,
          DOR3 = DO3 * D,
          Sustrato = Consumo/1000) %>%
  gather(key = "DOReal", value = "Abs", 2:4) %>%
  mutate(Biomasa = predict(curva_biomasa, newdata = data.frame(D.O.REAL = data_creAlm$Abs)))


