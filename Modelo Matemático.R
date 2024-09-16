library(readxl)
library(deSolve)

circuits <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    dN <- u * N * (1 - N/Nmax)
    dS <- -N * Vmax * S/(Km+S)
    
    return(list(c(dN, dS)))
    
  })
}

t <- seq(0, 32, by = 4)

state <- c( N = 0.1, S = 111)

parameters <- c( u = 0.37, Nmax = 2.5, Vmax = 11, Km = 76)

output1 <- ode(circuits, times = t, y = state, parms = parameters)

dfout <- as.data.frame(output1)
dfout <- dfout %>%
  mutate(Sutrato = S/1275,
         Biomasa = predict(curva_biomasa, newdata = data.frame(D.O.REAL = dfout$N)))
                           
dfout  %>%
  ggplot(aes(x = time)) +
  geom_smooth(aes(y = Biomasa, color = "Biomasa (g/L)"), size = 1, linetype = "dotted") +
  geom_point(aes(y = Biomasa, color = "Biomasa (g/L)"), size = 2) +
  geom_smooth(aes(y = S/1275, color = "Sustrato (g/L)"), size = 1, linetype = "dashed") +  # Ajustar el Sustrato a escala
  geom_point(aes(y = S/1275, color = "Sustrato (g/L)"), size = 2) +  # Ajustar el Sustrato a escala
  scale_y_continuous(
    name = "Biomasa (g/L)",
    sec.axis = sec_axis(~ . *1000, name = "Sustrato (g/L)")
  ) +
  labs(x = "Tiempo (h)", color = "") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Biomasa (g/L)" = "blue", "Sustrato (g/L)" = "orange"))


dfout$Biomasa

data_ES <- data_creGlu %>%
  group_by(Time) %>%
  reframe(Biomasa_exp = mean(Biomasa)) %>%
  mutate(Biomasa_sim = dfout$Biomasa) 

data_ES %>%
  ggplot(aes(x = Time)) +
  geom_smooth(aes(y = Biomasa_exp, color = "Biomasa experimental (g/L)"), size = 1, linetype = "dotted") +
  geom_point(aes(y = Biomasa_exp, color = "Biomasa experimental (g/L)"), size = 2) +
  geom_smooth(aes(y = Biomasa_sim, color = "Biomasa simulada (g/L)"), size = 1, linetype = "dashed") +  # Ajustar el Sustrato a escala
  geom_point(aes(y = Biomasa_sim, color = "Biomasa simulada (g/L)"), size = 2) +  # Ajustar el Sustrato a escala
  scale_y_continuous(
    name = "Biomasa (g/L)",
    sec.axis = sec_axis(~ . , name = "Biomasa simulada (g/L)")
  ) +
  labs(x = "Tiempo (h)", color = "") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Biomasa experimental (g/L)" = "blue", "Biomasa simulada (g/L)" = "orange"))

write.csv(dfout, file = "data_modelada.csv")
