library(tidyverse)
library(ggpmisc)
library(readxl)

data_exp <- read_excel("Tablas.xlsx", sheet = "FC")

data_exp %>% 
  gather(key = "DO", value = "Abs", 2:4) %>%
  ggplot(aes(x =Conc, y = D.O.REAL)) +
  geom_smooth(method = "lm", se = T) + 
  geom_point(aes(y = Abs)) +
  stat_boxplot(aes(group = as.factor(Conc), y = Abs), geom = "errorbar", width = 2.5) +
  labs(y = "D.O 625 nm",
       x = "Concentración Glucosa (mg/L) ") +
  scale_x_continuous(breaks = seq(10, 100, 5)) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                                                             formula = y ~ x, parse = TRUE, size = 5, color = "black")+
  theme_classic()
  

data_cp <- read_excel("Tablas.xlsx", sheet = "CP")

data_cp[, c(2, 4, 5, 6, 8)] %>% 
  gather(key = "DO", value = "Abs", 2:4)  %>%
  ggplot(aes(x =DB, y = D.O.REAL)) +
  geom_smooth(method = "lm", se = T) + 
  geom_point(aes(y = Abs)) +
  stat_boxplot(aes(group = as.factor(DB), y = Abs), geom = "errorbar", width = 0.04) +
  labs(y = "D.O 620 nm",
       x = "Dilucion Biomasa") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE, size = 5, color = "black")+
  theme_classic()


data_cps <- read_excel("Tablas.xlsx", sheet = "CPS")

curva_biomasa <- lm(Biomasa ~ D.O.REAL, data = data_cps)

data_cps %>% 
  ggplot(aes(x =Biomasa, y = D.O.REAL)) +
  geom_smooth(method = "lm", se = T) + 
  geom_point() +
  labs(y = "D.O 620 nm",
       x = "Biomasa (g/L) ") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE, size = 5, color = "black")+
  theme_classic()
  
