library(tidyverse)
library(ggExtra)
library(scales)
options(scipen=999)

tuesdata <- tidytuesdayR::tt_load('2021-01-05')
transit_cost <- tuesdata$transit_cost

transit_cost <- 
  transit_cost %>% 
  mutate(real_cost=as.numeric(real_cost)) %>% 
  filter(!is.na(length), !is.na(real_cost),real_cost > 0)

transit_cost %>% ggplot(aes(y=length,x=real_cost)) +
  geom_point(alpha=0.5, color = "white") + 
  geom_smooth(method="lm",lty=2,color="black",se = FALSE) +
  scale_y_log10() + 
  scale_x_log10(labels = comma_format(big.mark = ".")) + 
  theme(panel.background = element_rect(fill = "#343536"),
        rect = element_rect(fill = "#343536", color = NA),
        plot.background = element_rect(color = NA),
        text = element_text( color = "white"),
        axis.text = element_text( color = "white"))  +
  ylab("Comprimento (km)") + 
  xlab("Custo real (USD)") + 
  ggtitle("Custo por comprimento de malhas urbanas.", 
          subtitle = "Dados extraídos do tidy tuesday de 05/01/2020 e utilizado como\nmodelo para um exercício de customização de gráficos no ggplot2.\n\nCada ponto representa uma malha construída em alguma cidade no mundo.\n")