# Tidy tuesday obras de arte

tuesdata <- tidytuesdayR::tt_load('2021-01-12')

artwork <- tuesdata$artwork

artists <- tuesdata$artists

library(tidyverse)
library(paletteer)
library(lemon)

glimpse(artwork)

word(artwork$medium,1,3)

slice_medium <- 
artwork %>% 
  count(medium) %>%
  arrange(desc(n)) %>%
  na.omit() %>% 
  slice(1:6) %>% 
  {.$medium}

artwork %>% filter(medium %in% slice_medium) %>% 
  select(medium, acquisitionYear) %>% na.omit() %>% 
  mutate(medium=case_when(medium=="Graphite on paper"~"Grafite em papel",
                          medium=="Watercolour on paper"~"Aquarela em papel",
                          medium=="Lithograph on paper"~"Litografia em papel",
                          medium=="Oil paint on canvas"~"Óleo sobre tela",
                          medium=="Etching on paper"~"Água-forte em papel",
                          medium=="Screenprint on paper"~"Impressão em papel")) %>% 
  ggplot(aes(x=acquisitionYear,fill=medium)) +
    geom_density() + facet_wrap(~medium, scales = 'free_y') +
    theme_bw() + theme(legend.position = "none") +
    scale_fill_paletteer_d("yarrr::appletv") +
    ylab("Densidade") + xlab("Ano de aquisição") +
    ggtitle("Distribuição dos anos de aquisição de acordo\ncom as técnicas mais frequentes na amostra")


# lista de paletas aqui
# https://github.com/PMassicotte/paletteer_gallery









