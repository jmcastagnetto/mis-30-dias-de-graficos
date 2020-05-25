library(tidyverse)
library(rpart)
library(ggdendro)

# un modelo de los
model <- rpart(skips ~ ., solder)
ddata <- dendro_data(model, uniform = TRUE,
                     branch = 0.5)

ggplot() +
  geom_segment(data = ddata$segments,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "blue", size = 1, alpha = 0.5) +
  geom_label(data = ddata$labels,
            aes(x = x, y = y, label = label),
            size = 4, vjust = 0) +
  geom_label(data = ddata$leaf_labels,
            aes(x = x, y = y, label = label),
            size = 4, vjust = 1, color = "red") +
  labs(
    title = "Clasificación de \"saltos de soldadura\" usando {rpart}",
    subtitle = "Fuente: {datasets::solder}",
    caption = "2020-05-26 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_dendro() +
  theme(
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(family = "Inconsolata", size = 14)
  )

ggsave(
  filename = "15-grafico-de-dendrograma-soldaduras.png",
  height = 8,
  width = 12
)
