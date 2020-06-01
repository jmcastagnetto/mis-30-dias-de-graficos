library(tidyverse)
library(gutenbergr)
library(tidytext)

poe <- gutenberg_download(25807, meta_fields=c("title", "author", "language"))
poe_tokens <- poe %>%
  unnest_tokens(word, text)
poe_frec <- poe_tokens %>%
  group_by(author, word) %>%
  tally(name = "n_poe") %>%
  filter(!str_detect(word, "_")) %>%
  filter(str_length(word) > 3) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  rename(
    aut1 = author
  )

dario <- gutenberg_download(50341, meta_fields=c("title", "author", "language"))
dario_tokens <- dario %>%
  unnest_tokens(word, text)
dario_frec <- dario_tokens %>%
  group_by(author, word) %>%
  tally(name = "n_dario") %>%
  filter(!str_detect(word, "_")) %>%
  filter(str_length(word) > 3) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  rename(
    aut2 = author
  )

joined_frec <- poe_frec %>%
  left_join(
    dario_frec,
    by = "word"
  ) %>%
  mutate(
    wlen = str_length(word),
    wlen_lbl = paste("Longitud =", wlen)
  ) %>%
  filter(!is.na(aut2)) %>%
  filter((n_dario + n_poe) > 2) %>%
  filter(wlen < 10)

text_plot <- ggplot(joined_frec,
       aes(x = n_dario, y = n_poe, label = word)) +
  geom_text(aes(color = wlen_lbl),
            show.legend = FALSE,
            position = position_jitter(width = .1, height = .1)) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks() +
  scale_color_brewer(palette = "Pastel1") +
  labs(
    x = "Ruben Darío: \"Cantos de Vida y Esperanza, Los Cisnes y otros poemas\"",
    y = "Edgard Allan Poe: \"Poemas\" (traducido por R. Darío)",
    title = "Comparación de la frecuencia de uso de términos: R. Darío y E. A. Poe",
    subtitle = "Fuentes: Gutenberg Project (https://www.gutenberg.org/ebooks/50341, https://www.gutenberg.org/ebooks/25807)",
    caption = "2020-06-02 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  facet_wrap(~wlen_lbl, scales = "free") +
  ggdark::dark_theme_classic(20) +
  theme(
    plot.margin = unit(rep(1, 4), "cm"),
    plot.caption = element_text(family = "Inconsolata")
  )

ggsave(
  plot = text_plot,
  filename = "22-datos-textuales-dario-poe.png",
  width = 16,
  height = 10
)
