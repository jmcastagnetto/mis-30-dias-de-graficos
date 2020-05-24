library(tidyverse)
library(patchwork)

minsa_casos_url <- "https://github.com/jmcastagnetto/covid-19-peru-limpiar-datos-minsa/raw/master/datos/DATOSABIERTOS_SISCOVID-utf8-limpio.csv.gz"

minsa_casos_raw <- read_csv(minsa_casos_url)

minsa_casos <- minsa_casos_raw %>%
  arrange(fecha_resultado) %>%
  group_by(fecha_resultado) %>%
  tally() %>%
  mutate(
    casos_acum = cumsum(n)
  ) %>%
  rename(
    ts = fecha_resultado,
    casos = n
  ) %>%
  mutate(
    fuente = "MINSA"
  )


who_casos_url <- "https://github.com/jmcastagnetto/covid-19-data-cleanup/raw/master/data/covid-19_ts_who_sitrep.csv.gz"

who_casos_raw <- read_csv(who_casos_url)

who_casos <- who_casos_raw %>%
  filter(iso3c == "PER") %>%
  select(ts, cases) %>%
  mutate(
    casos = cases - lag(cases) %>%
      replace_na(0)
  ) %>%
  filter(casos > 0) %>%
  rename(
    casos_acum = cases
  ) %>%
  select(ts, casos, casos_acum) %>%
  mutate(
    fuente = "WHO (OMS)"
  )

df <- bind_rows(minsa_casos, who_casos) %>%
  arrange(fuente, ts)

Sys.setlocale("LC_TIME", "es_PE.utf8")
p_acum <- ggplot(df, aes(x = ts, y = casos_acum,
               group = fuente, color = fuente)) +
  geom_line(size = 1.5) +
  scale_color_discrete(name = "Fuentes: ") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "",
    y = "",
    title = "Casos acumulados de COVID-19 (Perú)"
  ) +
  theme_minimal(18) +
  theme(
    legend.position = "bottom",
    plot.margin = unit(rep(1, 4), "cm")
  )

p_casos <- ggplot(df, aes(x = ts, y = casos,
                     group = fuente, color = fuente)) +
  geom_line(size = 1.5) +
  scale_color_discrete(name = "Fuentes: ") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "",
    y = "",
    title = "Casos diarios de COVID-19 (Perú)"
  ) +
  theme_minimal(18) +
  theme(
    legend.position = "bottom",
    plot.margin = unit(rep(1, 4), "cm")
  )

df3 <- df %>%
  select(-casos_acum) %>%
  pivot_wider(
    id_cols = ts,
    names_from = fuente,
    values_from = casos
  )

m3 <- lm(`WHO (OMS)` ~ MINSA, df3)

p_lm_casos <- ggplot(df3, aes(x = MINSA, y = `WHO (OMS)`)) +
  geom_point(color = "red") +
  geom_abline(slope = 1, intercept = 0,
              size = 1.5, linetype = "dashed") +
  annotate("text", x = 1000, y = 4000, size = 5,
           label = paste0("R2(adj) = ",
                          round(summary(m3)$adj.r.squared, 3))) +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(18) +
  labs(
    title = "Casos diarios en Perú:\nMINSA vs WHO (OMS)"
  )

df4 <- df %>%
  select(-casos) %>%
  pivot_wider(
    id_cols = ts,
    names_from = fuente,
    values_from = casos_acum
  )

m4 <- lm(`WHO (OMS)` ~ MINSA, df4)

p_lm_acum <- ggplot(df4, aes(x = MINSA, y = `WHO (OMS)`)) +
  geom_point(color = "red") +
  geom_abline(slope = 1, intercept = 0,
              size = 1.5, linetype = "dashed") +
  annotate("text", x = 15000, y = 60000, size = 5,
           label = paste0("R2(adj) = ",
                          round(summary(m4)$adj.r.squared, 3))) +
  coord_equal() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(18) +
  labs(
    title = "Casos acumulados de COVID-19:\nMINSA vs WHO (OMS)"
  )

# gráficos combinados

p_comb_casos <- (p_casos + p_lm_casos) +
  plot_layout(
    widths = c(3, 2)
  ) +
  plot_annotation(
    title = "Comparación de dos fuentes de casos diarios de COVID-19 en Perú",
    subtitle = "Fuentes: MINSA (datos abiertos), WHO/OMS",
    caption = "2020-05-24 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) &
  theme(
    text = element_text(size = 18, family = "Roboto"),
    plot.caption = element_text(family = "Inconsolata"),
    plot.margin = unit(rep(1, 4), "cm")
  )

p_comb_acum <- (p_acum + p_lm_acum) +
  plot_layout(
    widths = c(3, 2)
  ) +
  plot_annotation(
    title = "Comparación de dos fuentes de casos acumulados de COVID-19 en Perú",
    subtitle = "Fuentes: MINSA (datos abiertos), WHO/OMS",
    caption = "2020-05-24 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) &
  theme(
    text = element_text(size = 18, family = "Roboto"),
    plot.caption = element_text(family = "Inconsolata"),
    plot.margin = unit(rep(1, 4), "cm")
  )

ggsave(
  p_comb_casos,
  filename = "13-graficos-datos-temporales-comp-casos.png",
  width = 14,
  height = 9
)

ggsave(
  p_comb_acum,
  filename = "13-graficos-datos-temporales-comp-acum.png",
  width = 14,
  height = 9
)
