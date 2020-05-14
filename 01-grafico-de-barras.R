# gráfico de barras
library(tidyverse)

# Fuente de datos:
# https://www.datosabiertos.gob.pe/dataset/bonos-covid-19-ministerio-de-desarrollo-inclusi%C3%B3n-social-midis

# Bonos COVID-19 (MIDIS, Perú)
bonos_raw <- read_csv2("datos/2020-bonos-covid19-midis.csv.gz",
                   col_types = cols(.default = col_character()))

bonos_covid19 <- bonos_raw %>%
  mutate( # corregir ceros iniciales faltantes en UBIGEOs
    UBIGEO = sprintf("%06d", as.numeric(UBIGEO)),
    FLAG_MAYEDAD = (FLAG_MAYEDAD == "1"),
    FLAG_PADRON_OLD = (FLAG_PADRON_OLD == "1"),
    FLAG_DISCAP_SEVERA = (FLAG_DISCAP_SEVERA == "1")
  ) %>%
  mutate_at(
    .vars = vars(CO_HOGAR, UBIGEO, DE_DEPARTAMENTO,
      DE_PROVINCIA, DE_DISTRITO, CO_RESTRI),
    factor
  )

save(bonos_covid19, file = "datos/2020-bonos-covid19-midis.Rdata")

plot_df <- bonos_covid19 %>%
  group_by(DE_DEPARTAMENTO, CO_HOGAR) %>%
  tally() %>%
  mutate(
    personas = cut(n, breaks = c(0, 5, 10, 25))
  ) %>%
  ungroup() %>%
  mutate(
    DE_DEPARTAMENTO = fct_infreq(DE_DEPARTAMENTO),
    personas = factor(personas,
                      levels = c("(10,25]", "(5,10]", "(0,5]"),
                      ordered = TRUE)
  )

ggplot(plot_df,
       aes(x = DE_DEPARTAMENTO,
           group = personas,
           fill = personas)) +
  geom_bar(width = .5) +
  labs(
    x = "",
    y = "",
    title = "Hogares recibiendo Bonos COVID-19, Perú",
    subtitle = "Fuente: Bonos COVID-19 (MIDIS)\nhttps://bit.ly/bonos-covid19-midis-peru",
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(
    name = "Cantidad de\npersonas\nen el hogar",
    type = "qual", palette = "Accent"
  ) +
  coord_flip() +
  theme_minimal(18) +
  theme(
    legend.position = c(.7, .7)
  )

ggsave(
  filename = "01-grafico-de-barras-bonos-covi19d-peru.png",
  width = 10,
  height = 9
)
