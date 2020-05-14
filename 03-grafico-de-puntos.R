library(tidyverse)
library(readxl)

# Indicadores de educación del BID
iadb_learning_indicators <- read_csv("datos/Learning_Improvement_Information_Center___Regional_Indicators_for_Learning.csv.gz")

# clasificación de países del World Bank
wb_class <- read_excel("datos/worldbank-classification-2020.xls",
                       sheet = 1, range = "C5:G224") %>%
  janitor::clean_names() %>%
  select(code, income_group) %>%
  filter(code != "x")

pisa_promedios <- iadb_learning_indicators %>%
  filter(
    Fuente_Especifica == "PISA" &
      Nombre_Indicador == "Puntaje_Prom" &
      Desagregacion == "No_Desagregado" &
      Materia %in% c("Matematicas", "Ciencias") &
      !Pais %in% c("_OECD", "_LAC")
  ) %>%
  select(Pais, Ano, Materia, Valor) %>%
  mutate(
    pais_nom = countrycode::countrycode(Pais,
                                        origin = "iso3c",
                                        destination = "country.name.en")
  ) %>%
  pivot_wider(
    names_from = "Materia",
    values_from = "Valor"
  ) %>%
  left_join(
    wb_class,
    by = c("Pais" = "code")
  ) %>%
  mutate(
    income_group = factor(
      income_group,
      levels = c(
        "High income",
        "Upper middle income",
        "Lower middle income"
      ),
      ordered = TRUE
    )
  ) %>%
  group_by(Pais) %>%
  mutate(
    last_yr = last(Ano),
    last_mate = last(Matematicas),
    last_cien = last(Ciencias)
  ) %>%
  select(-Matematicas, -Ciencias, -Ano) %>%
  distinct() %>%
  ungroup() %>%
  mutate(
    pnt_lbl = glue::glue("{nom}\n({yr})", nom = pais_nom, yr = last_yr)
  )

ggplot(data = pisa_promedios,
       aes(x = last_mate, y = last_cien,
           group = income_group,
           color = income_group)) +
  geom_point(size = 3, alpha = .6) +
  ggrepel::geom_text_repel(aes(label = pnt_lbl), show.legend = FALSE) +
  scale_color_brewer(name = "Clasificación del World Bank (2020):",
                     type = "qual", palette = "Set1") +
  theme_light(18) +
  theme(legend.position = "bottom") +
  labs(
    x = "Matemáticas",
    y = "Ciencias",
    title = "Relación entre promedios para la prueba PISA (2017-2018)",
    subtitle = "Fuente: IADB Data (https://bit.ly/2LsB2Yb) y World Bank (https://bit.ly/2AoeqFX)",
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  )

ggsave(
  filename = "03-grafico-de-puntos-pisa-ciencias-lac.png",
  width = 12,
  height = 9
)
