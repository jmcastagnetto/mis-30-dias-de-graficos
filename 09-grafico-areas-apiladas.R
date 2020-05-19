library(tidyverse)

csv <- "https://github.com/jmcastagnetto/covid-19-data-cleanup/raw/master/data/covid-19_ts_who_sitrep.csv.gz"

wb_class <- readxl::read_excel("datos/worldbank-classification-2020.xls",
                       sheet = 1, range = "C5:G224") %>%
  janitor::clean_names() %>%
  select(code, region) %>%
  filter(code != "x")

wb_pop <- readxl::read_excel("datos/API_SP.POP.TOTL_DS2_en_excel_v2_988396.xls",
                             sheet = 1, range = "A4:BK268") %>%
  janitor::clean_names() %>%
  select(country_code, pop_2018 = x2018)


covid19_raw <- read_csv(csv)

america_summary_df <- covid19_raw %>%
  filter(continent == "Americas") %>%
  mutate(
    ts = lubridate::ymd(ts),
    world_bank_income_group = str_replace_all(world_bank_income_group, "-", " ") %>%
      str_replace("Low income", "Ingresos bajos") %>%
      str_replace("Lower middle income", "Ingresos medios bajos") %>%
      str_replace("Upper middle income", "Ingresos medios altos") %>%
      str_replace("High income", "Ingresos altos") %>%
      factor(
        levels = c("Ingresos bajos", "Ingresos medios bajos",
                   "Ingresos medios altos", "Ingresos altos"),
        ordered = TRUE
      )
  ) %>%
  distinct() %>%
  left_join(
    wb_class,
    by = c("iso3c" = "code")
  ) %>%
  filter(str_detect(region, "America")) %>%
  left_join(wb_pop,
            by = c("iso3c" = "country_code")) %>%
  group_by(ts, world_bank_income_group, region) %>%
  summarise(
    casos = sum(cases, na.rm = TRUE),
    poblacion = sum(pop_2018, na.rm = TRUE) / 1e6,
    casos_pob = casos / poblacion
  ) %>%
  ungroup() %>%
  filter(!is.na(world_bank_income_group)
         & !is.na(ts)
         & casos > 0) %>%
  mutate(
    region = region %>%
      str_replace("Latin America & Caribbean", "Latinoamérica y el Caribe") %>%
      str_replace("Nort America", "Norteamérica")
  )

# set locale to
Sys.setlocale("LC_TIME", "es_PE.utf8")
ggplot(america_summary_df,
       aes(x = ts, y = casos_pob,
           group = world_bank_income_group,
           fill = world_bank_income_group)) +
  geom_area() +
  scale_fill_ordinal(
    name = "Clasificación del\nBanco Mundial"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    y = "Casos acumulados por millón de personas",
    x = "",
    title = "Casos de COVID-19 reportados por millón de personas en las Américas",
    subtitle = paste0(
      "Del ", format(min(america_summary_df$ts)),
      " al ", format(max(america_summary_df$ts)),
      " — Fuentes: OMS(WHO), WorldBank<br/>",
      "*<span style='color: \"red\"'>LATAM y el Caribe, han tenido menos casos per cápita que Norteamérica.</span>*"
    ),
    caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  facet_wrap(~region) +
  theme_linedraw(16) +
  theme(
    plot.caption = element_text(family = "Inconsolata"),
    plot.subtitle = ggtext::element_markdown(),
    plot.title.position = "plot",
    axis.title.y = element_text(size = 12),
    legend.position = c(.65, .7)
  )

ggsave(
  filename = "09-grafico-areas-apiladas-casos-americas.png",
  width = 10,
  height = 6
)
