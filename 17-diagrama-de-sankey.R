library(tidyverse)
library(readxl)
library(ggalluvial)

un_locations_raw <- read_excel("datos/WPP2019_F01_LOCATIONS.XLSX",
                               sheet = 2)
un_loc_paises <- un_locations_raw %>%
  filter(LocTypeName == "Country/Area") %>%
  select(Location, ISO3_Code, LocTypeName, GeoRegName,
         SubRegName, SDGRegName, SDGSubRegName,
         WB_HIC, WB_UMIC, WB_LMIC, WB_LIC,
         WB_NoIncomeGroup) %>%
  mutate_at(
    vars(Location, ISO3_Code, LocTypeName, GeoRegName,
         SubRegName, SDGRegName, SDGSubRegName),
    factor
  ) %>%
  mutate_at(
    vars(WB_HIC, WB_UMIC, WB_LMIC, WB_LIC,
         WB_NoIncomeGroup),
    as.logical
  ) %>%
  pivot_longer(
    cols = c(WB_HIC, WB_UMIC, WB_LMIC, WB_LIC,
             WB_NoIncomeGroup),
    names_to = "WB_IncomeGroup_Class",
    values_to = "WB_IncomeGroup"
  ) %>%
  filter(!is.na(WB_IncomeGroup)) %>%
  arrange(
    GeoRegName, Location
  ) %>%
  janitor::clean_names()

save(
  un_loc_paises,
  file = "datos/un_loc_paises.Rdata"
)

wb_pop <- readxl::read_excel("datos/API_SP.POP.TOTL_DS2_en_excel_v2_988396.xls",
                             sheet = 1, range = "A4:BK268") %>%
  janitor::clean_names() %>%
  select(country_code, pop_2018 = x2018)

covid19_csv <- "https://github.com/jmcastagnetto/covid-19-data-cleanup/raw/master/data/covid-19_ts_who_sitrep.csv.gz"

covid19_raw <- read_csv(covid19_csv)
covid19_americas <- covid19_raw %>%
  left_join(
    un_loc_paises,
    by = c("iso3c" = "iso3_code")
  ) %>%
  left_join(
    wb_pop,
    by = c("iso3c" = "country_code")
  ) %>%
  filter(
    geo_reg_name %in% c("Northern America", "Latin America and the Caribbean")
  ) %>%
  mutate_at(
    vars(geo_reg_name, sub_reg_name),
    as.character
  ) %>%
  select(
    country_region,
    iso3c,
    ts,
    cases,
    continent,
    geo_reg_name,
    sub_reg_name,
    wb_income_group_class,
    pop_2018
  ) %>%
  mutate(
    sub_reg_name = if_else(is.na(sub_reg_name),
                           "North America",
                           sub_reg_name)
  ) %>%
  mutate_at(
    vars(country_region, iso3c,
         continent, geo_reg_name,
         sub_reg_name, wb_income_group_class),
    factor
  ) %>%
  filter(!is.na(pop_2018))

covid19_americas_ultimo <- covid19_americas %>%
  group_by(country_region, iso3c,
           continent, geo_reg_name,
           sub_reg_name, wb_income_group_class) %>%
  summarise(
    last_ts = max(ts, na.rm = TRUE),
    last_casos = last(cases),
    pop_2018 = last(pop_2018)
  ) %>%
  ungroup() %>%
  group_by(continent, geo_reg_name,
           sub_reg_name, wb_income_group_class) %>%
  summarise(
    ts = max(last_ts),
    casos_totales = sum(last_casos),
    casos_por_millon = casos_totales * 1e6 / sum(pop_2018, na.rm = TRUE)
  ) %>%
  mutate(
    wb_income_group_class = as.character(wb_income_group_class) %>%
      str_replace_all(c(
        "WB_HIC" = "Alto",
        "WB_UMIC" = "Medio Alto",
        "WB_LMIC" = "Medio Bajo",
        "WB_LIC" = "Bajo",
        "WB_NoIncomeGroup" = "Sin Clasificación"
      )) %>%
      factor(
        levels = c("Sin Clasificación",
                   "Bajo",
                   "Medio Bajo",
                   "Medio Alto",
                   "Alto"),
        ordered = TRUE
      )
  ) %>%
  arrange(geo_reg_name, sub_reg_name)

fecha <- unique(covid19_americas_ultimo$ts)

ggplot(covid19_americas_ultimo,
       aes(y = casos_por_millon,
           axis1 = continent,
           axis2 = wb_income_group_class,
           axis3 = str_wrap(geo_reg_name, 15),
           axis4 = str_wrap(sub_reg_name, 10)
           )) +
  scale_x_discrete(limits =
                     c("Continente",
                       "Clasificación\npor Ingreso",
                       "Región",
                       "Subregión")
                   ) +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 9000, 1000),
                     sec.axis = sec_axis(
                       "identity",
                       labels = scales::comma,
                       breaks = seq(0, 9000, 1000)),
                     ) +
  scale_fill_brewer(direction = -1, palette = "Dark2") +
  geom_alluvium(aes(fill = wb_income_group_class),
                color = "black") +
  geom_stratum(fill = "grey90") +
  geom_text(stat = "stratum", infer.label = TRUE) +
  labs(
    x = "",
    y = "",
    title = "Casos de COVID-19 por millón de habitantes en las Américas",
    subtitle = paste0("Al ", fecha, " - Fuentes: OMS(WHO), UNPP, World Bank"),
    caption = "2020-05-28 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_minimal(24) +
  theme(
    legend.position = "none",
    plot.margin = unit(rep(1, 4), "cm"),
    plot.caption = element_text(family = "Inconsolata")
  )

ggsave(
  filename = "17-diagrama-de-sankey-covid19-americas.png",
  width = 16,
  height = 12
)
