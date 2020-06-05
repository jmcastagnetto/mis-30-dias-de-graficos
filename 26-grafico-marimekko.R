library(tidyverse)
library(foreign)
library(countrycode)
library(ggmosaic)

pisa2018_raw <- read.spss(
  file = "datos/pisa2018-datos/SCH/CY07_MSU_SCH_QQQ.sav",
  to.data.frame = TRUE
)

pisa2018_df <- pisa2018_raw %>%
  select(
    CNTRYID,
    OECD,
    SC001Q01TA,
    SC013Q01TA
  ) %>%
  mutate(
    CNTRYID = as.character(CNTRYID),
    CNTRYID = if_else(str_detect(CNTRYID, "(RUS)"),
                      "Rusia",
                      CNTRYID),
    tipo = if_else(str_detect(SC013Q01TA, "private"),
                   "Privado",
                   "Público") %>%
      factor(
        levels = c("Público", "Privado"),
        ordered = TRUE
      ),
    lugar_pob = case_when(
      str_detect(SC001Q01TA, "A village, hamlet") ~ "Aldea/Rural\n(< 3,000)",
      str_detect(SC001Q01TA, "A small town") ~ "Pueblo pequeño\n(3,000 - 15,000)",
      str_detect(SC001Q01TA, "A town") ~ "Pueblo\n(15,000 - 100,000)",
      str_detect(SC001Q01TA, "A city") ~ "Ciudad\n(100,000 - 1'000,000)",
      str_detect(SC001Q01TA, "A large city") ~ "Ciudad grande\n(> 1'000,000)",
    ) %>%
      factor(
        levels = c("Aldea/Rural\n(< 3,000)",
                 "Pueblo pequeño\n(3,000 - 15,000)",
                 "Pueblo\n(15,000 - 100,000)",
                 "Ciudad\n(100,000 - 1'000,000)",
                 "Ciudad grande\n(> 1'000,000)"
                 ),
        ordered = TRUE
      ),
    # iso3c = countryname(CNTRYID, destination = "iso3c"),
    # continent = countryname(CNTRYID, destination = "continent"),
    # Fix Kosovo
    # continent = if_else(CNTRYID == "Kosovo", "Europe", continent),
    region = countryname(CNTRYID, destination = "region")
  ) %>%
  select(-SC001Q01TA, -SC013Q01TA)

mmplot <- ggplot(pisa2018_df) +
  geom_mosaic(
    aes(x = product(lugar_pob, tipo), fill = lugar_pob),
    na.rm = TRUE, divider = ddecker(), show.legend = FALSE
  ) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_x_productlist(name = "") +
  labs(
    y = "",
    title = "PISA 2018: Colegios encuestados por región geográfica, tamaño de población y tipo",
    subtitle = "Fuente: https://www.oecd.org/pisa/data/2018database/",
    caption = "2020-06-06 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  facet_wrap(~region, scales = "free_x", ncol = 5) +
  theme_linedraw(24) +
  theme(
    axis.text = element_text(10),
    axis.text.y = element_text(hjust = 0),
    plot.title.position = "plot",
    plot.caption = element_text(family = "Inconsolata"),
    plot.margin = unit(rep(1, 4), "cm")
  )

ggsave(
  plot = mmplot,
  filename = "26-grafico-marimekko-pisa2018-colegios-lugar-tipo.png",
  width = 24,
  height = 10
)

img <- magick::image_read("26-grafico-marimekko-pisa2018-colegios-lugar-tipo.png")
img_resized <- magick::image_scale(img, "20%")
magick::image_write(
  img_resized,
  "26-grafico-marimekko-pisa2018-colegios-lugar-tipo-resized20.png"
)
