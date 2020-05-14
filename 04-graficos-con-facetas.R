  library(tidyverse)
  load("datos/2020-bonos-covid19-midis.Rdata")

  moquegua <- bonos_covid19 %>%
    filter(DE_DEPARTAMENTO == "MOQUEGUA") %>%
    mutate(
      Sexo = if_else(DE_GENERO == "1", "Masculino", "Femenino"),
      edad = if_else(FLAG_MAYEDAD == TRUE, "Mayor de edad", "Menor de edad")
    ) %>%
    group_by(DE_PROVINCIA, Sexo, edad) %>%
    tally()

  ggplot(moquegua, aes(x = edad, y = n,
                      group = Sexo, fill = Sexo)) +
    geom_col(position = position_dodge(width = .6), width = .5) +
    facet_wrap(~DE_PROVINCIA, ncol = 3) +
    theme_light(18) +
    theme(
      legend.position = "bottom"
    ) +
    labs(
      x = "",
      y = "",
      title = "Personas en Moquegua (por provincia) en el registro del \"Bono COVID-19\"",
      subtitle = "Fuente: Bonos COVID-19 (MIDIS) https://bit.ly/bonos-covid19-midis-peru",
      caption = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto"

    )

  ggsave(
    filename = "04-graficos-con-facetas-bono-covid19-moquegua.png",
    width = 12,
    height = 9
  )
