library(tidyverse)
library(circlize)

dune_libros <- read_csv("datos/dune-libros.csv") %>%
  # unite(
  #   col = "book", sep = ", ",
  #   title, pub_year
  # ) %>%
  mutate(
    series = replace_na(series, "Sin serie") %>%
      str_wrap(12),
    decada = sprintf("%d's", (pub_year %/% 10) * 10),
    author = str_replace(author, fixed("/"), " &\n")
  )

dune_edges <- bind_rows(
  dune_libros %>%
    select(author, genre) %>%
    rename(from = 1, to = 2),
    # %>%
    # mutate(type = "author:genre"),
  dune_libros %>%
    select(author, series) %>%
    rename(from = 1, to = 2),
    # %>%
    # mutate(type = "author:series"),
  dune_libros %>%
    select(author, decada) %>%
    rename(from = 1, to = 2),
    # %>%
    # mutate(type = "author:decada"),
  dune_libros %>%
    select(genre, series) %>%
    rename(from = 1, to = 2),
    # %>%
    # mutate(type = "genre:series"),
  dune_libros %>%
    select(genre, decada) %>%
    rename(from = 1, to = 2),
    # %>%
    # mutate(type = "genre:decada"),
  dune_libros %>%
    select(series, decada) %>%
    rename(from = 1, to = 2)
    # %>%
    # mutate(type = "series:decada")
)

# %>%
#   mutate(
#     value = as.numeric(as.factor(type))
#   ) %>%
  # select(-type)

png(
  filename = "28-diagrama-de-cuerdas-dune-libros.png",
  width = 2500, height = 1800,
  type = "cairo", antialias = "subpixel"
)
plot.new()

chordDiagram(
  dune_edges,
  annotationTrack = "grid",
  preAllocateTracks = 1,
  transparency = .3)
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1] + .1,
                cex = 2,
                sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
    circos.axis(h = "top",
                labels.cex = 1,
                major.tick.percentage = 0.2,
                sector.index = sector.name,
                track.index = 2)
  },
  bg.border = NA
)
text(x = -.8, y = .8,
     labels = "La saga de\"Dune\"",
     cex = 6, font = 2, pos = 3)

text(x = -.8, y = -.8,
     labels = "#30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto",
     cex = 2, font = 1, pos = 3)

dev.off()

circos.clear()
