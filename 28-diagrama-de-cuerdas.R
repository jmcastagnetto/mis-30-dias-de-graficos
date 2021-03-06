library(tidyverse)
library(circlize)

dune_libros <- read_csv("datos/dune-libros.csv") %>%
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
  dune_libros %>%
    select(author, series) %>%
    rename(from = 1, to = 2),
  dune_libros %>%
    select(author, decada) %>%
    rename(from = 1, to = 2),
  dune_libros %>%
    select(genre, series) %>%
    rename(from = 1, to = 2),
  dune_libros %>%
    select(genre, decada) %>%
    rename(from = 1, to = 2),
  dune_libros %>%
    select(series, decada) %>%
    rename(from = 1, to = 2)
)

png(
  filename = "28-diagrama-de-cuerdas-dune-libros.png",
  width = 2000, height = 2000,
  type = "cairo-png", antialias = "subpixel"
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

title(main = "La saga de \"Dune\"", 
	  cex.main = 6, line = -6, outer = FALSE)

text(x = 0, y = -.9,
     labels = "2020-06-08 // #30diasdegráficos // @jmcastagnetto, Jesus M. Castagnetto",
     cex = 3, font = 1)

text(x = 0, y = -1,
     labels = "Código: https://github.com/jmcastagnetto/mis-30-dias-de-graficos",
     cex = 3, font = 1)

dev.off()

circos.clear()

