# regenerate README.md
rmarkdown::render(
  input = "19-streamgraph.Rmd",
  output_format = "html_document",
  output_file = "19-streamgraph.html"
)
