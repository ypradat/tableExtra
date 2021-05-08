suppressMessages(library(dplyr))
suppressMessages(library(tibble))

load("testdata/DBS.rda")
load("testdata/DEXP.rda")
load("testdata/pcawg_counts.rda")
load("testdata/sbs_aetiologies.rda")

plot_grob <- function(g, name, width=NULL, height=NULL){
  grDevices::pdf(file=file.path(system.file("tests", "testthat", "pdfs_plots", package="tableExtra"), name),
                 width=width,
                 height=height,
                 onefile=T)
  grid::grid.draw(g)
  grDevices::dev.off()
  invisible(list(plot_success=T))
}
