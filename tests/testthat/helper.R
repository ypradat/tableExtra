suppressMessages(library(dplyr))
suppressMessages(library(tibble))

load("testdata/DBS.rda")
load("testdata/DEXP.rda")

plot_grob <- function(g, name, width=NULL, height=NULL){
  grDevices::pdf(file=file.path("pdfs_plots", name),
                 width=width,
                 height=height,
                 onefile=T)
  grid::grid.draw(g)
  grDevices::dev.off()
  invisible(list(plot_success=T))
}

# Aetiologies for PCAWG
SBS_aetiology <- read.csv("../../data-raw/cosmic_v3_aetiologies.csv", stringsAsFactors=F)
