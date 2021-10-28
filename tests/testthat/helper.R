suppressMessages(library(dplyr))
suppressMessages(library(tibble))

load(system.file("testdata", "DBS.rda", package="tableExtra"))
load(system.file("testdata", "DEXP.rda", package="tableExtra"))
load(system.file("testdata", "pcawg_counts.rda", package="tableExtra"))
load(system.file("testdata", "sbs_aetiologies.rda", package="tableExtra"))
cat(paste("tempdir located at", tempdir(), "\n"))

plot_grob <- function(g, name, width=NULL, height=NULL){
  grDevices::pdf(file=file.path(tempdir(), name),
                 width=width,
                 height=height,
                 onefile=T)
  grid::grid.draw(g)
  grDevices::dev.off()
  invisible(list(plot_success=T))
}

pcawg_plot_data <- function(){
  scale_breaks <- seq(from=0, to=1, by=0.1)
  color_palette <- c("#ffc651", "#ffa759", "#ff8962", "#ff6b6b", "#cc6999", "#9968c8", "#6767f8", "#4459ce", "#224ba5",
                     "#013d7c")
  color_breaks <- c(0, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 25, 1e6)
  color_bg <- c("#f8f9fa", "#e9ecef")

  theme <- ttheme_awesome(base_size=12,
                          rep_mode="col",
                          core_size=5, 
                          scale_breaks=scale_breaks,
                          color_palette=color_palette, 
                          color_breaks=color_breaks, 
                          core=list(bg_params=list(fill=color_bg)))

  # define dscale and cols_more from PCAWG data
  dscale <- pcawg_counts %>%
    dplyr::group_by(Cancer.Types) %>%
    dplyr::mutate(n=dplyr::n()) %>%
    dplyr::summarize_at(dplyr::vars(-Sample.Names, -Accuracy), ~sum(.x>0)) %>%
    dplyr::mutate_at(dplyr::vars(-Cancer.Types,-n), ~./n)

  cols_more <- list("n="=dscale$n)
  dscale$n <- NULL
  dscale <- tibble::column_to_rownames(.data=dscale, var="Cancer.Types")
  dscale <- t(as.matrix(dscale))
  
  # define dcolor and rows_more from PCAWG data
  rows_more <- list("Aetiology"=sbs_aetiologies[sbs_aetiologies$Signature %in% rownames(dscale), "Aetiology"])

  dcolor <- pcawg_counts %>%
    dplyr::group_by(Cancer.Types) %>%
    dplyr::summarize_at(dplyr::vars(-Sample.Names, -Accuracy), ~median(.[.!=0]*1e6/3.2e9)) %>%
    replace(is.na(.),0)

  dcolor <- tibble::column_to_rownames(.data=dcolor, var="Cancer.Types")
  dcolor <- t(as.matrix(dcolor))

  list(dscale=dscale, dcolor=dcolor, cols_more=cols_more, rows_more=rows_more, theme=theme)
}
