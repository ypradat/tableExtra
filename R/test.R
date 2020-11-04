# library(grid)
# library(gtable)
# library(gridExtra)
# 
# 
# #' @describeIn tableGrob default theme for text tables
# #' @param base_size default font size
# #' @param base_colour default font colour
# #' @param base_family default font family
# #' @param parse logical, default behaviour for parsing text as plotmath
# #' @param padding length-2 unit vector specifying the horizontal and vertical padding of text within each cell
# #' @importFrom utils modifyList
# #' @export
# ttheme_awesome <- function(base_size=12, 
#                            base_colour="black", 
#                            base_family="",
#                            parse=FALSE, 
#                            padding = unit(c(4, 4), "mm"), ...){
#   
#   core <- list(fg_fun = text_grob, 
#                fg_params = list(parse=parse, col=base_colour,
#                                 fontsize = base_size,
#                                 fontfamily = base_family),
#                bg_fun = rect_grob, 
#                bg_params = list(fill = c("grey95","grey90"), 
#                                 lwd=1.5, col="white"),
#                padding = padding)
#   
#   colhead <- list(fg_fun = text_grob, 
#                   fg_params = list(parse=parse, col=base_colour,
#                                    fontface=2L,
#                                    fontsize = base_size,
#                                    fontfamily = base_family),
#                   bg_fun = rect_grob, 
#                   bg_params = list(fill = c("grey80"), 
#                                    lwd=1.5, col="white"),
#                   padding = padding)
#   
#   rowhead <- list(fg_fun = text_grob, 
#                   fg_params = list(parse=parse, col=base_colour,
#                                    fontface=3L,
#                                    fontsize = base_size,
#                                    fontfamily = base_family,
#                                    hjust = 1, x = 0.95),
#                   bg_fun = rect_grob, 
#                   bg_params = list(fill=NA, lwd=1.5, col="white"),
#                   padding = padding)
#   
#   default <- list(
#     core = core,
#     colhead = colhead,
#     rowhead= rowhead
#   )
#   
#   modifyList(default, list(...))
#   
# }
# 
# 
# 
# 
# 
# load("../tests/testthat/testdata/DBS.rda")
# 
# pdf(
#   file="DBS_background.pdf",
#   width=10,
#   height=6
# )
# 
# 
# 
# # define theme
# 
# ttheme_awesome <- function(){
# }
# 
# theme <- theme
# 
# 
# layout.matrix <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE)
# widths <- c(6,1)
# heights <- c(1,6)
# 
# grid.arrange(rectGrob(), rectGrob(), rectGrob(), rectGrob(), layout_matrix=layout.matrix, widths=widths, heights=heights)
# 
# 
# grid.table(
#   d = assays(DBS)$proportion,
#   rows = rownames(DBS),
#   cols = colnames(DBS)
# )
# 
# dev.off()
# 
# 
# #### plot.layout
# if (!draw.row.bar & !draw.col.bar){
#     mat.lo <- matrix(data = c(1,2,3), nrow = 3, ncol = 1, byrow = TRUE)
#     lo <- graphics::layout(mat = mat.lo, heights = c(12, ann.height, leg.height))
# } else if (!draw.row.bar) {
#     mat.lo <- matrix(data = c(1,2,3,4), nrow = 4, ncol = 1, byrow = TRUE)
#     lo <- graphics::layout(mat = mat.lo, heights = c(2, 12, ann.height, leg.height))
# } else if (!draw.col.bar){
#     mat.lo <- matrix(data = c(1,2,3,4,5,5), nrow = 3, ncol = 2, byrow = TRUE)
#     lo <- graphics::layout(mat = mat.lo, heights = c(12, ann.height, leg.height), widths = c(6, 1))
# } else {
#     mat.lo <- matrix(data = c(1,2,3,4,5,6,7,7), nrow = 4, ncol = 2, byrow = TRUE)
#     lo <- graphics::layout(mat = mat.lo, widths = c(6, 1), heights = c(2, 12, 1, leg.height))
# }
# 
# 
# 
# 
# a <- rectGrob(gp = gpar(fill = "red"))
# b <- circleGrob()
# c <- linesGrob()
# 
# row <- matrix(list(a, b, c), nrow = 1)
# col <- matrix(list(a, b, c), ncol = 1)
# mat <- matrix(list(a, b, c, nullGrob()), nrow = 2)
# 
# grid.draw(gtable_matrix("demo", row, unit(c(1, 1, 1), "null"), unit(1, "null")))
# grid.draw(gtable_matrix("demo", col, unit(1, "null"), unit(c(1, 1, 1), "null")))
# grid.draw(gtable_matrix("demo", mat, unit(c(1, 1), "null"), unit(c(1, 1), "null")))
# 
# # Can specify z ordering
# z <- matrix(c(3, 1, 2, 4), nrow = 2)
# grid.draw(gtable_matrix("demo", mat, unit(c(1, 1), "null"), unit(c(1, 1), "null"), z = z))
# 
# d <- head(iris[,1:3])
# grid.table(d)
 
 
