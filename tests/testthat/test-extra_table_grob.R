# test_that("extra table grob", {
#   load("testdata/DBS.rda")
# 
#   d <- SummarizedExperiment::assays(DBS)$proportion
#   theme <- ttheme_awesome()
# 
#   g <- extra_table_grob(d, rows=rownames(d), cols=colnames(d),
#                         theme=theme)
# 
#   out <- plot_grob(g, name="extra_table_grob.pdf")
#   expect_true(out$plot.success)
# })
