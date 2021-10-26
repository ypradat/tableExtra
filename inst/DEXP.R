# R code for producing synthetic data for tests

rows <- sapply(1:50, function(i) paste0("ENSG00000",i))
cols <- c("Biliary-AdenoCA", "Bladder-TCC", "Bone-Osteosarc", "Bone-Other", "Breast", "Cervix", "CNS-GBM", 
          "CNS-Medullo", "CNS-Oligo", "CNS-PiloAstro", "Colorect-AdenoCA", "Eso-AdenoCA")

assays <- list()

# metadata =============================================================================================================

metadata <- list("pvalue" = "adjusted pvalue multiple testing",
                 "sign" = "sign of the beta coefficient")

# proportions assay ====================================================================================================

data <- matrix(runif(length(rows)*length(cols), min=0, max=1), nrow=length(rows))

rownames(data) <- rows
colnames(data) <- cols

assays[["pvalue"]] <- data

# median numbers assay =================================================================================================

data <- matrix(sample(c(-1,1), size=length(rows)*length(cols), replace=T), nrow=length(rows))

rownames(data) <- rows
colnames(data) <- cols

assays[["sign"]] <- data

# row data for list object =============================================================================

rowData <- data.frame(name=rows, stringsAsFactors=F)

# col data for list object =============================================================================

cols.desc <- c(8, 10, 0, 0, 18, 0, 2, 0, 0, 0, 13, 1)
colData <- data.frame(name=cols, description=cols.desc, stringsAsFactors=F)

# list object ==========================================================================================

DEXP <- list(
  assays = assays,
  colData = colData,
  rowData = rowData,
  metadata = metadata
)

# save data ============================================================================================================

dir.create("inst/testdata", showWarnings=F, recursive=T)
save(DEXP, file="inst/testdata/DEXP.rda")
