# R code for producing synthetic data for tests

rows <- sapply(1:11, function(i) paste0("DBS",i))
cols <- c("Biliary-AdenoCA", "Bladder-TCC", "Bone-Osteosarc", "Bone-Other", "Breast", "Cervix", "CNS-GBM", 
          "CNS-Medullo", "CNS-Oligo", "CNS-PiloAstro", "Colorect-AdenoCA", "Eso-AdenoCA")

assays <- list()

# metadata =============================================================================================================

metadata <- list("proportion" = "Proportion of tumors with the signature",
                 "median" = "median mutations per Mb due to the signature")

# proportions assay ====================================================================================================

data <- matrix(c(rep(0., 12),
                 c(1.0, 1.0, 0, 0, 0.8, 0, 0.95, 0, 0, 0, 0.45, 0),
                 c(rep(0., 10), 0.62, 0),
                 c(0.12, 0.18, 0, 0, 0.49, 0, 0.82, 0, 0, 0, 0.41, 0),
                 c(rep(0, 12)),
                 c(rep(0, 4), 0.23, rep(0, 5), 0.08, 0.01),
                 c(rep(0, 10), 0.16, 0),
                 c(rep(0, 10), 0.29, 0.97),
                 c(rep(0,4), 0.38, rep(0, 7)),
                 c(rep(0,10), 0.23, 0),
                 c(0, 0.42, 0, 0, 0.83, 0, 0.97, 0, 0, 0, 0.11, 0)),  nrow=length(rows), ncol=length(cols), byrow=T)

rownames(data) <- rows
colnames(data) <- cols

assays[["proportion"]] <- data

# median numbers assay =================================================================================================

data <- matrix(c(rep(0., 12),
                 c(0.025, 0.04, 0, 0, 0.015, 0, 0.016, 0, 0, 0, 0.00005, 0),
                 c(rep(0., 10), 0.62, 0),
                 c(0.015, 0.009, 0.0095, 0, 0.0087, 0, 0.00009, 0, 0, 0, 0.41, 0),
                 c(rep(0, 12)),
                 c(rep(0, 4), 0.022, rep(0, 5), 0.00009, 0.01),
                 c(rep(0, 10), 0.026, 0),
                 c(rep(0, 10), 0.06, 0.23),
                 c(rep(0,4), 0.023, rep(0, 7)),
                 c(rep(0,10), 0.63, 0),
                 c(0, 0.021, 0, 0, 0.0006, 0, 0.021, 0, 0, 0, 0.00005, 0)),  nrow=length(rows), ncol=length(cols), byrow=T)

rownames(data) <- rows
colnames(data) <- cols

assays[["median"]] <- data

# row data for list object =============================================================================

rows.desc <- c("Ultraviolet light exposure", "Tobacco Smokin and other", "POLE mutation", "", "Platinum treatment", "",
               "Defective DNA repair", "", "", "Defective DNA repair", "APOBEC activity")
rowData <- data.frame(name=rows, description=rows.desc, stringsAsFactors=F)

# col data for list object =============================================================================

cols.desc <- c(8, 10, 0, 0, 18, 0, 2, 0, 0, 0, 13, 1)
colData <- data.frame(name=cols, description=cols.desc, stringsAsFactors=F)

# list object ==========================================================================================

DBS <- list(
  assays = assays,
  colData = colData,
  rowData = rowData,
  metadata = metadata
)

# save data ============================================================================================================

dir.create("inst/testdata", showWarnings=F, recursive=T)
save(DBS, file="inst/testdata/DBS.rda")
