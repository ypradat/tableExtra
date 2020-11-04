repository <- "viz"

if (grepl(repository, getwd())){
  path <- unlist(strsplit(getwd(), repository))[1]
  renv::load(file.path(path, repository))
} else {
  cat(paste("WARNING: The repository", repository, "is not in the current path. Cannont load the renv environment.\n"))
  cat("To remove this warning, set the correct value for 'repository' in your .Rprofile.\n")
}

suppressMessages(require(colorout))
options(prompt = "R> ", digits=4, continue = "+  ", useFancyQuotes = FALSE)
