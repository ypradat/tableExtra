## Misc. gtable functions
## Note: these functions were copied from the gridExtra package.

#' Insert unit
#'
#' Helper for [rbind_2] and [cbind_2].
#'
#' @param x x param
#' @param values values param
#' @param after (optional) after param
#'
#' @keywords internal
insert.unit <- function (x, values, after = length(x)) {
  lengx <- length(x)
  if (lengx == 0) return(values)
  if (length(values) == 0) return(x)
  
  if (after <= 0) {
    unit.c(values, x)
  } else if (after >= lengx) {
    unit.c(x, values)
  } else {
    unit.c(x[1L:after], values, x[(after + 1L):lengx])
  }
}

z_normalise <- function (x, i = 1) 
{
  x$layout$z <- rank(x$layout$z, ties.method = "first") + i - 1
  x
}

z_arrange_gtables <- function (gtables, z) 
{
  if (length(gtables) != length(z)) {
    stop("'gtables' and 'z' must be the same length")
  }
  zmax <- 0
  for (i in order(z)) {
    if (nrow(gtables[[i]]$layout) > 0) {
      gtables[[i]] <- z_normalise(gtables[[i]], zmax + 1)
      zmax <- max(gtables[[i]]$layout$z)
    }
  }
  gtables
}

#' rbind two or more gtables
#'
#' @rdname bind
#' @param ... gtables
#' @param size how should the widths be calculated?
#' \enumerate{
#'   \item \code{max} maximum of all widths
#'   \item \code{min} minimum of all widths
#'   \item \code{first} widths/heights of first gtable
#'   \item \code{last} widths/heights of last gtable
#' }
#' @param height padding height between grobs
#' @param z optional z level
#' @return a \code{gtable} object
#' @keywords internal
gtable_rbind <- function(..., size = "max", height = NULL, z = NULL) {
  gtables <- list(...)
  if (!is.null(z)) {
    gtables <- z_arrange_gtables(gtables, z)
  }
  Reduce(function(x, y) rbind_2(x, y, size=size, height=height), gtables)
}

#' cbind two or more gtables
#'
#' @rdname bind
#' @param width padding width between grobs
#' @return a \code{gtable} object
#' @keywords internal
gtable_cbind <- function(..., size = "max", width = NULL, z = NULL) {
  gtables <- list(...)
  if (!is.null(z)) {
    gtables <- z_arrange_gtables(gtables, z)
  }
  Reduce(function(x, y) cbind_2(x, y, size=size, width=width), gtables)
}

#' rbind two gtables
#'
#' @param x a \code{gtable} object
#' @param y a \code{gtable} object
#' @param size how should the widths be calculated?
#' \enumerate{
#'   \item \code{max} maximum of all widths
#'   \item \code{min} minimum of all widths
#'   \item \code{first} widths/heights of first gtable
#'   \item \code{last} widths/heights of last gtable
#' }
#' @param height padding height between grobs
#' @return a \code{gtable} object with containing row-binded gtables \code{x} and \code{y}
#'
#' @importFrom gtable gtable_add_rows
#'
#' @keywords internal
rbind_2 <- function(x, y, size = "max", height=NULL) {
  stopifnot(ncol(x) == ncol(y))
  if (nrow(x) == 0) return(y)
  if (nrow(y) == 0) return(x)

  if (!is.null(height)){
    x <- gtable_add_rows(x, height)
  }
  
  y$layout$t <- y$layout$t + nrow(x)
  y$layout$b <- y$layout$b + nrow(x)
  x$layout <- rbind(x$layout, y$layout)
  
  x$heights <- insert.unit(x$heights, y$heights)
  x$rownames <- c(x$rownames, y$rownames)
  
  size <- match.arg(size, c("first", "last", 
                            "max", "min"))
  x$widths <- switch(size,
                     first = x$widths,
                     last = y$widths,
                     min = unit.pmin(x$widths, y$widths),
                     max = unit.pmax(x$widths, y$widths)
  )
  
  x$grobs <- append(x$grobs, y$grobs)
  
  x
}

#' cbind two gtables
#'
#' @param x a \code{gtable} object
#' @param y a \code{gtable} object
#' @param size how should the widths be calculated?
#' \enumerate{
#'   \item \code{max} maximum of all widths
#'   \item \code{min} minimum of all widths
#'   \item \code{first} widths/heights of first gtable
#'   \item \code{last} widths/heights of last gtable
#' }
#' @param width padding width between grobs
#' @return a \code{gtable} object with containing column-binded gtables \code{x} and \code{y}
#'
#' @importFrom gtable gtable_add_cols
#'
#' @keywords internal
cbind_2 <- function(x, y, size = "max", width=NULL) {
  stopifnot(nrow(x) == nrow(y))
  if (ncol(x) == 0) return(y)
  if (ncol(y) == 0) return(x)

  if (!is.null(width)){
    x <- gtable_add_cols(x, width)
  }
  
  y$layout$l <- y$layout$l + ncol(x)
  y$layout$r <- y$layout$r + ncol(x)
  x$layout <- rbind(x$layout, y$layout)
  
  x$widths <- insert.unit(x$widths, y$widths)
  x$colnames <- c(x$colnames, y$colnames)
  
  size <- match.arg(size, c("first", "last", 
                            "max", "min"))
  
  x$heights <- switch(size,
                      first = x$heights,
                      last = y$heights,
                      min = unit.pmin(x$heights, y$heights),
                      max = unit.pmax(x$heights, y$heights))
  
  x$grobs <- append(x$grobs, y$grobs)
  
  x
}
