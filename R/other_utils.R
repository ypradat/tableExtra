
"%contains%" <- function(x, y) all(y %in% x)

rep_along <- function(x, y) {
  if (length(y) == 0) return(NULL)
  rep(x, length(y))
}

row_heights <- function(m){
  do.call(grid::unit.c, apply(m, 1, function(l)
    max(do.call(grid::unit.c, lapply(l, grid::grobHeight)))))
}

col_widths <- function(m){
  do.call(grid::unit.c, apply(m, 2, function(l)
    max(do.call(grid::unit.c, lapply(l, grid::grobWidth)))))
}

scalecat <- function(m, n_cat=10, vmax=0.5){
  if (min(m) == max(m)){
    if (min(m) != 0){
      m <- m/max(m)
    } else {
      m <- m
    }
  } else {
    m <- (m-min(m))/(max(m) - min(m))
  }
  
  m <- apply(m, 1:2, function(x) round(x*n_cat)/(n_cat/vmax))
  m
}

rep_ifshort <- function(x, n, nc, nr){
    if(length(x) >= n){
      return(x[1:n]) 
    } else # recycle 
      return(rep(rep(x, length.out = nr), length.out= n)) 
}
