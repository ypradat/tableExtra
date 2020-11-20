
"%contains%" <- function(x, y) all(y %in% x)

rep_along <- function(x, y) {
  if (length(y) == 0) return(NULL)
  rep(x, length(y))
}

row_heights <- function(m){
  do.call(grid::unit.c, apply(m, 1, function(l)
    1.1*max(do.call(grid::unit.c, lapply(l, grid::grobHeight)))))
}

col_widths <- function(m){
  do.call(grid::unit.c, apply(m, 2, function(l)
    1.1*max(do.call(grid::unit.c, lapply(l, grid::grobWidth)))))
}

rep_ifshort <- function(x, n, nc, nr){
    if(length(x) >= n){
      return(x[1:n]) 
    } else # recycle 
      return(rep(rep(x, length.out = nr), length.out= n)) 
}

breaks_scale <- function(d, d_min=NULL, d_max=NULL, breaks=10){
  if (is.null(d_min)){
    d_min = min(d)
  }

  if (is.null(d_max)){
    d_max = max(d)
  }

  if (d_min == d_max){
    if (d_min != 0){
      d <- d/d_max
    } else {
      d <- d
    }
  } else {
    d <- (d-d_min)/(d_max - d_min)
  }

  if (length(breaks)==1){
    breaks <- seq(from=0, to=1, length.out=breaks+1)
  } else {
    breaks <- (breaks-d_min)/(d_max-d_min)
  }

  dint <- cut(d, labels=F, breaks=breaks, left.open=T)
  dint <- (breaks[dint] + breaks[dint+1])/2
  dint[is.na(dint)] <- 0
  dcut <- matrix(dint, nrow=nrow(d), byrow=F)

  # set max scale is 1
  dcut <- dcut/max(dcut)

  dcut
}
