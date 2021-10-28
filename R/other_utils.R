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

rep_ifshort <- function(x, n, nc, nr, rep_mode){
    if(length(x) >= n){
      return(x[1:n]) 
    } else {
      # recycle
      if (rep_mode=="row"){
        return(rep(rep(x, length.out=nr), length.out=n)) 
      } else if (rep_mode=="col") {
        return(as.vector(matrix(rep(rep(x, length.out=nc), length.out=n), byrow=TRUE, nrow=nr)))
      } else {
        stop(paste0("Unsupported value '", rep_mode,"' of rep_mode. Choose 'col' or 'row'"))
      }
    }
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

  dint <- cut(d, labels=F, breaks=breaks, left.open=TRUE)
  dint <- (breaks[dint] + breaks[dint+1])/2
  dint[is.na(dint)] <- 0
  dcut <- matrix(dint, nrow=nrow(d), byrow=F)

  # set max scale is 1
  dcut <- dcut/max(dcut)

  dcut
}



extract_breadth_first <- function(lst, n){
  sapply(lst, `[`, n)
}


extract_strwidth_max_label <- function(labels, fontsize, unit="inches"){
  label_max <- labels[which(nchar(labels)==max(nchar(labels)))[1]]
  label_max_width <- strwidth(label_max, font=1, cex=fontsize/12, units=unit, ps=par(ps=12))
  unit(label_max_width, unit)
}

extract_strheight_max_label <- function(labels, fontsize, unit="inches"){
  label_max <- labels[which(nchar(labels)==max(nchar(labels)))[1]]
  label_max_height <- strheight(label_max, font=1, cex=fontsize/12, units=unit, ps=par(ps=12))
  unit(label_max_height, unit)
}


get_value_from_unit <- function(x){
  as.numeric(gsub("[a-zA-Z]+", "", as.character(x)))
}

get_unit_from_unit <- function(x) {
  as.character(gsub("[0-9\\.]+", "", as.character(x)))
}

convert_unit <- function(x, to, from=NULL){
  if (!is.unit(x) & is.null(from)){
    stop("if unit is not specified, x has to be a grid::unit object")
  } else if (is.unit(x)){
    value <- get_value_from_unit(x)
    from <- get_unit_from_unit(x)
  } else {
    value <- x
  }

  convert_to_cm <- list("cm"=1, "centimetre"=1, "centimeter"=1, "mm"=0.1, "in"=2.54, "inch"=2.54, "inches"=2.54,
                        "points"=2.54/72.27, "picas"=12*2.54/72.27)

  value*convert_to_cm[[from]]/convert_to_cm[[to]]
}
