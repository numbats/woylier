# now runs as
# f <- flea[, 1:6]
# animate(f, grand_tour_givens(), display_xy())

grand_tour_givens <- function(d = 2, ...) {
  generator <- function(current, data, ...) {
    if (is.null(current)) {
      return(basis_init(ncol(data), d))
    }
    
    target <- basis_random(ncol(data), d)
    list(target = target)
  }
  
  new_givens_path("grand", generator)
}

new_givens_path <- function(name, generator, frozen = NULL, ...) {
  tries <- 1 # Needed for guided
  
  tour_path <- function(current, data, ...) {
    if (is.null(current)) {
      return(generator(NULL, data, tries, ...))
    }
    
    # Keep trying until we get a frame that's not too close to the
    # current frame
    dist <- 0
    while (dist < 1e-3) {
      if (name %in% c("guided", "frozen-guided")) tries <<- tries + 1
      
      gen <- generator(current, data, tries, ...)
      target <- gen$target
      
      # generator has run out, so give up
      if (is.null(target)) {
        return(NULL)
      }
      
      dist <- proj_dist(current, target)
      if (dist < 1e-2) {
        return(NULL)
      }
      
      #cat("generation:  dist =  ", dist, "\n")
    }
    list(ingred = givens_path(current, target, frozen, ...), index = gen$index, tries = tries)
  }
  
  structure(
    tour_path,
    name = name,
    class = c("tour_path", "function")
  )
}