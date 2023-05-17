frozen <- NULL

#' Create a grand tour with Givens interpolation
#' @param d dimension of projection
#' @param ... additional parameters to pass through
#' @return creates grand tour
#' @export
#' @examples
#' data(sine_curve)
#' tourr::animate(sine_curve, grand_tour_givens(), tourr::display_xy())
grand_tour_givens <- function(d = 2, ...) {
  generator <- function(current, data, ...) {
    if (is.null(current)) {
      return(tourr::basis_init(ncol(data), d))
    }

    target <- tourr::basis_random(ncol(data), d)
    list(target = target)
  }

  new_givens_path("grand", generator)
}

#' Create a guided tour with Givens interpolation
#' @param index_f the index function to optimize.
#' @param d target dimensionality
#' @param alpha the initial size of the search window, in radians
#' @param cooling the amount the size of the search window should be adjusted
#'   by after each step
#' @param optim character indicating the search strategy to use: \code{search_geodesic}, \code{search_better},
#'   \code{search_better_random}, \code{search_polish}. Default is \code{search_geodesic}.
#' @param max.tries the maximum number of unsuccessful attempts to find
#'   a better projection before giving up
#' @param max.i the maximum index value, stop search if a larger value is found
#' @param n_sample number of samples to generate if \code{search_f} is \code{search_polish}
#' @param ... arguments sent to the search_f
#' @return creates guided tour
#' @export
#' @examples
#' data(sine_curve)
#' tourr::animate_xy(sine_curve, guided_tour_givens(tourr::splines2d()), sphere=FALSE)
guided_tour_givens <- function(index_f, d = 2, alpha = 0.5, cooling = 0.99, max.tries = 25,
                               max.i = Inf, optim = "search_geodesic", n_sample = 100, ...) {
  generator <- function(current, data, tries, ...) {
    index <- function(proj) {
      index_f(as.matrix(data) %*% proj)
    }
    valid_fun <- c(
      "search_geodesic", "search_better", "search_better_random",
      "search_polish", "search_posse"
    )
    method <- valid_fun[vapply(valid_fun, function(x) {
      identical(x, optim)
    }, logical(1))]
    search_f <- switch(method,
      search_geodesic = tourr::search_geodesic,
      search_better = tourr::search_better,
      search_better_random = tourr::search_better_random,
      search_polish = tourr::search_polish,
      search_posse = tourr::search_posse
    )
    if (is.null(current)) {
      current <- tourr::basis_random(ncol(data), d)
      cur_index <- index(current)
      tryCatch({
        rcd_env <- parent.frame(n = 3)
        rcd_env[["record"]] <- dplyr::add_row(
          rcd_env[["record"]],
          basis = list(current),
          index_val = cur_index,
          info = "new_basis",
          method = method,
          alpha = formals(tourr::guided_tour)$alpha,
          tries = 1,
          loop = 1
        )
      },
      error = function(e){
        assign("record",
               tibble::tibble(basis = list(),
                              index_val = numeric(),
                              info = character(),
                              method = character(),
                              alpha = numeric(),
                              tries = numeric(),
                              loop = numeric()),
               envir = parent.frame())
        rcd_env[["record"]] <- tibble::tibble(
          basis = list(current),
          index_val = cur_index,
          info = "new_basis",
          method = method,
          alpha = formals(tourr::guided_tour)$alpha,
          tries = 1,
          loop = 1)
      }
      )
      return(current)
    }
    cur_index <- index(current)
    if (cur_index > max.i) {
      message("Found index ", cur_index, ", larger than selected maximum ", max.i, ". Stopping search.\n",
          sep = ""
      )
      message("Final projection: \n")
      if (ncol(current) == 1) {
        for (i in 1:length(current)) {
          message(sprintf("%.3f", current[i]), " ")
        }
        message("\n")
      }
      else {
        for (i in 1:nrow(current)) {
          for (j in 1:ncol(current)) {
            message(sprintf("%.3f", current[i, j]), " ")
          }
          message("\n")
        }
      }
      return(NULL)
    }
    # current, alpha = 1, index, max.tries = 5, n = 5, delta = 0.01, cur_index = NA, ..
    basis <- search_f(current, alpha, index, tries, max.tries, cur_index = cur_index, frozen = frozen, n_sample = n_sample, ...)
    if (method == "search_posse") {
      if (!is.null(basis$h)) {
        if (basis$h > 30) {
          alpha <<- alpha * cooling
        }
      }
    } else {
      alpha <<- alpha * cooling
    }
    list(target = basis$target, index = index)
  }
  new_givens_path("guided", generator)
}

#' Path needed for tour with Givens interpolation
#' @param name name to give tour path
#' @param generate basis generator function
#' @param frozen matrix giving frozen variables, as described in
#'   \code{freeze}
#' @return creates path for Givens interpolation
#' @keywords internal
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
      givens_components <- givens_path(current, target, frozen, ...)
      dist <- sum(abs(unlist(givens_components$tau)))
      if (dist < 1e-2) {
        return(NULL)
      }
      #message("generation:  dist =  ", dist, "\n")
    }
    list(ingred = givens_components, index = gen$index, tries = tries)
  }

  structure(
    tour_path,
    name = name,
    class = c("tour_path", "function")
  )
}

#' A planned tour path using frame-to-frame interpolation.
#'
#' The planned tour takes you from one basis to the next in a
#' set order.  Once you have visited all the planned bases, you either stop
#' or start from the beginning once more (if \code{cycle = TRUE}).
#'
#' Usually, you will not call this function directly, but will pass it to
#' a method that works with tour paths like \code{\link{animate}},
#' \code{\link{save_history}} or \code{\link{render}}.
#'
#' @param basis_set the set of bases as a list of projection matrices
#'   or a 3d array
#' @param cycle cycle through continuously (\code{TRUE}) or stop after
#'   first pass (\code{FALSE})
#' @keywords hplot dynamic
#' @seealso The \code{\link{little_tour}}, a special type of planned tour
#'   which cycles between all axis parallel projections.
#' @return creates planned tour path
#' @export
#' @examples
#' library(tourr)
#' twod <- save_history(flea[, 1:3], max = 5)
#' str(twod)
#' animate_xy(flea[, 1:3], planned_tour_givens(twod))
#' animate_xy(flea[, 1:3], planned_tour_givens(twod, TRUE))
#' oned <- save_history(flea[, 1:6], grand_tour(1), max = 3)
#' animate_dist(flea[, 1:6], planned_tour_givens(oned))
planned_tour_givens <- function(basis_set, cycle = FALSE) {
  index <- 1
  basis_set <- as.list(basis_set)
  n <- length(basis_set)
  if (cycle) {
    generator <- function(current, data, ...) {
      if (is.null(current)) {
        return(basis_set[[1]])
      }
      index <<- (index %% n) + 1
      target <- basis_set[[index]]
      list(target = target)
    }
  } else {
    generator <- function(current, data, ...) {
      if (is.null(current)) {
        return(basis_set[[1]])
      }
      index <<- index + 1
      if (index > n) {
        return(NULL)
      }
      target <- basis_set[[index]]
      list(target = target)
    }
  }
  new_givens_path("planned", generator)
}
