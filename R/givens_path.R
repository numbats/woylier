#' Create a grand tour with Givens interpolation
#'
#' @param d dimension of projection
#' @param ... additional parameters to pass through
#' @export
#' @examples
#' data(sine_curve)
#' tourr::animate(sine_curve, grand_tour_givens(), tourr::display_xy())
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

#' Create a guided tour with Givens interpolation
#'
#' @param index_f the index function to optimise.
#' @param d target dimensionality
#' @param alpha the initial size of the search window, in radians
#' @param cooling the amount the size of the search window should be adjusted
#'   by after each step
#' @param search_f the search strategy to use: \code{\link{search_geodesic}}, \code{\link{search_better}},
#'   \code{\link{search_better_random}}, \code{\link{search_polish}}. Default is \code{\link{search_geodesic}}.
#' @param max.tries the maximum number of unsuccessful attempts to find
#'   a better projection before giving up
#' @param max.i the maximum index value, stop search if a larger value is found
#' @param n_sample number of samples to generate if \code{search_f} is \code{\link{search_polish}}
#' @param ... arguments sent to the search_f
#' @seealso \code{\link{cmass}}, \code{\link{holes}} and \code{\link{lda_pp}}
#'   for examples of index functions.  The function should take a numeric
#'   matrix and return a single number, preferably between 0 and 1.
#' \code{\link{search_geodesic}}, \code{\link{search_better}},
#'   \code{\link{search_better_random}} for different search strategies
#' @export
#' @examples
#' data(sine_curve)
#' tourr::animate(sine_curve, guided_tour_givens(tourr::splines2d()), tourr::display_xy())
guided_tour_givens <- function(index_f, d = 2, alpha = 0.5, cooling = 0.99, max.tries = 25,
                               max.i = Inf, search_f = search_geodesic, n_sample = 100, ...) {
  generator <- function(current, data, tries, ...) {
    index <- function(proj) {
      index_f(as.matrix(data) %*% proj)
    }

    valid_fun <- c(
      "search_geodesic", "search_better", "search_better_random",
      "search_polish", "search_posse"
    )
    method <- valid_fun[vapply(valid_fun, function(x) {
      identical(get(x), search_f)
    }, logical(1))]

    if (is.null(current)) {
      current <- basis_random(ncol(data), d)

      cur_index <- index(current)

      tryCatch({
        rcd_env <- parent.frame(n = 3)
        rcd_env[["record"]] <- dplyr::add_row(
          rcd_env[["record"]],
          basis = list(current),
          index_val = cur_index,
          info = "new_basis",
          method = method,
          alpha = formals(guided_tour)$alpha,
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
          alpha = formals(guided_tour)$alpha,
          tries = 1,
          loop = 1)
      }
      )

      return(current)
    }

    cur_index <- index(current)

    if (cur_index > max.i) {
      cat("Found index ", cur_index, ", larger than selected maximum ", max.i, ". Stopping search.\n",
          sep = ""
      )
      cat("Final projection: \n")
      if (ncol(current) == 1) {
        for (i in 1:length(current)) {
          cat(sprintf("%.3f", current[i]), " ")
        }
        cat("\n")
      }
      else {
        for (i in 1:nrow(current)) {
          for (j in 1:ncol(current)) {
            cat(sprintf("%.3f", current[i, j]), " ")
          }
          cat("\n")
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

  new_givens_path("grand", generator)
}

#' Path needed for tour with Givens interpolation
#'
#' @param name name to give tour path
#' @param generate basis generator function
#' @param frozen matrix giving frozen variables, as described in
#'   \code{\link{freeze}}
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

      #cat("generation:  dist =  ", dist, "\n")
    }
    list(ingred = givens_components, index = gen$index, tries = tries)
  }

  structure(
    tour_path,
    name = name,
    class = c("tour_path", "function")
  )
}
