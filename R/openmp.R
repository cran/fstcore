#  fstcore - R bindings to the fstlib library
#
#  Copyright (C) 2017-present, Mark AJ Klik
#
#  This file is part of the fstcore R package.
#
#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this file,
#  You can obtain one at https://mozilla.org/MPL/2.0/.
#
#  https://www.mozilla.org/en-US/MPL/2.0/FAQ/
#
#  You can contact the author at:
#  - fstcore R package source repository : https://github.com/fstpackage/fstcore


#' Get or set the number of threads used in parallel operations
#'
#' For parallel operations, the performance is determined to a great extend by the number of threads
#' used. More threads will allow the CPU to perform more computational intensive tasks simultaneously,
#' speeding up the operation. Using more threads also introduces some overhead that will scale with the
#' number of threads used. Therefore, using the maximum number of available threads is not always the
#' fastest solution. With \code{threads_fstlib} the number of threads can be adjusted to the users
#' specific requirements. As a default, \code{fstcore} uses a number of threads equal to the number of
#' logical cores in the system.
#'
#' The number of threads can also be set with \code{options(fst_threads = N)}.
#' NOTE: This option is only read when the package's namespace is first loaded, with commands like
#' \code{library}, \code{require}, or \code{::}. If you have already used one of these, you
#' must use \code{threads_fstlib} to set the number of threads.
#'
#' @param nr_of_threads number of threads to use or \code{NULL} to get the current number of threads used in
#' multithreaded operations.
#' @param reset_after_fork when \code{fstcore} is running in a forked process, the usage of OpenMP can
#' create problems. To prevent these, \code{fstcore} switches back to single core usage when it detects a fork.
#' After the fork, the number of threads is reset to it's initial setting. However, on some compilers
#' (e.g. Intel), switching back to multi-threaded mode can lead to issues. When \code{reset_after_fork}
#' is set to \code{FALSE}, \code{fstcore} is left in single-threaded mode after the fork ends. After the fork,
#' multithreading can be activated again manually by calling \code{threads_fstlib} with an appropriate value
#' for \code{nr_of_threads}. The default (\code{reset_after_fork = NULL}) leaves the fork behavior unchanged.
#'
#' @return the number of threads (previously) used
#' @export
#' @examples
#' # get current number of threads
#' threads_fstlib()
#'
#' # set the number of threads
#' threads_fstlib(12)
#'
#' # leave in single threaded mode after a fork
#' threads_fstlib(12, reset_after_fork = FALSE)
#'
#' # reset number of threads after a fork
#' threads_fstlib(12, reset_after_fork = TRUE)
threads_fstlib <- function(nr_of_threads = NULL, reset_after_fork = NULL) {

  if (!is.null(reset_after_fork)) {
    if (!is.logical(reset_after_fork) || length(reset_after_fork) != 1 || is.na(reset_after_fork)) {
      stop("Parameter reset_after_fork should be set to TRUE or FALSE")
    }

    # change unfork behavior
    restore_after_fork(reset_after_fork)
  }

  if (is.null(nr_of_threads)) {
    return(getnrofthreads())
  }

  invisible(setnrofthreads(nr_of_threads))
}
