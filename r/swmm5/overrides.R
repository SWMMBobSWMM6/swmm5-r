# overrides.R - hand ports of the parts of EPA SWMM 5.2.4 that use C pointer arithmetic the translator does not
# map: odesolve.c (whole file), mempool.c (whole file) and six helpers from swmm5.c and project.c; and
# three functions of rdii.c, whose arrays are kept short (see there). Each keeps the
# C statement order and floating-point expressions; comparisons of doubles are written so that NaN gives C's
# answer. (After the Python port's overrides.py.) Written by Claude (Anthropic) for Robert Dickinson.

sstrncpy <- function(dest, src, n) {
  dest; src; n
  offset <- 0
  if (n > 0) {
    while (rt_cget(src, offset) != 0) {
      if (offset == n) break
      rt_cset(dest, offset, rt_cget(src, offset))
      offset <- offset + 1
    }
  }
  rt_cset(dest, offset, 0)
  rt_strlen(dest)
}

sstrcat <- function(dest, src, size) {
  dest; src; size
  offset <- rt_strlen(dest)
  src_index <- 0
  while (rt_cget(src, src_index) != 0) {
    rt_cset(dest, offset, rt_cget(src, src_index))
    offset <- offset + 1
    src_index <- src_index + 1
    if (offset == size - 1) break
  }
  rt_cset(dest, offset, 0)
  rt_strlen(dest)
}

getAbsolutePath <- function(fname, absPath, size) {
  fname; absPath; size
  if (is.null(fname) || rt_strlen(fname) == 0) return(invisible(NULL))
  if (isRelativePath(fname) != 0) {
    full <- suppressWarnings(normalizePath(rt_str(fname), mustWork = FALSE))
    if (!startsWith(full, "/")) full <- file.path(getwd(), full)
    b <- rt_bytes2num(charToRaw(full))
    m <- min(length(b), length(absPath[[1L]]$v) - absPath[[2L]] - 1)
    rt_put(absPath, b[seq_len(m)])
  } else {
    sstrncpy(absPath, fname, rt_strlen(fname))
  }
  endOfDir <- rt_strrchr(absPath, 47)
  if (!is.null(endOfDir)) rt_cset(endOfDir, 1, 0)
  invisible(NULL)
}

getTempFileName <- function(fname) {
  fname
  sstrncpy(fname, rt_lit("swmmXXXXXX"), 259)
  rt_mkstemp(fname)
  fname
}

project_createMatrix <- function(nrows, ncols) {
  nrows; ncols
  if (nrows <= 0 || ncols <= 0) return(NULL)
  # EPA zeroes only an eighth of the matrix (lessons/02, defect 5); the port zeroes all of it
  rt_ra(lapply(seq_len(nrows), function(i) rt_ra(numeric(ncols))))
}
project_freeMatrix <- function(a) invisible(NULL)

# mempool.c: EPA keeps object ID strings in a pool; each Alloc is a fresh buffer here
AllocInit <- function() TRUE
Alloc <- function(n) rt_cbuf(n)
AllocSetPool <- function(p) p
AllocReset <- function() invisible(NULL)
AllocFreePool <- function() invisible(NULL)

# ------------------------------------------------------------------ odesolve.c (Cash-Karp Runge-Kutta)
.ode <- new.env(parent = emptyenv()); .ode$nmax <- 0
odesolve_open <- function(n) {
  n
  .ode$nmax <- 0
  for (k in c("y", "yscal", "dydx", "yerr", "ytemp", "ak2", "ak3", "ak4", "ak5", "ak6")) assign(k, rt_ra(numeric(n)), envir = .ode)
  .ode$nmax <- n
  1
}
odesolve_close <- function() { .ode$nmax <- 0; invisible(NULL) }

odesolve_integrate <- function(ystart, n, x1, x2, eps, h1, derivs) {
  ystart; n; x1; x2; eps; h1; derivs
  y <- .ode$y; yscal <- .ode$yscal; dydx <- .ode$dydx
  hdid <- rt_box(0); hnext <- rt_box(0)
  x <- rt_box(x1)
  h <- h1
  if (.ode$nmax < n) return(1)
  for (i in seq_len(n)) y$v[[i]] <- ystart$v[[i]]
  for (nstp in 1:10000) {
    derivs(x$v, y, dydx)
    for (i in seq_len(n)) yscal$v[[i]] <- abs(y$v[[i]]) + abs(dydx$v[[i]] * h) + 1e-30
    if (!is.na(.c <- ((x$v + h - x2) * (x$v + h - x1) > 0)) && .c) h <- x2 - x$v
    errcode <- rkqs(x, n, h, eps, hdid, hnext, derivs)
    if (errcode != 0) break
    if (!is.na(.c <- ((x$v - x2) * (x2 - x1) >= 0)) && .c) {
      for (i in seq_len(n)) ystart$v[[i]] <- y$v[[i]]
      return(0)
    }
    if (!is.na(.c <- (abs(hnext$v) <= 0)) && .c) return(2)
    h <- hnext$v
  }
  3
}

rkqs <- function(x, n, htry, eps, hdid, hnext, derivs) {
  y <- .ode$y; yerr <- .ode$yerr; yscal <- .ode$yscal; ytemp <- .ode$ytemp
  xold <- x$v
  h <- htry
  repeat {
    rkck(xold, n, h, derivs)
    errmax <- 0
    for (i in seq_len(n)) {
      err <- abs(yerr$v[[i]] / yscal$v[[i]])
      if (!is.na(.c <- (err > errmax)) && .c) errmax <- err
    }
    errmax <- errmax / eps
    if (!is.na(.c <- (errmax > 1)) && .c) {
      htemp <- 0.9 * h * rt_powc(errmax, -0.25)
      if (!is.na(.c <- (h >= 0)) && .c) {
        if (!is.na(.c <- (htemp > 0.1 * h)) && .c) h <- htemp else h <- 0.1 * h
      } else {
        if (!is.na(.c <- (htemp < 0.1 * h)) && .c) h <- htemp else h <- 0.1 * h
      }
      xnew <- xold + h
      if (!is.na(.c <- (xnew == xold)) && .c) return(2)
      next
    } else {
      if (!is.na(.c <- (errmax > 1.89e-4)) && .c) hnext$v <- 0.9 * h * rt_powc(errmax, -0.2)
      else hnext$v <- 5 * h
      hdid$v <- h
      x$v <- x$v + h
      for (i in seq_len(n)) y$v[[i]] <- ytemp$v[[i]]
      return(0)
    }
  }
}

rkck <- function(x, n, h, derivs) {
  y <- .ode$y; dydx <- .ode$dydx; ytemp <- .ode$ytemp; yerr <- .ode$yerr
  ak2 <- .ode$ak2; ak3 <- .ode$ak3; ak4 <- .ode$ak4; ak5 <- .ode$ak5; ak6 <- .ode$ak6
  a2 <- 0.2; a3 <- 0.3; a4 <- 0.6; a5 <- 1.0; a6 <- 0.875
  b21 <- 0.2; b31 <- 3.0 / 40.0; b32 <- 9.0 / 40.0; b41 <- 0.3; b42 <- -0.9; b43 <- 1.2
  b51 <- -11.0 / 54.0; b52 <- 2.5; b53 <- -70.0 / 27.0; b54 <- 35.0 / 27.0
  b61 <- 1631.0 / 55296.0; b62 <- 175.0 / 512.0; b63 <- 575.0 / 13824.0
  b64 <- 44275.0 / 110592.0; b65 <- 253.0 / 4096.0; c1 <- 37.0 / 378.0
  c3 <- 250.0 / 621.0; c4 <- 125.0 / 594.0; c6 <- 512.0 / 1771.0
  dc5 <- -277.0 / 14336.0
  dc1 <- c1 - 2825.0 / 27648.0; dc3 <- c3 - 18575.0 / 48384.0
  dc4 <- c4 - 13525.0 / 55296.0; dc6 <- c6 - 0.25
  s <- seq_len(n)
  for (i in s) ytemp$v[[i]] <- y$v[[i]] + b21 * h * dydx$v[[i]]
  derivs(x + a2 * h, ytemp, ak2)
  for (i in s) ytemp$v[[i]] <- y$v[[i]] + h * (b31 * dydx$v[[i]] + b32 * ak2$v[[i]])
  derivs(x + a3 * h, ytemp, ak3)
  for (i in s) ytemp$v[[i]] <- y$v[[i]] + h * (b41 * dydx$v[[i]] + b42 * ak2$v[[i]] + b43 * ak3$v[[i]])
  derivs(x + a4 * h, ytemp, ak4)
  for (i in s) ytemp$v[[i]] <- y$v[[i]] + h * (b51 * dydx$v[[i]] + b52 * ak2$v[[i]] + b53 * ak3$v[[i]] + b54 * ak4$v[[i]])
  derivs(x + a5 * h, ytemp, ak5)
  for (i in s) ytemp$v[[i]] <- y$v[[i]] + h * (b61 * dydx$v[[i]] + b62 * ak2$v[[i]] + b63 * ak3$v[[i]] + b64 * ak4$v[[i]] +
                                              b65 * ak5$v[[i]])
  derivs(x + a6 * h, ytemp, ak6)
  for (i in s) ytemp$v[[i]] <- y$v[[i]] + h * (c1 * dydx$v[[i]] + c3 * ak3$v[[i]] + c4 * ak4$v[[i]] + c6 * ak6$v[[i]])
  for (i in s) yerr$v[[i]] <- h * (dc1 * dydx$v[[i]] + dc3 * ak3$v[[i]] + dc4 * ak4$v[[i]] + dc5 * ak5$v[[i]] + dc6 * ak6$v[[i]])
  invisible(NULL)
}

# rdii.c: allocRdiiMemory, updateDryPeriod and getUnitHydConvol. EPA callocs, for each unit hydrograph, maxPeriods
# past rainfall values and months (maxPeriods = the UH's time base over the rain interval, 594,001 for a 9,900-hour
# base at a 60-second WET_STEP). calloc's memory costs nothing until it is touched; EPA writes entry `period`
# (0, 1, 2, ... wrapping at maxPeriods) once per rain interval, and sets every past rainfall value to 0 at the start
# of an RDII event (then `period` = 0). An R vector is zero-filled when it is made, so here both arrays start
# empty, the writes (always at the next index, as above) extend them, an empty array stands for all zeros (so the
# start of an event empties pastRain), and the one reader, getUnitHydConvol, takes an entry past the end as 0
# (and stops if a write ever left a gap, which R fills with NA). Hydrology/sewer_rdii_problem.inp needs 6 GB
# the other way, and zeroing its 603 unused hydrographs' arrays one entry at a time takes R minutes.
allocRdiiMemory <- function() {
  k <- 0
  n <- 0
  UHGroup <<- rt_recs(rt_u64(Nobjects$v[[13]]), T_TUHGroup)
  if (!(!is.null(UHGroup))) return(0)
  i <- 0
  while (i < Nobjects$v[[13]]) {
    {.e1 <- UHGroup[[i + 1]]; .e1$rainInterval <- getRainInterval(i)}
    k <- 0
    while (k < 3) {
      .e2 <- ((UHGroup[[i + 1]])$uh)[[k + 1]]
      .e2$pastRain <- NULL
      .e2$pastMonth <- NULL
      .e2$maxPeriods <- getMaxPeriods(i, k)
      n <- .e2$maxPeriods
      if (n > 0) {
        .e2$pastRain <- rt_ra(numeric(0))
        .e2$pastMonth <- list(rt_ra(numeric(0)), 0)
      }
      k <- k + 1
    }
    i <- i + 1
  }
  RdiiNodeIndex <<- rt_ra(numeric(rt_u64(NumRdiiNodes$v)))
  if (!(!is.null(RdiiNodeIndex))) return(0)
  RdiiNodeFlow <<- rt_ra(numeric(rt_u64(NumRdiiNodes$v)))
  if (!(!is.null(RdiiNodeFlow))) return(0)
  return(1)
}

updateDryPeriod <- function(j, k, rainDepth, rainInterval) {
  j; k; rainDepth; rainInterval
  uh <- ((UHGroup[[j + 1]])$uh)[[k + 1]]
  if (!is.na(.c <- (rainDepth > 0)) && .c) {
    if (uh$drySeconds >= (rainInterval * uh$maxPeriods)) {
      # EPA sets all maxPeriods entries to 0.0; an empty array reads as all zeros
      if (uh$maxPeriods > 0) uh$pastRain$v <- numeric(0)
      uh$period <- 0
    }
    uh$drySeconds <- 0
    uh$hasPastRain <- 1
  } else {
    uh$drySeconds <- uh$drySeconds + rainInterval
    if (uh$drySeconds >= (rainInterval * uh$maxPeriods)) {
      uh$hasPastRain <- 0
    } else {
      uh$hasPastRain <- 1
    }
  }
}

getUnitHydConvol <- function(j, k, rainInterval) {
  j; k; rainInterval
  # EPA's loop visits p = 1 .. maxPeriods-1 with i = period-1, period-2, ... (wrapping), and adds a term only
  # where pastRain[i] > 0. The same terms are added here in the same order of p; the visits that add nothing
  # (rain 0, NaN, or an entry not yet written) are skipped, which in R saves a loop over up to 594,000 entries.
  rdii <- 0
  uh <- ((UHGroup[[j + 1]])$uh)[[k + 1]]
  nm <- length(uh$pastMonth[[1L]]$v)
  if (anyNA(uh$pastRain$v) && any(is.na(uh$pastRain$v) & !is.nan(uh$pastRain$v)))
    stop("RDII past rainfall written out of order")
  i0 <- uh$period - 1
  if (i0 < 0) i0 <- uh$maxPeriods - 1
  pMax <- uh$maxPeriods
  wet <- which(uh$pastRain$v > 0) - 1
  pw <- ((i0 - wet) %% pMax) + 1
  keep <- pw < pMax
  wet <- wet[keep]
  pw <- pw[keep]
  for (q in order(pw)) {
    i <- wet[[q]]
    p <- pw[[q]]
    v <- uh$pastRain$v[[i + 1]]
    m <- if (i < nm) uh$pastMonth[[1L]]$v[[i + 1]] else 0
    t <- (p - 0.5) * rainInterval
    u <- getUnitHydOrd(j, m, k, t) * ((((UnitHyd[[j + 1]])$r)$v[[m + 1]])$v[[k + 1]])
    rdii <- rdii + (u * v)
  }
  return(rdii)
}
