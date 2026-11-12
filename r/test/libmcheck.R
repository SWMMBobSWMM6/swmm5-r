# libmcheck.R RT_DIR VECTORS... - compare the math the R engine uses with the system libm (the library EPA's runswmm
# calls), bit for bit, NaN payloads and signs included, on vectors written by tools/libm/test/mkvectors.c and
# mkedge.c and by test/mkexact.c (records: name\0, n:u32, then n x (args..., result) as doubles). Each function is
# called as the engine calls it: exp, sin, cos, tan, asin, acos, atan, sqrt, floor, ceiling and abs are R's own;
# log, log10 and pow go through rt.R (rt_log, rt_log10, rt_pow); pow with a constant exponent that is not an
# integer is the inline form the translator writes; (float) is rt_f32; (int) and (long) are the translator's inline
# forms, the safe engine's and the fast engine's (the fast one must stop with an error for NaN). A difference whose
# argument is a signaling NaN is counted apart: R hands a NaN argument back unchanged without calling the C library,
# glibc returns it quieted; no SWMM value is a signaling NaN (arithmetic and strtod give quiet ones).
# Written by Claude (Anthropic) for Robert Dickinson; see ../README.md.
a <- commandArgs(TRUE)
E <- new.env(parent = baseenv())
sys.source(file.path(a[1], "rt.R"), envir = E)
for (nm in ls(E)) { f <- get(nm, E); if (is.function(f) && identical(environment(f), E)) assign(nm, compiler::cmpfun(f), envir = E) }
powc <- compiler::cmpfun(function(x, c) (if ((.p <- x) > -Inf || is.na(.p)) .p ^ c else E$rt_pow(.p, c)))
d2i_safe <- compiler::cmpfun(function(x) (if (is.na(.d <- x) || .d >= 2147483648 || .d <= -2147483649) -2147483648 else trunc(.d)))
d2i_fast <- compiler::cmpfun(function(x) (if ((.d <- x) < 2147483648 && .d > -2147483649) trunc(.d) else -2147483648))
d2l_safe <- compiler::cmpfun(function(x) (if (is.na(.d <- x) || .d >= 9223372036854775808 || .d <= -9223372036854777856) -9223372036854775808 else trunc(.d)))
d2l_fast <- compiler::cmpfun(function(x) (if ((.d <- x) < 9223372036854775808 && .d > -9223372036854777856) trunc(.d) else -9223372036854775808))
neg <- function(x) -x
fns <- list(exp = exp, log = E$rt_log, log10 = E$rt_log10, pow = E$rt_pow, sin = sin, cos = cos, tan = tan, asin = asin,
            acos = acos, atan = atan, sqrt = sqrt, floor = floor, ceil = ceiling, fabs = abs, neg = neg, f32 = E$rt_f32,
            d2i = d2i_safe, d2l = d2l_safe)
NARGS <- c(pow = 2, fma = 3)
bits <- function(x) writeBin(x, raw(), endian = "little")
issnan <- function(x) is.na(x) && !bitwAnd(as.integer(bits(x)[7]), 8L)    # quiet bit (bit 51) clear
i64 <- function(x) E$rt_i64_bytes(x)
for (path in a[-1]) {
  con <- file(path, "rb"); dat <- readBin(con, "raw", file.size(path)); close(con)
  pos <- 1L
  cat("== ", basename(path), "\n", sep = "")
  while (pos <= length(dat)) {
    z <- pos; while (dat[z] != as.raw(0)) z <- z + 1L
    name <- rawToChar(dat[pos:(z - 1L)])
    n <- readBin(dat[(z + 1L):(z + 4L)], "integer", size = 4, endian = "little")
    na <- if (name %in% names(NARGS)) NARGS[[name]] else 1
    m <- n * (na + 1)
    v <- readBin(dat[(z + 5L):(z + 4L + 8L * m)], "double", n = m, size = 8, endian = "little")
    pos <- z + 5L + 8L * m
    if (is.null(fns[[name]])) { cat(sprintf("%-14s skipped (not used by the R engine)\n", name)); next }
    M <- matrix(v, nrow = na + 1)
    runs <- list(list(name, fns[[name]]))
    if (name == "pow") runs[[2]] <- list("pow_const_y", powc)
    if (name == "d2i") runs[[2]] <- list("d2i_fast", d2i_fast)
    if (name == "d2l") runs[[2]] <- list("d2l_fast", d2l_fast)
    for (run in runs) {
      label <- run[[1]]; fn <- run[[2]]
      bad <- 0; snan <- 0; cnt <- 0; shown <- 0; errs <- 0
      t0 <- proc.time()[[3]]
      for (i in seq_len(n)) {
        x <- M[1, i]; want <- M[na + 1, i]
        if (label == "pow_const_y") { y <- M[2, i]; if (!is.finite(y) || y == trunc(y)) next }
        cnt <- cnt + 1
        if (label %in% c("d2i_fast", "d2l_fast") && is.na(x)) {
          r <- tryCatch(fn(x), error = function(e) "stopped")
          if (!identical(r, "stopped")) { bad <- bad + 1 } else errs <- errs + 1
          next
        }
        got <- if (na == 1) fn(x) else fn(x, M[2, i])
        ok <- if (name %in% c("d2i", "d2l")) identical(i64(if (name == "d2i" && got < 0 && got >= -2147483648) got else got), bits(want)) else identical(bits(got), bits(want))
        if (!ok) {
          if (is.na(x) && issnan(x) || (na > 1 && is.na(M[2, i]) && issnan(M[2, i]))) snan <- snan + 1
          else { bad <- bad + 1; if (shown < 3) { shown <- shown + 1
            cat(sprintf("   %s(%s%s) want %s got %s\n", label, sprintf("%a", x), if (na > 1) paste0(", ", sprintf("%a", M[2, i])) else "",
                        paste(rev(as.character(bits(want))), collapse = ""), paste(rev(as.character(if (name %in% c("d2i", "d2l")) i64(got) else bits(got))), collapse = ""))) } }
        }
      }
      cat(sprintf("%-14s %8d of %8d differ%s%s  (%.2f us/call)\n", label, bad, cnt,
                  if (snan) sprintf(" (+%d with a signaling NaN argument)", snan) else "",
                  if (errs) sprintf(", NaN stopped the fast form %d times as it should", errs) else "",
                  (proc.time()[[3]] - t0) / max(cnt, 1) * 1e6))
    }
  }
}
