# crtcheck.R RT_DIR CASES - replay the glibc reference cases of tools/crt/test/mkfmt.c (printf with doubles, ints,
# longs and strings; strtod; sscanf "%d %lf %s") through the R port's runtime (rt.R) and count the differences.
# Run by run_crtcheck.sh. Written by Claude (Anthropic) for Robert Dickinson; see ../README.md.
a <- commandArgs(TRUE)
E <- new.env(parent = baseenv())
sys.source(file.path(a[1], "rt.R"), envir = E)
for (nm in ls(E)) { f <- get(nm, E); if (is.function(f) && identical(environment(f), E)) assign(nm, compiler::cmpfun(f), envir = E) }
L <- readLines(a[2])
hex2raw <- function(h) if (nchar(h) == 0) raw(0) else as.raw(strtoi(substring(h, seq(1, nchar(h), 2), seq(2, nchar(h), 2)), 16L))
raw2hex <- function(r) paste(as.character(r), collapse = "")
bits2d <- function(h) readBin(rev(hex2raw(h)), "double", endian = "little")
d2bits <- function(x) raw2hex(rev(writeBin(x, raw(), endian = "little")))
lit <- function(r) E$rt_lit_bytes(E$rt_bytes2num(r))
cnt <- list(); bad <- list(); shown <- 0; skipped <- 0
note <- function(k, ok, msg) {
  cnt[[k]] <<- (if (is.null(cnt[[k]])) 0 else cnt[[k]]) + 1
  if (!ok) { bad[[k]] <<- (if (is.null(bad[[k]])) 0 else bad[[k]]) + 1
    if (shown < 30) { shown <<- shown + 1; cat("DIFF", msg, "\n") } }
}
for (line in L) {
  p <- strsplit(line, "\t", fixed = TRUE)[[1]]
  if (length(p) < 4) p <- c(p, rep("", 4 - length(p)))
  kind <- p[1]; fmt <- p[2]; arg <- p[3]; want <- p[4]
  if (kind == "f") {
    x <- bits2d(arg)
    args <- if (fmt == "%*.*f") list(9, 3, x) else list(x)
    got <- raw2hex(E$rt_format(E$rt_lit(fmt), args))
    note(paste("printf", "%f-family"), got == want, paste(fmt, arg, want, got))
  } else if (kind == "i") {
    v <- strtoi(arg, 16L); if (is.na(v)) v <- as.numeric(paste0("0x", arg)) - 4294967296
    v <- as.numeric(v); if (v >= 2147483648) v <- v - 4294967296
    got <- raw2hex(E$rt_format(E$rt_lit(fmt), list(v)))
    note("printf int", got == want, paste(fmt, arg, want, got))
  } else if (kind == "l") {
    hi <- strtoi(substr(arg, 1, 8), 16L); lo <- strtoi(substr(arg, 9, 16), 16L)
    hi <- as.numeric(if (is.na(hi)) as.numeric(paste0("0x", substr(arg, 1, 8))) else hi)
    lo <- as.numeric(if (is.na(lo)) as.numeric(paste0("0x", substr(arg, 9, 16))) else lo)
    if (hi >= 2147483648) hi <- hi - 4294967296
    v <- hi * 4294967296 + lo
    if (abs(v) >= 2^53) { skipped <- skipped + 1; next }
    got <- raw2hex(E$rt_format(E$rt_lit(fmt), list(v)))
    note("printf long", got == want, paste(fmt, arg, want, got))
  } else if (kind == "s") {
    got <- raw2hex(E$rt_format(E$rt_lit(fmt), list(lit(hex2raw(arg)))))
    note("printf string", got == want, paste(fmt, arg, want, got))
  } else if (kind == "d") {
    s <- lit(hex2raw(arg)); endb <- E$rt_box(NULL)
    v <- E$rt_strtod(s, endb)
    got <- paste0(d2bits(v), ":", endb$v[[2L]])
    note("strtod", got == want, paste(rawToChar(hex2raw(arg)), want, got))
  } else if (kind == "n") {
    s <- lit(hex2raw(arg)); bi <- E$rt_box(-77); bd <- E$rt_box(-77); buf <- E$rt_cbuf(32); E$rt_put(buf, 63)
    n <- E$rt_sscanf(s, E$rt_lit(gsub("%%", "%", fmt, fixed = TRUE)), list(list("i", bi), list("d", bd), buf))
    got <- paste0(n, ":", bi$v, ":", d2bits(bd$v), ":", raw2hex(E$rt_raw_of(E$rt_bytes_of(buf))))
    note("sscanf", got == want, paste(rawToChar(hex2raw(arg)), want, got))
  }
}
tot <- 0; totbad <- 0
for (k in names(cnt)) { b <- if (is.null(bad[[k]])) 0 else bad[[k]]; tot <- tot + cnt[[k]]; totbad <- totbad + b
  cat(sprintf("%-20s %8d cases, %d differ\n", k, cnt[[k]], b)) }
cat(sprintf("all                  %8d cases, %d differ; %d %%ld cases skipped (|value| >= 2^53, not representable in an R double)\n", tot, totbad, skipped))
