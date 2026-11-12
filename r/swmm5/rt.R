# rt.R - the C library pieces EPA SWMM 5.2.4 uses, for the R port: C strings (CPtr), boxes, reference arrays,
# integer conversions as x86-64 does them, glibc's printf, strtod and sscanf, stdio on R connections, and the
# math functions where R's own differ from glibc's (log, log10, pow). Base R only.
# Written by Claude (Anthropic) for Robert Dickinson; see ../README.md.

# ------------------------------------------------------------------------------------------- references
# a reference array (C array of numbers or pointers) and a box (C pointer to one number) are environments
# with field v: R vectors and lists are values, environments are references
rt_ra <- function(v) { e <- new.env(hash = FALSE, parent = emptyenv()); e$v <- v; e }
rt_box <- function(x) { e <- new.env(hash = FALSE, parent = emptyenv()); e$v <- x; e }
rt_recs <- function(n, ctor) if (n <= 0) list() else lapply(seq_len(n), function(i) ctor())
rt_rec_init <- function(e, ...) { a <- list(...); for (k in names(a)) assign(k, a[[k]], envir = e); e }
rt_racopy <- function(d, s) {
  v <- s$v
  if (is.list(v)) {
    for (i in seq_along(v)) {
      x <- v[[i]]
      if (is.environment(x) && is.environment(d$v[[i]])) rt_racopy(d$v[[i]], x)
      else if (is.list(x) && length(x) == 2L && is.environment(x[[1L]]) && is.list(d$v[[i]])) rt_ccopy(d$v[[i]], x)
      else d$v[[i]] <- x
    }
  } else d$v <- v
  invisible(d)
}
rt_realloc_ref <- function(a, n) {
  if (is.null(a)) return(rt_ra(numeric(n)))
  v <- a$v
  if (length(v) >= n) a$v <- v[seq_len(n)] else a$v <- c(v, if (is.list(v)) vector("list", n - length(v)) else numeric(n - length(v)))
  a
}
rt_realloc_recs <- function(a, n, ctor) {
  if (is.null(a)) return(rt_recs(n, ctor))
  if (length(a) >= n) a[seq_len(n)] else c(a, rt_recs(n - length(a), ctor))
}
rt_realloc_char <- function(p, n) {
  q <- rt_cbuf(n)
  if (!is.null(p)) {
    v <- p[[1L]]$v; o <- p[[2L]]
    m <- min(n, length(v) - o)
    if (m > 0) q[[1L]]$v[1:m] <- v[(o + 1):(o + m)]
  }
  q
}
rt_memset <- function(a, val, n) {
  if (is.list(a)) { for (i in seq_len(n)) rt_cset(a, i - 1, val); return(invisible(a)) }
  k <- min(trunc(n / 8), length(a$v))
  if (k > 0) a$v[1:k] <- 0
  invisible(a)
}

# ------------------------------------------------------------------------------------------- integers
# (int)x and (long)x of a double, as x86-64's cvttsd2si: truncation, and the "integer indefinite" value
# (INT_MIN / LONG_MIN) for NaN and out-of-range values
rt_d2i <- function(x) if (is.na(x) || x >= 2147483648 || x <= -2147483649) -2147483648 else trunc(x)
rt_d2l <- function(x) if (is.na(x) || x >= 9223372036854775808 || x < -9223372036854775808) -9223372036854775808 else trunc(x)
rt_i32 <- function(x) { x <- x %% 4294967296; if (x >= 2147483648) x - 4294967296 else x }
rt_u64 <- function(x) if (x < 0) x + 18446744073709551616 else x
rt_imod <- function(a, b) a - trunc(a / b) * b
rt_u16 <- function(x) { x <- x %% 4294967296; c(x %/% 65536, x %% 65536) }
rt_band <- function(a, b) { x <- rt_u16(a); y <- rt_u16(b); bitwAnd(x[1], y[1]) * 65536 + bitwAnd(x[2], y[2]) }
rt_bor <- function(a, b) { x <- rt_u16(a); y <- rt_u16(b); bitwOr(x[1], y[1]) * 65536 + bitwOr(x[2], y[2]) }
rt_bxor <- function(a, b) { x <- rt_u16(a); y <- rt_u16(b); bitwXor(x[1], y[1]) * 65536 + bitwXor(x[2], y[2]) }

# (float)x: writeBin with size 4 is C's (float) cast (cvtsd2ss: round to nearest even, NaN quieted with its sign)
rt_f32 <- function(x) readBin(writeBin(x, raw(), size = 4L), "double", size = 4L)

# ------------------------------------------------------------------------------------------- math
# R's log returns R_NaN (sign bit clear) for x < 0 and for NaN; glibc's log@GLIBC_2.29 returns
# (x - x) / (x - x): x86's default NaN (sign bit set) for x < 0, and x itself (quieted) for NaN
rt_log <- function(x) if (is.na(x)) x + 0 else if (x > 0) log(x) else if (x == 0) -Inf else (x - x) / (x - x)
# glibc's log10 (the SVID wrapper on x86-64) returns __ieee754_log10(x) = x + x for NaN; R returns R_NaN
rt_log10 <- function(x) if (is.na(x)) x + x else log10(x)
rt_signbit <- function(x) {
  if (is.na(x)) return(writeBin(x, raw(), endian = "little")[8L] >= as.raw(0x80))
  x < 0 || (x == 0 && 1 / x < 0)
}
rt_checkint <- function(y) {          # glibc pow's checkint: 0 not an integer, 1 odd, 2 even
  if (y != trunc(y)) return(0)
  if (abs(y) >= 9007199254740992) return(2)
  if ((abs(y) %% 2) == 1) 1 else 2
}
rt_qweibull <- stats::qweibull
# C's pow(x, y). R's x ^ y is glibc's pow(x, y) for finite nonzero x and finite y, except y == 2, which R
# computes as x * x (glibc's pow(x, 2) is not always x * x). For y == 2 and finite nonzero x, R's qweibull
# returns 1 * pow(-p, 1 / 0.5) with p = -|x|, glibc's pow(|x|, 2) = pow(x, 2) (r/test/run_libm_tests.sh).
# Zero, infinite and NaN arguments follow glibc's e_pow.c special cases, written out here.
rt_pow <- function(x, y) {
  if (is.finite(x) && is.finite(y) && x != 0) {
    if (y != 2) return(x ^ y)
    return(rt_qweibull(-abs(x), 0.5, 1, FALSE, TRUE))
  }
  if (!is.na(y) && y == 2) return(x * x)
  if (is.na(y) || is.infinite(y) || y == 0) {           # zeroinfnan(y)
    if (!is.na(y) && y == 0) return(1)
    if (!is.na(x) && x == 1 && !rt_signbit(x)) return(1)
    if (is.na(x) || is.na(y)) return(x + y)
    if (abs(x) == 1) return(1)
    if ((abs(x) < 1) == (y > 0)) return(0)
    return(y * y)
  }
  # x is zero, infinite or NaN; y finite and nonzero
  x2 <- x * x
  if (rt_signbit(x) && rt_checkint(y) == 1) x2 <- -x2
  if (y < 0) 1 / x2 else x2
}
# pow(x, c) for a constant c that is not an integer: R's x ^ c is glibc's pow(x, c) for every x but -Inf
rt_powc <- function(x, c) if (x > -Inf || is.na(x)) x ^ c else rt_pow(x, c)

# ------------------------------------------------------------------------------------------- C strings
# char* is a CPtr: list(buffer, offset); the buffer is a reference array of byte values (signed char)
rt_cbuf <- function(n) list(rt_ra(numeric(max(n, 1))), 0)
rt_bytes2num <- function(r) { v <- as.numeric(r); v[v > 127] <- v[v > 127] - 256; v }
rt_lit <- function(s) {
  b <- rt_ra(c(rt_bytes2num(charToRaw(s)), 0))
  b$const <- TRUE
  list(b, 0)
}
rt_lit_bytes <- function(v) { b <- rt_ra(c(v, 0)); b$const <- TRUE; list(b, 0) }
rt_cbuf_from <- function(p, n) { q <- rt_cbuf(n); rt_strcpy(q, p); q }
rt_cget <- function(p, i) p[[1L]]$v[[p[[2L]] + i + 1]]
rt_cset <- function(p, i, c) { b <- p[[1L]]; b$v[[p[[2L]] + i + 1]] <- c; c }
rt_cadd <- function(p, n) list(p[[1L]], p[[2L]] + n)
rt_cdiff <- function(a, b) a[[2L]] - b[[2L]]
rt_cpcmp <- function(a, b, op) {
  if (is.null(a) || is.null(b)) return(switch(op, "==" = is.null(a) && is.null(b), "!=" = !(is.null(a) && is.null(b)), FALSE))
  same <- identical(a[[1L]], b[[1L]])
  switch(op, "==" = same && a[[2L]] == b[[2L]], "!=" = !(same && a[[2L]] == b[[2L]]),
         "<" = a[[2L]] < b[[2L]], ">" = a[[2L]] > b[[2L]], "<=" = a[[2L]] <= b[[2L]], ">=" = a[[2L]] >= b[[2L]])
}
rt_ccopy <- function(d, s) { d[[1L]]$v <- s[[1L]]$v; invisible(d) }
# position (0-based, relative to the pointer) of the terminating NUL
rt_strlen <- function(p) {
  v <- p[[1L]]$v; o <- p[[2L]]
  k <- match(0, if (o == 0) v else v[(o + 1):length(v)])
  if (is.na(k)) length(v) - o else k - 1
}
rt_bytes_of <- function(p) { n <- rt_strlen(p); if (n == 0) numeric(0) else p[[1L]]$v[(p[[2L]] + 1):(p[[2L]] + n)] }
rt_raw_of <- function(v) as.raw(v %% 256)
rt_str <- function(p) rawToChar(rt_raw_of(rt_bytes_of(p)))
rt_put <- function(p, v) {      # write the byte values v and a NUL at p
  b <- p[[1L]]; o <- p[[2L]]; n <- length(v)
  if (n > 0) b$v[(o + 1):(o + n)] <- v
  b$v[[o + n + 1]] <- 0
  invisible(p)
}
rt_strcpy <- function(d, s) { rt_put(d, rt_bytes_of(s)); d }
rt_strcat <- function(d, s) { rt_put(rt_cadd(d, rt_strlen(d)), rt_bytes_of(s)); d }
rt_strcmp <- function(a, b) {
  x <- rt_bytes_of(a) %% 256; y <- rt_bytes_of(b) %% 256
  n <- min(length(x), length(y))
  if (n > 0) { d <- which(x[1:n] != y[1:n]); if (length(d)) return(x[d[1]] - y[d[1]]) }
  if (length(x) == length(y)) 0 else if (length(x) > length(y)) x[n + 1] else -y[n + 1]
}
rt_strchr <- function(s, c) {
  c <- c %% 256; if (c > 127) c <- c - 256
  v <- s[[1L]]$v; o <- s[[2L]]
  n <- rt_strlen(s)
  if (c == 0) return(list(s[[1L]], o + n))
  if (n == 0) return(NULL)
  k <- match(c, v[(o + 1):(o + n)])
  if (is.na(k)) NULL else list(s[[1L]], o + k - 1)
}
rt_strrchr <- function(s, c) {
  c <- c %% 256; if (c > 127) c <- c - 256
  n <- rt_strlen(s); o <- s[[2L]]
  if (c == 0) return(list(s[[1L]], o + n))
  if (n == 0) return(NULL)
  k <- which(s[[1L]]$v[(o + 1):(o + n)] == c)
  if (length(k) == 0) NULL else list(s[[1L]], o + k[length(k)] - 1)
}
rt_strstr <- function(h, nd) {
  x <- rt_bytes_of(h); y <- rt_bytes_of(nd)
  m <- length(y); n <- length(x)
  if (m == 0) return(h)
  if (m > n) return(NULL)
  for (i in which(x[1:(n - m + 1)] == y[1])) if (all(x[i:(i + m - 1)] == y)) return(list(h[[1L]], h[[2L]] + i - 1))
  NULL
}
rt_strcspn <- function(s, rej) {
  x <- rt_bytes_of(s); r <- rt_bytes_of(rej)
  k <- which(x %in% r)
  if (length(k)) k[1] - 1 else length(x)
}
rt_tok <- new.env(parent = emptyenv()); rt_tok$p <- NULL
rt_strtok <- function(s, delim) {
  if (!is.null(s)) rt_tok$p <- s
  p <- rt_tok$p
  if (is.null(p)) return(NULL)
  ds <- rt_bytes_of(delim)
  b <- p[[1L]]; v <- b$v; i <- p[[2L]] + 1
  while (v[[i]] != 0 && v[[i]] %in% ds) i <- i + 1
  if (v[[i]] == 0) { rt_tok$p <- NULL; return(NULL) }
  start <- i
  while (v[[i]] != 0 && !(v[[i]] %in% ds)) i <- i + 1
  if (v[[i]] != 0) { b$v[[i]] <- 0; rt_tok$p <- list(b, i) } else rt_tok$p <- NULL
  list(b, start - 1)
}
rt_toupper <- function(c) if (c >= 97 && c <= 122) c - 32 else c
rt_SPACE <- c(32, 9, 10, 11, 12, 13)

# ------------------------------------------------------------------------------------------- big integers
# nonnegative integers as numeric vectors of base-2^24 limbs, least significant first; used only where a
# decimal number cannot be converted exactly by one floating-point operation
.B <- 16777216
rt_bn_norm <- function(a) {
  repeat {
    hi <- a %/% .B
    if (all(hi == 0)) break
    a <- a %% .B + c(0, hi[-length(hi)])
    if (hi[length(hi)] != 0) a <- c(a, hi[length(hi)])
  }
  n <- length(a); while (n > 1 && a[n] == 0) n <- n - 1
  a[seq_len(n)]
}
rt_bn_mul_small <- function(a, s) rt_bn_norm(a * s)          # s < 2^24
rt_bn_shl <- function(a, k) {                                  # a * 2^k
  if (k <= 0) return(a)
  a <- c(numeric(k %/% 24), a)
  r <- k %% 24
  if (r > 0) a <- rt_bn_mul_small(a, 2^r)
  a
}
rt_bn_mul_pow5 <- function(a, k) { while (k >= 10) { a <- rt_bn_mul_small(a, 9765625); k <- k - 10 }; if (k > 0) a <- rt_bn_mul_small(a, 5^k); a }
rt_bn_cmp <- function(a, b) {
  if (length(a) != length(b)) return(sign(length(a) - length(b)))
  d <- which(a != b)
  if (length(d) == 0) 0 else sign(a[d[length(d)]] - b[d[length(d)]])
}
rt_bn_from_digits <- function(d) {                             # decimal digit values, most significant first
  a <- 0
  i <- 1; n <- length(d)
  while (i <= n) {
    j <- min(n, i + 6)
    chunk <- sum(d[i:j] * 10^((j - i):0))
    a <- rt_bn_norm(a * 10^(j - i + 1) + c(chunk, numeric(length(a) - 1)))
    i <- j + 1
  }
  a
}
rt_bn_from_int <- function(k) rt_bn_norm(c(k %% .B, (k %/% .B) %% .B, k %/% (.B * .B)))   # k < 2^53, exact
rt_bn_add1 <- function(a, s) {                                 # a + s for s = +1 or -1 (a >= 1)
  a[1] <- a[1] + s
  i <- 1
  while (a[i] < 0) { a[i] <- a[i] + .B; a[i + 1] <- a[i + 1] - 1; i <- i + 1 }
  rt_bn_norm(a)
}
# compare the exact decimal D * 10^E with (mult * k + add) * 2^q (D as big integer, k < 2^53, mult 2 or 4,
# add +1 or -1: the midpoints between neighbouring doubles, which a double cannot hold when k >= 2^52)
rt_dec_cmp <- function(D, E, k, q, mult = 1, add = 0) {
  a <- D; b <- rt_bn_from_int(k)
  if (mult != 1) b <- rt_bn_mul_small(b, mult)
  if (add != 0) b <- rt_bn_add1(b, add)
  if (E >= 0) a <- rt_bn_mul_pow5(a, E) else b <- rt_bn_mul_pow5(b, -E)
  t <- q - E                                                    # compare a vs b * 2^t
  if (t >= 0) b <- rt_bn_shl(b, t) else a <- rt_bn_shl(a, -t)
  rt_bn_cmp(a, b)
}
# x = k * 2^e with k < 2^53 integer (x finite, >= 0)
rt_ke <- function(x) {
  if (x == 0) return(c(0, -1074))
  p <- floor(log2(x)); if (2^p > x) p <- p - 1; if (2^(p + 1) <= x) p <- p + 1
  e <- max(p - 52, -1074)
  c(x / 2^e, e)
}
rt_next_up <- function(x) {
  if (x == 0) return(4.9406564584124654e-324)
  ke <- rt_ke(x); k <- ke[1] + 1; e <- ke[2]
  if (k == 9007199254740992) { k <- 4503599627370496; e <- e + 1 }
  if (e > 971) Inf else k * 2^e
}
rt_next_down <- function(x) {
  ke <- rt_ke(x); k <- ke[1] - 1; e <- ke[2]
  if (k < 4503599627370496 && e > -1074) { k <- 2 * k + 1; e <- e - 1 }
  k * 2^e
}
# the double nearest to D * 10^E (ties to even), starting from an estimate within a few ulps
rt_dec_round <- function(D, E, est) {
  if (length(D) == 1 && D == 0) return(0)
  x <- if (is.infinite(est)) 1.7976931348623157e308 else est
  repeat {
    ke <- rt_ke(x); k <- ke[1]; e <- ke[2]
    # upper midpoint (2k + 1) * 2^(e - 1)
    cu <- rt_dec_cmp(D, E, k, e - 1, 2, 1)
    if (cu > 0 || (cu == 0 && k %% 2 == 1)) { if (x == 1.7976931348623157e308) return(Inf); x <- rt_next_up(x); next }
    if (x == 0) return(0)
    # lower midpoint
    if (k == 4503599627370496 && e > -1074) cl <- rt_dec_cmp(D, E, k, e - 2, 4, -1)
    else cl <- rt_dec_cmp(D, E, k, e - 1, 2, -1)
    if (cl < 0 || (cl == 0 && k %% 2 == 1)) { x <- rt_next_down(x); next }
    return(x)
  }
}
.P10 <- 10^(0:22)

# ------------------------------------------------------------------------------------------- strtod
# glibc's strtod on the byte values v from 0-based index i, at most maxw bytes (after leading space when skip);
# returns c(value, end index) with end = -1 when no number was found
rt_scan_double <- function(v, i, maxw = Inf, skip = TRUE) {
  n <- length(v)
  if (skip) while (i < n && v[[i + 1]] %in% rt_SPACE) i <- i + 1
  lim <- min(n, i + maxw)
  g <- function(k) if (k < lim) v[[k + 1]] else 0
  low <- function(c) if (c >= 65 && c <= 90) c + 32 else c
  neg <- FALSE
  if (g(i) == 43 || g(i) == 45) { neg <- g(i) == 45; i <- i + 1 }
  sg <- function(x) if (neg) -x else x
  if (low(g(i)) == 105 && low(g(i + 1)) == 110 && low(g(i + 2)) == 102) {
    i <- i + 3
    if (all(vapply(0:4, function(k) low(g(i + k)), 0) == c(105, 110, 105, 116, 121))) i <- i + 5
    return(c(sg(Inf), i))
  }
  if (low(g(i)) == 110 && low(g(i + 1)) == 97 && low(g(i + 2)) == 110) {
    i <- i + 3
    x <- 0 / 0; x <- -x          # glibc's NAN: quiet, sign bit clear
    if (g(i) == 40) {
      k <- i + 1
      isal <- function(c) (c >= 48 && c <= 57) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || c == 95
      while (isal(g(k))) k <- k + 1
      if (g(k) == 41) {
        x <- rt_nan_payload(v[seq_len(k - i - 1) + i + 1], x)
        i <- k + 1
      }
    }
    return(c(sg(x), i))
  }
  if (g(i) == 48 && low(g(i + 1)) == 120) {
    r <- rt_scan_hex(g, i + 2)
    if (!is.null(r)) return(c(sg(r[1]), r[2]))
    # "0x" without hex digits: the number 0, ending after the 0
    return(c(sg(0), i + 1))
  }
  ms <- i; nd <- 0
  while (g(i) >= 48 && g(i) <= 57) { i <- i + 1; nd <- nd + 1 }
  ip <- i
  if (g(i) == 46) { i <- i + 1; while (g(i) >= 48 && g(i) <= 57) { i <- i + 1; nd <- nd + 1 } }
  if (nd == 0) return(c(0, -1))
  me <- i
  ex <- 0
  if (low(g(i)) == 101) {
    k <- i + 1; es <- 1
    if (g(k) == 43 || g(k) == 45) { es <- if (g(k) == 45) -1 else 1; k <- k + 1 }
    if (g(k) >= 48 && g(k) <= 57) {
      while (g(k) >= 48 && g(k) <= 57) { if (ex < 100000) ex <- ex * 10 + g(k) - 48; k <- k + 1 }
      ex <- es * ex
      i <- k
    }
  }
  digs <- v[(ms + 1):me]; digs <- digs[digs != 46] - 48
  E <- ex - (me - ip - (if (me > ip) 1 else 0))   # digits after the point lower the exponent
  # strip leading and trailing zeros
  nz <- which(digs != 0)
  if (length(nz) == 0) return(c(sg(0), i))
  digs <- digs[nz[1]:nz[length(nz)]]
  E <- E + (me - ms - (if (me > ip) 1 else 0)) - (nz[length(nz)])
  nd <- length(digs)
  if (nd <= 15) {
    M <- sum(digs * 10^((nd - 1):0))
    if (E >= 0 && E <= 22) return(c(sg(M * .P10[E + 1]), i))
    if (E < 0 && E >= -22) return(c(sg(M / .P10[-E + 1]), i))
    if (E > 22 && E <= 22 + 15 - nd) return(c(sg((M * .P10[E - 22 + 1]) * 1e22), i))
  }
  if (E + nd > 330) return(c(sg(Inf), i))
  if (E + nd < -345) return(c(sg(0), i))
  est <- suppressWarnings(as.numeric(paste0(paste(digs, collapse = ""), "e", E)))
  if (is.na(est)) est <- 1
  c(sg(rt_dec_round(rt_bn_from_digits(digs), E, est)), i)
}
rt_scan_hex <- function(g, i) {
  hv <- function(c) if (c >= 48 && c <= 57) c - 48 else if (c >= 97 && c <= 102) c - 87 else if (c >= 65 && c <= 70) c - 55 else -1
  d <- numeric(0); nd <- 0; frac <- 0; seen <- FALSE
  while (hv(g(i)) >= 0) { d <- c(d, hv(g(i))); i <- i + 1; seen <- TRUE }
  if (g(i) == 46) {
    k <- i + 1
    while (hv(g(k)) >= 0) { d <- c(d, hv(g(k))); frac <- frac + 1; k <- k + 1; seen <- TRUE }
    if (seen) i <- k
  }
  if (!seen) return(NULL)
  ex <- 0
  if (g(i) == 112 || g(i) == 80) {
    k <- i + 1; es <- 1
    if (g(k) == 43 || g(k) == 45) { es <- if (g(k) == 45) -1 else 1; k <- k + 1 }
    if (g(k) >= 48 && g(k) <= 57) {
      while (g(k) >= 48 && g(k) <= 57) { if (ex < 100000) ex <- ex * 10 + g(k) - 48; k <- k + 1 }
      ex <- es * ex; i <- k
    }
  }
  # bits, most significant first; value = bits * 2^(ex - 4 * frac)
  bits <- as.vector(t(outer(d, c(8, 4, 2, 1), function(a, b) (a %/% b) %% 2)))
  nz <- which(bits == 1)
  if (length(nz) == 0) return(c(0, i))
  bits <- bits[nz[1]:length(bits)]
  e2 <- ex - 4 * frac + (length(bits) - 1)        # exponent of the leading bit... of the value's top bit
  low <- e2 - (length(bits) - 1)                   # weight 2^low of the last bit
  c(rt_round_bits(bits, low), i)
}
# the double nearest to sum(bits * 2^(low + (n-1):0)), ties to even
rt_round_bits <- function(bits, low) {
  n <- length(bits); top <- low + n - 1
  if (top > 1023) return(Inf)
  keep <- min(n, 53, top + 1075)                   # significant bits a double can hold at this exponent
  if (keep <= 0) {
    # below half the smallest subnormal unless exactly the bits round up
    if (keep == 0 && (n > 1 && any(bits[-1] == 1))) return(4.9406564584124654e-324)
    return(0)
  }
  m <- sum(bits[1:keep] * 2^((keep - 1):0))
  if (n > keep) {
    r <- bits[keep + 1]; sticky <- n > keep + 1 && any(bits[(keep + 2):n] == 1)
    if (r == 1 && (sticky || m %% 2 == 1)) m <- m + 1
  }
  m * 2^(top - keep + 1)
}
rt_nan_payload <- function(chars, x) {
  s <- rawToChar(as.raw(chars))
  base <- 10; t <- s
  if (grepl("^0[xX]", s)) { base <- 16; t <- substring(s, 3) } else if (grepl("^0", s) && nchar(s) > 1) { base <- 8; t <- substring(s, 2) }
  if (nchar(t) == 0 && base != 8) return(x)
  dig <- utf8ToInt(tolower(t)); val <- ifelse(dig >= 97, dig - 87, dig - 48)
  if (any(val < 0 | val >= base)) return(x)
  # mantissa = value mod 2^51, as four 16-bit pieces (the quiet bit is set by glibc)
  p <- numeric(4)
  for (dv in val) {
    carry <- dv
    for (k in 1:4) { z <- p[k] * base + carry; p[k] <- z %% 65536; carry <- z %/% 65536 }
  }
  p[4] <- (p[4] %% 8) + 8 + 0x7ff0
  readBin(writeBin(as.integer(p), raw(), size = 2L, endian = "little"), "double", endian = "little")
}
rt_strtod <- function(s, endref) {
  v <- s[[1L]]$v
  r <- rt_scan_double(v, s[[2L]])
  if (!is.null(endref)) endref$v <- if (r[2] < 0) s else list(s[[1L]], r[2])
  if (r[2] < 0) 0 else r[1]
}
rt_atof <- function(s) rt_strtod(s, NULL)
rt_atoi <- function(s) {
  v <- s[[1L]]$v; i <- s[[2L]] + 1
  while (v[[i]] %in% rt_SPACE) i <- i + 1
  neg <- FALSE
  if (v[[i]] == 43 || v[[i]] == 45) { neg <- v[[i]] == 45; i <- i + 1 }
  x <- 0
  while (i <= length(v) && v[[i]] >= 48 && v[[i]] <= 57) { x <- x * 10 + v[[i]] - 48; i <- i + 1 }
  if (neg) x <- -x
  rt_l2i(x)
}
# (int) of a long that glibc's strtol produced: saturated at LONG_MAX / LONG_MIN (whose low 32 bits are -1 and 0)
rt_l2i <- function(x) if (x >= 9223372036854775807) -1 else if (x <= -9223372036854775808) 0 else rt_i32(x)

# ------------------------------------------------------------------------------------------- sscanf
# glibc's sscanf for the conversions EPA uses (%d %i %u %ld %f %lf %e %g %s %c, widths, *, literal text and
# white space). Targets: list(tag, box) for numbers, a CPtr for %s and %c.
rt_sscanf <- function(s, fmt, args) {
  v <- s[[1L]]$v; i <- s[[2L]]; n <- length(v)
  f <- rt_bytes_of(fmt); nf <- length(f)
  ch <- function(j) if (j < n) v[[j + 1]] else 0
  k <- 0; ai <- 1; count <- 0; any <- FALSE
  fail <- function() if (count == 0 && !any) -1 else count
  while (k < nf) {
    fc <- f[k + 1]
    if (fc %in% rt_SPACE) {
      while (k < nf && f[k + 1] %in% rt_SPACE) k <- k + 1
      while (ch(i) != 0 && ch(i) %in% rt_SPACE) i <- i + 1
      next
    }
    if (fc != 37) {
      if (ch(i) == 0) return(fail())
      if (ch(i) != fc) return(count)
      i <- i + 1; k <- k + 1
      next
    }
    k <- k + 1
    if (f[k + 1] == 37) {
      while (ch(i) != 0 && ch(i) %in% rt_SPACE) i <- i + 1
      if (ch(i) != 37) return(count)
      i <- i + 1; k <- k + 1
      next
    }
    suppress <- FALSE
    if (f[k + 1] == 42) { suppress <- TRUE; k <- k + 1 }
    width <- 0
    while (f[k + 1] >= 48 && f[k + 1] <= 57) { width <- width * 10 + f[k + 1] - 48; k <- k + 1 }
    lmod <- 0
    while (f[k + 1] %in% c(108, 104, 76)) { if (f[k + 1] == 108) lmod <- lmod + 1; k <- k + 1 }
    conv <- f[k + 1]; k <- k + 1
    if (!(conv %in% c(99, 91, 110))) while (ch(i) != 0 && ch(i) %in% rt_SPACE) i <- i + 1
    if (ch(i) == 0 && conv != 110) return(fail())
    if (conv %in% c(100, 105, 117)) {            # d i u
      j <- i; lim <- if (width > 0) i + width else n
      neg <- FALSE
      if (j < lim && (ch(j) == 43 || ch(j) == 45)) { neg <- ch(j) == 45; j <- j + 1 }
      ds <- j; x <- 0
      while (j < lim && ch(j) >= 48 && ch(j) <= 57) { x <- x * 10 + ch(j) - 48; j <- j + 1 }
      if (j == ds) return(count)
      i <- j; any <- TRUE
      if (neg) x <- -x
      if (!suppress) {
        a <- args[[ai]]; ai <- ai + 1
        a[[2L]]$v <- if (lmod > 0) max(min(x, 9223372036854775807), -9223372036854775808) else rt_l2i(x)
        count <- count + 1
      }
    } else if (conv %in% c(102, 101, 103)) {     # f e g
      r <- rt_scan_double(v, i, if (width > 0) width else Inf, FALSE)
      if (r[2] < 0) return(count)
      i0 <- i; i <- r[2]; any <- TRUE
      if (!suppress) {
        a <- args[[ai]]; ai <- ai + 1
        a[[2L]]$v <- if (lmod > 0 || a[[1L]] == "d") r[1] else rt_strtof(v, i0, r)
        count <- count + 1
      }
    } else if (conv == 115) {                    # s
      j <- i; w <- if (width > 0) width else Inf; out <- numeric(0)
      while (ch(j) != 0 && !(ch(j) %in% rt_SPACE) && length(out) < w) { out <- c(out, ch(j)); j <- j + 1 }
      if (!suppress) { rt_put(args[[ai]], out); ai <- ai + 1; count <- count + 1 }
      i <- j; any <- TRUE
    } else if (conv == 99) {                     # c
      cnt <- if (width > 0) width else 1
      if (ch(i) == 0) return(fail())
      a <- NULL
      if (!suppress) { a <- args[[ai]]; ai <- ai + 1 }
      t <- 0
      while (t < cnt && ch(i) != 0) {
        if (!suppress) { if (is.environment(a[[2L]])) a[[2L]]$v <- ch(i) else rt_cset(a, t, ch(i)) }
        i <- i + 1; t <- t + 1
      }
      any <- TRUE
      if (!suppress) count <- count + 1
    } else stop("sscanf conversion not supported: ", intToUtf8(conv))
  }
  count
}
# sscanf's %f into a C float: glibc's strtof rounds the decimal once; rounding the nearest double to a float
# rounds twice, which differs only when that double is exactly halfway between two floats
rt_strtof <- function(v, i0, r) {
  x <- r[1]; y <- rt_f32(x)
  if (!is.finite(x) || x == 0 || y == x || !is.finite(y)) return(y)
  nb <- rt_f32_neighbour(y, x > y)
  mid <- (y + nb) / 2
  if (x != mid) return(y)
  txt <- rawToChar(as.raw(v[(i0 + 1):r[2]] %% 256))
  m <- regmatches(txt, regexec("^[ \t\n\v\f\r]*[-+]?([0-9]*)\\.?([0-9]*)(?:[eE]([-+]?[0-9]+))?", txt, perl = TRUE))[[1]]
  if (length(m) == 0 || grepl("[xX]", txt)) return(y)
  ip <- m[2]; fp <- m[3]; ex <- if (nchar(m[4])) as.numeric(m[4]) else 0
  d <- utf8ToInt(paste0(ip, fp)) - 48
  E <- ex - nchar(fp)
  ke <- rt_ke(abs(mid))
  c <- rt_dec_cmp(rt_bn_from_digits(d), E, ke[1], ke[2])
  if (c == 0) return(y)
  big <- if (abs(y) > abs(nb)) y else nb; small <- if (abs(y) > abs(nb)) nb else y
  if (c > 0) big else small
}
rt_f32_neighbour <- function(y, upward) {
  b <- readBin(writeBin(y, raw(), size = 4L), "integer", size = 4L)
  b <- if ((y > 0) == upward) b + 1 else b - 1
  readBin(writeBin(as.integer(b), raw(), size = 4L), "double", size = 4L)
}

# ------------------------------------------------------------------------------------------- printf
rt_fmt_parse <- function(fb) {
  parts <- list(); n <- length(fb); i <- 1; lit <- 1
  addlit <- function(a, b) if (b >= a) parts[[length(parts) + 1]] <<- list(TRUE, fb[a:b])
  while (i <= n) {
    if (fb[i] != 37) { i <- i + 1; next }
    if (i < n && fb[i + 1] == 37) { addlit(lit, i); i <- i + 2; lit <- i; next }
    addlit(lit, i - 1)
    j <- i + 1; flags <- ""
    while (j <= n && fb[j] %in% c(45, 43, 32, 35, 48)) { flags <- paste0(flags, intToUtf8(fb[j])); j <- j + 1 }
    width <- ""
    if (j <= n && fb[j] == 42) { width <- "*"; j <- j + 1 } else while (j <= n && fb[j] >= 48 && fb[j] <= 57) { width <- paste0(width, intToUtf8(fb[j])); j <- j + 1 }
    prec <- NA
    if (j <= n && fb[j] == 46) {
      j <- j + 1; prec <- ""
      if (j <= n && fb[j] == 42) { prec <- "*"; j <- j + 1 } else while (j <= n && fb[j] >= 48 && fb[j] <= 57) { prec <- paste0(prec, intToUtf8(fb[j])); j <- j + 1 }
    }
    lm <- 0
    while (j <= n && fb[j] %in% c(104, 108, 76, 113, 106, 122, 116)) { if (fb[j] == 108) lm <- lm + 1; j <- j + 1 }
    parts[[length(parts) + 1]] <- list(FALSE, flags, width, prec, intToUtf8(fb[j]), lm)
    i <- j + 1; lit <- i
  }
  addlit(lit, n)
  parts
}
rt_pad <- function(s, w, left) {          # s raw; pad with spaces to width w
  if (length(s) >= w) return(s)
  pad <- rep(as.raw(32), w - length(s))
  if (left) c(s, pad) else c(pad, s)
}
# C's printf: a raw vector. Formats that are string literals are parsed once (lessons/39: never cache a buffer)
rt_format <- function(fmt, args) {
  b <- fmt[[1L]]
  if (fmt[[2L]] == 0 && isTRUE(b$const)) {
    parts <- b$fmt
    if (is.null(parts)) { parts <- rt_fmt_parse(rt_bytes_of(fmt)); b$fmt <- parts }
  } else parts <- rt_fmt_parse(rt_bytes_of(fmt))
  out <- vector("list", length(parts)); ai <- 1
  for (pi in seq_along(parts)) {
    p <- parts[[pi]]
    if (p[[1L]]) { out[[pi]] <- rt_raw_of(p[[2L]]); next }
    flags <- p[[2L]]; width <- p[[3L]]; prec <- p[[4L]]; conv <- p[[5L]]; lm <- p[[6L]]
    if (width == "*") { w <- args[[ai]]; ai <- ai + 1; if (w < 0) { flags <- paste0(flags, "-"); w <- -w }; width <- as.character(w) }
    if (!is.na(prec) && prec == "*") { pr <- args[[ai]]; ai <- ai + 1; prec <- if (pr >= 0) as.character(pr) else NA }
    a <- args[[ai]]; ai <- ai + 1
    w <- if (nchar(width)) as.numeric(width) else 0
    left <- grepl("-", flags, fixed = TRUE)
    if (conv == "s") {
      sv <- if (is.null(a)) charToRaw("(null)") else rt_raw_of(rt_bytes_of(a))
      if (!is.na(prec)) { pn <- if (prec == "") 0 else as.numeric(prec); if (length(sv) > pn) sv <- sv[seq_len(pn)] }
      out[[pi]] <- rt_pad(sv, w, left)
    } else if (conv == "c") {
      out[[pi]] <- rt_pad(as.raw(a %% 256), w, left)
    } else if (conv %in% c("f", "F", "e", "E", "g", "G")) {
      spec <- paste0("%", flags, width, if (is.na(prec)) "" else paste0(".", if (prec == "") "0" else prec), conv)
      if (is.finite(a)) out[[pi]] <- charToRaw(sprintf(spec, a))
      else {
        body <- if (is.na(a)) "nan" else "inf"
        if (conv %in% c("F", "E", "G")) body <- toupper(body)
        sgn <- if (rt_signbit(a)) "-" else if (grepl("+", flags, fixed = TRUE)) "+" else if (grepl(" ", flags, fixed = TRUE)) " " else ""
        out[[pi]] <- rt_pad(charToRaw(paste0(sgn, body)), w, left)
      }
    } else if (conv %in% c("d", "i", "u", "x", "X", "o")) {
      x <- a
      if (conv %in% c("u", "x", "X", "o")) {
        if (lm == 0) x <- x %% 4294967296
        else if (x < 0) { out[[pi]] <- rt_bigint_fmt(x, flags, w, prec, conv, TRUE); next }
      }
      if (abs(x) < 2147483648) {
        spec <- paste0("%", flags, width, if (is.na(prec)) "" else paste0(".", if (prec == "") "0" else prec), if (conv == "u" || conv == "i") "d" else conv)
        out[[pi]] <- charToRaw(sprintf(spec, as.integer(x)))
      } else {
        out[[pi]] <- rt_bigint_fmt(x, flags, w, prec, conv)
      }
    } else stop("printf conversion not supported: %", conv)
  }
  if (length(out) == 0) raw(0) else unlist(out, use.names = FALSE)
}
# the digits of a nonnegative integer given as 16-bit limbs (least significant first), in base 8, 10 or 16
rt_limbs_str <- function(l, base) {
  d <- character(0)
  repeat {
    r <- 0
    for (i in length(l):1) { z <- r * 65536 + l[i]; l[i] <- z %/% base; r <- z %% base }
    d <- c(c(0:9, letters[1:6])[r + 1], d)
    if (all(l == 0)) break
  }
  paste(d, collapse = "")
}
rt_bigint_fmt <- function(x, flags, w, prec, conv, u64 = FALSE) {
  neg <- x < 0 && !u64; m <- abs(x)
  l <- numeric(4); for (i in 1:4) { l[i] <- m %% 65536; m <- m %/% 65536 }
  if (u64 && x < 0) {                      # two's complement: 2^64 - |x|
    l <- 65535 - l; c <- 1
    for (i in 1:4) { z <- l[i] + c; l[i] <- z %% 65536; c <- z %/% 65536 }
  }
  digs <- rt_limbs_str(l, switch(conv, o = 8, x = , X = 16, 10))
  if (conv == "X") digs <- toupper(digs)
  if (!is.na(prec)) { pn <- if (prec == "") 0 else as.numeric(prec); if (nchar(digs) < pn) digs <- paste0(strrep("0", pn - nchar(digs)), digs) }
  sgn <- if (neg) "-" else if (grepl("+", flags, fixed = TRUE)) "+" else if (grepl(" ", flags, fixed = TRUE)) " " else ""
  s <- paste0(sgn, digs)
  if (grepl("0", flags, fixed = TRUE) && !grepl("-", flags, fixed = TRUE) && is.na(prec) && nchar(s) < w)
    s <- paste0(sgn, strrep("0", w - nchar(s)), digs)
  rt_pad(charToRaw(s), w, grepl("-", flags, fixed = TRUE))
}
rt_fprintf <- function(f, fmt, args) { b <- rt_format(fmt, args); if (is.null(f)) return(-1); rt_write(f, b); length(b) }
rt_printf <- function(fmt, args) rt_fprintf(rt_stdout, fmt, args)
rt_snprintf <- function(buf, n, fmt, args) {
  b <- rt_format(fmt, args)
  if (n > 0) { m <- min(length(b), n - 1); rt_put(buf, rt_bytes2num(b[seq_len(m)])) }
  length(b)
}
rt_sprintf <- function(buf, fmt, args) { b <- rt_format(fmt, args); rt_put(buf, rt_bytes2num(b)); length(b) }

# ------------------------------------------------------------------------------------------- files
# A FILE is an environment. Files opened only for reading are read whole into a raw vector at fopen (fgets,
# fread and fseek work on it); files opened for writing (w, w+, r+, a) use an R binary connection with the
# C file position kept here (R keeps separate read and write positions; this runtime seeks before switching).
rt_open_files <- new.env(parent = emptyenv()); rt_open_files$list <- list()
rt_fopen <- function(name, mode) {
  fn <- rt_str(name); m <- gsub("t", "", rt_str(mode), fixed = TRUE)
  f <- new.env(parent = emptyenv())
  f$eof <- FALSE; f$pos <- 0; f$std <- FALSE; f$con <- NULL; f$data <- NULL; f$last <- ""; f$name <- fn
  if (m %in% c("r", "rb")) {
    if (!file.exists(fn) || dir.exists(fn)) return(NULL)
    sz <- file.size(fn)
    con <- tryCatch(file(fn, "rb"), error = function(e) NULL, warning = function(w) NULL)
    if (is.null(con)) return(NULL)
    f$data <- if (sz > 0) readBin(con, "raw", sz) else raw(0)
    close(con)
    f$size <- length(f$data)
    return(f)
  }
  rm <- switch(m, "w" = , "wb" = "w+b", "w+" = , "w+b" = , "wb+" = "w+b", "r+" = , "r+b" = , "rb+" = "r+b",
               "a" = , "ab" = "a+b", "a+" = , "a+b" = , "ab+" = "a+b", NULL)
  if (is.null(rm)) return(NULL)
  if (rm == "r+b" && !file.exists(fn)) return(NULL)
  con <- tryCatch(file(fn, rm), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(con)) return(NULL)
  f$con <- con
  f$size <- if (rm == "w+b") 0 else file.size(fn)
  if (rm == "a+b") f$pos <- f$size
  f$append <- rm == "a+b"
  rt_open_files$list[[length(rt_open_files$list) + 1]] <- f
  f
}
rt_stdout <- local({ f <- new.env(parent = emptyenv()); f$std <- TRUE; f$eof <- FALSE; f })
rt_stderr <- rt_stdout
rt_write <- function(f, b) {
  if (f$std) { cat(rawToChar(b[b != as.raw(0)])); return(invisible(length(b))) }
  if (f$append) f$pos <- f$size
  if (f$last != "w" || f$wpos != f$pos) { seek(f$con, f$pos, rw = "write"); f$last <- "w" }
  writeBin(b, f$con)
  f$pos <- f$pos + length(b); f$wpos <- f$pos
  if (f$pos > f$size) f$size <- f$pos
  invisible(length(b))
}
rt_read <- function(f, n) {           # up to n raw bytes at the file position
  if (!is.null(f$data)) {
    m <- min(n, f$size - f$pos)
    if (m <= 0) { f$eof <- TRUE; return(raw(0)) }
    r <- f$data[(f$pos + 1):(f$pos + m)]
    f$pos <- f$pos + m
    if (m < n) f$eof <- TRUE
    return(r)
  }
  if (f$last != "r" || f$rpos != f$pos) { flush(f$con); seek(f$con, f$pos, rw = "read"); f$last <- "r" }
  r <- readBin(f$con, "raw", n)
  f$pos <- f$pos + length(r); f$rpos <- f$pos
  if (length(r) < n) f$eof <- TRUE
  r
}
rt_fclose <- function(f) {
  if (is.null(f)) return(-1)
  if (!is.null(f$con)) { close(f$con); f$con <- NULL }
  f$data <- NULL
  0
}
rt_close_all <- function() {
  for (f in rt_open_files$list) if (!is.null(f$con)) { try(close(f$con), silent = TRUE); f$con <- NULL }
  rt_open_files$list <- list()
}
rt_fflush <- function(f) { if (!is.null(f) && !is.null(f$con)) flush(f$con); 0 }
rt_feof <- function(f) if (f$eof) 1 else 0
rt_rewind <- function(f) { f$pos <- 0; f$eof <- FALSE; invisible(NULL) }
rt_fseek <- function(f, off, whence) {
  p <- switch(whence + 1, off, f$pos + off, f$size + off)
  if (p < 0) return(-1)
  f$pos <- p; f$eof <- FALSE
  0
}
rt_ftell <- function(f) f$pos
rt_fgets <- function(buf, n, f) {
  if (!is.null(f$data)) {
    if (f$pos >= f$size) { f$eof <- TRUE; return(NULL) }
    m <- min(n - 1, f$size - f$pos)
    seg <- f$data[(f$pos + 1):(f$pos + m)]
    k <- match(as.raw(10), seg)
    if (!is.na(k)) seg <- seg[seq_len(k)] else if (m < n - 1) f$eof <- TRUE
    f$pos <- f$pos + length(seg)
  } else {
    seg <- raw(0)
    repeat {
      r <- rt_read(f, 1)
      if (length(r) == 0) break
      seg <- c(seg, r)
      if (r == as.raw(10) || length(seg) >= n - 1) break
    }
    if (length(seg) == 0) return(NULL)
  }
  rt_put(buf, rt_bytes2num(seg))
  buf
}
rt_remove <- function(name) { fn <- rt_str(name); if (file.exists(fn) && file.remove(fn)) 0 else -1 }

.SZ <- c(d = 8L, f = 4L, i = 4L, I = 4L, q = 8L, c = 1L)
rt_fwrite1 <- function(code, size, v, f) {
  if (code == "c") { rt_write(f, as.raw(v %% 256)); return(1) }
  b <- switch(code, d = writeBin(v, raw(), size = 8L), f = writeBin(v, raw(), size = 4L),
              i = , I = writeBin(as.integer(rt_i32(v)), raw(), size = 4L),
              q = rt_i64_bytes(v))
  if (size < length(b)) b <- b[seq_len(size)]
  rt_write(f, b); 1
}
rt_i64_bytes <- function(v) {          # the 8 bytes of a C long (two's complement), exact for every integer double
  m <- abs(v); l <- numeric(4)
  for (i in 1:4) { l[i] <- m %% 65536; m <- m %/% 65536 }
  if (v < 0) { l <- 65535 - l; c <- 1; for (i in 1:4) { z <- l[i] + c; l[i] <- z %% 65536; c <- z %/% 65536 } }
  as.raw(c(rbind(l %% 256, l %/% 256)))
}
rt_bytes_i64 <- function(b) {
  lo <- readBin(b[1:4], "integer", size = 4L); hi <- readBin(b[5:8], "integer", size = 4L)
  hi * 4294967296 + (lo %% 4294967296)
}
rt_fread1 <- function(code, size, ref, f) {
  b <- rt_read(f, size)
  if (length(b) < size) { f$eof <- TRUE; return(0) }
  nat <- .SZ[[code]]
  if (size < nat) {
    old <- switch(code, d = writeBin(ref$v, raw(), size = 8L), f = writeBin(ref$v, raw(), size = 4L),
                  i = , I = writeBin(as.integer(rt_i32(ref$v)), raw(), size = 4L), q = rt_i64_bytes(ref$v))
    b <- c(b, old[(size + 1):nat])
  }
  ref$v <- switch(code, d = readBin(b, "double", size = 8L), f = readBin(b, "double", size = 4L),
                  i = readBin(b, "integer", size = 4L), I = readBin(b, "integer", size = 4L) %% 4294967296,
                  q = rt_bytes_i64(b), c = rt_bytes2num(b))
  1
}
rt_fwrite_arr <- function(code, size, arr, off, n, f) {
  if (n <= 0) return(0)
  if (code == "c") { rt_write(f, rt_raw_of(arr[[1L]]$v[(arr[[2L]] + off + 1):(arr[[2L]] + off + n)])); return(n) }
  v <- arr$v[(off + 1):(off + n)]
  b <- switch(code, d = writeBin(v, raw(), size = 8L), f = writeBin(v, raw(), size = 4L),
              i = , I = writeBin(as.integer(vapply(v, rt_i32, 0)), raw(), size = 4L),
              q = unlist(lapply(v, rt_i64_bytes)))
  rt_write(f, b); n
}
rt_fread_arr <- function(code, size, arr, off, n, f) {
  if (n <= 0) return(0)
  if (code == "c") {
    b <- rt_read(f, n)
    if (length(b)) { bb <- arr[[1L]]; o <- arr[[2L]] + off; bb$v[(o + 1):(o + length(b))] <- rt_bytes2num(b) }
    return(length(b))
  }
  nat <- .SZ[[code]]
  b <- rt_read(f, n * nat)
  m <- length(b) %/% nat
  if (m > 0) {
    x <- switch(code, d = readBin(b, "double", m, size = 8L), f = readBin(b, "double", m, size = 4L),
                i = readBin(b, "integer", m, size = 4L), I = readBin(b, "integer", m, size = 4L) %% 4294967296,
                q = vapply(seq_len(m), function(k) rt_bytes_i64(b[(8 * k - 7):(8 * k)]), 0))
    arr$v[(off + 1):(off + m)] <- as.numeric(x)
  }
  m
}

# ------------------------------------------------------------------------------------------- time
rt_time <- function() floor(as.numeric(Sys.time()))
rt_ctime <- function(t) {
  lt <- as.POSIXlt(t, origin = "1970-01-01")
  s <- paste0(format(lt, "%a %b "), sprintf("%2d", lt$mday), format(lt, " %H:%M:%S %Y"), "\n")
  rt_lit(s)
}
rt_temps <- new.env(parent = emptyenv()); rt_temps$names <- character(0)
rt_remove_temps <- function() { for (fn in rt_temps$names) if (file.exists(fn)) file.remove(fn); rt_temps$names <- character(0) }
rt_mkstemp <- function(template) {
  n <- rt_strlen(template)
  chars <- utf8ToInt("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
  for (try in 1:1000) {
    for (i in (n - 6):(n - 1)) rt_cset(template, i, chars[sample.int(62, 1)])
    fn <- rt_str(template)
    if (!file.exists(fn)) { if (file.create(fn, showWarnings = FALSE)) { rt_temps$names <- c(rt_temps$names, normalizePath(fn)); return(3) } }
  }
  -1
}
