# runswmm.R - the command line of the R port of EPA SWMM 5.2.4: EPA's run/main.c. Runs one model on the fast
# engine (swmm5/engine_fast.rds, made by build.sh), whose comparisons of floating values are R's own: a NaN that
# reaches one is an NA, and R stops with an error. The model is then run again from the start on the safe engine
# (swmm5/engine_safe.rds), which gives C's answer for every comparison with NaN; its outputs replace the first
# run's. Usage: Rscript runswmm.R input.inp report.rpt [output.out]
# Written by Claude (Anthropic) for Robert Dickinson; see README.md.
main <- function() {
  argv <- commandArgs(TRUE)
  self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  here <- dirname(normalizePath(self))
  E <- readRDS(file.path(here, "swmm5", "engine_fast.rds"))
  out <- function(...) cat(..., sep = "")
  version <- E$swmm_getVersion()
  vMajor <- version %/% 10000
  vMinor <- (version - 10000 * vMajor) %/% 1000
  vRelease <- version - 10000 * vMajor - 1000 * vMinor
  start <- Sys.time()
  if (length(argv) == 0) {
    out("\nNot Enough Arguments (See Help --help)\n\n")
  } else if (length(argv) == 1) {
    a <- argv[1]
    if (a %in% c("--help", "-h")) {
      out("\n\nSTORMWATER MANAGEMENT MODEL (SWMM) HELP\n\nCOMMANDS:\n\t--help (-h)       SWMM Help\n",
          "\t--version (-v)    Build Version\n\nRUNNING A SIMULATION:\n",
          "\t runswmm <input file> <report file> <optional output file>\n\n")
    } else if (a %in% c("--version", "-v")) {
      out(sprintf("\n%d.%d.%d\n\n", vMajor, vMinor, vRelease))
    } else out("\nUnknown Argument (See Help --help)\n\n")
  } else {
    out(sprintf("\n... EPA SWMM %d.%d (Build %d.%d.%d) [R port]\n", vMajor, vMinor, vMajor, vMinor, vRelease))
    run <- function(E) E$swmm_run(E$rt_lit(argv[1]), E$rt_lit(argv[2]), E$rt_lit(if (length(argv) > 2) argv[3] else ""))
    r <- tryCatch({ run(E); NULL }, error = function(e) e)
    if (!is.null(r)) {
      E$rt_close_all()
      E$rt_remove_temps()
      if (nzchar(Sys.getenv("SWMM_R_VERBOSE"))) message("fast engine stopped (", conditionMessage(r), "); running the safe engine")
      E <- readRDS(file.path(here, "swmm5", "engine_safe.rds"))
      run(E)
    }
    out(sprintf("\n\n... EPA SWMM completed in %.2f seconds.", as.numeric(difftime(Sys.time(), start, units = "secs"))))
    err <- E$rt_cbuf(128)
    if (E$swmm_getError(err, 127) > 0) out(" There are errors.\n")
    else if (E$swmm_getWarnings() > 0) out(" There are warnings.\n")
    else out("\n")
  }
  E$rt_close_all()
  invisible(0)
}
options(warn = -1, expressions = 50000)
main()
quit(save = "no", status = 0)
