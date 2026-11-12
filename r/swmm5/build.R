# build.R ENGINE_DIR - load the R port into one environment per engine (rt.R, overrides.R and the generated
# engine_fast.R or engine.R; the environment's parent is R's base environment), byte-compile every function with
# compiler::cmpfun, and save each environment (engine_fast.rds, engine_safe.rds), so that a run loads compiled
# code instead of compiling 37,000 lines on each start. Written by Claude (Anthropic) for Robert Dickinson.
dir <- commandArgs(TRUE)[1]
for (eng in list(c("engine_fast.R", "engine_fast.rds"), c("engine.R", "engine_safe.rds"))) {
  E <- new.env(parent = baseenv())
  for (f in c("rt.R", "overrides.R", eng[1])) sys.source(file.path(dir, f), envir = E, keep.source = FALSE)
  n <- 0
  for (nm in ls(E, all.names = TRUE)) {
    f <- get(nm, envir = E)
    if (is.function(f) && !is.primitive(f) && identical(environment(f), E)) {
      assign(nm, compiler::cmpfun(f, options = list(optimize = 2)), envir = E); n <- n + 1
    }
  }
  saveRDS(E, file.path(dir, eng[2]), compress = FALSE)
  cat(sprintf("compiled %d functions of %s into %s\n", n, eng[1], eng[2]))
}
