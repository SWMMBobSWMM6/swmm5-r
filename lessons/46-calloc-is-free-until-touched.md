C's calloc costs nothing until the memory is touched; a language that zero-fills its arrays pays for all of it at once.

The R port's gate run of Hydrology/sewer_rdii_problem.inp ended after 21 seconds with return code -9, and the
kernel log said why:

    Memory cgroup out of memory: Killed process 29204 (R) total-vm:6145868kB, anon-rss:6079744kB

EPA 5.2.4 runs the same deck in 11 seconds. rdii.c (`allocRdiiMemory`) callocs, for each of the deck's 613 unit
hydrographs and each of its three responses, `maxPeriods` past rainfall values (`double`) and months (`char`), where
`maxPeriods` is the response's time base over the rain interval. The long response has T = 900 h and K = 10, a time
base of 9,900 hours; the rain interval is the WET_STEP, 60 s; so `maxPeriods` is 594,001, and the deck asks for 375
million of each. calloc gets that much address space from the kernel as zero pages and nothing is resident until it
is written. EPA writes the arrays in order, one entry per rain interval (7,140 in this 5-day run), and zeroes a
whole past-rainfall array when an RDII event starts. R's
`numeric(n)` zero-fills its vector when it makes it, and the port stores a `char` as a double, so the R port asked
for 6 GB before the simulation began.

The fix keeps the arithmetic and changes only the representation: an empty array stands for an array of zeros.
`r/swmm5/overrides.R` has hand ports of three functions. `allocRdiiMemory` makes both arrays empty; the generated
writes extend them, always at the next index, as EPA writes them (R over-allocates a vector it extends by one
element, so this does not copy it each time). `updateDryPeriod`, which sets every past rainfall value to 0.0 when
an RDII event starts, empties the array instead: in this deck that is 603 unused hydrographs times 594,001 values
at the first rain, which costs EPA a zeroing loop over 2.9 GB and would cost R minutes of interpreted stores and the same
2.9 GB. `getUnitHydConvol`, the only reader, takes an entry past the end as 0, and adds the same terms in the same
order as EPA's loop while skipping the visits that add nothing (rain 0 or NaN). A run of the deck with that engine
was at 340 MB resident after 10 minutes and 454 MB after 22 (it is slow for other reasons: it was stopped at
1700 s, still making the RDII file). The emitter leaves the three functions out (`OVERRIDES` in `tools/c2x/emit_r.py`), and the
regenerated engines differ from the earlier ones only by those functions, which run only when a node has RDII
inflow (`createRdiiFile` returns first otherwise), so only the decks with RDII were gated again (r_4a to r_4d).

What to take from it: memory a C program allocates but never touches is not memory it uses. A large calloc in the
source marks a place where a port to a language with eager, initialised arrays can need far more memory than the C
program, and the gate shows it only on a deck that asks for that much. When EPA's peak memory and the port's differ
by a large factor, look for a calloc first.
