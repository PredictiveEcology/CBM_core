# CBM_core (development version)

* `reqdPkgs` now lists `reproducible` and `SpaDES.core`, which the module's code uses.
Known issues: <https://github.com/PredictiveEcology/CBM_core/issues>

# CBM_core (development version)

* `Init()` now stops if `.useCache` includes `"init"` (or is `TRUE`): a cached init skips selecting the Python environment and resetting the output database, which later makes spinup fail with `ModuleNotFoundError: No module named 'libcbm'`.
