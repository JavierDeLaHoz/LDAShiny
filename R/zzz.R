# ======================================================================
# zzz.R — Package startup message
# ======================================================================

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "============================================================\n",
    "LDAShiny: Bibliometric Topic Modeling Suite\n",
    "============================================================\n",
    "\n",
    "To launch the interactive application, run:\n",
    "    run_LDAShiny()\n",
    "\n",
    "If you use LDAShiny in your research, please cite:\n",
    "    De la Hoz-M., J.; Fernandez-Gomez, M. J.; Mendes, S. (2021).\n",
    "    LDAShiny: An R Package for Exploratory Review of Scientific\n",
    "    Literature Based on a Bayesian Probabilistic Model and\n",
    "    Machine Learning Tools. Mathematics, 9(14), 1671.\n",
    "    DOI: 10.3390/math9141671\n",
    "\n",
    "Thank you for using LDAShiny!\n",
    "============================================================\n"
  )
}
