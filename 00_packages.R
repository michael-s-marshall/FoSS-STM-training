if("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
  }

devtools::install_github("mikajoh/tidystm", dependencies = TRUE)

pacman::p_load(tidyverse, quanteda, tidytext, stm, tidystm, ggstance)

