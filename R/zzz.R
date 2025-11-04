# file zzz.R

#' @import tidyverse
#' @import ggpubr
#' @import ggrepel
#' @import lmerTest
#' @import AER
#' @import MuMIn
#' @import writexl
#' @import xlsx
#' @import gridExtra
#' @import FELLA
#' @import igraph
#' @import pals
#' @import ggdendro
#' @import ggdendroplot
#' @import ggnewscale
#' @import httr
#' @import jsonlite
#' @import utils
#' @import classyfireR

.onLoad <- function(libname, pkgname) {
  library(tidyverse)
  library(ggpubr)
  library(ggrepel)
  library(lmerTest)
  library(AER)
  library(MuMIn)
  library(writexl)
  library(xlsx)
  library(gridExtra)
  library(FELLA)
  library(igraph)
  library(pals)
  library(ggdendro)
  library(ggdendroplot)
  library(ggnewscale)
  library(httr)
  library(jsonlite)
  library(utils)
  library(classyfireR)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nGetFeatistics v", packageVersion("GetFeatistics"), "\n")
}