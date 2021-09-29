setwd("P:/2019 0970 151 000/User Data/Faraz-Export IDAVE folders/Models2.5")
library(tidyverse)
library(h2o)
library(inspectdf)
library(gtsummary)
tabna <- function(x){table(x, useNA = "ifany")}