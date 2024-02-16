# load packages
library(rvest)
library(tidyverse)
library(flora)
library(qdapRegex)
library(readxl)
library(rWCVP)
library(rWCVPdata)
library(ggpubr)
library(ggalluvial)
library(patchwork)
library(hrbrthemes)
library(sysfonts)
library(showtext)
library(ggchicklet)
library(gg.layers)
library(caret)
library(emmeans)

# add Arial Narrow for plotting
font_paths("C:/Windows/Fonts")
font_add("Arial Narrow", regular = "arialn.ttf", italic = "arialni.ttf", 
         bold = "arialnb.ttf")
showtext_auto()
