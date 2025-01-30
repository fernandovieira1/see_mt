#################### 0. CARREGAR PACOTES ####################
rm(list = ls())
setwd('C:/Users/ferna/OneDrive/1. Educacao/2. Academia/3. DOUTORADO/USP - Economia Aplicada/MATERIAS/Eco II - Daniel/Artigo Final - Eco II/Github/see_mt2/qedu')

## Carregar pacotes ####
library(readxl)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(gridExtra)
library(PNADcIBGE)
library(GGally)
library(ggpubr)
library(psych)
library(janitor)
library(kableExtra) 
library(stargazer)
cat('\014')

descritivas <- 1 # 0 para não rodar, 1 para rodar
imp_categ <- 0 # 0 para não rodar, 1 para rodar