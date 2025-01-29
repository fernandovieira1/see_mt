#################### 2. TAXA DE RENDIMENTO ####################
# rm(list = ls())
# setwd('C:/Users/ferna/OneDrive/1. Educacao/2. Academia/3. DOUTORADO/USP - Economia Aplicada/MATERIAS/Eco II - Daniel/Artigo Final - Eco II/Github/see_mt2/qedu')

## Carregar pacotes ####
library(tidyverse)
library(readxl)
library(ggthemes)
library(lubridate) 
library(kableExtra) 
library(stargazer)
cat('\014')

descritivas <- 1 # 0 para não rodar, 1 para rodar
imp_categ <- 0 # 0 para não rodar, 1 para rodar


######################## 2. TAXA DE RENDIMENTO  ########################
# Dados QEdu

## 2.1 Carregar bases ####
cat('\014')
source('d_taxa_rendimento_AI.R')
source('e_taxa_rendimento_AF.R')
source('f_taxa_rendimento_AIAF.R')

glimpse(txRend)
# summary(txRend)