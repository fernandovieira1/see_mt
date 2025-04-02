######################## 1. IDEB  ########################
# Dados QEdu

## 2.1 Carregar bases ####
cat('\014')
source('a_ideb_AI.R') # Anos inicias
source('b_ideb_AF.R') # Anos finais
source('c_ideb_AIAF.R') # Anos iniciais e finais

glimpse(ideb)
# summary(ideb)