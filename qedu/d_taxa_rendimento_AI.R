cat('\014')

######################## a. TAXA DE RENDIMENTO ######################## 

## 1.1 Anos Iniciais (AI) ####

# **2015 ####
txRend_AI_2015 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos iniciais\\tx_rend_iniciais_2015.xlsx',
                           sheet = 'municipios')

# **2017 ####
txRend_AI_2017 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos iniciais\\tx_rend_iniciais_2017.xlsx',
                           sheet = 'municipios')

# **2019 ####
txRend_AI_2019 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos iniciais\\tx_rend_iniciais_2019.xlsx',
                           sheet = 'municipios')

# **2021 ####
txRend_AI_2021 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos iniciais\\tx_rend_iniciais_2021.xlsx',
                           sheet = 'municipios')

# **2023 ####
txRend_AI_2023 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos iniciais\\tx_rend_iniciais_2023.xlsx',
                           sheet = 'municipios')

# 1.2 Juntar bases ####
txRend_AI <- bind_rows(txRend_AI_2015, txRend_AI_2017, txRend_AI_2019, txRend_AI_2021, txRend_AI_2023)

# 1.3 Filtrar bases ####
# dependencia_id
txRend_AI %>%
  filter(dependencia_id %in% c(2, 3)) -> txRend_AI

# serie_id
txRend_AI %>%
  filter(serie_id == 5) -> txRend_AI

# localizacao_id
# txRend_AI %>%
#   filter(localizacao_id == 1) -> txRend_AI # Urbana

# 1.4 Criar ciclo_id ####
txRend_AI %>%
  mutate(ciclo_id = 'AI') -> txRend_AI

# 1.5 Selecionar as colunas de interesse ####
txRend_AI <- txRend_AI %>%
  select(ano, ibge_id, dependencia_id, ciclo_id, aprovados, abandonos)

# 1.6 Mudar tipos das colunas ####
# ano
txRend_AI$ano <- as.factor(txRend_AI$ano)

# ibge_id
txRend_AI$ibge_id <- as.factor(txRend_AI$ibge_id)

# dependencia_id
txRend_AI$dependencia_id <- as.factor(txRend_AI$dependencia_id)
txRend_AI$dependencia_id  <- factor(txRend_AI$dependencia_id , 
                                  levels = c('2', '3'), 
                                  labels = c('Estadual', 'Municipal'))

# ciclo_id
txRend_AI$ciclo_id <- as.factor(txRend_AI$ciclo_id)

# 1.7 Renomear colunas ####
txRend_AI <- txRend_AI %>%
  rename(aprovacao = aprovados,
         abandono = abandonos)

## Substituir 0 por NA nos anos de 2017 e 2019
# txRend_AI <- txRend_AI %>%
#   mutate(aprovacao = ifelse(ano %in% c(2017, 2019) & aprovacao == 0, NA, aprovacao))
# 
# txRend_AI <- txRend_AI %>%
#   mutate(
#     aprovacao = ifelse(ano %in% c(2017, 2019) & aprovacao == 0, NA_real_, aprovacao),
#     abandono = ifelse(ano %in% c(2017, 2019) & abandono == 0, NA_real_, abandono)
#   )


# Verificar se a substituição foi feita corretamente
# txRend_estadual_AI %>%
#   filter(ano %in% c(2017, 2019)) %>%
#   summarise(
#     total_obs = n(),
#     total_NA = sum(is.na(aprovacao)),
#     total_zeros = sum(aprovacao == 0, na.rm = TRUE)
#   )

# 1.8 Renomear colunas ####
colnames(txRend_AI) <- paste0(colnames(txRend_AI), '_q')
