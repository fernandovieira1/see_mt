cat('\014')

######################## a. TAXA DE RENDIMENTO ######################## 

## 1.1 Anos Finais (AF) ####

# **2015 ####
txRend_AF_2015 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos finais\\tx_rend_finais_2015.xlsx',
                             sheet = 'municipios')

# **2017 ####
txRend_AF_2017 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos finais\\tx_rend_finais_2017.xlsx',
                             sheet = 'municipios')

# **2019 ####
txRend_AF_2019 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos finais\\tx_rend_finais_2019.xlsx',
                             sheet = 'municipios')

# **2021 ####
txRend_AF_2021 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos finais\\tx_rend_finais_2021.xlsx',
                             sheet = 'municipios')

# **2023 ####
txRend_AF_2023 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\Taxa Rendimento\\Anos finais\\tx_rend_finais_2023.xlsx',
                             sheet = 'municipios')

# 1.2 Juntar bases ####
txRend_AF <- bind_rows(txRend_AF_2015, txRend_AF_2017, txRend_AF_2019, txRend_AF_2021, txRend_AF_2023)

# 1.3 Filtrar bases ####
# dependencia_id
txRend_AF %>%
  filter(dependencia_id %in% c(2, 3)) -> txRend_AF

# serie_id
txRend_AF %>%
  filter(serie_id == 9) -> txRend_AF

# localizacao_id
txRend_AF %>%
  filter(localizacao_id == 1) -> txRend_AF

# 1.4 Criar ciclo_id ####
txRend_AF %>%
  mutate(ciclo_id = 'AF') -> txRend_AF

# 1.5 Selecionar as colunas de interesse ####
txRend_AF <- txRend_AF %>%
  select(ano, ibge_id, dependencia_id, ciclo_id, aprovados, abandonos)

# 1.6 Mudar tipos das colunas ####
# ano
txRend_AF$ano <- as.factor(txRend_AF$ano)

# ibge_id
txRend_AF$ibge_id <- as.factor(txRend_AF$ibge_id)

# dependencia_id
txRend_AF$dependencia_id <- as.factor(txRend_AF$dependencia_id)
txRend_AF$dependencia_id  <- factor(txRend_AF$dependencia_id , 
                                    levels = c('2', '3'), 
                                    labels = c('Estadual', 'Municipal'))

# ciclo_id
txRend_AF$ciclo_id <- as.factor(txRend_AF$ciclo_id)

# 1.7 Renomear colunas ####
txRend_AF <- txRend_AF %>%
  rename(aprovacao = aprovados,
         abandono = abandonos)

# Substituir 0 por NA nos anos de 2017 e 2019
# txRend_AF <- txRend_AF %>%
#   mutate(aprovacao = ifelse(ano %in% c(2017, 2019) & aprovacao == 0, NA, aprovacao))
# 
# txRend_AF <- txRend_AF %>%
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
colnames(txRend_AF) <- paste0(colnames(txRend_AF), '_q')
