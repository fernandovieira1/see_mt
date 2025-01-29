cat('\014')

######################## a. IDEB/SAEB ######################## 

## 1.1 Anos Iniciais (AI) ####

# **2015 ####
ideb_AI_2015 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos iniciais\\ideb_anos_iniciais_2015.xlsx',
                           sheet = 'municipios')

# **2017 ####
ideb_AI_2017 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos iniciais\\ideb_anos_iniciais_2017.xlsx',
                           sheet = 'municipios')

# **2019 ####
ideb_AI_2019 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos iniciais\\ideb_anos_iniciais_2019.xlsx',
                           sheet = 'municipios')

# **2021 ####
ideb_AI_2021 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos iniciais\\ideb_anos_iniciais_2021.xlsx',
                           sheet = 'municipios')

# **2023 ####
ideb_AI_2023 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos iniciais\\ideb_anos_iniciais_2023.xlsx',
                           sheet = 'municipios')

# 1.2 Juntar bases ####
ideb_AI <- bind_rows(ideb_AI_2015, ideb_AI_2017, ideb_AI_2019, ideb_AI_2021, ideb_AI_2023)

# 1.3 Filtrar bases ####
ideb_AI %>%
  filter(dependencia_id %in% c(2, 3)) -> ideb_AI

# 1.4 Selecionar as colunas de interesse ####
ideb_AI <- ideb_AI %>%
  select(ano, ibge_id, dependencia_id, ciclo_id, nota_mt, nota_lp, ideb)

# 1.5 Padronizar notas de matemática por ano
ideb_AI <- ideb_AI %>%
  group_by(ano) %>%
  mutate(nota_mt = (nota_mt - min(nota_mt, na.rm = TRUE)) /
           (max(nota_mt, na.rm = TRUE) - min(nota_mt, na.rm = TRUE)) * 10) %>%
  ungroup()

# 1.6 Padronizar notas de português por ano
ideb_AI <- ideb_AI %>%
  group_by(ano) %>%
  mutate(nota_lp = (nota_lp - min(nota_lp, na.rm = TRUE)) /
           (max(nota_lp, na.rm = TRUE) - min(nota_lp, na.rm = TRUE)) * 10) %>%
  ungroup()

# 1.7 Mudar tipos das colunas ####
# ano
ideb_AI$ano <- as.factor(ideb_AI$ano)

# ibge_id
ideb_AI$ibge_id <- as.factor(ideb_AI$ibge_id)

# dependencia_id
ideb_AI$dependencia_id <- as.factor(ideb_AI$dependencia_id)
ideb_AI$dependencia_id  <- factor(ideb_AI$dependencia_id , 
                                levels = c('2', '3'), 
                                labels = c('Estadual', 'Municipal'))

# ciclo_id
ideb_AI$ciclo_id <- as.factor(ideb_AI$ciclo_id)

# 1.7 Renomear colunas ####
colnames(ideb_AI) <- paste0(colnames(ideb_AI), '_q')

