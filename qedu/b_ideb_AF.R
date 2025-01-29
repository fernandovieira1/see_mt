cat('\014')

######################## b. IDEB/SAEB ######################## 

## 1.1 Anos Finais (AF) ####

# **2015 ####
ideb_AF_2015 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos finais\\ideb_anos_finais_2015.xlsx',
                           sheet = 'municipios')

# **2017 ####
ideb_AF_2017 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos finais\\ideb_anos_finais_2017.xlsx',
                           sheet = 'municipios')

# **2019 ####
ideb_AF_2019 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos finais\\ideb_anos_finais_2019.xlsx',
                           sheet = 'municipios')

# **2021 ####
ideb_AF_2021 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos finais\\ideb_anos_finais_2021.xlsx',
                           sheet = 'municipios')

# **2023 ####
ideb_AF_2023 <- read_excel('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\QEdu\\IDEB\\Anos finais\\ideb_anos_finais_2023.xlsx',
                           sheet = 'municipios')

# 1.2 Juntar bases ####
ideb_AF <- bind_rows(ideb_AF_2015, ideb_AF_2017, ideb_AF_2019, ideb_AF_2021, ideb_AF_2023)

# 1.3 Filtrar bases ####
ideb_AF %>%
  filter(dependencia_id %in% c(2, 3)) -> ideb_AF

# 1.4 Selecionar as colunas de interesse ####
ideb_AF <- ideb_AF %>%
  select(ano, ibge_id, dependencia_id, ciclo_id, nota_mt, nota_lp, ideb)

# 1.5 Padronizar notas de matemática por ano
ideb_AF <- ideb_AF %>%
  group_by(ano) %>%
  mutate(nota_mt = (nota_mt - min(nota_mt, na.rm = TRUE)) /
           (max(nota_mt, na.rm = TRUE) - min(nota_mt, na.rm = TRUE)) * 10) %>%
  ungroup()

# 1.6 Padronizar notas de português por ano
ideb_AF <- ideb_AF %>%
  group_by(ano) %>%
  mutate(nota_lp = (nota_lp - min(nota_lp, na.rm = TRUE)) /
           (max(nota_lp, na.rm = TRUE) - min(nota_lp, na.rm = TRUE)) * 10) %>%
  ungroup()

# 1.7 Mudar tipos das colunas ####
# ano
ideb_AF$ano <- as.factor(ideb_AF$ano)

# ibge_id
ideb_AF$ibge_id <- as.factor(ideb_AF$ibge_id)

# dependencia_id
ideb_AF$dependencia_id <- as.factor(ideb_AF$dependencia_id)
ideb_AF$dependencia_id  <- factor(ideb_AF$dependencia_id , 
                                  levels = c('2', '3'), 
                                  labels = c('Estadual', 'Municipal'))

# ciclo_id
ideb_AF$ciclo_id <- as.factor(ideb_AF$ciclo_id)

# 1.7 Renomear colunas ####
colnames(ideb_AF) <- paste0(colnames(ideb_AF), '_q')
