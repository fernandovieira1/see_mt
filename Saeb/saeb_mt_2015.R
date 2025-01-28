cat('\014')

######################## 1. SAEB 2015 ######################## 

# 1.1 Alunos do 5º ano ####

# **Importar dados ####
saeb_2015_alunos5 <- read.csv('C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\Saeb\\saeb_2015-2023\\2015\\TS_ALUNO_5EF_2015.csv')

# **Selecionar variáveis ####
saeb_2015_alunos5 <- saeb_2015_alunos5 %>% 
  select(
    # Portugues e Matemática
    starts_with('PROFICIENCIA'), 
    
    # Identificação
    'ID_PROVA_BRASIL', 'ID_UF', 'ID_MUNICIPIO', 'ID_ESCOLA', 'IN_PUBLICA', 'ID_LOCALIZACAO', 
    
    # Características dos alunos
    'TX_RESP_Q001', # Sexo
    'TX_RESP_Q002', # Cor/Raça
    'TX_RESP_Q004', # Idade
    'TX_RESP_Q023', # Escolaridade pai
    'TX_RESP_Q019' # Escolaridade mãe
  ) %>%
  filter(ID_UF == 51) %>% # MT
  filter(IN_PUBLICA == 1) %>% # Pública
  select(-c(ID_UF, IN_PUBLICA)); gc()

# **Renomear variáveis ####
names(saeb_2015_alunos5) <- c('port_p', 'port', 'mat_p', 'mat',
                              'ano', 'id_municipio', 'id_escola', 'id_localizacao', 
                              'sexo', 'cor', 'idade', 'escolar_pai', 'escolar_mae')

saeb_2015_alunos5 %>% select('ano', 
                             'id_escola', 'id_municipio','id_localizacao',
                             'sexo', 'cor', 'idade', 'escolar_pai', 'escolar_mae',
                             'port_p', 'mat_p', 'port', 'mat') -> saeb_2015_alunos5

# **Padronizar valores ausentes para NA ####
saeb_2015_alunos5 <- saeb_2015_alunos5 %>%
  mutate(across(where(is.character), ~ na_if(.x, ''))) %>%  # Substitui valores em branco por NA nas colunas de texto
  mutate(across(where(is.character), ~ na_if(.x, 'NA')))    # Substitui strings 'NA' por NA nas colunas de texto


## **Renomear categorias ####

# Ano
saeb_2015_alunos5$ano <- as.factor(saeb_2015_alunos5$ano)

# id_escola
saeb_2015_alunos5$id_escola <- as.factor(saeb_2015_alunos5$id_escola)

# id_municipio
saeb_2015_alunos5$id_municipio <- as.factor(saeb_2015_alunos5$id_municipio)

# id_localizacao  
saeb_2015_alunos5$id_localizacao <- factor(saeb_2015_alunos5$id_localizacao, 
                                           levels = c('1', '2'),
                                           labels = c('Urbana', 'Rural'))

# Sexo
saeb_2015_alunos5$sexo <- factor(saeb_2015_alunos5$sexo, 
                                 levels = c('A', 'B'), 
                                 labels = c('Masculino', 'Feminino'))

# Cor
saeb_2015_alunos5$cor <- factor(saeb_2015_alunos5$cor, 
                                levels = c('A', 'B', 'C', 'D', 'E', 'F'), 
                                labels = c('Branco', 'Pardo', 'Preto', 'Amarelo', 'Indígena', 'Não declarada'))

# Idade
saeb_2015_alunos5$idade <- factor(saeb_2015_alunos5$idade, 
                                  levels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'), 
                                  labels = c('8', '9', '10', '11', '12', '13', '14', '15'))
saeb_2015_alunos5$idade <- as.numeric(as.character(saeb_2015_alunos5$idade))

# Escolaridade pai
saeb_2015_alunos5$escolar_pai <- factor(saeb_2015_alunos5$escolar_pai, 
                                        levels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'), 
                                        labels = c('Analfabeto', # Nunca estudou. 
                                                   'Semi-Analfabeto', # Não completou a 4.ª série/5.º ano. 
                                                   'Primário', # Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano.
                                                   'Fundamental', # Completou a 8.ª série/9.º ano, mas não completou o Ensino Médio.
                                                   'Ensino Médio', # Completou o Ensino Médio, mas não completou a Faculdade.
                                                   'Superior', # Completou a Faculdade.
                                                   'Não sabe')) # Não sei.

# Escolaridade mãe
saeb_2015_alunos5$escolar_mae <- factor(saeb_2015_alunos5$escolar_mae, 
                                        levels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'), 
                                        labels = c('Analfabeto', # Nunca estudou. 
                                                   'Semi-Analfabeto', # Não completou a 4.ª série/5.º ano. 
                                                   'Primário', # Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano.
                                                   'Fundamental', # Completou a 8.ª série/9.º ano, mas não completou o Ensino Médio.
                                                   'Ensino Médio', # Completou o Ensino Médio, mas não completou a Faculdade.
                                                   'Superior', # Completou a Faculdade.
                                                   'Não sabe')) # Não sei.

# **Sumário ####
if (descritivas == 1) {
  # Glimpse
  cat('\014')
  glimpse(saeb_2015_alunos5)
  
  # Apenas numéricas
  saeb_2015_alunos5 %>%
    select_if(is.numeric) %>%
    summary()
  
  # Apenas categóricas
  saeb_2015_alunos5 %>%
    select(where(is.factor)) %>%
    lapply(function(col) {
      abs_freq <- table(col)
      rel_freq <- prop.table(abs_freq) * 100
      paste0(abs_freq, ' (', sprintf('%.2f', rel_freq), '%)')
    })
  
} else {
  cat('\014')
  print('Saeb 2015 descritivas não rodadas.')
  }






