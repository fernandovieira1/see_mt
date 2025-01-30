## 1. Definir anos e variáveis ####
anos <- c(2015, 2017, 2019, 2021, 2023)

vars <- c(
  # Identificação e Controle
  'UF', 'V1008', 'V1014', # Código do município e localização geográfica
  'V2001', 'V1022', 'V1023', # Estrutura familiar e domicílio
  
  # Características do Domicílio
  'V2003', 'V2005', 'V2007', 'V20081', 'V20082', 
  'V2009', 'V2010', # Dados do indivíduo
  
  # Educação
  'V3002', 'V3003A', 'V3006', 'V3009A', 'VD3005', 'VD3006',
  
  # Trabalho e Renda
  'V4001', 'V4002', 'VD4019', 'VD4020', 'VD4009'
)

## 2. Baixar e processar os dados ####
variaveis_disponiveis <- function(ano) {
  dados_pnad <- get_pnadc(year = ano, quarter = 1, vars = NULL, design = FALSE)
  return(names(dados_pnad))
}

# Criar função para baixar e processar os dados
processar_pnad <- function(ano) {
  message('Processando ano: ', ano)
  
  # Verificar variáveis disponíveis no ano
  variaveis_ano <- variaveis_disponiveis(ano)
  
  # Selecionar apenas variáveis disponíveis para o ano
  vars_disponiveis <- vars[vars %in% variaveis_ano]
  
  # Adicionar variáveis extras se disponíveis
  extras <- c('VD3002', 'VD4047', 'V2023')  # Algumas dessas variáveis não estão disponíveis em 2015
  extras_disponiveis <- extras[extras %in% variaveis_ano]
  
  # Carregar dados apenas com variáveis disponíveis
  dados_pnad <- get_pnadc(year = ano, quarter = 1, vars = c(vars_disponiveis, extras_disponiveis), design = FALSE)
  
  # Filtrar apenas Mato Grosso
  dados_mt <- dados_pnad %>% filter(UF == 'Mato Grosso')
  
  # Criar variáveis derivadas
  dados_mt <- dados_mt %>%
    mutate(
      renda_per_capita = VD4019 / V2001,  # Renda per capita domiciliar
      trabalha = ifelse(V4001 == 1, 1, 0),  # Indicador binário se trabalha
      escolaridade_mae = as.numeric(VD3005),
      escolaridade_pai = as.numeric(VD3006),
      urbana = ifelse(V1014 == 1, 1, 0)  # 1 = Urbana, 0 = Rural
    )
  
  # Criar variáveis apenas se existirem no ano
  if ('VD3002' %in% names(dados_mt)) {
    dados_mt <- dados_mt %>% mutate(evasao = ifelse(VD3002 == 2, 1, 0))
  }
  if ('VD4047' %in% names(dados_mt)) {
    dados_mt <- dados_mt %>% mutate(beneficio_social = ifelse(VD4047 > 0, 1, 0))
  }
  if ('V2023' %in% names(dados_mt)) {
    dados_mt <- dados_mt %>% mutate(internet = ifelse(V2023 == 1, 1, 0))
  }
  
  # Agregar por município
  dados_municipio <- dados_mt %>%
    group_by(V1008) %>%
    summarise(
      populacao = sum(V2001, na.rm = TRUE),
      media_renda_pc = mean(renda_per_capita, na.rm = TRUE),
      taxa_trabalho = mean(trabalha, na.rm = TRUE),
      escolaridade_mae_media = mean(escolaridade_mae, na.rm = TRUE),
      escolaridade_pai_media = mean(escolaridade_pai, na.rm = TRUE),
      percentual_urbano = mean(urbana, na.rm = TRUE) * 100,
      taxa_desemprego = mean(VD4009, na.rm = TRUE),
      taxa_evasao = if ('evasao' %in% names(dados_mt)) mean(evasao, na.rm = TRUE) else NA,
      taxa_beneficio_social = if ('beneficio_social' %in% names(dados_mt)) mean(beneficio_social, na.rm = TRUE) else NA,
      percentual_internet = if ('internet' %in% names(dados_mt)) mean(internet, na.rm = TRUE) * 100 else NA
    ) %>%
    mutate(ano = ano)  # Adicionar o ano como variável
  
  return(dados_municipio)
}


## 3. Processar dados para todos os anos ####
# Aplicar a função para todos os anos e consolidar os dados
dados_completos <- bind_rows(lapply(anos, processar_pnad))

# Salvar os dados processados
write.csv(dados_completos, 
          'C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\PNADc\\pnad_mt_see.csv', 
          row.names = FALSE)
