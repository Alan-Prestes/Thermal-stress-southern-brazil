# =============================================================================
# CLIMATOLOGIA DIÁRIA DE ESTRESSE TÉRMICO
# MUNICÍPIOS DA REGIÃO SUL DO BRASIL
#
# Período: 2006-01-01 a 2025-12-31
#
# Metodologia:
#   1. Municípios e coordenadas: geobr
#   2. T2M e RH2M horários: NASA POWER
#   3. Extremos diários de temperatura e umidade
#   4. Dia anticíclico:
#        ITU mínimo = f(T mínima, UR máxima)
#        ITU máximo = f(T máxima, UR mínima)
#   5. THIload 
#   6. D = duração diária acima do limiar de ITU = 70
#   7. Climatologia = média dos valores diários de 2006-2025
#   8. Exclusão de 29/02 -> exatamente 365 dias por município
#
# IMPORTANTE:
#   THIload e D são calculados PARA CADA DIA DE CADA ANO.
#   Somente depois é calculada a média histórica.
# =============================================================================


# =============================================================================
# 0. PACOTES - OBS: Se necessário. Senão desconsiderar
# =============================================================================

pacotes <- c(
  "geobr",
  "sf",
  "nasapower",
  "dplyr",
  "lubridate",
  "writexl"
)

novos <- pacotes[
  !(pacotes %in% installed.packages()[, "Package"])
]

if (length(novos) > 0) {
  install.packages(novos)
}

library(geobr)
library(sf)
library(nasapower)
library(dplyr)
library(lubridate)
library(writexl)

rm(list=ls())
# =============================================================================
# 1. CONFIGURAÇÕES GERAIS
# =============================================================================

DATA_INICIO <- "2006-01-01"
DATA_FIM    <- "2025-12-31"

# Limiar de ITU
THI_LIMIAR <- 70

# Período diário
PERIODO <- 24

# Exigir 24 observações válidas de T e UR
MIN_HORAS <- 24


# -----------------------------------------------------------------------------
# ALTERAR SOMENTE O DIRETÓRIO, SE NECESSÁRIO
# -----------------------------------------------------------------------------

PASTA_PRINCIPAL <- "Definir a pasta principal de trabalho"



# =============================================================================
# 2. CRIAR DIRETÓRIOS
# =============================================================================

PASTA_HISTORICO <- file.path(
  PASTA_PRINCIPAL,
  "01_Climatologia"
)

PASTA_PROBLEMAS <- file.path(
  PASTA_PRINCIPAL,
  "02_Problemas"
)

PASTA_RELATORIOS <- file.path(
  PASTA_PRINCIPAL,
  "03_Relatorios"
)


dir.create(
  PASTA_PRINCIPAL,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  PASTA_HISTORICO,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  PASTA_PROBLEMAS,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  PASTA_RELATORIOS,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 3. MUNICÍPIOS DA REGIÃO SUL
# =============================================================================

message("Obtendo municípios da Região Sul...")

municipios_sf <- geobr::read_municipality(
  code_muni = "all",
  year = 2024,
  simplified = TRUE
) %>%
  filter(
    abbrev_state %in% c("PR", "SC", "RS")
  )


# Conferência
table(municipios_sf$abbrev_state)

nrow(municipios_sf)


# =============================================================================
# 4. COORDENADAS REPRESENTATIVAS DOS MUNICÍPIOS
# =============================================================================

# Projetar para sistema métrico
municipios_proj <- sf::st_transform(
  municipios_sf,
  5880
)


# Ponto garantidamente dentro do município
municipios_pontos <- sf::st_point_on_surface(
  municipios_proj
)


# Retornar para SIRGAS 2000 - longitude/latitude
municipios_pontos <- sf::st_transform(
  municipios_pontos,
  4674
)


# Extrair coordenadas
coord <- sf::st_coordinates(
  municipios_pontos
)


sul <- municipios_pontos %>%
  sf::st_drop_geometry() %>%
  mutate(
    lon = coord[, 1],
    lat = coord[, 2]
  ) %>%
  select(
    code_muni,
    name_muni,
    abbrev_state,
    lon,
    lat
  ) %>%
  arrange(
    abbrev_state,
    name_muni
  )


# Conferências
head(sul)

str(sul)

table(sul$abbrev_state)

nrow(sul)


# Salvar coordenadas
saveRDS(
  sul,
  file.path(
    PASTA_PRINCIPAL,
    "Coordenadas_municipios_SUL.rds"
  )
)


writexl::write_xlsx(
  sul,
  file.path(
    PASTA_PRINCIPAL,
    "Coordenadas_municipios_SUL.xlsx"
  )
)


# =============================================================================
# 5. CALENDÁRIO COMPLETO DE 2006 A 2025
# =============================================================================

calendario_completo <- tibble(
  DATA = seq.Date(
    from = as.Date(DATA_INICIO),
    to   = as.Date(DATA_FIM),
    by   = "day"
  )
) %>%
  mutate(
    YEAR = lubridate::year(DATA),
    MO   = lubridate::month(DATA),
    DY   = lubridate::day(DATA)
  )


N_DIAS_ESPERADOS <- nrow(
  calendario_completo
)


# 2006-2025 = 7.305 dias
N_DIAS_ESPERADOS

stopifnot(
  N_DIAS_ESPERADOS == 7305
)


# =============================================================================
# 6. FUNÇÃO PARA CÁLCULO DO ITU
# =============================================================================

calc_ITU <- function(T, UR) {
  
  ((1.8 * T) + 32) -
    (
      (0.55 - (0.0055 * UR)) *
        ((1.8 * T) - 26)
    )
}


# =============================================================================
# 7. PROTEÇÃO NUMÉRICA PARA ASIN()
# Evita erros numéricos quando, por arredondamento,
# o argumento fica ligeiramente acima de 1 ou abaixo de -1.
# =============================================================================

limitar_asin <- function(x) {
  
  pmax(
    -1,
    pmin(1, x)
  )
}


# =============================================================================
# 8. FUNÇÃO PRINCIPAL
# =============================================================================

GETPOWER <- function(
    tb,
    L,
    data_inicio = DATA_INICIO,
    data_fim = DATA_FIM,
    min_horas = MIN_HORAS,
    THIlimiar = THI_LIMIAR,
    periodo = PERIODO
) {
  
  
  # ===========================================================================
  # 8.1 IDENTIFICAÇÃO DO MUNICÍPIO
  # ===========================================================================
  
  code_muni <- tb$code_muni[L]
  name_muni <- tb$name_muni[L]
  estado    <- tb$abbrev_state[L]
  lon       <- tb$lon[L]
  lat       <- tb$lat[L]
  
  
  message(
    "\n=============================================================="
  )
  
  message(
    "Município: ",
    name_muni,
    " - ",
    estado
  )
  
  message(
    "Código IBGE: ",
    code_muni
  )
  
  message(
    "Posição: ",
    L,
    " de ",
    nrow(tb)
  )
  
  message(
    "Longitude: ",
    round(lon, 5),
    " | Latitude: ",
    round(lat, 5)
  )
  
  message(
    "=============================================================="
  )
  
  
  # ===========================================================================
  # 8.2 ARQUIVOS DE SAÍDA
  # ===========================================================================
  
  arquivo_historico <- file.path(
    PASTA_HISTORICO,
    paste0(
      code_muni,
      "_historico.rds"
    )
  )
  
  
  arquivo_problemas <- file.path(
    PASTA_PROBLEMAS,
    paste0(
      code_muni,
      "_problemas.rds"
    )
  )
  
  
  # ===========================================================================
  # 8.3 VERIFICAR SE O MUNICÍPIO JÁ FOI PROCESSADO
  # ===========================================================================
  
  if (file.exists(arquivo_historico)) {
    
    historico_existente <- tryCatch(
      readRDS(arquivo_historico),
      error = function(e) NULL
    )
    
    
    arquivo_valido <-
      !is.null(historico_existente) &&
      nrow(historico_existente) == 365 &&
      all(historico_existente$n_anos == 20)
    
    
    if (isTRUE(arquivo_valido)) {
      
      message(
        "Arquivo existente, válido e completo. Município ignorado."
      )
      
      
      return(
        tibble(
          code_muni = code_muni,
          name_muni = name_muni,
          Estado = estado,
          dias_climatologia = 365,
          completude = 100,
          status = "COMPLETO"
        )
      )
    }
  }
  
  
  # ===========================================================================
  # 8.4 DOWNLOAD DOS DADOS HORÁRIOS
  # ===========================================================================
  
  message(
    "Baixando dados horários da NASA POWER..."
  )
  
  
  dados <- nasapower::get_power(
    
    community = "ag",
    
    lonlat = c(
      lon,
      lat
    ),
    
    pars = c(
      "T2M",
      "RH2M"
    ),
    
    dates = c(
      data_inicio,
      data_fim
    ),
    
    temporal_api = "hourly",
    
    time_standard = "LST"
  )
  
  
  # ===========================================================================
  # 8.5 VERIFICAR COLUNAS RETORNADAS
  # ===========================================================================
  
  colunas_necessarias <- c(
    "YEAR",
    "MO",
    "DY",
    "T2M",
    "RH2M"
  )
  
  
  if (
    !all(
      colunas_necessarias %in%
      names(dados)
    )
  ) {
    
    stop(
      "NASA POWER não retornou todas as colunas necessárias."
    )
  }
  
  
  # ===========================================================================
  # 8.6 CALCULAR EXTREMOS DE CADA DIA DE CADA ANO
  # ===========================================================================
  
  diario_original <- dados %>%
    
    group_by(
      YEAR,
      MO,
      DY
    ) %>%
    
    summarise(
      
      # Número total de registros
      n_registros = n(),
      
      # Número de registros válidos
      n_T = sum(
        !is.na(T2M)
      ),
      
      n_UR = sum(
        !is.na(RH2M)
      ),
      
      
      # -----------------------------------------------------------------------
      # TEMPERATURA
      # -----------------------------------------------------------------------
      
      T_max = if (
        all(is.na(T2M))
      ) {
        
        NA_real_
        
      } else {
        
        max(
          T2M,
          na.rm = TRUE
        )
      },
      
      
      T_min = if (
        all(is.na(T2M))
      ) {
        
        NA_real_
        
      } else {
        
        min(
          T2M,
          na.rm = TRUE
        )
      },
      
      
      T_med = if (
        all(is.na(T2M))
      ) {
        
        NA_real_
        
      } else {
        
        mean(
          T2M,
          na.rm = TRUE
        )
      },
      
      
      # -----------------------------------------------------------------------
      # UMIDADE RELATIVA
      # -----------------------------------------------------------------------
      
      UR_max = if (
        all(is.na(RH2M))
      ) {
        
        NA_real_
        
      } else {
        
        max(
          RH2M,
          na.rm = TRUE
        )
      },
      
      
      UR_min = if (
        all(is.na(RH2M))
      ) {
        
        NA_real_
        
      } else {
        
        min(
          RH2M,
          na.rm = TRUE
        )
      },
      
      
      UR_med = if (
        all(is.na(RH2M))
      ) {
        
        NA_real_
        
      } else {
        
        mean(
          RH2M,
          na.rm = TRUE
        )
      },
      
      .groups = "drop"
    )
  
  
  # ===========================================================================
  # 8.7 GARANTIR A PRESENÇA DE TODOS OS 7.305 DIAS
  # ===========================================================================
  
  diario <- calendario_completo %>%
    
    left_join(
      diario_original,
      by = c(
        "YEAR",
        "MO",
        "DY"
      )
    )
  
  
  # ===========================================================================
  # 8.8 VERIFICAR COMPLETUDE DOS DIAS
  # ===========================================================================
  
  diario <- diario %>%
    
    mutate(
      
      dia_presente =
        !is.na(n_registros),
      
      horas_completas =
        !is.na(n_T) &
        !is.na(n_UR) &
        n_T >= min_horas &
        n_UR >= min_horas,
      
      dia_valido =
        dia_presente &
        horas_completas
    )
  
  
  # ===========================================================================
  # 8.9 CALCULAR ITU ANTICÍCLICO
  #
  # ITU mínimo = temperatura mínima + umidade máxima
  # ITU máximo = temperatura máxima + umidade mínima
  # ===========================================================================
  
  diario <- diario %>%
    
    mutate(
      
      ITU_min = ifelse(
        
        dia_valido,
        
        calc_ITU(
          T_min,
          UR_max
        ),
        
        NA_real_
      ),
      
      
      ITU_max = ifelse(
        
        dia_valido,
        
        calc_ITU(
          T_max,
          UR_min
        ),
        
        NA_real_
      )
    )
  
  
  # ===========================================================================
  # 8.10 CÁLCULO DOS PARÂMETROS DO THIload
  # ===========================================================================
  
  diario <- diario %>%
    
    mutate(
      
      media =
        (ITU_max + ITU_min) / 2,
      
      amplitude =
        (ITU_max - ITU_min) / 2,
      
      
      # -----------------------------------------------------------------------
      # Proteção apenas para amplitude = 0
      # -----------------------------------------------------------------------
      
      argumento_x1 = ifelse(
        !is.na(amplitude) &
          amplitude != 0,
        
        (THIlimiar - media) /
          amplitude,
        
        NA_real_
      ),
      
      
      argumento_x22 = ifelse(
        !is.na(amplitude) &
          amplitude != 0,
        
        (media - THIlimiar) /
          amplitude,
        
        NA_real_
      ),
      
      
      x1 = asin(
        limitar_asin(
          argumento_x1
        )
      ),
      
      
      x2 =
        pi - x1,
      
      
      x11 =
        pi,
      
      
      x22 =
        pi +
        asin(
          limitar_asin(
            argumento_x22
          )
        ),
      
      
      P =
        (
          cos(x22) -
            cos(x11)
        ) *
        amplitude *
        (periodo / pi)
    )
  
  
  # ===========================================================================
  # 8.11 CÁLCULO DO THIload
  # ===========================================================================
  
  diario <- diario %>%
    
    mutate(
      
      THIload = case_when(
        
        # ---------------------------------------------------------------------
        # ITU máximo <= limiar
        # ---------------------------------------------------------------------
        
        is.na(ITU_max) |
          is.na(ITU_min) ~
          NA_real_,
        
        
        THIlimiar >= ITU_max ~
          0,
        
        
        # ---------------------------------------------------------------------
        # ITU mínimo > limiar
        # ---------------------------------------------------------------------
        
        THIlimiar < ITU_min ~
          
          periodo *
          (
            media -
              THIlimiar
          ),
        
        
        # ---------------------------------------------------------------------
        # Limiar >= média
        # ---------------------------------------------------------------------
        
        THIlimiar >= media ~
          
          (
            cos(x1) -
              cos(x2) *
              amplitude *
              (
                (periodo / 2) / pi -
                  (x2 - x1)
              ) *
              (
                THIlimiar -
                  media
              )
          ),
        
        
        # ---------------------------------------------------------------------
        # Limiar < média
        # ---------------------------------------------------------------------
        
        TRUE ~
          
          (
            amplitude *
              (
                (periodo / pi) +
                  (
                    media -
                      THIlimiar
                  )
              ) *
              (
                (periodo / 2) +
                  (
                    media -
                      THIlimiar
                  )
              ) *
              (
                (
                  x22 -
                    pi
                ) *
                  (
                    periodo /
                      pi
                  ) -
                  P
              )
          )
      )
    )
  
  
  # ===========================================================================
  # 8.12 CÁLCULO DA DURAÇÃO DO ESTRESSE - D
  # ===========================================================================
  
  diario <- diario %>%
    
    mutate(
      
      D = case_when(
        
        # Sem dados válidos
        is.na(ITU_max) |
          is.na(ITU_min) ~
          NA_real_,
        
        
        # ---------------------------------------------------------------
        # Todo o dia abaixo do limiar
        # ---------------------------------------------------------------
        
        THIlimiar > ITU_max ~
          0,
        
        
        # ---------------------------------------------------------------
        # Todo o dia acima do limiar
        # ---------------------------------------------------------------
        
        THIlimiar < ITU_min ~
          24,
        
        
        # ---------------------------------------------------------------
        # Caso especial: amplitude zero
        # ---------------------------------------------------------------
        
        ITU_max == media ~
          ifelse(
            media >= THIlimiar,
            24,
            0
          ),
        
        
        # ---------------------------------------------------------------
        # Limiar acima da média
        # ---------------------------------------------------------------
        
        THIlimiar > media ~
          
          (
            pi -
              2 *
              asin(
                limitar_asin(
                  (
                    THIlimiar -
                      media
                  ) /
                    (
                      ITU_max -
                        media
                    )
                )
              )
          ) /
          (2 * pi) *
          24,
        
        
        # ---------------------------------------------------------------
        # Limiar abaixo ou igual à média
        # ---------------------------------------------------------------
        
        TRUE ~
          
          (
            pi +
              2 *
              asin(
                limitar_asin(
                  (
                    media -
                      THIlimiar
                  ) /
                    (
                      ITU_max -
                        media
                    )
                )
              )
          ) /
          (2 * pi) *
          24
      )
    )
  
  
  # ===========================================================================
  # 8.13 GARANTIA NUMÉRICA DA DURAÇÃO
  #
  # D deve estar obrigatoriamente entre 0 e 24 horas.
  # ===========================================================================
  
  diario <- diario %>%
    
    mutate(
      
      D = ifelse(
        is.na(D),
        NA_real_,
        pmax(
          0,
          pmin(
            24,
            D
          )
        )
      )
    )
  
  
  # ===========================================================================
  # 8.14 IDENTIFICAR DIAS PROBLEMÁTICOS
  # ===========================================================================
  
  problemas <- diario %>%
    
    filter(
      !dia_valido
    ) %>%
    
    mutate(
      code_muni = code_muni,
      name_muni = name_muni,
      Estado = estado,
      lon = lon,
      lat = lat,
      .before = 1
    )
  
  
  # Salvar somente se houver problemas
  if (nrow(problemas) > 0) {
    
    saveRDS(
      problemas,
      arquivo_problemas
    )
    
  } else {
    
    # Remove arquivo antigo caso uma nova tentativa
    # tenha corrigido o problema.
    
    if (
      file.exists(
        arquivo_problemas
      )
    ) {
      
      file.remove(
        arquivo_problemas
      )
    }
  }
  
  
  # ===========================================================================
  # 8.15 CONTROLE DE COMPLETUDE
  # ===========================================================================
  
  n_dias_presentes <- sum(
    diario$dia_presente,
    na.rm = TRUE
  )
  
  
  n_dias_validos <- sum(
    diario$dia_valido,
    na.rm = TRUE
  )
  
  
  n_dias_ausentes <- sum(
    !diario$dia_presente,
    na.rm = TRUE
  )
  
  
  n_dias_incompletos <- sum(
    diario$dia_presente &
      !diario$horas_completas,
    na.rm = TRUE
  )
  
  
  completude <- 100 *
    n_dias_validos /
    N_DIAS_ESPERADOS
  
  
  # ===========================================================================
  # 8.16 RETIRAR 29 DE FEVEREIRO
  # ===========================================================================
  
  diario_clima <- diario %>%
    
    filter(
      !(MO == 2 & DY == 29)
    )
  
  
  # ===========================================================================
  # 8.17 CLIMATOLOGIA DIÁRIA
  # ===========================================================================
  
  historico <- diario_clima %>%
    
    filter(
      dia_valido
    ) %>%
    
    group_by(
      MO,
      DY
    ) %>%
    
    summarise(
      
      # -----------------------------------------------------------------------
      # Número de anos utilizados
      # -----------------------------------------------------------------------
      
      n_anos = n(),
      
      
      # -----------------------------------------------------------------------
      # Temperatura
      # -----------------------------------------------------------------------
      
      T_max = mean(
        T_max,
        na.rm = TRUE
      ),
      
      T_min = mean(
        T_min,
        na.rm = TRUE
      ),
      
      T_med = mean(
        T_med,
        na.rm = TRUE
      ),
      
      
      # -----------------------------------------------------------------------
      # Umidade relativa
      # -----------------------------------------------------------------------
      
      UR_max = mean(
        UR_max,
        na.rm = TRUE
      ),
      
      UR_min = mean(
        UR_min,
        na.rm = TRUE
      ),
      
      UR_med = mean(
        UR_med,
        na.rm = TRUE
      ),
      
      
      # -----------------------------------------------------------------------
      # ITU ANTICÍCLICO
      # -----------------------------------------------------------------------
      
      ITU_max = mean(
        ITU_max,
        na.rm = TRUE
      ),
      
      ITU_min = mean(
        ITU_min,
        na.rm = TRUE
      ),
      
      
      # -----------------------------------------------------------------------
      # THI LOAD
      # -----------------------------------------------------------------------
      
      THIload = mean(
        THIload,
        na.rm = TRUE
      ),
      
      
      # -----------------------------------------------------------------------
      # DURAÇÃO MÉDIA DIÁRIA DO ESTRESSE
      # -----------------------------------------------------------------------
      
      horas_estresse = mean(
        D,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  
  
  # ===========================================================================
  # 8.18 CRIAR CALENDÁRIO CLIMATOLÓGICO DE 365 DIAS
  # ===========================================================================
  
  calendario_365 <- calendario_completo %>%
    
    filter(
      !(MO == 2 & DY == 29)
    ) %>%
    
    distinct(
      MO,
      DY
    ) %>%
    
    arrange(
      MO,
      DY
    ) %>%
    
    mutate(
      DOY = row_number()
    )
  
  
  # Conferência interna
  stopifnot(
    nrow(calendario_365) == 365
  )
  
  
  # ===========================================================================
  # 8.19 JUNTAR CLIMATOLOGIA AO CALENDÁRIO COMPLETO
  # ===========================================================================
  
  historico <- calendario_365 %>%
    
    left_join(
      historico,
      by = c(
        "MO",
        "DY"
      )
    ) %>%
    
    mutate(
      
      code_muni = code_muni,
      name_muni = name_muni,
      Estado = estado,
      lon = lon,
      lat = lat,
      
      # Cada dia deve conter os 20 anos
      climatologia_completa =
        !is.na(n_anos) &
        n_anos == 20,
      
      .before = 1
    )
  
  
  # ===========================================================================
  # 8.20 ORGANIZAR COLUNAS
  # ===========================================================================
  
  historico <- historico %>%
    
    select(
      
      code_muni,
      name_muni,
      Estado,
      lon,
      lat,
      
      MO,
      DY,
      DOY,
      
      n_anos,
      climatologia_completa,
      
      T_max,
      T_min,
      T_med,
      
      UR_max,
      UR_min,
      UR_med,
      
      ITU_max,
      ITU_min,
      
      THIload,
      horas_estresse
    )
  
  
  # ===========================================================================
  # 8.21 VERIFICAÇÕES INTERNAS
  # ===========================================================================
  
  if (
    nrow(historico) != 365
  ) {
    
    stop(
      "A climatologia não possui exatamente 365 dias."
    )
  }
  
  
  # Verificar duração
  if (
    any(
      historico$horas_estresse < 0 |
      historico$horas_estresse > 24,
      na.rm = TRUE
    )
  ) {
    
    stop(
      "Foram encontradas durações fora do intervalo 0-24 horas."
    )
  }
  
  
  # ===========================================================================
  # 8.22 DEFINIR STATUS
  # ===========================================================================
  
  climatologia_ok <- all(
    historico$climatologia_completa
  )
  
  
  status <- if (
    n_dias_validos == N_DIAS_ESPERADOS &&
    isTRUE(climatologia_ok)
  ) {
    
    "COMPLETO"
    
  } else {
    
    "INCOMPLETO"
  }
  
  
  # ===========================================================================
  # 8.23 SALVAR APENAS A CLIMATOLOGIA DE 365 DIAS
  # ===========================================================================
  
  saveRDS(
    historico,
    arquivo_historico
  )
  
  
  # ===========================================================================
  # 8.24 MENSAGENS
  # ===========================================================================
  
  message(
    "Dias esperados (2006-2025): ",
    N_DIAS_ESPERADOS
  )
  
  message(
    "Dias presentes: ",
    n_dias_presentes
  )
  
  message(
    "Dias válidos: ",
    n_dias_validos
  )
  
  message(
    "Dias ausentes: ",
    n_dias_ausentes
  )
  
  message(
    "Dias incompletos: ",
    n_dias_incompletos
  )
  
  message(
    "Completude: ",
    round(
      completude,
      4
    ),
    "%"
  )
  
  message(
    "Dias da climatologia: ",
    nrow(historico)
  )
  
  message(
    "STATUS: ",
    status
  )
  
  
  # ===========================================================================
  # 8.25 RETORNAR RELATÓRIO
  # ===========================================================================
  
  return(
    
    tibble(
      
      code_muni = code_muni,
      name_muni = name_muni,
      Estado = estado,
      
      dias_esperados =
        N_DIAS_ESPERADOS,
      
      dias_presentes =
        n_dias_presentes,
      
      dias_validos =
        n_dias_validos,
      
      dias_ausentes =
        n_dias_ausentes,
      
      dias_incompletos =
        n_dias_incompletos,
      
      completude =
        completude,
      
      dias_climatologia =
        nrow(historico),
      
      climatologia_completa =
        isTRUE(climatologia_ok),
      
      status =
        status
    )
  )
}


# =============================================================================
# 9. TESTAR PRIMEIRO COM UM ÚNICO MUNICÍPIO
# =============================================================================

teste <- GETPOWER(
  tb = sul,
  L = 1
)


teste


# =============================================================================
# 10. ABRIR A CLIMATOLOGIA DO MUNICÍPIO TESTE
# =============================================================================

codigo_teste <- sul$code_muni[1]


teste_historico <- readRDS(
  
  file.path(
    
    PASTA_HISTORICO,
    
    paste0(
      codigo_teste,
      "_historico.rds"
    )
  )
)


# =============================================================================
# 11. AUDITORIA DO MUNICÍPIO TESTE
# =============================================================================

# Deve retornar 365
nrow(
  teste_historico
)


# Deve retornar TRUE = 365
table(
  teste_historico$climatologia_completa,
  useNA = "ifany"
)


# Todos devem ter n_anos = 20
table(
  teste_historico$n_anos,
  useNA = "ifany"
)


# Duração entre 0 e 24 horas
range(
  teste_historico$horas_estresse,
  na.rm = TRUE
)


# ITU
summary(
  teste_historico$ITU_min
)

summary(
  teste_historico$ITU_max
)


# THIload
summary(
  teste_historico$THIload
)


# Visualizar
View(
  teste_historico
)


# =============================================================================
# 12. EXECUTAR TODOS OS MUNICÍPIOS
# EXECUTAR SOMENTE DEPOIS DE VALIDAR O MUNICÍPIO TESTE
# =============================================================================

relatorio_lista <- vector(
  "list",
  nrow(sul)
)


for (
  i in seq_len(
    nrow(sul)
  )
) {
  
  relatorio_lista[[i]] <- tryCatch(
    
    {
      
      GETPOWER(
        tb = sul,
        L = i
      )
      
    },
    
    error = function(e) {
      
      message(
        "\nERRO EM: ",
        sul$name_muni[i],
        " - ",
        sul$abbrev_state[i]
      )
      
      message(
        "Mensagem: ",
        e$message
      )
      
      
      tibble(
        
        code_muni =
          sul$code_muni[i],
        
        name_muni =
          sul$name_muni[i],
        
        Estado =
          sul$abbrev_state[i],
        
        dias_esperados =
          N_DIAS_ESPERADOS,
        
        dias_presentes =
          NA_integer_,
        
        dias_validos =
          NA_integer_,
        
        dias_ausentes =
          NA_integer_,
        
        dias_incompletos =
          NA_integer_,
        
        completude =
          NA_real_,
        
        dias_climatologia =
          NA_integer_,
        
        climatologia_completa =
          FALSE,
        
        status =
          paste0(
            "ERRO: ",
            e$message
          )
      )
    }
  )
  
  
  # ---------------------------------------------------------------------------
  # SALVAR RELATÓRIO PARCIAL APÓS CADA MUNICÍPIO
  # ---------------------------------------------------------------------------
  
  relatorio_parcial <- bind_rows(
    relatorio_lista[
      seq_len(i)
    ]
  )
  
  
  saveRDS(
    
    relatorio_parcial,
    
    file.path(
      PASTA_RELATORIOS,
      "Relatorio_parcial.rds"
    )
  )
  
  
  # Pequena pausa entre requisições
  Sys.sleep(1)
}


# =============================================================================
# 13. RELATÓRIO FINAL DE COMPLETUDE
# =============================================================================

relatorio_final <- bind_rows(
  relatorio_lista
)


saveRDS(
  
  relatorio_final,
  
  file.path(
    PASTA_RELATORIOS,
    "Relatorio_completude_SUL.rds"
  )
)


writexl::write_xlsx(
  
  relatorio_final,
  
  file.path(
    PASTA_RELATORIOS,
    "Relatorio_completude_SUL.xlsx"
  )
)


# Status
table(
  relatorio_final$status
)


# Municípios problemáticos
municipios_incompletos <- relatorio_final %>%
  
  filter(
    status != "COMPLETO"
  )


municipios_incompletos


writexl::write_xlsx(
  
  municipios_incompletos,
  
  file.path(
    PASTA_RELATORIOS,
    "Municipios_incompletos.xlsx"
  )
)


# =============================================================================
# 14. IMPORTAR TODAS AS CLIMATOLOGIAS
# =============================================================================

arquivos_historico <- list.files(
  
  PASTA_HISTORICO,
  
  pattern = "_historico\\.rds$",
  
  full.names = TRUE
)


DATA_FINAL <- arquivos_historico %>%
  
  lapply(
    readRDS
  ) %>%
  
  bind_rows()


# =============================================================================
# 15. AUDITORIA DA BASE FINAL
# =============================================================================

# Número de municípios processados
n_distinct(
  DATA_FINAL$code_muni
)


# Municípios por estado
municipios_estado <- DATA_FINAL %>%
  
  distinct(
    code_muni,
    Estado
  ) %>%
  
  count(
    Estado
  )


municipios_estado


# =============================================================================
# 16. CADA MUNICÍPIO DEVE TER EXATAMENTE 365 DIAS
# =============================================================================

controle_linhas <- DATA_FINAL %>%
  
  count(
    code_muni,
    name_muni,
    Estado,
    name = "n_dias"
  )


table(
  controle_linhas$n_dias
)


# Deve retornar zero linhas
controle_linhas %>%
  
  filter(
    n_dias != 365
  )


# =============================================================================
# 17. CADA DIA DEVE SER FORMADO PELOS 20 ANOS
# =============================================================================

controle_anos <- DATA_FINAL %>%
  
  filter(
    is.na(n_anos) |
      n_anos != 20
  )


# Deve retornar zero
nrow(
  controle_anos
)


# =============================================================================
# 18. VERIFICAR DURAÇÃO DO ESTRESSE
# =============================================================================

range(
  DATA_FINAL$horas_estresse,
  na.rm = TRUE
)


# Nenhum valor deve ser < 0 ou > 24
controle_D <- DATA_FINAL %>%
  
  filter(
    horas_estresse < 0 |
      horas_estresse > 24 |
      is.na(horas_estresse)
  )


nrow(
  controle_D
)


# =============================================================================
# 19. VERIFICAR RELAÇÃO ITU MÍNIMO <= ITU MÁXIMO
# =============================================================================

controle_ITU <- DATA_FINAL %>%
  
  filter(
    ITU_min > ITU_max |
      is.na(ITU_min) |
      is.na(ITU_max)
  )


nrow(
  controle_ITU
)


# =============================================================================
# 20. VERIFICAR TODOS OS 365 DIAS PARA TODOS OS MUNICÍPIOS
# =============================================================================

controle_final <- DATA_FINAL %>%
  
  group_by(
    code_muni,
    name_muni,
    Estado
  ) %>%
  
  summarise(
    
    n_dias = n(),
    
    n_dias_20_anos =
      sum(
        n_anos == 20,
        na.rm = TRUE
      ),
    
    n_dias_completos =
      sum(
        climatologia_completa,
        na.rm = TRUE
      ),
    
    ITU_minimo =
      min(
        ITU_min,
        na.rm = TRUE
      ),
    
    ITU_maximo =
      max(
        ITU_max,
        na.rm = TRUE
      ),
    
    horas_estresse_min =
      min(
        horas_estresse,
        na.rm = TRUE
      ),
    
    horas_estresse_max =
      max(
        horas_estresse,
        na.rm = TRUE
      ),
    
    completo =
      n_dias == 365 &
      n_dias_20_anos == 365 &
      n_dias_completos == 365,
    
    .groups = "drop"
  )


table(
  controle_final$completo
)


# Municípios que não passaram na auditoria
controle_final %>%
  
  filter(
    !completo
  )


# =============================================================================
# 21. SALVAR BASE FINAL
# =============================================================================

saveRDS(
  
  DATA_FINAL,
  
  file.path(
    PASTA_PRINCIPAL,
    "Climatologia_diaria_SUL_2006_2025.rds"
  )
)


# =============================================================================
# 22. SALVAR AUDITORIA FINAL
# =============================================================================

writexl::write_xlsx(
  
  list(
    
    "Relatorio_download" =
      relatorio_final,
    
    "Municipios_estado" =
      municipios_estado,
    
    "Controle_365_dias" =
      controle_linhas,
    
    "Controle_final" =
      controle_final
    
  ),
  
  file.path(
    PASTA_RELATORIOS,
    "Auditoria_final_SUL.xlsx"
  )
)


# =============================================================================
# 23. RESUMO FINAL
# =============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "PROCESSAMENTO FINALIZADO\n"
)

cat(
  "============================================================\n"
)

cat(
  "Municípios esperados:",
  nrow(sul),
  "\n"
)

cat(
  "Municípios na base final:",
  n_distinct(DATA_FINAL$code_muni),
  "\n"
)

cat(
  "Municípios aprovados na auditoria:",
  sum(
    controle_final$completo,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Municípios com problemas:",
  sum(
    !controle_final$completo,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Dias por município: 365\n"
)

cat(
  "Período histórico: 2006-2025\n"
)

cat(
  "Limiar de ITU:",
  THI_LIMIAR,
  "\n"
)

cat(
  "Linhas da base final:",
  nrow(DATA_FINAL),
  "\n"
)

cat(
  "============================================================\n"
)

# =============================================================================
# 24. Fazendo o download de algum município que estava incompleto
# Utilizando a filtragem no aquico relatorio_final
# =============================================================================

# Identificar municípios incompletos
codigos_erro <- relatorio_final %>%
  filter(status != "COMPLETO") %>%
  pull(code_muni)

# Encontrar suas posições na base sul
indices_erro <- match(
  codigos_erro,
  sul$code_muni
)

# Conferir
sul[indices_erro, ]


# Refazer somente os municípios com erro
resultados_erro <- lapply(
  indices_erro,
  function(i) {
    
    tryCatch(
      
      GETPOWER(
        tb = sul,
        L = i
      ),
      
      error = function(e) {
        
        message(
          "Erro em ",
          sul$name_muni[i],
          ": ",
          e$message
        )
        
        NULL
      }
    )
  }
)

# Ver resultados
bind_rows(resultados_erro)

# =============================================================================
# Se foi realizado o download de todos os dados, 
# execute a partir do bloco "14. IMPORTAR TODAS AS CLIMATOLOGIAS"
# =============================================================================