# ====================================================================
# ANÁLISE DE CONTEÚDO DE BARDIN - SCRIPT COMPLETO
# Análise de diferenciação familiar por gênero em comentários do Instagram
# ====================================================================

# ====================================================================
# CONFIGURAÇÕES - AJUSTE AQUI SUAS VARIÁVEIS
# ====================================================================

CONFIG <- list(
  # DADOS DE ENTRADA
  arquivo_dados = "../resultados/comentarios_instagram.csv",
  encoding = "UTF-8",

  # TOPIC MODELING HÍBRIDO
  usar_lda_automatico = TRUE,
  usar_topicos_manuais = TRUE, 
  usar_validacao_cruzada = TRUE,
  gerar_graficos_comparativos = TRUE,

  # DEFINIÇÃO DE TÓPICOS TEÓRICOS
  topicos_teoricos = list(
    "Trabalho Doméstico" = c("lavar", "cozinhar", "limpar", "louça", "casa", "faxina", "arrumar"),
    "Liberdades e Autonomia" = c("sair", "namorar", "festa", "liberdade", "permissão", "hora", "voltar"),
    "Educação e Desenvolvimento" = c("escola", "estudar", "faculdade", "inteligente", "nota", "curso"),
    "Relações Afetivas" = c("carinho", "amor", "atenção", "cuidado", "proteger", "favorito"),
    "Recursos Materiais" = c("quarto", "dinheiro", "presente", "espaço", "brinquedo", "roupa"),
    "Consciência e Resistência" = c("injusto", "revolta", "machismo", "questionar", "desigual", "brigar")
  ),

  # FILTROS DO CORPUS
  min_caracteres = 15,
  palavras_contexto = c("irmão", "irmã", "pai", "mãe", "casa", "família", 
                       "diferente", "tratamento", "machismo", 
                       "filhos", "criação", "educação", "responsabilidade"),
  
  # ANÁLISES ATIVADAS
  usar_sentimentos = TRUE,
  usar_topic_modeling = TRUE,
  gerar_graficos = TRUE,
  
  # TOPIC MODELING
  k_topicos = 6,
  avaliar_k_automatico = TRUE,
  k_range = 3:8,
  
  # OUTPUTS
  dir_resultados = "resultados",
  dir_graficos = "resultados/graficos",
  formato_graficos = "png",
  dpi = 300,
  
  # PROCESSAMENTO
  seed = 1234
)

# ====================================================================
# SISTEMA DE CATEGORIAS DE BARDIN - CUSTOMIZE AQUI
# ====================================================================

SISTEMA_CATEGORIAS <- list(
  diferenciacao_domestica = c("lavar", "cozinhar", "limpar", "cuidar", "casa", "louça", "roupa", "faxina", "arrumar"),
  diferenciacao_liberdades = c("sair", "namorar", "amigos", "festa", "voltar", "hora", "permissão", "liberdade"),
  diferenciacao_educacional = c("escola", "estudar", "nota", "faculdade", "inteligente", "curso", "profissão"),
  diferenciacao_afetiva = c("proteger", "cuidado", "carinho", "atenção", "preocupação", "amor", "favorito"),
  diferenciacao_material = c("quarto", "dinheiro", "presente", "roupa", "brinquedo", "espaço", "material"),
  consciencia_resistencia = c("injusto", "revolta", "questionar", "brigar", "machismo", "desigual")
)

# ====================================================================
# LÉXICO DE SENTIMENTOS EM PORTUGUÊS - CUSTOMIZE AQUI
# ====================================================================

LEXICON_PT <- data.frame(
  word = c(
    # Negativos fortes
    "triste", "raiva", "injusto", "revolta", "indignada", "chateada", 
    "magoada", "frustrada", "irritada", "decepcionada", "ressentida",
    "excluída", "rejeitada", "inferior", "diminuída", "desvalorizada",
    
    # ADICIONAR MAIS NEGATIVOS:
    "cansada", "estressada", "sobrecarregada", "oprimida", "sufocada",
    "invisível", "negligenciada", "abandonada", "explorada",
    
    # Positivos
    "feliz", "grata", "orgulhosa", "aliviada", "satisfeita", "contente",
    "valorizada", "respeitada", "igual", "justa",
    
    # ADICIONAR MAIS POSITIVOS:
    "empoderada", "livre", "independente", "realizada", "apoiada",
    
    # Contextuais negativos
    "desigual", "injusta", "preferido", "favorito", "discriminação",
    "machismo", "machista", "preconceito", "diferença", "diferente"
  ),
  polarity = c(
    rep(-1, 25),      # Negativos fortes (16 + 9 novos)
    rep(1, 15),       # Positivos (10 + 5 novos)  
    rep(-0.5, 10)     # Contextuais negativos
  ),
  stringsAsFactors = FALSE
)


# ====================================================================
# CARREGAR BIBLIOTECAS
# ====================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(quanteda)
  library(quanteda.textstats)
  library(topicmodels)
  library(tidytext)
  library(wordcloud2)
  library(lubridate)
  library(jsonlite)
})

# Carregar funções auxiliares se disponível
if(file.exists("funcoes/funcoes_auxiliares.R")) {
  source("funcoes/funcoes_auxiliares.R")
  cat("✓ Funções auxiliares carregadas\n")
} else {
  cat("⚠ Usando funções básicas integradas\n")
}

# ====================================================================
# FUNÇÕES AUXILIARES
# ====================================================================

setup_dirs <- function() {
  dirs <- c(CONFIG$dir_resultados, CONFIG$dir_graficos)
  for(dir in dirs) {
    if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  }
}

clean_text <- function(text) {
  if(exists("clean_text_basic")) {
    return(clean_text_basic(text))
  }
  
  text %>%
    str_replace_all("(?i)\\bmãe|mae|mamae\\b", "mãe") %>%
    str_replace_all("(?i)\\bpai|papai\\b", "pai") %>%
    str_replace_all("(?i)\\birmao|irmão\\b", "irmão") %>%
    str_replace_all("(?i)\\birma|irmã\\b", "irmã") %>%
    str_replace_all("(?i)\\bdiferente|diferença|diferenciar\\b", "diferenciação") %>%
    str_replace_all("(?i)\\bmachismo|machista\\b", "machismo") %>%
    str_remove_all("http[s]?://[^\\s]+") %>%
    str_remove_all("@[A-Za-z0-9_]+") %>%
    str_remove_all("#[A-Za-z0-9_]+") %>%
    str_remove_all("[^\\p{L}\\p{N}\\s]") %>%
    str_squish()
}

categorizar_documento <- function(texto, sistema) {
  if(exists("detect_theme")) {
    categorias_encontradas <- c()
    for(categoria in names(sistema)) {
      if(detect_theme(tolower(texto), sistema[[categoria]])) {
        categorias_encontradas <- c(categorias_encontradas, categoria)
      }
    }
  } else {
    texto_lower <- tolower(texto)
    categorias_encontradas <- c()
    
    for(categoria in names(sistema)) {
      indicadores <- sistema[[categoria]]
      if(any(str_detect(texto_lower, paste(indicadores, collapse = "|")))) {
        categorias_encontradas <- c(categorias_encontradas, categoria)
      }
    }
  }
  
  return(list(
    categorias = categorias_encontradas,
    n_categorias = length(categorias_encontradas),
    categoria_principal = if(length(categorias_encontradas) > 0) categorias_encontradas[1] else "sem_categoria"
  ))
}

analisar_sentimento <- function(texto, lexicon) {
  palavras <- str_split(tolower(str_remove_all(texto, "[^\\p{L}\\s]")), "\\s+")[[1]]
  sentimentos <- palavras[palavras %in% lexicon$word]
  
  if(length(sentimentos) == 0) {
    return(list(score = 0, polaridade = "neutro", intensidade = 0, n_palavras = 0))
  }
  
  polaridades <- lexicon$polarity[match(sentimentos, lexicon$word)]
  score <- sum(polaridades, na.rm = TRUE) / length(sentimentos)
  
  polaridade <- case_when(
    score > 0.1 ~ "positivo",
    score < -0.1 ~ "negativo",
    TRUE ~ "neutro"
  )
  
  return(list(
    score = round(score, 3),
    polaridade = polaridade,
    intensidade = round(abs(score), 3),
    n_palavras = length(sentimentos)
  ))
}

calcular_metricas_qualidade <- function(dados_categorizados) {
  if(exists("calculate_categorization_quality")) {
    return(calculate_categorization_quality(dados_categorizados))
  }
  
  return(list(
    exclusao_mutua = round(mean(dados_categorizados$n_categorias == 1, na.rm = TRUE), 3),
    cobertura = round(mean(dados_categorizados$categoria_principal != "sem_categoria", na.rm = TRUE), 3),
    categorias_ativas = sum(table(dados_categorizados$categoria_principal) >= 5)
  ))
}

salvar_resultado <- function(objeto, nome_arquivo, tipo = "rds") {
  caminho <- file.path(CONFIG$dir_resultados, paste0(nome_arquivo, ".", tipo))
  
  if(tipo == "rds") {
    saveRDS(objeto, caminho)
  } else if(tipo == "csv") {
    write_csv(objeto, caminho)
  } else if(tipo == "json") {
    write_json(objeto, caminho, pretty = TRUE, auto_unbox = TRUE)
  }
  
  cat("✓ Salvo:", nome_arquivo, ".", tipo, "\n")
}

# ====================================================================
# PIPELINE PRINCIPAL
# ====================================================================

executar_analise_bardin <- function() {
  
  cat("=== INICIANDO ANÁLISE DE BARDIN ===\n\n")
  set.seed(CONFIG$seed)
  setup_dirs()
  
  # ----------------------------------------------------------------
  # 1. PRÉ-ANÁLISE: CARREGAR E FILTRAR DADOS
  # ----------------------------------------------------------------
  
  cat("1. PRÉ-ANÁLISE: Carregando dados...\n")
  
  dados_brutos <- read_csv(CONFIG$arquivo_dados, locale = locale(encoding = CONFIG$encoding))
  
  corpus_filtrado <- dados_brutos %>%
    filter(
      nchar(textoComentario) >= CONFIG$min_caracteres,
      !str_detect(textoComentario, "^@"),
      str_detect(tolower(textoComentario), paste(CONFIG$palavras_contexto, collapse = "|"))
    ) %>%
    mutate(
      id_comentario = row_number(),
      texto_limpo = clean_text(textoComentario),
      n_palavras = str_count(texto_limpo, "\\S+")
    )
  
  cat("   Corpus original:", nrow(dados_brutos), "comentários\n")
  cat("   Corpus filtrado:", nrow(corpus_filtrado), "comentários\n")
  cat("   Taxa aproveitamento:", round(nrow(corpus_filtrado)/nrow(dados_brutos)*100, 2), "%\n\n")
  
  # ----------------------------------------------------------------
  # 2. EXPLORAÇÃO: CATEGORIZAÇÃO TEMÁTICA
  # ----------------------------------------------------------------
  
  cat("2. EXPLORAÇÃO: Categorizando relatos...\n")
  
  dados_categorizados <- corpus_filtrado %>%
    rowwise() %>%
    mutate(
      cat_resultado = list(categorizar_documento(texto_limpo, SISTEMA_CATEGORIAS)),
      categorias = list(cat_resultado$categorias),
      n_categorias = cat_resultado$n_categorias,
      categoria_principal = cat_resultado$categoria_principal
    ) %>%
    ungroup() %>%
    select(-cat_resultado)
  
  freq_categorias <- dados_categorizados %>%
    count(categoria_principal, sort = TRUE) %>%
    mutate(prop = round(n / sum(n) * 100, 2))
  
  metricas_qualidade <- calcular_metricas_qualidade(dados_categorizados)
  
  cat("   Categorias identificadas:", nrow(freq_categorias), "\n")
  cat("   Cobertura categorização:", round(mean(dados_categorizados$categoria_principal != "sem_categoria") * 100, 2), "%\n")
  cat("   Exclusão mútua:", round(metricas_qualidade$exclusao_mutua * 100, 2), "%\n")
  cat("   Categoria principal:", freq_categorias$categoria_principal[1], "(", freq_categorias$n[1], "relatos )\n\n")
  
  # ----------------------------------------------------------------
  # 3. ANÁLISE DE SENTIMENTOS
  # ----------------------------------------------------------------
  
  if(CONFIG$usar_sentimentos) {
    cat("3. ANÁLISE DE SENTIMENTOS: Processando...\n")
    
    dados_sentimentos <- dados_categorizados %>%
      rowwise() %>%
      mutate(
        sent_resultado = list(analisar_sentimento(textoComentario, LEXICON_PT)),
        sent_score = sent_resultado$score,
        sent_polaridade = sent_resultado$polaridade,
        sent_intensidade = sent_resultado$intensidade,
        sent_n_palavras = sent_resultado$n_palavras
      ) %>%
      ungroup() %>%
      select(-sent_resultado)
    
    dist_sentimentos <- dados_sentimentos %>%
      count(sent_polaridade) %>%
      mutate(prop = round(n / sum(n) * 100, 2))
    
    sentimento_por_categoria <- dados_sentimentos %>%
      filter(categoria_principal != "sem_categoria") %>%
      group_by(categoria_principal) %>%
      summarise(
        n_relatos = n(),
        prop_negativo = round(mean(sent_polaridade == "negativo") * 100, 2),
        prop_neutro = round(mean(sent_polaridade == "neutro") * 100, 2),
        prop_positivo = round(mean(sent_polaridade == "positivo") * 100, 2),
        media_score = round(mean(sent_score, na.rm = TRUE), 3),
        .groups = "drop"
      )
    
    neg_prop <- dist_sentimentos$prop[dist_sentimentos$sent_polaridade == "negativo"]
    if(length(neg_prop) == 0) neg_prop <- 0
    
    cat("   Sentimentos negativos:", neg_prop, "%\n")
    cat("   Score médio geral:", round(mean(dados_sentimentos$sent_score, na.rm = TRUE), 3), "\n\n")
    
  } else {
    dados_sentimentos <- dados_categorizados
    dist_sentimentos <- NULL
    sentimento_por_categoria <- NULL
  }
  
  # ----------------------------------------------------------------
  # 4. CORPUS QUANTEDA E PROCESSAMENTO
  # ----------------------------------------------------------------

  cat("4. PROCESSAMENTO: Criando corpus quanteda...\n")

  corp <- corpus(dados_sentimentos$texto_limpo)

  tokens_obj <- tokens(corp, 
                      remove_punct = TRUE, 
                      remove_symbols = TRUE,
                      remove_numbers = FALSE) %>%
    tokens_tolower() %>%
    tokens_remove(c(stopwords("pt"), "né", "aí", "pra", "pro", "tá", "tô", "vc", "vcs",
                    "ser", "ter", "fazer", "dar", "ir", "ver", "saber", "estar", "ficar")) %>%
    tokens_keep(pattern = "^[a-záéíóúâêîôûãõç]{3,}$", valuetype = "regex")

  dfm_obj <- dfm(tokens_obj) %>%
    dfm_trim(min_docfreq = 3,
            max_docfreq = 0.90,
            docfreq_type = "count")

  # Verificar se há tokens
  if(sum(ntoken(tokens_obj)) == 0) {
    cat("   ⚠️ Aviso: Corpus vazio após limpeza. Usando limpeza mínima.\n")
    tokens_obj <- tokens(corp, remove_punct = TRUE) %>%
      tokens_tolower() %>%
      tokens_remove(stopwords("pt"))
  }

  dfm_obj <- dfm(tokens_obj)

  # Aplicar filtros graduais
  if(nfeat(dfm_obj) > 100) {
    dfm_obj <- dfm_trim(dfm_obj, min_docfreq = 2, max_docfreq = 0.95, docfreq_type = "count")
  } else if(nfeat(dfm_obj) > 50) {
    dfm_obj <- dfm_trim(dfm_obj, min_docfreq = 1, max_docfreq = 0.98, docfreq_type = "count")
  }

  # Verificar se DFM ainda tem conteúdo
  if(nfeat(dfm_obj) == 0 || sum(dfm_obj) == 0) {
    cat("   ⚠️ DFM vazio. Usando versão sem filtros.\n")
    dfm_obj <- dfm(tokens_obj)
  }

  if(nfeat(dfm_obj) > 0) {
    freq_palavras <- textstat_frequency(dfm_obj, n = min(50, nfeat(dfm_obj)))
  } else {
    freq_palavras <- data.frame(feature = character(0), frequency = numeric(0))
    cat("   ⚠️ Nenhuma palavra encontrada após processamento.\n")
  }

  cat("   Documents:", ndoc(dfm_obj), "\n")
  cat("   Features:", nfeat(dfm_obj), "\n")
  if(nfeat(dfm_obj) > 0) {
    cat("   Sparsity:", round(sparsity(dfm_obj), 4), "\n")
  }
  cat("\n")
  
  # ----------------------------------------------------------------
  # 5. TOPIC MODELING HÍBRIDO - LDA + TÓPICOS TEÓRICOS
  # ----------------------------------------------------------------

  lda_model <- NULL
  topics_terms_lda <- NULL
  topicos_manuais_resultado <- NULL
  validacao_cruzada <- NULL
  k_otimo <- CONFIG$k_topicos

  cat("5. TOPIC MODELING: Análise híbrida (LDA + Tópicos teóricos)...\n")

  # === PARTE A: LDA AUTOMÁTICO ===
  if(CONFIG$usar_lda_automatico && nfeat(dfm_obj) > 10) {
    cat("   5.1. Executando LDA automático...\n")
    
    dtm_obj <- convert(dfm_obj, to = "topicmodels")
    
    # Otimização automática do K
    if(CONFIG$avaliar_k_automatico && nrow(dtm_obj) > 0) {
      perplexidades <- sapply(CONFIG$k_range, function(k) {
        tryCatch({
          lda_temp <- LDA(dtm_obj, k = k, method = "VEM", control = list(seed = CONFIG$seed))
          perplexity(lda_temp, dtm_obj)
        }, error = function(e) NA)
      })
      
      valid_k <- CONFIG$k_range[!is.na(perplexidades)]
      if(length(valid_k) > 0) {
        k_otimo <- valid_k[which.min(perplexidades[!is.na(perplexidades)])]
      }
    }
    
    # Executar LDA final
    tryCatch({
      lda_model <- LDA(dtm_obj, k = k_otimo, method = "VEM", control = list(seed = CONFIG$seed))
      topics_terms_lda <- tidy(lda_model, matrix = "beta") %>%
        group_by(topic) %>%
        slice_max(beta, n = 10) %>%
        ungroup() %>%
        mutate(metodo = "LDA_automatico")

        # LDA descobre os tópicos sozinho baseado em padrões estatísticos
        topics_terms <- tidy(lda_model, matrix = "beta") %>%
          group_by(topic) %>%
          slice_max(beta, n = 10) %>%      # ← Pega as 10 palavras mais importantes
          ungroup()
      
      cat("       ✓ Tópicos LDA:", k_otimo, "\n")
      cat("       ✓ Perplexidade:", round(perplexity(lda_model, dtm_obj), 2), "\n")
    }, error = function(e) {
      cat("       ✗ Erro no LDA:", e$message, "\n")
    })
  }

  # === PARTE B: TÓPICOS TEÓRICOS (BARDIN) ===
  if(CONFIG$usar_topicos_manuais) {
    cat("   5.2. Aplicando tópicos teóricos (Bardin)...\n")
    
    # Calcular presença dos tópicos teóricos
    topicos_manuais_resultado <- dados_sentimentos %>%
      mutate(
        topicos_teoria_encontrados = map(texto_limpo, function(texto) {
          texto_lower <- tolower(texto)
          topicos_presentes <- c()
          
          for(nome_topico in names(CONFIG$topicos_teoricos)) {
            palavras_topico <- CONFIG$topicos_teoricos[[nome_topico]]
            if(any(str_detect(texto_lower, paste(palavras_topico, collapse = "|")))) {
              topicos_presentes <- c(topicos_presentes, nome_topico)
            }
          }
          return(topicos_presentes)
        }),
        n_topicos_teoria = map_int(topicos_teoria_encontrados, length),
        topico_teoria_principal = map_chr(topicos_teoria_encontrados, ~ifelse(length(.x) > 0, .x[1], "sem_topico"))
      )
    
    # Estatísticas dos tópicos teóricos
    freq_topicos_teoria <- topicos_manuais_resultado %>%
      count(topico_teoria_principal, sort = TRUE) %>%
      mutate(prop = round(n / sum(n) * 100, 2)) %>%
      filter(topico_teoria_principal != "sem_topico")
    
    cobertura_teoria <- round(mean(topicos_manuais_resultado$topico_teoria_principal != "sem_topico") * 100, 1)
    
    cat("       ✓ Tópicos teóricos:", length(CONFIG$topicos_teoricos), "\n")
    cat("       ✓ Cobertura:", cobertura_teoria, "%\n")
  }

  # === PARTE C: VALIDAÇÃO CRUZADA ===
  if(CONFIG$usar_validacao_cruzada && !is.null(topics_terms_lda) && !is.null(topicos_manuais_resultado)) {
    cat("   5.3. Validação cruzada LDA ↔ Teoria...\n")
    
    # Calcular similaridade entre tópicos LDA e teóricos
    validacao_cruzada <- data.frame()
    
    for(topico_lda in unique(topics_terms_lda$topic)) {
      palavras_lda <- topics_terms_lda %>%
        filter(topic == topico_lda) %>%
        pull(term)
      
      for(nome_teoria in names(CONFIG$topicos_teoricos)) {
        palavras_teoria <- CONFIG$topicos_teoricos[[nome_teoria]]
        
        # Calcular Jaccard similarity
        intersecao <- length(intersect(palavras_lda, palavras_teoria))
        uniao <- length(union(palavras_lda, palavras_teoria))
        similaridade <- if(uniao > 0) intersecao / uniao else 0
        
        validacao_cruzada <- rbind(validacao_cruzada, data.frame(
          topico_lda = topico_lda,
          topico_teoria = nome_teoria,
          similaridade = similaridade,
          palavras_comuns = intersecao,
          palavras_lda = paste(palavras_lda[1:5], collapse = ", "),
          palavras_teoria = paste(palavras_teoria[1:5], collapse = ", ")
        ))
      }
    }
    
    # Encontrar melhor correspondência para cada tópico LDA
    melhor_correspondencia <- validacao_cruzada %>%
      group_by(topico_lda) %>%
      slice_max(similaridade, n = 1) %>%
      ungroup()
    
    similaridade_media <- round(mean(melhor_correspondencia$similaridade), 3)
    
    cat("       ✓ Correspondência média:", similaridade_media, "\n")
    cat("       ✓ Melhor match:", melhor_correspondencia$topico_teoria[which.max(melhor_correspondencia$similaridade)], "\n")
  }

  cat("\n")
# ----------------------------------------------------------------
# 5. TOPIC MODELING - ANÁLISE COMPARATIVA
# ----------------------------------------------------------------

cat("5. TOPIC MODELING: Executando análise comparativa...\n")

# === LDA AUTOMÁTICO (ATUAL) ===
lda_model_auto <- NULL
topics_terms_auto <- NULL
k_otimo <- CONFIG$k_topicos

if(CONFIG$usar_topic_modeling && nfeat(dfm_obj) > 10) {
  
  dtm_obj <- convert(dfm_obj, to = "topicmodels")
  
  # LDA automático (como está)
  tryCatch({
    lda_model_auto <- LDA(dtm_obj, k = k_otimo, method = "VEM", control = list(seed = CONFIG$seed))
    topics_terms_auto <- tidy(lda_model_auto, matrix = "beta") %>%
      group_by(topic) %>%
      slice_max(beta, n = 10) %>%
      ungroup()
    
    cat("   LDA Automático - Tópicos:", k_otimo, "\n")
    cat("   Perplexidade:", round(perplexity(lda_model_auto, dtm_obj), 2), "\n")
  }, error = function(e) {
    cat("   Erro no LDA automático:", e$message, "\n")
  })
  
  # === LDA GUIADO PELAS CATEGORIAS DE BARDIN ===
  cat("\n   Comparando com categorias de Bardin...\n")
  
  # Criar "sementes" para cada tópico baseadas nas categorias
  sementes_topicos <- list(
    domestica = SISTEMA_CATEGORIAS$diferenciacao_domestica,
    liberdades = SISTEMA_CATEGORIAS$diferenciacao_liberdades,
    educacional = SISTEMA_CATEGORIAS$diferenciacao_educacional,
    afetiva = SISTEMA_CATEGORIAS$diferenciacao_afetiva,
    material = SISTEMA_CATEGORIAS$diferenciacao_material,
    resistencia = SISTEMA_CATEGORIAS$consciencia_resistencia
  )
  
  # Executar LDA para o mesmo número de tópicos que temos categorias
  k_bardin <- length(sementes_topicos)
  
  tryCatch({
    lda_model_bardin <- LDA(dtm_obj, k = k_bardin, method = "VEM", control = list(seed = CONFIG$seed))
    topics_terms_bardin <- tidy(lda_model_bardin, matrix = "beta") %>%
      group_by(topic) %>%
      slice_max(beta, n = 10) %>%
      ungroup()
    
    cat("   LDA Bardin-guiado - Tópicos:", k_bardin, "\n")
    cat("   Perplexidade:", round(perplexity(lda_model_bardin, dtm_obj), 2), "\n")
    
    # === ANÁLISE DE SIMILARIDADE ===
    similaridades <- calcular_similaridade_topicos(topics_terms_bardin, sementes_topicos)
    
    cat("   Similaridade média LDA-Bardin:", round(mean(similaridades$similaridade), 3), "\n\n")
    
  }, error = function(e) {
    cat("   Erro no LDA guiado:", e$message, "\n\n")
  })
}
  
  # ----------------------------------------------------------------
  # 6. VISUALIZAÇÕES
  # ----------------------------------------------------------------
  
  if(CONFIG$gerar_graficos) {
    cat("6. VISUALIZAÇÕES: Gerando gráficos...\n")
    # Distribuição de categorias
    p1 <- freq_categorias %>%
      filter(categoria_principal != "sem_categoria") %>%
      mutate(categoria_clean = str_replace_all(categoria_principal, "_", " ") %>% str_to_title()) %>%
      ggplot(aes(x = reorder(categoria_clean, n), y = n)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      geom_text(aes(label = paste0(n, " (", prop, "%)")), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = "Distribuição das Categorias Temáticas",
        x = "Categorias", 
        y = "Número de Relatos"
      ) +
      theme_minimal()
    
    ggsave(file.path(CONFIG$dir_graficos, "distribuicao_categorias.png"), 
           p1, width = 10, height = 6, dpi = CONFIG$dpi, bg = "white")
    
    if(CONFIG$usar_sentimentos && !is.null(dist_sentimentos)) {
      # Distribuição de sentimentos
      cores_sent <- c("negativo" = "#e74c3c", "neutro" = "#95a5a6", "positivo" = "#27ae60")
      
      p2 <- dist_sentimentos %>%
        ggplot(aes(x = sent_polaridade, y = n, fill = sent_polaridade)) +
        geom_col(alpha = 0.8) +
        geom_text(aes(label = paste0(n, " (", prop, "%)")), vjust = -0.5) +
        scale_fill_manual(values = cores_sent) +
        labs(
          title = "Distribuição de Sentimentos",
          x = "Polaridade", 
          y = "Número de Relatos"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
      ggsave(file.path(CONFIG$dir_graficos, "distribuicao_sentimentos.png"), 
             p2, width = 8, height = 6, dpi = CONFIG$dpi, bg = "white")
      
      # Sentimentos por categoria
      p3 <- sentimento_por_categoria %>%
        select(categoria_principal, prop_negativo, prop_neutro, prop_positivo) %>%
        pivot_longer(cols = starts_with("prop_"), names_to = "polaridade", values_to = "proporcao") %>%
        mutate(
          polaridade = str_remove(polaridade, "prop_"),
          categoria_clean = str_wrap(str_replace_all(categoria_principal, "_", " "), 15)
        ) %>%
        ggplot(aes(x = categoria_clean, y = proporcao, fill = polaridade)) +
        geom_col(position = "stack", alpha = 0.8) +
        scale_fill_manual(values = cores_sent) +
        labs(
          title = "Sentimentos por Categoria",
          x = "Categoria", 
          y = "Proporção (%)",
          fill = "Polaridade"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(file.path(CONFIG$dir_graficos, "sentimentos_por_categoria.png"), 
             p3, width = 12, height = 8, dpi = CONFIG$dpi, bg = "white")
    }
    
    cat("   Gráficos salvos em:", CONFIG$dir_graficos, "\n\n")
  }

  # === HEATMAP COMPARAÇÃO LDA vs BARDIN ===
  if(exists("similaridades") && nrow(similaridades) > 0) {
    
    p_comparacao <- similaridades %>%
      ggplot(aes(x = categoria_bardin, y = factor(topico_lda), fill = similaridade)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(similaridade, 2)), color = "white", fontface = "bold") +
      scale_fill_gradient(low = "#f8f9fa", high = "#2c3e50", 
                        name = "Similaridade", labels = scales::percent) +
      labs(
        title = "Correspondência: Tópicos LDA ↔ Categorias Bardin",
        subtitle = "Validação cruzada metodológica",
        x = "Categorias de Bardin",
        y = "Tópicos LDA"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
    ggsave(file.path(CONFIG$dir_graficos, "comparacao_lda_bardin.png"), 
          p_comparacao, width = 10, height = 6, dpi = CONFIG$dpi, bg = "white")
  }

# === GRÁFICOS COMPARATIVOS LDA vs TEORIA ===
if(CONFIG$gerar_graficos_comparativos && !is.null(validacao_cruzada)) {
  cat("   6.7. GRÁFICOS COMPARATIVOS: LDA vs Teoria...\n")
  
  # Heatmap de correspondência
  p_correspondencia <- validacao_cruzada %>%
    ggplot(aes(x = str_wrap(topico_teoria, 12), y = factor(topico_lda), fill = similaridade)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = round(similaridade, 2)), color = "white", fontface = "bold", size = 3) +
    scale_fill_gradient2(low = "#f8f9fa", mid = "#3498db", high = "#2c3e50", 
                        midpoint = 0.5, name = "Similaridade\n(Jaccard)") +
    labs(
      title = "Correspondência: Tópicos LDA ↔ Categorias Teóricas (Bardin)",
      subtitle = "Validação cruzada metodológica - valores mais altos = maior correspondência",
      x = "Categorias Teóricas (Bardin)",
      y = "Tópicos LDA (descobertos automaticamente)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      panel.grid = element_blank()
    )
  
  ggsave(file.path(CONFIG$dir_graficos, "correspondencia_lda_teoria.png"), 
         p_correspondencia, width = 12, height = 8, dpi = CONFIG$dpi, bg = "white")
  
  # Gráfico de barras - frequência dos tópicos teóricos
  if(exists("freq_topicos_teoria")) {
    p_freq_teoria <- freq_topicos_teoria %>%
      mutate(topico_clean = str_wrap(str_replace_all(topico_teoria_principal, "_", " "), 20)) %>%
      ggplot(aes(x = reorder(topico_clean, n), y = n)) +
      geom_col(fill = "#e74c3c", alpha = 0.8) +
      geom_text(aes(label = paste0(n, "\n(", prop, "%)")), hjust = -0.1, size = 3.5, fontface = "bold") +
      coord_flip() +
      labs(
        title = "Frequência dos Tópicos Teóricos (Bardin)",
        subtitle = paste("Aplicação manual das categorias em", sum(freq_topicos_teoria$n), "relatos"),
        x = "Tópicos Teóricos",
        y = "Número de Relatos",
        caption = "Baseado nas categorias predefinidas de diferenciação familiar"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1)
      )
    
    ggsave(file.path(CONFIG$dir_graficos, "frequencia_topicos_teoricos.png"), 
           p_freq_teoria, width = 12, height = 8, dpi = CONFIG$dpi, bg = "white")
  }
  
  # Gráfico de dispersão - similaridade vs frequência
  if(exists("melhor_correspondencia")) {
    dados_dispersao <- melhor_correspondencia %>%
      left_join(freq_topicos_teoria, by = c("topico_teoria" = "topico_teoria_principal"))
    
    p_dispersao <- dados_dispersao %>%
      ggplot(aes(x = similaridade, y = n, size = palavras_comuns)) +
      geom_point(color = "#3498db", alpha = 0.7) +
      geom_text(aes(label = str_wrap(topico_teoria, 15)), 
                vjust = -0.5, hjust = 0.5, size = 3) +
      scale_size_continuous(name = "Palavras\nComuns", range = c(3, 10)) +
      labs(
        title = "Relação: Correspondência vs Frequência dos Tópicos",
        subtitle = "Tópicos mais frequentes têm maior correspondência com LDA?",
        x = "Similaridade LDA ↔ Teoria (Jaccard)",
        y = "Frequência nos Dados (nº relatos)",
        caption = "Tamanho dos pontos = número de palavras em comum"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1)
      )
    
    ggsave(file.path(CONFIG$dir_graficos, "dispersao_similaridade_frequencia.png"), 
           p_dispersao, width = 10, height = 8, dpi = CONFIG$dpi, bg = "white")
  }
  
  cat("       ✓ Gráficos comparativos gerados\n\n")
}  
  
  # ----------------------------------------------------------------
  # 6.5. VISUALIZAÇÕES EXTRAS - WORDCLOUDS E ANÁLISES AVANÇADAS
  # ----------------------------------------------------------------
  
  if(CONFIG$gerar_graficos && nfeat(dfm_obj) > 0) {
    cat("6.5. VISUALIZAÇÕES EXTRAS: Gerando wordclouds e análises...\n")
    
    # === WORDCLOUD GERAL ===
    if(require("wordcloud", quietly = TRUE)) {
      
      # Preparar dados para wordcloud
      freq_data <- freq_palavras %>%
        filter(frequency >= 3, nchar(feature) >= 3) %>%
        head(100)
      
      if(nrow(freq_data) > 0) {
        # Wordcloud tradicional
        png(file.path(CONFIG$dir_graficos, "wordcloud_geral.png"), 
            width = 800, height = 600, bg = "white")
        
        wordcloud(words = freq_data$feature, 
                  freq = freq_data$frequency,
                  min.freq = 3,
                  max.words = 100,
                  random.order = FALSE,
                  rot.perc = 0.3,
                  colors = brewer.pal(8, "Dark2"),
                  scale = c(3, 0.5))
        
        dev.off()
        
        # Salvar dados para wordcloud interativo
        write_csv(freq_data, file.path(CONFIG$dir_resultados, "dados_wordcloud_geral.csv"))
      }
    }
    
    # === WORDCLOUDS POR CATEGORIA ===
    for(categoria in names(SISTEMA_CATEGORIAS)) {
      
      # Filtrar textos da categoria
      textos_categoria <- dados_sentimentos %>%
        filter(map_lgl(categorias, ~ categoria %in% .x)) %>%
        pull(texto_limpo)
      
      if(length(textos_categoria) >= 10) {  # Mínimo de textos para fazer wordcloud
        
        # Criar corpus específico da categoria
        corp_cat <- corpus(textos_categoria)
        tokens_cat <- tokens(corp_cat, remove_punct = TRUE, remove_symbols = TRUE) %>%
          tokens_tolower() %>%
          tokens_remove(c(stopwords("pt"), "né", "aí", "pra", "pro")) %>%
          tokens_keep(pattern = "^[a-záéíóúâêîôûãõç]{3,}$", valuetype = "regex")
        
        dfm_cat <- dfm(tokens_cat) %>%
          dfm_trim(min_docfreq = 2)
        
        if(nfeat(dfm_cat) > 5) {
          freq_cat <- textstat_frequency(dfm_cat, n = 50)
          
          # Salvar dados da categoria
          write_csv(freq_cat, file.path(CONFIG$dir_resultados, paste0("wordcloud_", categoria, ".csv")))
          
          # Wordcloud da categoria
          if(require("wordcloud", quietly = TRUE) && nrow(freq_cat) > 5) {
            
            png(file.path(CONFIG$dir_graficos, paste0("wordcloud_", categoria, ".png")), 
                width = 600, height = 400, bg = "white")
            
            wordcloud(words = freq_cat$feature, 
                      freq = freq_cat$frequency,
                      min.freq = 1,
                      max.words = 30,
                      random.order = FALSE,
                      colors = brewer.pal(6, "Set2"),
                      scale = c(2, 0.5))
            
            title(main = str_to_title(str_replace_all(categoria, "_", " ")), 
                  col.main = "darkblue", cex.main = 1.2)
            
            dev.off()
          }
        }
      }
    }
    
    # === REDE DE CO-OCORRÊNCIA ===
    if(require("igraph", quietly = TRUE) && require("ggraph", quietly = TRUE)) {
      
      # Criar matriz de co-ocorrência
      if(nfeat(dfm_obj) > 20) {
        fcm_obj <- fcm(tokens_obj, context = "window", window = 5)
        
        # Filtrar apenas palavras mais frequentes - LINHA CORRIGIDA
        top_features <- freq_palavras$feature[1:min(30, nrow(freq_palavras))]
        fcm_subset <- fcm_select(fcm_obj, pattern = top_features)
        
        # Converter para grafo
        if(sum(fcm_subset) > 0) {
          graph_obj <- graph_from_adjacency_matrix(as.matrix(fcm_subset), 
                                                   mode = "undirected", 
                                                   weighted = TRUE)
          
          # Remover vértices isolados
          graph_obj <- delete.vertices(graph_obj, degree(graph_obj) == 0)
          
          if(vcount(graph_obj) > 3) {
            # Plot da rede
            p_rede <- ggraph(graph_obj, layout = "fr") +
              geom_edge_link(aes(width = weight), alpha = 0.6, color = "gray70") +
              geom_node_point(aes(size = degree(graph_obj)), color = "steelblue", alpha = 0.8) +
              geom_node_text(aes(label = name), size = 3, repel = TRUE) +
              scale_edge_width(range = c(0.2, 2), guide = "none") +
              scale_size(range = c(3, 8), guide = "none") +
              labs(title = "Rede de Co-ocorrência de Palavras",
                   subtitle = "Palavras que aparecem frequentemente juntas") +
              theme_void() +
              theme(plot.title = element_text(hjust = 0.5),
                    plot.subtitle = element_text(hjust = 0.5))
            
            ggsave(file.path(CONFIG$dir_graficos, "rede_coocorrencia.png"), 
                   p_rede, width = 10, height = 8, dpi = CONFIG$dpi, bg = "white")
          }
        }
      }
    }
    
    # === HEATMAP CATEGORIAS vs SENTIMENTOS ===
    if(CONFIG$usar_sentimentos && !is.null(sentimento_por_categoria)) {
      
      # Preparar dados para heatmap
      heatmap_data <- sentimento_por_categoria %>%
        select(categoria_principal, prop_negativo, prop_neutro, prop_positivo) %>%
        pivot_longer(cols = starts_with("prop_"), 
                     names_to = "sentimento", 
                     values_to = "proporcao") %>%
        mutate(
          sentimento = str_remove(sentimento, "prop_"),
          categoria_clean = str_wrap(str_replace_all(categoria_principal, "_", " "), 15)
        )
      
      # Heatmap
      p_heatmap <- heatmap_data %>%
        ggplot(aes(x = sentimento, y = categoria_clean, fill = proporcao)) +
        geom_tile(color = "white", size = 0.1) +
        geom_text(aes(label = paste0(round(proporcao), "%")), 
                  color = "white", fontface = "bold", size = 3) +
        scale_fill_gradient(low = "#f8f9fa", high = "#e74c3c", 
                            name = "Proporção (%)", labels = function(x) paste0(x, "%")) +
        labs(
          title = "Heatmap: Sentimentos por Categoria",
          subtitle = "Distribuição de polaridades por tipo de diferenciação",
          x = "Polaridade do Sentimento",
          y = "Categoria Temática"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
        )
      
      ggsave(file.path(CONFIG$dir_graficos, "heatmap_sentimentos_categorias.png"), 
             p_heatmap, width = 8, height = 6, dpi = CONFIG$dpi, bg = "white")
    }
    
    # === GRÁFICO DE BARRAS EMPILHADAS CATEGORIAS ===
    p_barras <- freq_categorias %>%
      filter(categoria_principal != "sem_categoria") %>%
      mutate(
        categoria_clean = str_to_title(str_replace_all(categoria_principal, "_", " ")),
        categoria_clean = fct_reorder(categoria_clean, n)
      ) %>%
      ggplot(aes(x = categoria_clean, y = n)) +
      geom_col(aes(fill = categoria_clean), alpha = 0.8, show.legend = FALSE) +
      geom_text(aes(label = paste0(n, "\n(", prop, "%)")), 
                hjust = -0.1, size = 3.5, fontface = "bold") +
      coord_flip() +
      scale_fill_viridis_d(option = "plasma") +
      labs(
        title = "Frequência das Categorias de Diferenciação Familiar",
        subtitle = paste("Análise de", sum(freq_categorias$n), "relatos categorizados"),
        x = "Categorias Temáticas",
        y = "Número de Relatos",
        caption = "Metodologia: Análise de Conteúdo de Bardin"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1),
        panel.grid.minor = element_blank()
      )
    
    ggsave(file.path(CONFIG$dir_graficos, "categorias_barras_detalhado.png"), 
           p_barras, width = 12, height = 8, dpi = CONFIG$dpi, bg = "white")
    
    # === BOXPLOT INTENSIDADE EMOCIONAL POR CATEGORIA ===
    if(CONFIG$usar_sentimentos) {
      
      # Preparar dados para boxplot
      dados_boxplot <- dados_sentimentos %>%
        filter(categoria_principal != "sem_categoria") %>%
        mutate(categoria_clean = str_wrap(str_replace_all(categoria_principal, "_", " "), 15))
      
      if(nrow(dados_boxplot) > 0) {
        p_boxplot <- dados_boxplot %>%
          ggplot(aes(x = categoria_clean, y = sent_intensidade, fill = categoria_clean)) +
          geom_boxplot(alpha = 0.7, show.legend = FALSE) +
          geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
          scale_fill_viridis_d(option = "mako") +
          labs(
            title = "Intensidade Emocional por Categoria",
            subtitle = "Distribuição da intensidade dos sentimentos expressos",
            x = "Categoria Temática",
            y = "Intensidade Emocional",
            caption = "Valores mais altos = maior intensidade emocional"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
          )
        
        ggsave(file.path(CONFIG$dir_graficos, "boxplot_intensidade_emocional.png"), 
               p_boxplot, width = 12, height = 8, dpi = CONFIG$dpi, bg = "white")
      }
    }
    
    # === TABELA RESUMO PARA VISUALIZAÇÃO ===
    if(CONFIG$usar_sentimentos) {
      
      resumo_visual <- sentimento_por_categoria %>%
        arrange(desc(n_relatos)) %>%
        mutate(
          categoria_clean = str_to_title(str_replace_all(categoria_principal, "_", " ")),
          dominancia_negativa = prop_negativo > 50,
          categoria_rank = row_number()
        ) %>%
        select(categoria_rank, categoria_clean, n_relatos, 
               prop_negativo, prop_neutro, prop_positivo, media_score)
      
      write_csv(resumo_visual, file.path(CONFIG$dir_resultados, "resumo_categorias_visual.csv"))
    }
    
    cat("   Visualizações extras geradas:\n")
    cat("   - Wordcloud geral\n")
    cat("   - Wordclouds por categoria\n")
    if(file.exists(file.path(CONFIG$dir_graficos, "rede_coocorrencia.png"))) {
      cat("   - Rede de co-ocorrência\n")
    }
    if(CONFIG$usar_sentimentos) {
      cat("   - Heatmap sentimentos vs categorias\n")
      cat("   - Boxplot intensidade emocional\n")
    }
    cat("   - Gráfico de barras detalhado\n")
    cat("\n")
  }

# ----------------------------------------------------------------
# 6.6. DASHBOARD INTERATIVO
# ----------------------------------------------------------------

if(CONFIG$gerar_graficos) {
  cat("6.6. DASHBOARD: Gerando dashboard interativo...\n")
  
  # Preparar dados para o dashboard
  resumo_analise <- list(
    total_documentos = nrow(dados_sentimentos),
    taxa_aproveitamento = round(nrow(dados_sentimentos) / nrow(dados_brutos) * 100, 1),
    categorias_ativas = nrow(freq_categorias) - 1,  # -1 para excluir "sem_categoria"
    cobertura = round(mean(dados_sentimentos$categoria_principal != "sem_categoria") * 100, 1),
    sentimentos_negativos = if(CONFIG$usar_sentimentos) round(mean(dados_sentimentos$sent_polaridade == "negativo") * 100, 1) else 0,
    features_processadas = nfeat(dfm_obj),
    topicos_lda = k_otimo
  )
  
  # HTML do dashboard
  html_content <- paste0('
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Dashboard - Análise de Bardin</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { 
            font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh; padding: 20px;
        }
        .dashboard { 
            max-width: 1200px; margin: 0 auto; background: white;
            border-radius: 15px; box-shadow: 0 20px 40px rgba(0,0,0,0.1);
            overflow: hidden;
        }
        .header { 
            background: linear-gradient(45deg, #2c3e50, #34495e);
            color: white; padding: 30px; text-align: center;
        }
        .header h1 { font-size: 2.5em; margin-bottom: 10px; }
        .header p { font-size: 1.2em; opacity: 0.9; }
        .metrics { 
            display: grid; 
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); 
            gap: 20px; padding: 30px; background: #f8f9fa;
        }
        .metric { 
            background: white; padding: 25px; border-radius: 10px; 
            text-align: center; box-shadow: 0 5px 15px rgba(0,0,0,0.08);
            transition: transform 0.3s ease;
        }
        .metric:hover { transform: translateY(-5px); }
        .metric-value { font-size: 2.5em; font-weight: bold; margin-bottom: 10px; }
        .metric-label { 
            color: #666; font-size: 0.9em; text-transform: uppercase;
            letter-spacing: 1px;
        }
        .success { color: #27ae60; }
        .warning { color: #f39c12; }
        .footer { 
            background: #2c3e50; color: white; padding: 20px; text-align: center;
        }
    </style>
</head>
<body>
    <div class="dashboard">
        <div class="header">
            <h1>📊 Análise de Conteúdo de Bardin</h1>
            <p>Diferenciação Familiar por Gênero em Comentários do Instagram</p>
        </div>
        <div class="metrics">
            <div class="metric">
                <div class="metric-value success">', resumo_analise$total_documentos, '</div>
                <div class="metric-label">Documentos Analisados</div>
            </div>
            <div class="metric">
                <div class="metric-value">', resumo_analise$taxa_aproveitamento, '%</div>
                <div class="metric-label">Taxa de Aproveitamento</div>
            </div>
            <div class="metric">
                <div class="metric-value success">', resumo_analise$categorias_ativas, '</div>
                <div class="metric-label">Categorias Ativas</div>
            </div>
            <div class="metric">
                <div class="metric-value">', resumo_analise$cobertura, '%</div>
                <div class="metric-label">Cobertura Categorização</div>
            </div>
            <div class="metric">
                <div class="metric-value warning">', resumo_analise$sentimentos_negativos, '%</div>
                <div class="metric-label">Sentimentos Negativos</div>
            </div>
            <div class="metric">
                <div class="metric-value">', resumo_analise$features_processadas, '</div>
                <div class="metric-label">Features Processadas</div>
            </div>
        </div>
        <div class="footer">
            <div>Análise gerada em: ', format(Sys.time(), "%d/%m/%Y às %H:%M"), '</div>
        </div>
    </div>
</body>
</html>')
  
  # Salvar dashboard
  writeLines(html_content, file.path(CONFIG$dir_resultados, "dashboard_interativo.html"))
  cat("   ✓ Dashboard interativo salvo: dashboard_interativo.html\n\n")
}
  
  # ----------------------------------------------------------------
  # 7. COMPILAR E SALVAR RESULTADOS
  # ----------------------------------------------------------------
  
  cat("7. RESULTADOS: Compilando e salvando...\n")
  
  # Estatísticas finais
  estatisticas <- list(
    corpus = list(
      total_original = nrow(dados_brutos),
      total_analisado = nrow(dados_sentimentos),
      taxa_aproveitamento = round(nrow(dados_sentimentos) / nrow(dados_brutos) * 100, 2),
      media_palavras = round(mean(dados_sentimentos$n_palavras, na.rm = TRUE), 2)
    ),
    categorizacao = list(
      categorias_identificadas = nrow(freq_categorias),
      categorias_ativas = sum(freq_categorias$n >= 5),
      cobertura = round(mean(dados_sentimentos$categoria_principal != "sem_categoria") * 100, 2),
      categoria_principal = freq_categorias$categoria_principal[1],
      freq_principal = freq_categorias$n[1],
      exclusao_mutua = round(metricas_qualidade$exclusao_mutua * 100, 2)
    ),
    processamento = list(
      features_dfm = nfeat(dfm_obj),
      sparsity = round(sparsity(dfm_obj), 4),
      k_topicos = k_otimo
    )
  )
  
  if(CONFIG$usar_sentimentos && !is.null(dist_sentimentos)) {
    neg_prop <- dist_sentimentos$prop[dist_sentimentos$sent_polaridade == "negativo"]
    neu_prop <- dist_sentimentos$prop[dist_sentimentos$sent_polaridade == "neutro"]  
    pos_prop <- dist_sentimentos$prop[dist_sentimentos$sent_polaridade == "positivo"]
    
    estatisticas$sentimentos <- list(
      prop_negativo = if(length(neg_prop) > 0) neg_prop else 0,
      prop_neutro = if(length(neu_prop) > 0) neu_prop else 0,
      prop_positivo = if(length(pos_prop) > 0) pos_prop else 0,
      score_medio = round(mean(dados_sentimentos$sent_score, na.rm = TRUE), 3)
    )
  }
  
  # Resultados principais
  resultados <- list(
    config = CONFIG,
    estatisticas = estatisticas,
    dados_processados = dados_sentimentos,
    freq_categorias = freq_categorias,
    freq_palavras = freq_palavras,
    sistema_categorias = SISTEMA_CATEGORIAS,
    lexicon_sentimentos = LEXICON_PT,
    dfm = dfm_obj,
    lda_model = lda_model,
    topics_terms = topics_terms,
    metricas_qualidade = metricas_qualidade
  )
  
  if(CONFIG$usar_sentimentos) {
    resultados$dist_sentimentos <- dist_sentimentos
    resultados$sentimento_por_categoria <- sentimento_por_categoria
  }
  
  # Salvar arquivos
  salvar_resultado(resultados, "analise_bardin_completa", "rds")
  salvar_resultado(dados_sentimentos, "dados_categorizados", "csv")
  salvar_resultado(freq_categorias, "frequencia_categorias", "csv")
  salvar_resultado(estatisticas, "estatisticas_resumo", "json")
  salvar_resultado(metricas_qualidade, "metricas_qualidade", "json")
  
  if(CONFIG$usar_sentimentos && !is.null(sentimento_por_categoria)) {
    salvar_resultado(sentimento_por_categoria, "sentimentos_por_categoria", "csv")
  }
  
  cat("\n=== ANÁLISE CONCLUÍDA ===\n")
  cat("Documentos analisados:", estatisticas$corpus$total_analisado, "\n")
  cat("Taxa de aproveitamento:", estatisticas$corpus$taxa_aproveitamento, "%\n")
  cat("Categorias ativas:", estatisticas$categorizacao$categorias_ativas, "\n")
  cat("Cobertura categorização:", estatisticas$categorizacao$cobertura, "%\n")
  cat("Exclusão mútua:", estatisticas$categorizacao$exclusao_mutua, "%\n")
  
  if(CONFIG$usar_sentimentos && !is.null(estatisticas$sentimentos)) {
    cat("Sentimentos negativos:", estatisticas$sentimentos$prop_negativo, "%\n")
  }
  
  cat("Features processadas:", estatisticas$processamento$features_dfm, "\n")
  cat("Tópicos LDA:", estatisticas$processamento$k_topicos, "\n")
  
  return(resultados)
}

# ====================================================================
# EXECUTAR ANÁLISE
# ====================================================================

if(!interactive() || !exists("SKIP_AUTO_RUN")) {
  resultados_finais <- executar_analise_bardin()
  cat("\n✓ Para acessar os resultados: resultados_finais\n")
  cat("✓ Para reexecutar: resultados_finais <- executar_analise_bardin()\n")
}
