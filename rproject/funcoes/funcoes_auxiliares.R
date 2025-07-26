# ====================================================================
# FUNCOES_AUXILIARES.R
# Funções customizadas para análise de Bardin
# ====================================================================

# Função para limpeza básica de texto
clean_text_basic <- function(text) {
  text %>%
    # Remover URLs
    str_remove_all("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+") %>%
    # Remover menções @
    str_remove_all("@[A-Za-z0-9_]+") %>%
    # Remover hashtags #
    str_remove_all("#[A-Za-z0-9_]+") %>%
    # Normalizar espaços múltiplos
    str_squish() %>%
    # Remover linhas vazias
    str_trim()
}

# Função para detectar temas baseado em indicadores
detect_theme <- function(text, indicators) {
  if(length(indicators) == 0) return(FALSE)
  
  # Criar padrão regex
  pattern <- paste(indicators, collapse = "|")
  
  # Detectar se algum indicador está presente
  str_detect(text, pattern)
}

# Função para contar marcadores emocionais
count_emotional_markers <- function(text) {
  # Marcadores de intensidade emocional
  markers <- c(
    "!!!", "!!!", "muito", "sempre", "nunca", "todos", "nada", 
    "completamente", "totalmente", "extremamente", "super", "mega"
  )
  
  text_lower <- tolower(text)
  
  # Contar ocorrências
  sum(sapply(markers, function(m) str_count(text_lower, m)), na.rm = TRUE)
}

# Função para normalizar termos familiares
normalize_family_terms <- function(text) {
  text %>%
    str_replace_all("(?i)\\bmãe|mae|mamae\\b", "mãe") %>%
    str_replace_all("(?i)\\bpai|papai\\b", "pai") %>%
    str_replace_all("(?i)\\birmao|irmão\\b", "irmão") %>%
    str_replace_all("(?i)\\birma|irmã\\b", "irmã") %>%
    str_replace_all("(?i)\\bfamilia|família\\b", "família")
}

# Função para extrair bigramas relevantes
extract_relevant_bigrams <- function(tokens_obj, min_freq = 3) {
  # Criar bigramas
  bigrams <- tokens_ngrams(tokens_obj, n = 2)
  
  # Converter para DFM e filtrar
  dfm_bigrams <- dfm(bigrams) %>%
    dfm_trim(min_docfreq = min_freq)
  
  # Extrair frequências
  textstat_frequency(dfm_bigrams, n = 50)
}

# Função para calcular diversidade lexical
calculate_lexical_diversity <- function(tokens_obj) {
  # TTR (Type-Token Ratio)
  ttr <- ntype(tokens_obj) / ntoken(tokens_obj)
  
  # MSTTR (Mean Segmental TTR) - média de TTR em segmentos de 100 palavras
  if(ntoken(tokens_obj) >= 100) {
    segments <- split(as.character(tokens_obj), 
                     ceiling(seq_along(as.character(tokens_obj))/100))
    msttr <- mean(sapply(segments, function(seg) {
      length(unique(seg)) / length(seg)
    }), na.rm = TRUE)
  } else {
    msttr <- ttr
  }
  
  return(list(
    ttr = round(ttr, 4),
    msttr = round(msttr, 4),
    n_types = ntype(tokens_obj),
    n_tokens = ntoken(tokens_obj)
  ))
}

# Função para criar matriz de co-ocorrência personalizada
create_coocurrence_matrix <- function(dfm_obj, window_size = 5) {
  # Converter DFM para tokens se necessário
  if(class(dfm_obj)[1] == "dfm") {
    # Reconstruir tokens a partir do DFM (aproximação)
    features <- featnames(dfm_obj)
    
    # Criar matriz de co-ocorrência baseada na DFM
    cooc_matrix <- t(dfm_obj) %*% dfm_obj
    
    # Remover diagonal (co-ocorrência de palavra consigo mesma)
    diag(cooc_matrix) <- 0
    
    return(cooc_matrix)
  }
  
  # Se já são tokens, usar FCM
  fcm(dfm_obj, context = "window", window = window_size)
}

# Função para detectar emoções específicas no contexto
detect_context_emotions <- function(text) {
  emotions <- list(
    raiva = c("raiva", "revolta", "ódio", "irritação", "irritada", "puta", "revoltada"),
    tristeza = c("triste", "tristeza", "chateada", "magoada", "deprimida", "melancólica"),
    injustica = c("injusto", "injustiça", "unfair", "desigual", "discriminação"),
    resignacao = c("conformada", "aceitei", "desisti", "normal", "assim mesmo"),
    resistencia = c("briguei", "contestei", "questionei", "enfrentei", "resisti")
  )
  
  text_lower <- tolower(text)
  
  detected <- sapply(emotions, function(emotion_words) {
    any(sapply(emotion_words, function(word) str_detect(text_lower, word)))
  })
  
  return(names(detected[detected == TRUE]))
}

# Função para extrair exemplos representativos por categoria
extract_representative_examples <- function(data, category_col, text_col, n_examples = 3) {
  data %>%
    group_by(!!sym(category_col)) %>%
    # Priorizar comentários com mais likes e maior diversidade lexical
    arrange(desc(likes), desc(nchar(!!sym(text_col)))) %>%
    slice_head(n = n_examples) %>%
    select(!!sym(category_col), !!sym(text_col), likes) %>%
    ungroup()
}

# Função para calcular métricas de qualidade da categorização
calculate_categorization_quality <- function(categorization_results) {
  # Exclusão mútua (documentos com apenas uma categoria)
  exclusao_mutua <- mean(categorization_results$n_categorias == 1, na.rm = TRUE)
  
  # Cobertura (documentos categorizados)
  cobertura <- mean(categorization_results$categoria_principal != "sem_categoria", na.rm = TRUE)
  
  # Distribuição equilibrada (coeficiente de variação)
  freq_cats <- table(categorization_results$categoria_principal)
  freq_cats <- freq_cats[names(freq_cats) != "sem_categoria"]
  cv_distribuicao <- sd(freq_cats) / mean(freq_cats)
  
  # Número de categorias ativas (com pelo menos 5 documentos)
  categorias_ativas <- sum(freq_cats >= 5)
  
  return(list(
    exclusao_mutua = round(exclusao_mutua, 3),
    cobertura = round(cobertura, 3),
    cv_distribuicao = round(cv_distribuicao, 3),
    categorias_ativas = categorias_ativas,
    total_categorias = length(freq_cats)
  ))
}

# Função para preparar dados para topic modeling
prepare_for_topic_modeling <- function(dfm_obj, min_docfreq = 3, max_docfreq = 0.9) {
  # Filtrar termos muito raros ou muito frequentes
  dfm_filtered <- dfm_obj %>%
    dfm_trim(min_docfreq = min_docfreq, 
             max_docfreq = max_docfreq, 
             docfreq_type = "prop") %>%
    dfm_remove(pattern = c(stopwords("pt"), "né", "aí", "pra", "pro"))
  
  # Remover documentos vazios
  dfm_filtered <- dfm_subset(dfm_filtered, ntoken(dfm_filtered) > 0)
  
  return(dfm_filtered)
}

# Função para criar relatório automático de uma etapa
create_stage_report <- function(stage_name, results_list, output_file = NULL) {
  if(is.null(output_file)) {
    output_file <- paste0("relatorio_", tolower(gsub(" ", "_", stage_name)), ".txt")
  }
  
  report_content <- c(
    paste("=== RELATÓRIO:", toupper(stage_name), "==="),
    paste("Data/Hora:", Sys.time()),
    "",
    "RESULTADOS PRINCIPAIS:",
    ""
  )
  
  # Adicionar resultados se fornecidos
  for(name in names(results_list)) {
    if(is.numeric(results_list[[name]]) && length(results_list[[name]]) == 1) {
      report_content <- c(report_content, paste("-", name, ":", results_list[[name]]))
    } else if(is.character(results_list[[name]]) && length(results_list[[name]]) == 1) {
      report_content <- c(report_content, paste("-", name, ":", results_list[[name]]))
    }
  }
  
  report_content <- c(report_content, "", "=== FIM DO RELATÓRIO ===")
  
  writeLines(report_content, output_file)
  
  cat("Relatório salvo em:", output_file, "\n")
}

# Função para validar estrutura de dados
validate_data_structure <- function(data, required_columns) {
  missing_cols <- setdiff(required_columns, names(data))
  
  if(length(missing_cols) > 0) {
    stop("Colunas obrigatórias ausentes: ", paste(missing_cols, collapse = ", "))
  }
  
  cat("Estrutura de dados validada. Colunas presentes:", paste(names(data), collapse = ", "), "\n")
  
  return(TRUE)
}

cat("Funções auxiliares carregadas com sucesso!\n")
cat("Funções disponíveis:\n")
cat("- clean_text_basic()\n")
cat("- detect_theme()\n") 
cat("- count_emotional_markers()\n")
cat("- normalize_family_terms()\n")
cat("- extract_relevant_bigrams()\n")
cat("- calculate_lexical_diversity()\n")
cat("- create_coocurrence_matrix()\n")
cat("- detect_context_emotions()\n")
cat("- extract_representative_examples()\n")
cat("- calculate_categorization_quality()\n")
cat("- prepare_for_topic_modeling()\n")
cat("- create_stage_report()\n")
cat("- validate_data_structure()\n")

calcular_similaridade_topicos <- function(topics_lda, categorias_bardin) {
  
  resultados <- data.frame(
    topico_lda = numeric(),
    categoria_bardin = character(),
    similaridade = numeric(),
    palavras_comuns = character()
  )
  
  for(i in 1:max(topics_lda$topic)) {
    # Palavras do tópico LDA
    palavras_topico <- topics_lda %>%
      filter(topic == i) %>%
      pull(term)
    
    # Comparar com cada categoria de Bardin
    for(cat_nome in names(categorias_bardin)) {
      palavras_categoria <- categorias_bardin[[cat_nome]]
      
      # Calcular interseção
      palavras_comuns <- intersect(palavras_topico, palavras_categoria)
      similaridade <- length(palavras_comuns) / length(union(palavras_topico, palavras_categoria))
      
      resultados <- rbind(resultados, data.frame(
        topico_lda = i,
        categoria_bardin = cat_nome,
        similaridade = similaridade,
        palavras_comuns = paste(palavras_comuns, collapse = ", ")
      ))
    }
  }
  
  return(resultados)
}

# Função para criar matriz de correspondência
criar_matriz_correspondencia <- function(similaridades) {
  
  matriz <- similaridades %>%
    select(topico_lda, categoria_bardin, similaridade) %>%
    pivot_wider(names_from = categoria_bardin, values_from = similaridade, values_fill = 0)
  
  return(matriz)
}
