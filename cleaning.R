library(dplyr)
library(magrittr)
library(rslp)
library(stringr)
library(tm)

#################################
## Limpeza de textos juridicos ##
#################################

pt_adverbios <-  c("acaso", "acinte", 
                   "adiante", "adrede",
                   "afinal", "afora", "agora",
                   "algures", "alem", "ali", 
                   "amanha", "antes", "aqui", "assim", "atras",
                   "bem", "breve",
                   "cedo", "certamente",
                   "debalde", "depois",
                   "efetivamente", "enfim",
                   "hoje",
                   "mal", "mais", "melhor", "menos", "muito",
                   "nao",
                   "ontem", 
                   "pior", "pouco",
                   "quanto", "quao", "quase",
                   "realmente",
                   "sera", "sim", 
                   "talvez", "tanto")
pt_conjuncoes <- c("e", "nem", "mas", "tambem", "como", "bem", "porem",
                   "todavia", "contudo", "entretanto", "entanto", "ou",
                   "ora", "quer", "ja", "logo", "portanto", "por", "assim",
                   "conseguinte", "que", "porque", "porquanto", "pois",
                   "sendo", "visto", "como", "tal", "tao", "tanto", 
                   "assim", "conforme", "segundo", "consoante", "mesmo",
                   "mais", "ainda", "se", "bem", "embora", "se", "caso",
                   "contanto", "salvo", "medida", "quanto", "fim",
                   "quando", "enquanto", "sempre", "depois")
pt_direito     <- c("acao", "acolhimento", "acordao",
                    "advocaticio", "advogado", 
                    "agravo", "alcada", "alegacao", 
                    "apelo", "apelacao", "apenso",
                    "aplicacao",
                    "artigo", "autos", "autor", "ato",
                    "causa", "camara", "civel", "civil", 
                    "codigo", "comarca", "comprovacao", "condenacao",
                    "dano", "data",
                    "decisao", "declaracao", "decorrente", 
                    "defesa", "dever", "desembargador", "devolucao",
                    "diante", "direito",
                    "embargo", "ementa", "estado", "exposto", 
                    "fato", "fundamento",
                    "honorarios", "inicial",
                    "improcedencia", "improcedente", "improvimento", 
                    "indevido",
                    "instancia", "instrumento",
                    "interposto",
                    "judiciario", 
                    "juiz", "julgamento", 
                    "juridico", "jurisprudencia", "juros", 
                    "justica",
                    "lei", "lide", 
                    "materia", "materialmente", "merito", 
                    "monocratico", "mora",
                    "nome",
                    "objeto",
                    "parcial", "parte", "passivo",
                    "pedido", "peticao",
                    "pleitear", "poder",
                    "prejuizo", "preposto", "presidente",
                    "pretensao", "previsto",
                    "procedencia", "procedente", 
                    "processo", "processual",
                    "provido", "provimento",
                    "razao",
                    "re", "recurso", 
                    "relator", "relatorio", 
                    "reu", "revisao",
                    "sentenca", "sucumbencia", 
                    "tribunal", "turma", 
                    "unanime", "valor", "vara", "vitima")
pt_direito_abr <- c("art", "cpc", "fls", "n", "tjsp")
pt_lucros      <- c("cessantes",
                    "ilicito",
                    "indenizacao",  "indenizatoria", 
                    "lucros", 
                    "reparar", "reparacao", 
                    "responsabilidade", "ressarcimento")
pt_preposicoes <- c("a", "ante", "apos", "ate", "com", "contra", "de", "desde",
                    "para", "per", "perante", "por", "sem", "sob", "sobre", "tras")
pt_pronomes    <- c("algo", "alguem", "algum", "alguns",
                    "cada", "cujo", 
                    "muitos",
                    "nada", "nenhum", "nenhuns", "ninguem",
                    "outrem", "outros",
                    "poucos",
                    "quaisquer", "qualquer", "quantos", "quem",
                    "tantos", "todos", "tudo",
                    "varios")
pt_meses       <- c("janeiro", "fevereiro", "abril", "maio", "junho", "julho",
                    "agosto", "setembro", "outubro", "novembro", "dezembro")
pt_numerais    <- c("primeira", "segunda", "terceira", "quarta", "quinta",
                    "sexta", "setima", "oitava", "nona", "decima")
pt_verbos      <- c("acordar", "afastar", "ajuizar", "alegar", 
                    "antecipar", "aplicar", "apresentar", "argumentar",
                    "caber", "caracterizar",
                    "comprovar", 
                    "condenar", "configurar", "conhecer", "considerar",
                    "dar", "demonstrar", "determinar", "discutir", "dizer",
                    "entender", "ficar", "fixar", 
                    "julgar", "juntar",
                    "indeferir", "integrar",
                    "manter", "negar", "observar",
                    "preparar", "pretender",
                    "realizar", "reconhecer", "recorrer", "reformar",
                    "registrar", "rejeitar", 
                    "relatar", "representar", "requerer", "revisar",
                    "sustentar", "tender", "tratar", "votar")
banned_words <- c(pt_adverbios,
                  pt_conjuncoes,
                  pt_direito,
                  pt_direito_abr,
                  pt_preposicoes,
                  pt_pronomes,
                  pt_meses,
                  pt_numerais,
                  pt_verbos,
                  stopwords("portuguese"))
banned_words_stem <- stemDocument(banned_words, language="portuguese")

limpa_juridico <- function(text)
{
  text %<>% gsub("[\f,\n,\r]", " ", .) %>%
            tolower %>%
            iconv(from="UTF-8", to="ASCII//TRANSLIT") %>%
            gsub("[[:digit:][:punct:]]", " ", .) %>%
            stripWhitespace %>%
            strsplit(" ") %>%
            unlist %>%
            stemDocument(language="pt") %>%
            paste(collapse=" ") %>%
            removeWords(banned_words_stem) %>%
            stripWhitespace
  return(text)
}

###################
## Limpeza do BD ##
###################
setwd("D:/Dropbox_Stern/Dropbox/papers/brunoSalama/code")
load("../data-raw/d_final.rda")
decisoes <- d_final$txt
indices <- !is.na(decisoes)
decisoes <- decisoes[indices]
decisoes_clean <- decisoes %>% 
                  plyr::llply(limpa_juridico, .progress="text") %>%
                  unlist
decisoes_corpus <- decisoes_clean %>% VectorSource %>%VCorpus
decisoes_tm <- decisoes_corpus %>% DocumentTermMatrix
save(decisoes_tm, file="../data-raw/decisoes_tm.rda")
colnames(decisoes_tm)[1:20]
