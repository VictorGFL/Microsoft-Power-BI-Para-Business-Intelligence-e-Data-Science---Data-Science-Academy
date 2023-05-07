#Verifica a pasta de trabalho
	getwd()

# Instala os pacotes
	install.packages("tidyverse")
	install.packages("dplyr")
	install.packages("solitude")
	install.packages("ggplot2")
	install.packages("readr")

# Carrega os pacotes nesta sessão R
	library(tidyverse)
	library(dplyr)
	library(solitude)
	library(ggplot2)
	library(readr)

# Carrega os dados históricos
	dados_historicos <- read_csv("dados_historicos.csv")
	View(dados_historicos)

# Cria o modelo de Machine Learning com algoritmo Isolation Forest
	modelo_mL = isolationForest$new() 

# Treina o modelo
	modelo_ml$fit(dados_historicos)

# Faz as previsões com o modelo usando os dados históricos
	previsoes_historico = dados_historicos %>%
 	modelo_ml$predict() %>%
  	arrange(desc(anomaly_score))

# Density Plot 
	plot(density(previsoes_historico$anomaly_score))

# Vamos definir como regra que anomaly score acima de 0.62 é uma anomalia
	indices_historico = previsoes_historico[which(previsoes_historico$anomaly_score > 0.62)]

# Faz o filtro
	anomalias_historico = dados_historicos[indices_historico$id, ]
	normais_historico = dados_historicos[-indices_historico$id, ]

# Gráfico
	colors()
	ggplot() + 
 	 geom_point(data = normais_historico, 
             mapping = aes(transacao1,transacao2), 
             col = "skyblue3", 
             alpha = 0.5) + 
 	 geom_point(data = anomalias_historico,
             mapping = aes(transacao1,transacao2), 
             col = "red2", 
             alpha = 0.8)

# Agora carregamos novos dados
	novos_dados <- read.csv("novos_dados.csv")

# Previsões com o modelo treinado
	previsoes_novos_dados = modelo_ml$predict(novos_dados)

# Se o anomaly score é maior que 0.62 consideramos como anomalia
	indices_novos_dados = previsoes_novos_dados[which(previsoes_novos_dados$anomaly_score > 0.62)]

# Filtro
	anomalias_novos_dados = novos_dados[indices_novos_dados$id, ]
	normais_novos_dados = novos_dados[-indices_novos_dados$id, ]

# Gráfico das previsões
	ggplot() + 
  	geom_point(data = normais_novos_dados, 
             mapping = aes(transacao1,transacao2), 
             col = "turquoise3", 
             alpha = 0.5) + 
  	geom_point(data = anomalias_novos_dados, 
             mapping = aes(transacao1,transacao2), 
             col = "tomato3", 
             alpha = 0.8)

# Arredondando a coluna 'anomaly_score' para 2 casas decimais
	previsoes_novos_dados <- previsoes_novos_dados %>%
  	mutate(anomaly_score = round(anomaly_score, 2))

# Criando uma nova coluna com base na condição
	previsoes_novos_dados <- previsoes_novos_dados %>%
  	mutate(status = ifelse(anomaly_score > 0.62, "anomalia", "normal"))

# Criando o box plot
	ggplot(previsoes_novos_dados, aes(x = status, y = anomaly_score, fill = status)) +
  	geom_boxplot() +
  	labs(title = "Box Plot de Anomalias e Normais",
       x = "Status",
       y = "Anomaly Score") +
  	theme_minimal() +
  	scale_fill_manual(values = c("anomalia" = "red", "normal" = "blue")) +
  	theme(legend.position = "none")

# Salva em CSV
	write.csv(previsoes_novos_dados, "previsoes_novos_dados.csv")


