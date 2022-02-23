library(caTools)
library(dplyr)
library(rpart)
library(rpart.plot)
require(Lahman)
require(maptree)
require(rpart)

# 1) - Construção da Árvore de decisão classificatória:
# Utilizaremos a base HallOfFame para sumarizar os jogadores com indicações e nomeações 
# para o HallOfFame criando um índice de performance.
# Analise das Bases de dados que utilizaremos:

# 1- HallOfFame;
# 2- Batting;
# 3- Pitching;
# 4- AwardsPlayers;  

# Obs: Dados coletados em https://cran.r-project.org/web/packages/Lahman/index.html

# Analisando as bases de dados que usarei para geração do modelo.

str(HallOfFame)
str(Batting)
str(Pitching)
str(AwardsPlayers)

# 1-) Aplicando técnicas de Data Wrangling para ajustar a base de dados HallOfFame para encontrar os 
# indicados para o Hall Of Fame.

# 1.1-) Agrupamos as informações da base HallOfFame por "playerID"e aplicamos o filtro para manter somente os 
# jogadores votados pela BBWAA e pela Special Election.
indicados_hall_of_fame <- HallOfFame %>% 
                            group_by(playerID) %>% 
                            filter(votedBy %in% c("BBWAA", "Special Election") & category == "Player")
head(indicados_hall_of_fame, 5)

# 1.2-) Sumarizamos as informações a partir os ajustes realizados anteriormente.
indicados_hall_of_fame <- indicados_hall_of_fame %>% 
                              summarise(anosEmVotacao = n(), 
                                        indicado = sum(inducted == "Y"), 
                                        melhor = max(votes/ballots))
head(indicados_hall_of_fame, 5)

# 1.3-) Classifica a base indicados_hall_of_fame por ordem descendente a partir dos 
# valores existentes na coluna "melhor"
indicados_hall_of_fame <- indicados_hall_of_fame %>% arrange(desc(melhor))
head(indicados_hall_of_fame,5)

# 2) Para treinar nosso modelo, precisamos de um conjunto de variáveis exploratórias 
# com os índices de performance durante os jogos dos candidatos. 
# Neste caso, incluiremos apenas duas estatísticas para rebatedores(batting): Hits (H) e Home runs (HR).

# 2.1-) Agrupando os dados da base Batting por playerID, sumarizando os dados por Hist e Home Runs.
rebatedores_hall_of_fame <- Batting %>% 
                              group_by(playerID) %>% 
                              summarise(numTemporadas = length(unique(yearID)), 
                                        ultimaTemporada = max(yearID),
                                        somaH = sum(H), 
                                        somaHR = sum(HR))
                                        
head(rebatedores_hall_of_fame, 5)

# 2.2-) Classifica a base "rebatedores_hall_of_fame" por ordem decrescente usando as 
# observacoes da variavel "somaH".
rebatedores_hall_of_fame <- rebatedores_hall_of_fame %>% arrange(desc(somaH))
head(rebatedores_hall_of_fame,5)

# 3) Para arremessadores(pitching), incluiremos as seguintes estatísticas: 
# Wins(W), Shutout (SO) e Save (SV)

# 3.1) Agrupamos a base de dados Pitching "Arremessadores" por playerID e aplicamos summarize 
# para as observacoes Wins(W), Shutout (SO) e Save (SV).
arremessadores_hall_of_fame <- Pitching %>% 
                                group_by(playerID) %>% 
                                summarize(numTemporadas = length(unique(yearID)), 
                                          ultimaTemporada = max(yearID), 
                                          somaW = sum(W),
                                          somaSO = sum(SO),
                                          somaSV = sum(SV))
head(arremessadores_hall_of_fame, 5)

# 3.3) Fazendo a ordenacao decrescente da base Pitching "Arremessadores" por Wins "somaW" 
arremessadores_hall_of_fame <- arremessadores_hall_of_fame %>% arrange(desc(somaW))
head(arremessadores_hall_of_fame, 5)

# 4) Incluiremos as principais premiações individuais da MLB que são: 
# MVP, Cy Young e Gold Glove, que estão contidas na base AwardsPlayers "Jogadores Premiados".

# 4.1) Agrupamos a base AwardsPlayers por playerID e sumarizamos os dados pelas premiacoes:
# MVP, Cy Young e Gold Glove.
premiados_hall_of_fame <- AwardsPlayers %>% 
                            group_by(playerID) %>% 
                            summarize(mvp = sum(awardID == "Most Valuable Player"), 
                                      gg = sum(awardID == "Gold Glove"), 
                                      cyya = sum(awardID == "Cy Young Award"))
head(premiados_hall_of_fame, 5)

# 5) Assim criamos quatro conjuntos de dados e precisaremos fazer o cruzamento das informacoes em: 

# * indicados_hall_of_fame  
# * rebatedores_hall_of_fame
# * arremessadores_hall_of_fame
# * premiados_hall_of_fame

# REGRA IMPORTANTE:
# Lembrando que nesta analise queremos apenas jogadores que já foram nomeados candidatos 
# para o Hall Da Fama portanto devemos unir os dados de acordo com essa regra.

candidatos <- merge(x=rebatedores_hall_of_fame, y=arremessadores_hall_of_fame, by="playerID", all=TRUE)
candidatos <- merge(x=candidatos, y=premiados_hall_of_fame, by="playerID", all.x=TRUE)
candidatos <- merge(x=candidatos, y=indicados_hall_of_fame, by="playerID")
head(candidatos, 5)

# Quando aplicamos o comando "head(candidatos, 5)", identificamos que existe na nossa base 
# uma serie de NAs, para solucionar esse problema substituiremos os NAs por 0 e assim 
# conseguiremos rodar nosso algoritimo sem problemas.
candidatos[is.na(candidatos)] <- 0

# Então podemos ver nossa base pronta para a aplicação do rpart.
head(candidatos, 5)

# Para criar a árvores de classificação em R utilizei o pacote rpart. 
# A função rpart criará uma árvore de decisão com uma expressão de entrada e 
# a base de dados que utilizaremos para criar o modelo.

model <- rpart::rpart(as.factor(indicado) ~ somaH + somaHR + mvp + somaW + somaSO + 
                 somaSV + gg+ cyya, data=candidatos)
prp(model)

# O comando abaixo nos permite analisar a quantidade de observações por nó folha existente 
# no modelo gerado.
rpart.plot(model,type = 3)

# Como podemos observar a árvore resultante está um pouco complicada de 
# interpretar e visualizar, podemos concluir que esta árvovre está 
# "overfitted" (muito ajustada). Para evitar o excesso de ajuste podemos 
# alterar o tamanho do minbucket, minsplit ou maxdepth.








