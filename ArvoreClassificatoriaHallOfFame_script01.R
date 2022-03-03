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

# 5) Assim criamos quatro conjuntos de dados e precisaremos fazer o cruzamento das 
# informacoes em: 

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
                      somaSV + gg+ cyya, 
                      data=candidatos,
                      method = 'class') # esse parametro é para indicar que é 
                                        # uma Classification Tree.
prp(model)

# O comando abaixo nos permite analisar a quantidade de observações por nó folha existente 
# no modelo gerado.
rpart.plot(model,type = 3)

# Como podemos observar a árvore resultante está um pouco complicada de 
# interpretar e visualizar, podemos concluir que esta árvovre está 
# "overfitted" (muito ajustada). Para evitar o excesso de ajuste podemos 
# alterar os HIPERPARÂMETROS: tamanho do minbucket, minsplit ou maxdepth.



##############################################################
# Construindo uma Árvore de Regressão

# Para esta construção usaremos o dataset chamado Hitters do pacote ISLR, que pode ser 
# obtido em: https://cran.r-project.org/web/packages/ISLR/index.html.

dim(Hitters)
Hitters %>% head

# Para construir essa árvore de regressão usarei as variáveis preditoras HomeRuns (HmRun)
# e Anos Jogados "Years" para prever o SALÁRIO de um determinado jogador.

# para isso usaremos somente os pacotes:
# 1- rpart:      para ajustar a biblioteca de árvores de decisão;
# 2- rpart.plot: para plotar árvores de decisão;

# PASSO -1) Primeiro construiremos uma grande árvore de regressão inicial. Podemos 
# garantir que essa árvores seja grande o suficiente atribuindo um valor pequeno para 
# o hiperparâmetro CP "complexity parameter".

# construção da árvore:
tree <- rpart::rpart(Salary ~ Years + HmRun, data=Hitters, 
                     control = rpart.control(cp = .0001))

# impressão dos resultados obtidos.
printcp(tree)

# Plotando a árvore recém criada:
paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(tree,
                       box.palette = paleta) # Paleta de cores

# Como resultado temos uma árvore com 19 Folhas, temos que reduzir esse overfitting.
# Para isso temos que encontrar o valor ideal para o parâmetro CP "complexity parameter".
# Sendo assim o valor ideal para o CP é aquele que leva ao menor xerror da saída.

#tree$cptable

valorIdeal_cp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
valorIdeal_cp

# produz uma árvore de regressão podada usando o melhor para CP com base no menor 
# valor de xerror.
pruned_tree <- prune(tree, cp = valorIdeal_cp)

#plotando a árvore resultante.
prp(pruned_tree, 
    faclen = 0,    # use nomes completos para rótulos de fatores
    extra = 1,     # exibe o numero de observacoes para cada nó terminal
    roundint = F,  # não arredonda para inteiros   
    digits = 5 )   #exibe 5 casas decimais na saída


# Como pode ser o observado na plotagem da árvore, temos 6 nós terminais, onde cada 
# mostra o salário previsto do jogador naquele nó junto com o número de observações 
# do conjunto de dados original que pertencem a essa nota.

# Agora podemos usar a árvore resultante para fazer PREVISÕES:

# 1-) Podemos ver no conjunto primeiro nó terminal de dados originado que há 
# 90 jogadores com menos de 4,5 anos de experiência e seu salário médio é era
# de US$ 225.83 mil.

# 2-) Podemos observar também que um jogador com 7 anos de experiência e com 
# uma média de 4 home runs tem um salário PREVISTO de US$ 502.81k

# Nós podemos usar a função predict do R para confirmar isto:
df_prova_real <- data.frame(Years = 7, HmRun=4)

# Usando a árvore podada para predizer o salário desse jogador.
predict(pruned_tree, newdata = df_prova_real)

