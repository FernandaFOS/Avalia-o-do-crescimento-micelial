atm <- proc.time()

# Comando que tem a funçãoo de instalar o pacote EBImage
require(EBImage)
k=0
j=0
i=0
image=list()
valores=list()
area=matrix()
raiom=matrix()
dia=matrix()
rep=matrix()

Lado do quadrado da área conhecida, 2 cm
ld=2

# Pasta onde está localizada as fotos para análise
setwd("C:/Users/Usu?rio/Desktop/Projeto R/Imagem Antracnose") 

# Fator de correção gama
g=3 

# Nova dimensão da imagem
rd = 800

# Tolerância da imagem
tol=0.2

# Número de dias do experimento
n=7 

# Números de repetições do experimento
r=1 
for (j in 1:r) # Loop  do experimento
{
  k=k+1
  dia[k]=0
  
  # Tamanho do micélio repicado no primeiro dia
  raiom[k]=0.5  
  area[k]=(pi*(0.5^2))/4
  rep[k]=j
  
  # i= número de dias que foi medido o mic?lio da col?nia do fungo
  for (i in 1:n) 
  {
    k=k+1
    image[[k]]= readImage(paste("ANT0",as.character(j),"0", as.character(i),".jpg", sep = ""))# Comando que salva as imagem e da o nome da imagem, j representa a repeti??o e i o dia do experimento
    image[[k]] = resize(image[[k]], rd) # Redimensionamento da imagem para o valor estabelecido em rd
    F1=normalize(image[[k]]) # Executa a interpolaçãoo linear dos valores de intensidade de uma imagem para a ft intervalo especificado
    F1 <- F1^g  #Corrção gama, tem como função de diminuir o brilho e aumentar o contraste entre colônia e fundo. A colônia fica avermelhada e mais escura, o quadrado fica um tom de branco mais escuro e mais visível comparado  imagem anterior.
    mx <- nrow(F1)/2; my <- ncol(F1)/2 # Coordenadas central
    F1 = floodFill (F1, c(mx, my), 'red', tolerance=tol) # Inundação em vermelho a partir do centro da imagem para encontrar a colônia - que está no centro.
    F1 <- channel(F1, 'red') # Altera a imagem para o modo cinza, onde se destaca a colônia - utiliza o sistema RBG para escolha da cor
    b=otsu(F1) 
    F1[F1 > b]  = 1 
    F1[F1 <= b] = 0 
    F1 <- fillHull(F1) 
    F1 <- paintObjects(F1, image[[k]], opac=c(1, 1), col=c(NA, "red")) # Preenche de vermelho a col?nia e o quadrado.
    F2=F1^g # Correção gama para colônia
    mx <- nrow(F2)/2; my <- ncol(F2)/2 # Coordenadas central
    F2 = floodFill (F2, c(mx, my), 'green', tolerance=0) # Inundação da cor verde para a col?nia a partir do centro da imagem
    F2 <- channel(F2, 'green') # retira a inundação e deixa a imagem binaria
    b=0.99 
    F2[F2 > b]  = 1 
    F2[F2 <= b] = 0 
    pincel = makeBrush(1,shape='disc') 
    F2 = erode(F2)
    F2 = dilate(F2)
    a <- paintObjects(F2, image[[k]], opac=c(NA, 1), col=c(NA, "red")) # preenche de vermelho a col?nia
    micel = computeFeatures.shape(F2) # calcula em pixel a área do peremetro, raio máximo, raio min., raio max.
    qx <- rd/6.66; qy <- rd/6.6 
    F3 = floodFill (F1, c(qx, qy), 'blue', tolerance=0) # Colori o quadrado de azul
    F3 <- channel(F3, 'blue') # altera a imagem para o modo binário e o quadrado fica branco.
    b=0.99 
    F3[F3 > b]  = 1 
    F3[F3 <= b] = 0 
    pincel = makeBrush(1,shape='disc')
    F3 = erode(F3)
    F3 = dilate(F3) 
    b <- paintObjects(F3, a, opac=c(NA, 1), col=c(NA, "blue")) # Preenche a colônia de vermelho e o quadrado de azul
    quadrado <- computeFeatures.shape(F3)
    if (length(quadrado) != 0)
    {cm = micel/quadrado*ld*ld
    area[k] = cm[1]
    cm2 = micel/quadrado*ld
    raiom[k] = cm2[3]
    dia[k]=i
    rep[k]=j
    writeImage(b, paste("Antracnose",as.character(j),"-",as.character(i),".jpeg"), "JPEG")
    }
  }
}

dados <- data.frame(dia,rep,raiom,area)# Cria??o de uma planilha com as  vari?veis dia, rep, raiom, ?rea
dados# Planilha
na.omit(dados) # Excluir a linha quando NA
write.table(dados, file="ANTRACNOSE.txt", row.names= F, dec=',',sep=';', quote=F)# Salva os valores da planilha em txt.
Tabela <- na.omit(dados)# Excluir os dados NA presente na tabela
i=0
j=0
ACPD=matrix()
for(j in 1:(length(unique(Tabela$rep))))# Loop para calcular a ACPD para repetição
{
  ACP=0
  T1 <- subset(Tabela, rep==j)
  for (i in 1:(nrow(T1)-1))
  {
    ACP <- ACP + ((T1[i+1,4]+T1[i,4])/2)*(T1[i+1,1]-T1[i,1])
  }
  ACPD[j] <- ACP
}
ACPD # Mostra os valores que foram calculados pela ACPD
proc.time()-atm
