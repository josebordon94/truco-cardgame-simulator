
#Bordon Jose Miguel - LU 48.363 - Modelo de juego del truco
cat("\014")  #Limpia la consola
rm(list=ls()) 
#Programa para simular manos de truco
#Si se elige una sola corrida, se muestra una mano completa.
c_corridas = as.integer(1000)
#Variable que indica que carta se tira primero ("mayor/central/menor")
primer_carta = "mayor"
if(primer_carta == "central"){
  cat("--- Simulación de manos de truco, donde el primer jugador tira su carta central en primer lugar ---\n")
}else{
cat("--- Simulación de manos de truco, donde el primer jugador tira su",  primer_carta, "carta en primer lugar ---\n")
}
#Cargamos un mazo estandar de 40 cartas españolas (sin ochos, nueves ni comodines)
cartas = c("4 de copa","4 de basto","4 de oro","4 de espada","5 de copa","5 de basto","5 de oro","5 de espada",
           "6 de copa","6 de basto","6 de oro","6 de espada","7 de copa","7 de basto","10 de copa","10 de basto","10 de oro","10 de espada","11 de copa",
           "11 de basto","11 de oro","11 de espada","12 de copa","12 de basto","12 de oro",
           "12 de espada","1 de copa","1 de oro","2 de copa","2 de basto","2 de oro","2 de espada","3 de copa",
           "3 de basto","3 de oro","3 de espada","7 de oro","7 de espada","1 de basto","1 de espada")
#Lo disponemos de mayor a menor
cartas = rev(cartas)
#Cargamos los valores logicos de cada carta
peso = c(14,13,12,11,10,10,10,10,9,9,9,9,8,8,7,7,7,7,6,6,6,6,5,5,5,5,4,4,3,3,3,3,2,2,2,2,1,1,1,1)

#print(data.frame(cartas, peso), right=FALSE) <- Codigo para imprimir tabla de valores
#Para comenzar a simular una mano, necesitamos extraer seis cartas sin reposicion (3 para cada jugador)
#O es lo mismo, obtener seis nuomeros naturales aleatorios entre 1 y 40 sin reposicion.
acum_g1 = 0
acum_g2 = 0
for (corrida in 1:c_corridas) {
manos = sample(1:40, 6, replace=F)
mano_j1= sort(manos[1:3])
mano_j2= sort(manos[4:6])
mano_bck_j1 = mano_j1
mano_bck_j2 = mano_j2
cartas_j1 = c(cartas[mano_j1[1]],cartas[mano_j1[2]],cartas[mano_j1[3]])
cartas_j2 = c(cartas[mano_j2[1]],cartas[mano_j2[2]],cartas[mano_j2[3]])
cj_j1 = c() #Cartas jugadas por el jugador 1
cj_j2 = c() #Cartas jugadas por el jugador 2
rondas_j1 = 0 #Rondas ganadas por el jugador 1
rondas_j2 = 0 #Rondas ganadas por el jugador 2
turno = 1 #Numero de jugador que le toca jugar.
ronda_cerrada = FALSE #Variable que determina una ronda que ya finalizo
ganador_mano = 0
#Ya disponemos de las manos de ambos jugadores
#Ahora disponemos a simular las cartas que se van jugando
  for (j in 1:3) { #Simulamos las 3 rondas de cartas tiradas
    ronda_cerrada = FALSE
    if(turno==1){ #Si es el turno del primer jugador
      #Realizamos un control en el primer turno, para saber con cual carta empezar.
      if( ((j==1) && (primer_carta == "mayor")) || ((j==1) && (primer_carta == "central")) ){
        aux = 0
        if(primer_carta=="mayor"){
          aux =1 #Tiramos la primer carta
        } else{
          aux =2 #Tiramos la segunda
        }
        cj_j1[j] = mano_j1[aux] #La mayor carta sera la primera al estar ordenadas de forma descendente
        carta_jugada = cj_j1[j] #Guardamos la ultima carta jugada en esta variable auxiliar
        mano_j1 = mano_j1[-c(aux)] #Quitamos la carta mas alta de la mano del jugador
      } else{
        cj_j1[j] = tail(mano_j1, n=1) #La menor carta sera la ultima al estar ordenadas de forma descendente
        carta_jugada = cj_j1[j] #Guardamos la ultima carta jugada en esta variable auxiliar
        mano_j1 = mano_j1[-c(length(mano_j1))] #Quitamos la carta mas baja de la mano del jugador   
      }
      #Ahora debemos determinar la carta que jugará el jugador 2, y de esta forma determinar quién gana la ronda
      for(i in length(mano_j2):1){ #Recorremos las cartas que le quedan de menor a mayor
        if(peso[mano_j2[i]]>peso[carta_jugada] ){
          #Si se cumple esta condicion, el jugador 2 juega esta carta
          cj_j2[j] = mano_j2[i] #Marcamos la carta que fue jugada
          mano_j2 = mano_j2[-c(i)] #Quitamos la carta ya jugada de la mano del jugador 2
          rondas_j2 = rondas_j2 + 1 #Marcamos que el jugador 2 gana una ronda
          ronda_cerrada = TRUE
          turno = 2
          break
        }
      }
      if(ronda_cerrada){ #Si el jugador 2 ya termino de jugar su carta, se finaliza la ronda
        next
      } else{ #De no ser asi, quiere decir que el jugador 2 juega su carta mas baja
        cj_j2[j] = tail(mano_j2, n=1)
        mano_j2 = mano_j2[-c(length(mano_j2))] #Quitamos la carta mas baja de la mano del jugador
        rondas_j1 = rondas_j1 + 1 #Marcamos que el jugador 1 gana una ronda
        ronda_cerrada = TRUE
        turno = 1
      }
    } else {#Si es el turno del jugador 2
      cj_j2[j] = tail(mano_j2, n=1) #La menor carta sera la ultima al estar ordenadas de forma ascendente
      carta_jugada = cj_j2[j] #Guardamos la ultima carta jugada en esta variable auxiliar
      mano_j2 = mano_j2[-c(length(mano_j2))] #Quitamos la carta mas baja de la mano del jugador   
      #Ahora debemos determinar la carta que jugara el jugador 1, y de esta forma determinar quien gana la mano
      for(i in length(mano_j1):1){ #Recorremos las cartas que le quedan de menor a mayor
        if(peso[mano_j1[i]] > peso[carta_jugada] ){
          #Si se cumple esta condicion, el jugador 1 juega esta carta
          cj_j1[j] = mano_j1[i] #Marcamos la carta que fue jugada
          mano_j1 = mano_j1[-c(i)] #Quitamos la carta ya jugada de la mano del jugador 1
          rondas_j1 = rondas_j1 + 1 #Marcamos que el jugador 1 gano una ronda
          ronda_cerrada = TRUE
          turno = 1
          break
        }
      }
      if(ronda_cerrada){ #Si el jugador 1 ya termino de jugar su carta, se finaliza la ronda
        next
      } else{ #De no ser asi, quiere decir que el jugador 1 juega su carta mas baja
        cj_j1[j] = tail(mano_j1, n=1)
        mano_j1 = mano_j1[-c(length(mano_j1))] #Quitamos la carta mas baja de la mano del jugador
        rondas_j2 = rondas_j2 + 1 #Marcamos que el jugador 2 gana una ronda
        ronda_cerrada = TRUE
        turno = 2
      }
    }
  } #Recorrido de las 3 rondas
if(rondas_j1> rondas_j2){ #Determinamos que jugador fue el que gano la mano segun las rondas
  ganador_mano= 1
} else{
  ganador_mano= 2
}
#Si se eligio una sola corrida, se muestran los datos de la mano jugada
if(c_corridas ==1){
  cat("Se ejecuto una sola mano.\n")
  cat("Cartas del jugador 1: ", cartas_j1[1], "|",cartas_j1[2], "|",cartas_j1[3], "|", "\n")
  cat("Cartas del jugador 2: ", cartas_j2[1], "|",cartas_j2[2], "|",cartas_j2[3], "|", "\n")
  #Revisamos si se presento un empate en la primer carta
  if(cj_j1[1] == cj_j2[1]){
    cat("Se presento un empate al comparar las cartas, lo que genera que el J1 gane al ser mano \n")
  } else{
    rondas_jugadas = 3
    #Vemos si la mano en total fue de 2 o 3 rondas
    #Si un jugador gana en las dos primeras rondas no se necesita la tercera
    if( (rondas_j1==3) || (rondas_j2==3)){
      rondas_jugadas = 2
    }
    cat("\n*** Rondas jugadas ***\n")
    for (i in 1:rondas_jugadas) {
      cat("-->Ronda", i, "\n")
      cat("Jugador 1 juega: ", cartas[cj_j1[i]],"\n")
      cat("Jugador 2 juega: ", cartas[cj_j2[i]],"\n")
      cat("\n")
    }
    leyendas = c("Rojo: jugador 1", "Verde: jugador 2")
    cat(" >>> Ganador de la mano: Jugador",ganador_mano)
    #Guardamos el peso de cada carta jugada
    peso_j1 = c(peso[cj_j1[1]], peso[cj_j1[2]], peso[cj_j1[3]])
    peso_j2 = c(peso[cj_j2[1]], peso[cj_j2[2]], peso[cj_j2[3]])
    par(xpd=TRUE)
    #Guardamos los nombres de las cartas
    labels_j1 = c(cartas[cj_j1[1]], cartas[cj_j1[2]], cartas[cj_j1[3]])
    labels_j2 = c(cartas[cj_j2[1]], cartas[cj_j2[2]], cartas[cj_j2[3]])
    rondas = c(1:3)
    #datos_mano = data.frame(cj_j1,cj_j2, rondas)
    par(mar = c(5, 5, 5, 5))
    plot(rondas,peso_j1,type="p",col="blue", main="Grafico de mano jugada", cex=2.5 ,xlab="Nro de ronda",ylab="Peso de carta jugada", xlim=c(1,3),ylim=c(1,14), pch = 19)
    points(rondas,peso_j2,col="red", pch = 19, cex=2.5)
    legend(2.3,-1.50, legend=c("Jugador 1", "Jugador 2"),
           col=c("blue", "red"), fill=c("blue", "red"), cex=0.9, bg='lightblue')
    text(rondas, peso_j1, labels=labels_j1,pos=4, cex= 0.8)
    text(rondas, peso_j2, labels=labels_j2,pos=4, cex= 0.8)
  }
} else{ #Si es una ejecucion de muchas manos, se debe contar cuantas gano cada jugador
  if(ganador_mano==1){
    acum_g1 = acum_g1 + 1
  } else {
    acum_g2 = acum_g2 + 1
  }
}
}#Bucle de corridas
if(c_corridas>1){ #Si se solicitaron varias corridas, mostramos el informe final.
  cat("\nSe ejecutaron", c_corridas , "manos\n\n")
  porc_g1 = (acum_g1 / c_corridas) * 100
  porc_g2 = (acum_g2 / c_corridas) * 100
  manos_ganadas = c(acum_g1, acum_g2)
  cat("-->Manos ganadas por el jugador 1: ", acum_g1, " (", porc_g1, "%)\n", sep="")
  cat("-->Manos ganadas por el jugador 2: ", acum_g2, " (", porc_g2, "%)\n", sep="")
  porcentajes = c(porc_g1, porc_g2)
  etiquetas = c("Jugador 1: ", "Jugador 2: ")
  etiquetas = paste(etiquetas, porcentajes)
  etiquetas = paste(etiquetas, "%", sep = "")
  pie(porcentajes, etiquetas,
      main = "Manos ganadas por jugador", col = c("blue", "red") )
  barplot(manos_ganadas, main="Manos ganadas por jugador", col=c("blue", "red"), names.arg=c("Jugador 1", "Jugador2"))
  
}

