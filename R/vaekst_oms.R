#' Udregning af vækstkategori baseret på udvikling i omsætning i de 3 seneste år
#'
#' Funktionen udregner vækstkategorien for henholdsvis omsætningsvækst.
#' @param x er en numerisk variabel og indeholder virksomhedens omsætning angivet i tusinde kroner til tiden t.
#' @param y er en numerisk variabel og indeholder virksomhedens omsætning angivet i tusinde kroner til tiden t+1.
#' @param z er en numerisk variabel og indeholder virksomhedens omsætning angivet i tusinde kroner til tiden t+2.
#' @param g er en numerisk variabel og indeholder virksomhedens omsætning angivet i tusinde kroner til tiden t+3.
#' 
#' Kategorisering Beskedent,Moderat, Højvækst bliver kun foretaget hvis virksomheden tilhører samme kategori i alle de seneste 3 år, dvs. at f.eks. højvækst eller beskedent i år1 og 2, men moderat i år3 resultere i NA. Kategorierne eksistere altså kun i deres "reneste form"
#' 
#' Forudsætning for at beregning foretages er at omsætning er steget min. 1 mio.kr. hvert i den seneste 3-årige periode
#'     
#' @keywords vækstkategori
#' @export
#' @examples
#' #data eksempel 
#' 
#' x <- 30000 # Omsætning i 2014
#' y <- 20000 # Omsætning i 2013
#' z <- 15000 # Omsætning i 2012
#' g <- 10000 # Omsætning i 2011
#' 
#' vaekst_oms(x,y,z,g)


 vaekst_oms<- function(x, y, z, g){
  abs.v <- ifelse(x-y>=1000,ifelse(y-z>=1000,ifelse(z-g>=1000, 'yes'  ,NA ),NA ), NA )
  
  v1 <-(x-y)/y
  v2 <-(y-z)/z
  v3 <-(z-g)/g
  
  #Kategorisering Beskedent,Moderat, Højvækst bliver kun foretaget hvis virksomheden tilhører samme kategori i alle de seneste 3 år, dvs. at f.eks. højvækst eller beskedent i år1 og 2, men moderat i år3 resultere i NA. Kategorierne eksistere altså kun i deres "reneste form"
  ifelse(abs.v=='yes', ifelse(0.2>v1 & v1>=0.1 & 0.2>v2 & v2>=0.1 & 0.2>v3 & v3>=0.1, 'Moderat', ifelse(0.1>v1 & v1>=0.0 & 0.1>v2 & v2>=0.0 & 0.1>v3 & v3>=0.0, 'Beskedent', ifelse(v1>=0.2 & v2>=0.2 & v3>=0.2, 'Højvækst',NA))), NA)                                 

}

