#' Udregning af vækstkategori baseret på udvikling i ansatte i de 3 seneste år
#'
#' Funktionen udregner vækstkategorien for udviklingen i antal ansatte 
#' 
#' Kategorisering Beskedent,Moderat, Højvækst bliver kun foretaget hvis virksomheden tilhører samme kategori i alle de seneste 3 år, dvs. at f.eks. højvækst eller beskedent i år1 og 2, men moderat i år3 resultere i NA. Kategorierne eksistere altså kun i deres "reneste form"
#' 
#' Forudsætning for at beregning foretages er at antal ansatte er min. 5 i starten(g) af den seneste 3-årige periode
#' 
#' @param x er en numerisk variabel og indeholder virksomhedens antal ansatte  til tiden t.
#' @param y er en numerisk variabel og indeholder virksomhedens antal ansatte  til tiden t+1.
#' @param z er en numerisk variabel og indeholder virksomhedens antal ansatte  til tiden t+2.
#' @param g er en numerisk variabel og indeholder virksomhedens antal ansatte  til tiden t+3.
#' 
#'     
#' @keywords vækstkategori
#' @export
#' @examples
#' #data eksempel 
#' 
#' x <- 8 # Antal ansatte i 2014
#' y <- 7 # Antal ansatte i 2013
#' z <- 6 # Antal ansatte i 2012
#' g <- 5 # Antal ansatte i 2011
#' 
#' vaekst_ans(x,y,z,g)


 vaekst_ans<- function(x, y, z, g){
  abs.v <- ifelse(g>=5,'yes', NA )
  
  v1 <-(x-y)/y
  v2 <-(y-z)/z
  v3 <-(z-g)/g
  
  #Kategorisering Beskedent,Moderat, Højvækst bliver kun foretaget hvis virksomheden tilhører samme kategori i alle de seneste 3 år, dvs. at f.eks. højvækst eller beskedent i år1 og 2, men moderat i år3 resultere i NA. Kategorierne eksistere altså kun i deres "reneste form"
  ifelse(abs.v=='yes', ifelse(0.2>v1 & v1>=0.1 & 0.2>v2 & v2>=0.1 & 0.2>v3 & v3>=0.1, 'Moderat', ifelse(0.1>v1 & v1>=0.0 & 0.1>v2 & v2>=0.0 & 0.1>v3 & v3>=0.0, 'Beskedent', ifelse(v1>=0.2 & v2>=0.2 & v3>=0.2, 'Højvækst',NA))), NA)                                 

}

