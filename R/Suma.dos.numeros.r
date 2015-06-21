#' Suma 2 Function
#'
#' This function allows you to add 2 numbers.
#' @param 2 numbers.
#' @keywords suma
#' @export
#' @examples
#' Suma.dos.numeros()

Suma.dos.numeros <- function(a,b=NA){
#comprueba que hay dos parametros numericos  
  if (!is.na(b))
    return (a+b)
#Separa en dos numeros antes y despues del punto
  x<-unlist(strsplit(toString(a),".", fixed = TRUE))
#asiganamos el primer numero  
  n1<-strtoi(x[1], base = 0L)
#asiganamos el segundo numero  
  n2<-strtoi(x[2], base = 0L)
#la traduccion de de 0 a int da NA hay que controlarlo
  if (is.na(n2))
    n2=0
  return (n1+n2)
}
#casos de prueba
#> Suma.dos.numeros(2,1)
#[1] 3
#> Suma.dos.numeros(2,0)
#[1] 2
#> Suma.dos.numeros(2.0)
#[1] 2
#> Suma.dos.numeros(2.4)
#[1] 6
