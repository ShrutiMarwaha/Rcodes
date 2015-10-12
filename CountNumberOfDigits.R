## I. count the number of digits in a number ########
CountDigits <- function(x){
  if(is.numeric(x)==F)
  {
    print("please enter a valid number")  
  }
  else if(x==0)
  {
    return(1)
  }
  else
  {
    num <- floor(log10(abs(x)))+1
    return(num)
  }
}

### less eficient way 
CountDigits <- function(x){
  if(is.numeric(x)==F)
  {
    print("please enter a valid number")  
  }
  else if(x==0)
  {
    return(1)
  }
  else
  {
    NumDigits <- 0
    while(abs(x)>=1)
    {
      NumDigits <- NumDigits+1
      x <- x/10
    }  
    return(NumDigits)
  }
}