ReverseNumber <- function(x)
{
  Reverse <- 0
  while(x>=1)  
  {
    Remainder <- x %% 10
    Reverse <- Reverse*10 + Remainder
    Quotient <- floor(x / 10)
    x <- Quotient 
  }
  return(Reverse)
}  

StringReverse <- function(x){
  x_array <- unlist(strsplit(as.character(x),""))
  reverse_array <- x_array[nchar(x):1]
  paste(reverse_array,collapse="")
}

Palindrome <- function(x)
{
  if(class(x)=="character")
  {
    string <- unlist(strsplit(x,""))
    n <- length(string)
    
    for(i in 0:(n/2))
    {
      if((string[i+1]!=string[n-i]))
      {
        return(FALSE)
      }    
    }
    return(TRUE)
  }
  else if (class(x)=="numeric" | class(x)=="integer")
  {
    inverted <- ReverseNumber(x)
    if(inverted==x)
    {
      return(TRUE)
    }
    else
    {
      return(FALSE)
    }
  }
}  

Palindrome2 <-  function(x){
  reversed <- StringReverse(x)
  if(x==reversed)
  {
    return(TRUE)  
  }
  else
  {
    return(FALSE)
  }
}

# for bonus points palindromes can also be phrases, but you need to remove the spaces and punctuation before checking. for example: madam i’m adam is a palindrome.
Palindrome3 <-  function(x){
  x <- tolower(gsub("[[:punct:]]?[[:space:]]?","",x))
  reversed <- StringReverse(x)
  if(x==reversed)
  {
    return(TRUE)  
  }
  else
  {
    return(FALSE)
  }
}
Palindrome3("Go hang a salami; I’m a lasagna hog.")
Palindrome3("Reviled did I live, said I, as evil I did deliver")