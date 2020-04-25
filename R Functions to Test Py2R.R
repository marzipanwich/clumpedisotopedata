# Functions to test pulling from R code


greeting <- function(){
  print("Hello World")
}

## to test:
#   greeting()

add <- function(x,y){
  sum <- x+y
  return(sum)
}

## to test:
#   add(2,4)

cfind_i <- function (v, t){
  total <- length(v)
  loc <- 0
  for (i in 1:total)
  {
    if(v[i] == t)
      loc <- i
  }
  return(loc)
}

## to test:
#    t <- c(1,2,3,4,5)   
#   cfind_i(t,3)

df_extract <- function(df, f_name){
  total <- ncol(df)
  for (i in 1:total)
  {
    if(df[1, i] == f_name) 
    {
      return(df[,i])
    }
  }
}

## to test:
#    df1 <- data.frame(cbind(c("Obs", 1, 2, 3), c("Letter", "a", "b", "c")))
#    f2 <- "Letter"
#    df_extract(df1, f2)

df_extract_mean <- function(df, f_name){
  total <- ncol(df)
  for (i in 1:total)
  {
    if(df[1, i] == f_name) 
    {
      return(mean(as.numeric(df[-1,i])))
    }
  }
}

## to test:
#    df1 <- data.frame(cbind(c("Obs", 1, 2, 3), c("Letter", "a", "b", "c")))
#    f1 <- "Obs"
#    df_extract_mean(df1, f1)

df_extract_samples <- function(df){
  total <- ncol(df)
  sub <- c()
  for (i in 1:total)
  {
    if(df[1, i] == "Sample") 
    {
      l <- length(df[,i])
      for (j in 2:l)
      {
        first_letter <- substr(as.character(df[j, i]),1,1)
        if (first_letter == "S")
        {
          sub <- c(sub,j)
        }
      }
    }
  }
  return(sub)
}

## to test:
#    df2 <- data.frame(cbind(c("Sample", "S1", "R1", "R2", "S2", "R1", "R2", "R3", "S3", "R1"), c("Letter", "a", "b", "c", "d", "e", "f", "g","h", "i")))
#    df_extract_samples(df2)


