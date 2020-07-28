create_group <- function(dictionary, dataset){
  t <- list()
  unlisted <- vector()
  for(i in colnames(dictionary)){
    for(j in dictionary[i]){
      temp1 <- vector()
      for(k in j){
        if(k %in% colnames(dataset)){
          temp1 <- c(temp1,k)
          if(!(k %in% unlisted)){
            unlisted <- c(unlisted,k)
          }
        }
      }
    }
    t[i] <- data.frame(temp1, stringsAsFactors=FALSE)
  }
  temp <- vector()
  for(i in colnames(dataset)){
    if(!(i %in% unlisted)){
      temp <- c(temp,i)
    }
  }
  t["unlisted"] <- data.frame(temp,stringsAsFactors=FALSE)
  return(t)
}
