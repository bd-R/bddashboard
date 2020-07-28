find_field_for_plot <- function(data, plot, group){
  
  new_list <- list()
  characters <-  names(dplyr::select_if(data,is.character))
  numeric <- names(dplyr::select_if(data,is.numeric))
  
  if(plot=="bubble" || plot=="line"){
    
    for(i in names(group)){
      if(i != "core"){
        
        new_list[[i]][["Select_X"]] <- Reduce(intersect, list(characters, group[[i]]))
        new_list[[i]][["Select_Y"]] <- Reduce(intersect, list(numeric, group[[i]]))
        new_list[[i]][["Default"]] <- group[[i]]
      }
      
    }
    
  }else if(plot == "pie" || plot == "bar"){
    for(i in names(group)){
      if(i != "core"){
        
        new_list[[i]][["Select_X"]] <- Reduce(intersect, list(characters, group[[i]]))
        new_list[[i]][["Default"]] <- group[[i]]
      }
      
    }
  }
  
  return(new_list)
  
}
