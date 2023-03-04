library("consort")
general_case_consort <- function(consort_plot_material,
                            list_coordinarte,
                            list_arrow
){
  levels_consort <- length(consort_plot_material)
  
  text_box <- vector(mode = "list", length=levels_consort)
  
  for(i in 1:levels_consort){
    text_box[[i]] <- rep(textbox(text = "init"), length(consort_plot_material[[i]]))
  }
  
  for(i in 1:levels_consort){
    size_each_level <- length(consort_plot_material[[i]])
    small_list <- vector(mode = "list", length=size_each_level)
    text_box[[i]] <- small_list
  }
  
  for(i in 1:length(consort_plot_material)){
    for(j in 1:length(consort_plot_material[[i]])){
      text_box[[i]][[j]] <- textbox(text = consort_plot_material[[i]][j],
                                    x=list_coordinarte[[i]][j,"x"],
                                    y=list_coordinarte[[i]][j,"y"])
    }
  }
  
  for(i in 1:length(consort_plot_material)){
    for(j in 1:length(consort_plot_material[[i]])){
      text_box[[i]][[j]] <- textbox(text = consort_plot_material[[i]][j],
                                    x=list_coordinarte[[i]][j,"x"],
                                    y=list_coordinarte[[i]][j,"y"])
    }
  }
  
  for(i in 1:length(consort_plot_material)){
    for(j in 1:length(consort_plot_material[[i]])){
      grid::grid.draw(text_box[[i]][[j]])
    }
  }
  
  for(i in 1:dim(list_arrow)[1]){
    if(any(is.na(list_arrow[i,]))){
      break
    }
    line_add <- connect_box(text_box[[list_arrow[i,"tail_i"]]][[list_arrow[i,"tail_j"]]]
                            ,text_box[[list_arrow[i,"head_i"]]][[list_arrow[i,"head_j"]]], 
                            connect = list_arrow[i,"connect"],
                            type = list_arrow[i,"type"])
    grid::grid.draw(line_add)
  }
}