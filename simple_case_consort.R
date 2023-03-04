simple_case_consort <- function(consort_plot_material){
  all_name_from_list <- names(consort_plot_material)
  # first one should always start with box
  concort_plot <- add_box(txt=consort_plot_material[[1]])
  for(i in 2:(length(consort_plot_material))){
    if(all_name_from_list[i]=="box"){
      concort_plot <- add_box(concort_plot, txt=consort_plot_material[[i]])
    } else if(all_name_from_list[i]=="side"){
      concort_plot <- add_side_box(concort_plot, txt=consort_plot_material[[i]])
    } else if(all_name_from_list[i]=="split"){
      concort_plot <- add_split(concort_plot, txt=consort_plot_material[[i]])
    }
  }
  #return(concort_plot)
  plot(concort_plot)
}