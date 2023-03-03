library("consort")
fg1 <- textbox(text = "This is a test")
fg2 <- textbox(text = "This is an other test", 0.1, 0.3)
grid::grid.draw(fg1)
grid::grid.draw(fg2)
line_add <- connect_box(fg1, fg2, connect = "bt", type = "p")
grid::grid.draw(line_add)

levels_consort <- length(consort_plot_material)

count_box <- rep(NA, length=levels_consort)

for(i in 1:levels_consort){
  count_box[i] <- length(consort_plot_material[[i]])
}

max_col <- max(count_box)

x_coor <- rep(1/(max_col-1), max_col-2)
x_coor <- cumsum(x_coor)
x_coor <- c(0,x_coor, 1)

y_coor <- rep(1/levels_consort, levels_consort-2)
y_coor <- cumsum(y_coor)
y_coor <- c(0, y_coor, 1)

# list of text box
text_box <- vector(mode = "list", length=levels_consort)

# initialize the list of text box
for(i in 1:levels_consort){
  for(j in 1:length(consort_plot_material[[i]])){
    text_box[[i]] <- rep(textbox(text = "init"), length(consort_plot_material[[i]]))
  }
}

for(i in 1:levels_consort){
  size_each_level <- length(consort_plot_material[[i]])
  small_list <- vector(mode = "list", length=size_each_level)
  text_box[[i]] <- small_list
}

x_coor <- c(0.2, 0.4, 0.6, 0.8)
y_coor <- seq(0.1, 0.9, length.out=levels_consort)
y_coor <- rev(y_coor)

for(i in 1:length(consort_plot_material)){
  for(j in 1:length(consort_plot_material[[i]])){
    text_box[[i]][[j]] <- textbox(text = consort_plot_material[[i]][j],
                                  x=x_coor[j],
                                  y=y_coor[i])
  }
}


for(i in 1:length(consort_plot_material)){
  for(j in 1:length(consort_plot_material[[i]])){
    grid::grid.draw(text_box[[i]][[j]])
  }
}

for(i in 1:(length(consort_plot_material)-1)){
  for(j in 1:length(consort_plot_material[[i]])){
    line_add <- connect_box(text_box[[i]][[j]],text_box[[i+1]][[1]],  connect = "bt", type = "p")
    grid::grid.draw(line_add)
  }
}


