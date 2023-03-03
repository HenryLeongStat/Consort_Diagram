library("consort")
# fg1 <- textbox(text = "This is a test")
# fg2 <- textbox(text = "This is an other test", 0.1, 0.3)
# grid::grid.draw(fg1)
# grid::grid.draw(fg2)
# line_add <- connect_box(fg1, fg2, connect = "bt", type = "p")
# grid::grid.draw(line_add)
# 

## example:
consort_plot_material <- vector(mode = "list", length=12)
names(consort_plot_material) <- c("box",
                                  "box",
                                  "side",
                                  "box",
                                  "box",
                                  "box",
                                  "box",
                                  "side",
                                  "box",
                                  "side",
                                  "box",
                                  "box")

consort_plot_material[[1]] <- c("Study 1 (n=10)", 
                                "Study 2 (n=20)",
                                "Study 3 (n=30)"
)

consort_plot_material[[2]] <- "Merge (n=60)"
consort_plot_material[[3]] <- "Exclude (n=10):\n\u2022Missing ID (n=5)\n\u2022Wrong information (n=5)"
consort_plot_material[[4]] <- "Randomization (n=50)"
consort_plot_material[[5]] <- c("Arm 1 (n=20)", "Arm 2 (n=30)")
consort_plot_material[[6]] <- c("Arm 1 (n=20)", "Arm 2 (n=30)")
consort_plot_material[[7]] <- c("Merge (n=50)")
consort_plot_material[[8]] <- c("Drop out (n=20)")
consort_plot_material[[9]] <- c("Merge (n=30)")
consort_plot_material[[10]] <- c("Drop out (n=5)")
consort_plot_material[[11]] <- c("Final randomization (n=25)")
consort_plot_material[[12]] <- c("Final split 1 (n=5)",
                                 "Final split 2 (n=5)",
                                 "Final split 3 (n=5)",
                                 "Final split 4 (n=10)"
                                 
                                 
)



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
# list of x and y coor for text_box
text_box_coor <- vector(mode = "list", length=levels_consort)
# list of arrow
text_box_arrow <- as.data.frame(matrix(NA, nrow=100, ncol=6))
names(text_box_arrow) <- c("tail_i", "tail_j",
                           "head_i", "head_j",
                           "connect", "type")
text_box_arrow[1,] <- c(1, 1, 2, 1, "bt", "p")
text_box_arrow[2,] <- c(1, 2, 2, 1, "bt", "p")
text_box_arrow[3,] <- c(1, 3, 2, 1, "bt", "p")
text_box_arrow[4,] <- c(2, 1, 4, 1, "bt", "p")
text_box_arrow[5,] <- c(2, 1, 3, 1, "bl", "p")
text_box_arrow[6,] <- c(4, 1, 5, 1, "bt", "p")
text_box_arrow[7,] <- c(4, 1, 5, 2, "bt", "p")
text_box_arrow[8,] <- c(5, 1, 6, 1, "bt", "p")
text_box_arrow[9,] <- c(5, 2, 6, 2, "bt", "p")
text_box_arrow[10,] <- c(6, 1, 7, 1, "bt", "p")
text_box_arrow[11,] <- c(6, 2, 7, 1, "bt", "p")
text_box_arrow[12,] <- c(7, 1, 8, 1, "bl", "p")
text_box_arrow[13,] <- c(7, 1, 9, 1, "bt", "p")
text_box_arrow[14,] <- c(9, 1, 10, 1, "bl", "p")
text_box_arrow[15,] <- c(9, 1, 11, 1, "bt", "p")
text_box_arrow[16,] <- c(11, 1, 12, 1, "bt", "p")
text_box_arrow[17,] <- c(11, 1, 12, 2, "bt", "p")
text_box_arrow[18,] <- c(11, 1, 12, 3, "bt", "p")
text_box_arrow[19,] <- c(11, 1, 12, 4, "bt", "p")





text_box_arrow[c("tail_i", "tail_j", "head_i", "head_j")] <- lapply(text_box_arrow[c("tail_i", "tail_j", "head_i", "head_j")], 
                                                                    as.numeric)

# initialize the list of text box
for(i in 1:levels_consort){
  text_box[[i]] <- rep(textbox(text = "init"), length(consort_plot_material[[i]]))
  text_box_coor[[i]] <- vector(mode="list", length(consort_plot_material[[i]]))
}

for(i in 1:levels_consort){
  size_each_level <- length(consort_plot_material[[i]])
  small_list <- vector(mode = "list", length=size_each_level)
  small_df <- as.data.frame(matrix(NA, nrow=size_each_level, ncol=2))
  names(small_df) <- c("x", "y")
  text_box_coor[[i]] <- small_df
  text_box[[i]] <- small_list
}

text_box_coor[[1]][,"x"] <- c(0.2, 0.5, 0.8)
text_box_coor[[1]][,"y"] <- 0.95
text_box_coor[[2]][,"x"] <- 0.5
text_box_coor[[2]][,"y"] <- 0.87
text_box_coor[[3]][,"x"] <- 0.7
text_box_coor[[3]][,"y"] <- 0.73
text_box_coor[[4]][,"x"] <- 0.5
text_box_coor[[4]][,"y"] <- 0.6
text_box_coor[[5]][,"x"] <- c(0.35, 0.7)
text_box_coor[[5]][,"y"] <- 0.52
text_box_coor[[6]][,"x"] <- c(0.35, 0.7)
text_box_coor[[6]][,"y"] <- 0.46
text_box_coor[[7]][,"x"] <- 0.5
text_box_coor[[7]][,"y"] <- 0.38
text_box_coor[[8]][,"x"] <- 0.7
text_box_coor[[8]][,"y"] <- 0.3
text_box_coor[[9]][,"x"] <- 0.5
text_box_coor[[9]][,"y"] <- 0.22
text_box_coor[[10]][,"x"] <- 0.7
text_box_coor[[10]][,"y"] <- 0.16
text_box_coor[[11]][,"x"] <- 0.5
text_box_coor[[11]][,"y"] <- 0.1
text_box_coor[[12]][,"x"] <- c(0.2, 0.4, 0.6, 0.8)
text_box_coor[[12]][,"y"] <- 0.05

x_coor <- c(0.2, 0.4, 0.6, 0.8)
y_coor <- seq(0.1, 0.9, length.out=levels_consort)
y_coor <- rev(y_coor)

for(i in 1:length(consort_plot_material)){
  for(j in 1:length(consort_plot_material[[i]])){
    text_box[[i]][[j]] <- textbox(text = consort_plot_material[[i]][j],
                                  x=text_box_coor[[i]][j,"x"],
                                  y=text_box_coor[[i]][j,"y"])
  }
}


for(i in 1:length(consort_plot_material)){
  for(j in 1:length(consort_plot_material[[i]])){
    grid::grid.draw(text_box[[i]][[j]])
  }
}

# for(i in 1:(length(consort_plot_material)-1)){
#   for(j in 1:length(consort_plot_material[[i]])){
#     if(names(consort_plot_material)[i+1]=="side"){
#       line_add <- connect_box(text_box[[i]][[j]],text_box[[i+1]][[1]],  connect = "bl", type = "p")
#       grid::grid.draw(line_add)
#     } else if(names(consort_plot_material)[i]=="side"){
#       line_add <- connect_box(text_box[[i]][[j]],text_box[[i+1]][[1]],  connect = "lt", type = "p")
#       grid::grid.draw(line_add)
#     } else {
#       line_add <- connect_box(text_box[[i]][[j]],text_box[[i+1]][[1]],  connect = "bt", type = "p")
#       grid::grid.draw(line_add)
#     }
#     
#   }
# }

for(i in 1:dim(text_box_arrow)[1]){
  if(any(is.na(text_box_arrow[i,]))){
    break
  }
  line_add <- connect_box(text_box[[text_box_arrow[i,"tail_i"]]][[text_box_arrow[i,"tail_j"]]]
                          ,text_box[[text_box_arrow[i,"head_i"]]][[text_box_arrow[i,"head_j"]]], 
                          connect = text_box_arrow[i,"connect"],
                          type = text_box_arrow[i,"type"])
  grid::grid.draw(line_add)
}

# add additional box from the existting

add_box <- textbox(text = "Add on (n=0)",
        x=text_box_coor[[4]][1,"x"]-0.4,
        y=text_box_coor[[4]][1,"y"])
grid::grid.draw(add_box)

add_line <- connect_box(add_box
            ,text_box[[5]][[1]], 
            connect = "bt",
            type = "p")
grid::grid.draw(add_line)

add_box <- textbox(text = "More Add on (n=0)",
                   x=text_box_coor[[6]][1,"x"]-0.2,
                   y=text_box_coor[[6]][1,"y"])
grid::grid.draw(add_box)

add_line <- connect_box(add_box
                        ,text_box[[7]][[1]], 
                        connect = "bt",
                        type = "p")
grid::grid.draw(add_line)

add_box <- textbox(text = "Add on to\nFinal split 1 (n=0)",
                   x=text_box_coor[[12]][1,"x"],
                   y=text_box_coor[[7]][1,"y"])
grid::grid.draw(add_box)

add_line <- connect_box(add_box
                        ,text_box[[12]][[1]], 
                        connect = "bt",
                        type = "p")
grid::grid.draw(add_line)
