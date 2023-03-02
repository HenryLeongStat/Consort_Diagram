# follow https://cran.r-project.org/web/packages/consort/vignettes/consrot_diagram.html
library("consort")
set.seed(1001)
N <- 300

trialno <- sample(c(1000:2000), N)
exc1 <- rep(NA, N)
exc1[sample(1:N, 15)] <- sample(c("Sample not collected", "MRI not collected",
                                  "Other"), 15, replace = T, prob = c(0.4, 0.4, 0.2))

induc <- rep(NA, N)
induc[is.na(exc1)] <- trialno[is.na(exc1)]

exc2 <- rep(NA, N)
exc2[sample(1:N, 20)] <- sample(c("Sample not collected", "Dead",
                                  "Other"), 20, replace = T, prob = c(0.4, 0.4, 0.2))
exc2[is.na(induc)] <- NA

exc <- ifelse(is.na(exc2), exc1, exc2)

arm <- rep(NA, N)
arm[is.na(exc)] <- sample(c("Conc", "Seq"), sum(is.na(exc)), replace = T)
arm3 <- sample(c("Trt A", "Trt B", "Trt C"), N, replace = T)
arm3[is.na(arm)] <- NA

fow1 <- rep(NA, N)
fow1[!is.na(arm)] <- sample(c("Withdraw", "Discontinued", "Death", "Other", NA),
                            sum(!is.na(arm)), replace = T, 
                            prob = c(0.05, 0.05, 0.05, 0.05, 0.8))
fow2 <- rep(NA, N)
fow2[!is.na(arm) & is.na(fow1)] <- sample(c("Protocol deviation", "Outcome missing", NA),
                                          sum(!is.na(arm) & is.na(fow1)), replace = T, 
                                          prob = c(0.05, 0.05, 0.9))

df <- data.frame(trialno, exc1, induc, exc2, exc, arm, arm3, fow1, fow2)

df$reas1[!is.na(arm)] <- sample(c("Protocol deviation", "Outcome missing", NA),
                                sum(!is.na(arm)), replace = T, 
                                prob = c(0.08, 0.07, 0.85))

df$reas2[!is.na(df$reas1)] <- sample(c("Withdraw", "Discontinued", "Death", "Other"),
                                     sum(!is.na(df$reas1)), replace = T, 
                                     prob = c(0.05, 0.05, 0.05, 0.05))

rm(trialno, exc1, induc, exc2, exc, arm, arm3, fow1, fow2, N)





df$exc1

df$arm <- factor(df$arm)

library(grid)
# Might want to change some settings
options(txt_gp = gpar(cex = 0.8))


# i.e. data have the information
# about ID
# whether exclude or not



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


label_consort <- c("1"="Initial Study",
                   "2"="Group all studies",
                   "4"="Randomized",
                   "5"="First follow-up",
                   "6"="Im 6")

# note side doesn't count as one level
consort_plot_henry <- function(consort_plot_material){
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
  return(concort_plot)
}

a <- consort_plot_henry(consort_plot_material)
plot(a)
