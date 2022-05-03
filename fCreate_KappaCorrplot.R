#2022-05-03
#Dabalà Alvise from a function of Tin Buenafe

#sol: list of solutions
#name_sol: list of the names of the different solution that will be visualised in
#          the final plot
#dir: directory where you want to save the excel file with the correlation values

fcreate_kappacorrplot <- function(sol, name_sol, dir) {
  
  library(irr)
  library(corrplot)
  
  s_list <- lapply(seq_along(sol), function(x) {
    sol[[x]] %>% 
      as_tibble() %>% 
      dplyr::select(solution_1) %>% 
      setNames(name_sol[[x]])
  }
  )
  
  y = 1
  s_matrix <- list()
  for(i in 1:length(s_list)){
    for(j in 1:length(s_list)){
      kappa_temp <- irr::kappa2(bind_cols(s_list[[i]], s_list[[j]]))
      kappa_corrvalue <- kappa_temp$value
      kappa_pvalue <- kappa_temp$p.value
      s_matrix[[y]] <- cbind(colnames(s_list[[i]]), colnames(s_list[[j]]), kappa_corrvalue, kappa_pvalue)
      y = y+1
    }
  }
  
  s_matrix_all <- do.call(rbind, s_matrix) %>% 
    as_tibble()
  colnames(s_matrix_all)[1:2] <- c('plan1','plan2')
  
  matrix_final1 <- s_matrix_all %>% 
    as_tibble() %>% 
    dplyr::select(-kappa_pvalue) %>% 
    pivot_wider(names_from = plan2, values_from = kappa_corrvalue) %>% 
    as.matrix()
  
  matrix_final2 <- s_matrix_all %>% 
    as_tibble()
  
  write_csv(matrix_final2, paste0(dir,"kappa_matrix.csv"))
  
  # creating corrplot
  rownames(matrix_final1) <- matrix_final1[,1]
  n <- length(s_list) + 1 # 4 is the number of inputted scenarios
  matrix_final2 <- matrix_final1[,2:n]
  class(matrix_final2) <- "numeric"
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  plot <- corrplot(matrix_final2, method = "shade", tl.col = "black", addCoef.col = "black",
                   col=col(200), tl.srt=45)
  return(plot)
}

