library(dplyr)
geneEdit_data <-  read.csv("C:/Users/612/Desktop/抗性澱粉T檢定.csv", header = T)
geneEdit_data <- subset(geneEdit_data, geneEdit_data$品系..種. !="S1")
geneEdit_data.name <- names(geneEdit_data)

S_list <- as.character(unique(geneEdit_data$品系..種.))

stat.compu <- function(geneEdit_data, S_list, contrast.var, t.test_format, alpha){
  
  statistic.data <- data.frame(parm = numeric(0), A= numeric(0), B= numeric(0), statistic.T= numeric(0),statistic.df= numeric(0),p.value_T= numeric(0), conf.int.L= numeric(0), conf.int.H= numeric(0),N.A = numeric(0), N.B = numeric(0),mean.A= numeric(0), mean.B= numeric(0),sd.A = numeric(0), sd.B = numeric(0),p.value_F = numeric(0),method = numeric(0))
    
    

  for(i in 4:13){
    for(j in 1:length(S_list)){
      temp.A = subset(geneEdit_data, 品系..種. == S_list[j])[,i]
      temp.B = subset(geneEdit_data, 品系..種. == contrast.var)[,i] 
      temp_T <- t.test(x = temp.A, 
                       y = temp.B,
                       alternative = t.test_format,
                       mu = 0, paired = FALSE, var.equal = FALSE,
                       conf.level = alpha)
      Temp_F <- 
        var.test(temp.A, temp.B, ratio = 1, alternative = "two.sided")
      statistic.data[j+(i-4)*(length(S_list)),] <- c(geneEdit_data.name[i], S_list[j], contrast.var, round(temp_T$statistic,4),round(temp_T$parameter,4), round(temp_T$p.value,4), round(temp_T$conf.int,4),length(temp.A),length(temp.B),round(temp_T$estimate,4),round(sd(temp.A),4), round(sd(temp.B),4), round(Temp_F$p.value,4),temp_T$method)
    }
  
  }
  return(statistic.data)
}


contrastByS20_less <- stat.compu(geneEdit_data, S_list, "S20", "less", 0.95)
write.csv(contrastByS20_less, "D:/contrastByS20_less.csv")
contrastByS21_less <- stat.compu(geneEdit_data, S_list, "S21", "less", 0.95)
write.csv(contrastByS20_less, "D:/contrastByS21_less.csv")
contrastByS20_two.sided <- stat.compu(geneEdit_data, S_list, "S20", "two.sided", 0.95)
write.csv(contrastByS20_less, "D:/contrastByS20_two.sided.csv")
contrastByS21_two.sided <- stat.compu(geneEdit_data, S_list, "S21", "two.sided", 0.95)
write.csv(contrastByS20_less, "D:/contrastByS21_two.sided.csv")

#----------------------------------------------------#
library(reshape2)
contrastByS20_less.ptable <- dcast(contrastByS20_less[,c("parm","A","B", "p.value_T")], A+B ~ parm)
contrastByS21_less.ptable <- dcast(contrastByS21_less[,c("parm","A","B", "p.value_T")], A+B ~ parm)
contrastByS20_two.sided.ptable <- dcast(contrastByS20_two.sided[,c("parm","A","B", "p.value_T")], A+B ~ parm)
contrastByS21_two.sided.ptable <- dcast(contrastByS21_two.sided[,c("parm","A","B", "p.value_T")], A+B ~ parm)
#----------------------------------------------------#
library(ggplot2)

p_plot <- function(data,breaks){
  data$ p.value_T = as.numeric(data$p.value_T)
  data$p.value_T_bin  <- cut(data$p.value_T, 
                                           breaks = c(-0.001,0.01, 0.05, 0.1, 0.3, 0.5, 1))
  data$A <- factor(data$A, levels=paste0("S",1:22))
  
  data.plot <- ggplot(data, aes(x=A, y=parm, fill = p.value_T_bin))+geom_tile()+
    scale_fill_brewer(palette = "Greens")+
    scale_x_discrete(name="", limits = levels(paste0("S",1:22)))+
    geom_text(aes(label = p.value_T %>% round(.,3)))
  return(data.plot)
}

p_plot(contrastByS20_less,c(-0.001,0.01, 0.05, 0.1, 0.3, 0.5, 1))
p_plot(contrastByS21_less,c(-0.001,0.01, 0.05, 0.1, 0.3, 0.5, 1))
p_plot(contrastByS20_two.sided,c(-0.001,0.01, 0.05, 0.1, 0.3, 0.5, 1))
p_plot(contrastByS21_two.sided,c(-0.001,0.01, 0.05, 0.1, 0.3, 0.5, 1))
