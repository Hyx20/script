GC <- function(dt,group,cat_vars){
  dt[[group]] <-as.factor(dt[[group]]) 
  g_type <- levels(dt[[group]])
  
  catsave <- data.frame()
  for (i in cat_vars) {
    dt[,c(i)] <- as.factor(dt[,c(i)])
  }
  vars <- names(dt)
  vars <- vars[vars!=group]
  num_vars <- vars[vars %in% cat_vars==F]

  numsave <- data.frame(matrix(NA, nrow = length(num_vars), ncol = length(g_type)+3))
  colnames(numsave) <- c("characteristics",g_type,"χ2/F","p-value")
  
  for (i in 1:length(cat_vars)) {
    freq <- table(dt[[group]],dt[[cat_vars[i]]])###性别
    prop <- prop.table(table(dt[[group]],dt[[cat_vars[i]]]),1)
    chisq <- chisq.test(table(dt[[group]],dt[[cat_vars[i]]]))
    type <- levels(dt[[cat_vars[i]]])
    
    save <- data.frame(matrix(NA, nrow = length(type)+1, ncol = length(g_type)+3))
    colnames(save) <- c("characteristics",g_type,"χ2/F","p-value")
    save[,1] <- c(cat_vars[i],type)
    for (x in 1:length(type)) {
      for (y in 1:length(g_type)) {
        save[x+1,y+1] <- paste0(round(freq[x,y],2),"(",round(prop[x,y],2),")")
      }
    }
    save[1,length(g_type)+2] <- round(chisq$statistic,3)
    save[1,length(g_type)+3] <- round(chisq$p.value,3)
    catsave <- rbind(catsave,save)
  }
  
  for (i in 1:length(num_vars)) {
    mean <- aggregate(dt[[num_vars[i]]],list(type=dt[[group]]),mean)
    sd <- aggregate(dt[[num_vars[i]]],list(type=dt[[group]]),sd)
    fml <- as.formula(paste0(num_vars[i],"~",group))
    aov.mis<-aov(fml,data=dt)
    numsave[i,1] <- num_vars[i]
    for (x in 1:length(g_type)) {
      numsave[i,x+1] <- paste0(round(mean[x,2],2),"±",round(sd[x,2],2))
    }
    numsave[i,length(g_type)+2] <- round(summary(aov.mis)[[1]][["F value"]][1],3)
    numsave[i,length(g_type)+3] <- round(summary(aov.mis)[[1]][["Pr(>F)"]][1],3)
  }
  all <- rbind(catsave,numsave)
  return(all)
}
