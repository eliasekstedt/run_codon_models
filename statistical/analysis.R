
rm(list = ls())

LRT <- function(lnl_complex, df_complex, lnl_simple, df_simple, lab='unlabeled') {
  lr <- 2*(lnl_complex - lnl_simple)
  dof <- df_complex - df_simple
  p_value <- pchisq(q = lr, df = dof, lower.tail = FALSE)
  data.frame('Models'= lab, 'Likelihood_ratio'=lr, 'degrees_of_freedom'=dof, 'p_value'=p_value)

}

chisq <- read.table('lnl_np.txt', sep='\t', header=T)
chisq

# LRT for m0f0 (complex) vs m0f1 (simple)
m0f0_lnl <- chisq[chisq[,'model']=='m0f0', 'lnl']
m0f0_np <- chisq[chisq[,'model']=='m0f0', 'np']
m0f1_lnl <- chisq[chisq[,'model']=='m0f1', 'lnl']
m0f1_np <- chisq[chisq[,'model']=='m0f1', 'np']
lrt_m0f0_m0f1 <- LRT(m0f0_lnl, m0f0_np, m0f1_lnl, m0f1_np, 'm0f0 v m0f1')

# LRT for m1f0 (complex) vs m0f0 (simple)
m1f0_lnl <- chisq[chisq[,'model']=='m1f0', 'lnl']
m1f0_np <- chisq[chisq[,'model']=='m1f0', 'np']
lrt_m1f0_m0f0 <- LRT(m1f0_lnl, m1f0_np, m0f0_lnl, m0f0_np, 'm1f0 v m0f0')

# LRT for m2f0 (complex) vs m2f1NS2 (simple)
m2f1_lnl <- chisq[chisq[,'model']=='m2f1', 'lnl']
m2f1_np <- chisq[chisq[,'model']=='m2f1', 'np']
m2f0_lnl <- chisq[chisq[,'model']=='m2f0', 'lnl']
m2f0_np <- chisq[chisq[,'model']=='m2f0', 'np']
lrt_m2f0_m2f1 <- LRT(m2f0_lnl, m2f0_np, m2f1_lnl, m2f1_np, 'm2f0 v m2f1')

lrt <- rbind(lrt_m0f0_m0f1,lrt_m1f0_m0f0, lrt_m2f0_m2f1)


# read file with dN/dS tables from models m0f1, m0f0 and m1f0
dnds <- read.table('dnds.txt', sep=',', header=T)
dnds_list <- split(dnds, dnds$model)
m0f0 <- as.data.frame(dnds_list[1])
m0f1 <- as.data.frame(dnds_list[2])
m1f0 <- as.data.frame(dnds_list[3])

colnames(m0f1) <- gsub('m0f1.', '', colnames(m0f1))
colnames(m0f0) <- gsub('m0f0.', '', colnames(m0f0))
colnames(m1f0) <- gsub('m1f0.', '', colnames(m1f0))

library(dplyr)
col_filter <- function(data) {
  data <- data %>%
    select(!c('model', 't', 'N', 'S', 'N.dN', 'S.dS'))
  #print(data)
  return(data)
}

m0f1 <- col_filter(m0f1)
m0f0 <- col_filter(m0f0)
m1f0 <- col_filter(m1f0)


transform_m2_results <- function(data) {
  rownames(data) <- data[,1]
  data <- data[,2:5]
  colnames(data) <- c('0', '1', '2a', '2b')
  return(t(data))
}

m2f1 <- transform_m2_results(read.table('m2f1.txt', sep=',', header=T))
m2f0 <- transform_m2_results(read.table('m2f0.txt', sep=',', header=T))

print(lrt)
print(m2f1)
print(m2f0)


#library(xlsx)
#write.xlsx(chisq, file="lnl_np.xlsx")
#write.xlsx(lrt, file="lrt.xlsx")
#write.xlsx(m0f1, file='m0f1.xlsx')
#write.xlsx(m0f0, file='m0f0.xlsx')
#write.xlsx(m1f0, file='m1f0.xlsx')
#write.xlsx(m2f0, file='m2f0.xlsx')
#write.xlsx(m2f1, file='m2f1.xlsx')

