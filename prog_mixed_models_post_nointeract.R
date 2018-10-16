knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)
library(broom)
#install.packages("tidyverse")
library(tidyverse)
#install.packages('ggbeeswarm')
library(ggbeeswarm)
#library(AMmisc) #install_github('amcdavid/AMmisc')
theme_set(theme_minimal())

install.packages("repmis")
library(repmis)

source_data("https://github.com/ambaran3/R-mixedmodel/blob/master/TSeq%20mixed%20model%20results%20no%20interactions.RData?raw=true")


#obj = load('TSeq mixed model results.RData')


##filter segments based on prevalence
#keep anything in more than 1 subject in more than one cell
idx_keep<-which(n_subs_instance_mat>1 & sum_per_instance_mat>1)
intercept_mat<-intercept_mat[idx_keep,]
female_mat<-female_mat[idx_keep,]
CD31pos_mat<-CD31pos_mat[idx_keep,]
PT_mat<-PT_mat[idx_keep,]


analyses = list(CD31pos_mat = CD31pos_mat, female_mat = female_mat, PT_mat = PT_mat)

#all_analysis = bind_rows(analyses, .id = 'analysis') %>% rename(segment = 'colnames(data_in[ind])') %>% mutate(FDR = p.adjust(p.value, method = 'fdr'))

comb_analysis<- bind_rows(analyses, .id = 'analysis')
comb_analysis<-rename(comb_analysis, segment='colnames(data_in[ind])')
all_analysis2<-mutate(comb_analysis, FDR=p.adjust(p.value, method='fdr'))

ggplot(all_analysis2, aes(x=estimate, y=-log10(FDR)))+geom_point()+facet_wrap(~term)+ geom_hline(yintercept = -log10(0.05), lty = 2) + xlab('Log odds ratio') +xlim(-2.5,2.5)
#ggplot(all_analysis, aes(x = clamp(estimate, 4), y= -log10(FDR))) + geom_point() + facet_wrap(~term) + geom_hline(yintercept = 1, lty = 2)  + xlab('Log odds ratio')


idx<-which(all_analysis2[,9]<0.05)
all_analysis2[idx,]


