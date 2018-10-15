library(lme4)
library(broom)
require(RCurl)

v_data <-read.csv(text=getURL("https://raw.githubusercontent.com/ambaran3/R-mixedmodel/master/v_counts_w_demo.csv"))
d_data <-read.csv(text=getURL("https://raw.githubusercontent.com/ambaran3/R-mixedmodel/master/d_counts_w_demo.csv"))
j_data <-read.csv(text=getURL("https://raw.githubusercontent.com/ambaran3/R-mixedmodel/master/j_counts_w_demo.csv"))



v_genes<-v_data[,5:98]
sum_productive_contigs<-rowSums(v_genes)
sum_per_instance_mat<-as.matrix(colSums(v_genes))
n_subs_instance_mat<-as.matrix(colSums(v_genes != 0))

data_in<-v_data
for (ind in 5:length(data_in)){
  fit_tr = glmer(cbind(data_in[,ind], sum_productive_contigs-data_in[,ind]) ~ female+CD31pos+PT + (1|participantID), 
                 family = 'binomial', data = data_in)
  tidy_mat<-tidy(fit_tr)
  if (ind>5) {
    
    intercept_mat<-rbind(intercept_mat,cbind(colnames(data_in[ind]),tidy_mat[1,]))
    female_mat<-rbind(female_mat,cbind(colnames(data_in[ind]),tidy_mat[2,]))
    CD31pos_mat<-rbind(CD31pos_mat,cbind(colnames(data_in[ind]),tidy_mat[3,]))
    PT_mat<-rbind(PT_mat,cbind(colnames(data_in[ind]),tidy_mat[4,]))
  } else {
    intercept_mat<-cbind(colnames(data_in[ind]),tidy_mat[1,])
    female_mat<-cbind(colnames(data_in[ind]),tidy_mat[2,])
    CD31pos_mat<-cbind(colnames(data_in[ind]),tidy_mat[3,])
    PT_mat<-cbind(colnames(data_in[ind]),tidy_mat[4,])
  }
}

d_genes<-d_data[,5:7]
sum_productive_contigs<-rowSums(d_genes)
sum_per_instance_d<-as.matrix(colSums(d_genes))
n_subs_instance_d<-as.matrix(colSums(d_genes != 0))
sum_per_instance_mat<-rbind(sum_per_instance_mat, sum_per_instance_d)
n_subs_instance_mat<-rbind(n_subs_instance_mat, n_subs_instance_d)

data_in<-d_data
for (ind in 5:length(data_in)){
  fit_tr = glmer(cbind(data_in[,ind], sum_productive_contigs-data_in[,ind]) ~ female+CD31pos+PT + (1|participantID), 
                 family = 'binomial', data = data_in)
  tidy_mat<-tidy(fit_tr)

    
    intercept_mat<-rbind(intercept_mat,cbind(colnames(data_in[ind]),tidy_mat[1,]))
    female_mat<-rbind(female_mat,cbind(colnames(data_in[ind]),tidy_mat[2,]))
    CD31pos_mat<-rbind(CD31pos_mat,cbind(colnames(data_in[ind]),tidy_mat[3,]))
    PT_mat<-rbind(PT_mat,cbind(colnames(data_in[ind]),tidy_mat[4,]))
  
}

j_genes<-j_data[,5:67]
sum_productive_contigs<-rowSums(j_genes)
sum_per_instance_j<-as.matrix(colSums(j_genes))
n_subs_instance_j<-as.matrix(colSums(j_genes != 0))
sum_per_instance_mat<-rbind(sum_per_instance_mat, sum_per_instance_j)
n_subs_instance_mat<-rbind(n_subs_instance_mat, n_subs_instance_j)

data_in<-j_data
for (ind in 5:length(data_in)){
  fit_tr = glmer(cbind(data_in[,ind], sum_productive_contigs-data_in[,ind]) ~ female+CD31pos+PT + (1|participantID), 
                 family = 'binomial', data = data_in)
  tidy_mat<-tidy(fit_tr)
  
  
  intercept_mat<-rbind(intercept_mat,cbind(colnames(data_in[ind]),tidy_mat[1,]))
  female_mat<-rbind(female_mat,cbind(colnames(data_in[ind]),tidy_mat[2,]))
  CD31pos_mat<-rbind(CD31pos_mat,cbind(colnames(data_in[ind]),tidy_mat[3,]))
  PT_mat<-rbind(PT_mat,cbind(colnames(data_in[ind]),tidy_mat[4,]))
  
}