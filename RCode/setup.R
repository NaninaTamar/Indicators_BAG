## Libraries
library(tidyverse)
theme_set(theme_bw())

## User-defined functions
plot_roc = function(output,textshift=0.03) {
  roc_summary = group_by(output,threshold) %>%
    summarise(mean_sst=mean(sensitivity),low_sst=quantile(sensitivity,0.025),high_sst=quantile(sensitivity,0.975),
              mean_spe=mean(specificity),low_spe=quantile(specificity,0.025),high_spe=quantile(specificity,0.975))
  ggplot(roc_summary) +
    geom_line(aes(x=1-low_spe,y=low_sst),linetype=3,colour="tomato") +
    geom_line(aes(x=1-high_spe,y=high_sst),linetype=3,colour="tomato") +
    geom_line(aes(x=1-mean_spe,y=mean_sst),colour="tomato") +
    geom_point(aes(x=1-mean_spe,y=mean_sst),colour="tomato") +
    geom_text(data=filter(roc4_summary,threshold<1e9 & threshold>-1e9),aes(x=1-mean_spe-textshift,y=mean_sst+textshift/2,label=paste0("+",100*threshold,"%")),size=3,hjust=1) +
    geom_segment(x=0,y=0,xend=1,yend=1,linetype=2,color="grey",size=.2) +
    coord_cartesian(xlim=c(0,1),ylim=c(0,1)) +
    scale_x_continuous(labels=function(x) paste0((1-x)*100,"%"),breaks=c(0,0.25,.5,.75,1)) +
    scale_y_continuous(labels=scales::percent) +
    labs(x="Specificity",y="Sensitivity")
}

get_roc = function(data,response,predictor,stratification) {
  require(pROC)
  levels_R0 = levels(factor(pull(data,R0)))
  levels_p = levels(factor(pull(data,p)))
  levels_seed = levels(factor(pull(data,seed)))
  
  output = list()
  for(i in 2:(length(levels_R0))) {
    for(j in 1:length(levels_p)) {
      for(k in 1:length(levels_seed)) {
        dd = filter(data,p==levels_p[[j]],seed==levels_seed[[k]])
        ro = suppressWarnings(roc(response=pull(dd,R0)>=as.numeric(levels_R0[[i]]),predictor=pull(dd,predictor)))
        ro_au = auc(ro)
        ro_auc_ci = ci(ro)
        ro_youdan = (ro$specificities+ro$sensitivities)
        ro_distance = sqrt((1-ro$specificities)^2 + (1-ro$sensitivities)^2)
        output = rbind(output,data.frame(R0=levels_R0[i],p=levels_p[j],seed=levels_seed[k],
                            thresholds=ro$thresholds,se=ro$sensitivities,sp=ro$specificities,
                            auc=ro_auc_ci[2],auc_low=ro_auc_ci[1],auc_high=ro_auc_ci[3], 
                            youdan=ro_youdan, 
                            dist=ro_distance))
        cat(i,j,k)
      }
    }
  }
  return(output)
}


plot_allR <- function(roc){
  tbl_df(roc) %>%
    group_by(R0,p,seed) %>%
    mutate(sim=group_indices(),
           auc_text=ifelse(thresholds==Inf & R0==1,paste0(round(auc*100),"% (",round(auc_low*100),"-",round(auc_high*100),")"),"")) %>%
    ggplot() +
    geom_line(aes(x=1-sp,y=se,group=sim,colour=as.numeric(as.character(R0)))) +
    geom_text(aes(x=.2,y=.9,label=auc_text),size=3) +
    facet_grid(seed~p) +
    scale_colour_gradient(low="green",high="tomato") +
    geom_segment(x=0,y=0,xend=1,yend=1,linetype=2,color="grey",size=.2) +
    coord_cartesian(xlim=c(0,1),ylim=c(0,1)) +
    scale_x_continuous(labels=function(x) paste0((1-x)*100,"%"),breaks=c(0,0.25,.5,.75,1)) +
    scale_y_continuous(labels=scales::percent) +
    labs(x="Specificity",y="Sensitivity",colour="R")
}  


plot_specificR <- function(roc, R0spec){
  tbl_df(roc) %>%
    group_by(R0,p,seed) %>%
    filter(R0==R0spec) %>%
    mutate(sim=group_indices(),
           youdan_cutoff=max(youdan),
           distance_cutoff=min(dist),
           auc_text=ifelse(thresholds==Inf & R0==1,paste0("AUC: ", round(auc*100),"% (",round(auc_low*100),"-",round(auc_high*100),")"),""),
           youden_text=ifelse(R0==1 & youdan==youdan_cutoff, paste0("Youden cut-off: ",  thresholds, sep=""),""),
           youden_sesp_text=ifelse(R0==1 & youdan==youdan_cutoff, paste0(" (Se: ", round(se*100), "%, Sp: ", round(sp*100), "%)", sep=""), ""),
           dist_text=ifelse(R0==1 & dist==distance_cutoff, paste0("Distance cut-off: ",  thresholds, sep=""),""),
           dist_sesp_text=ifelse(R0==1 & dist==distance_cutoff, paste0(" (Se: ", round(se*100), "%, Sp: ", round(sp*100), "%)", sep=""), ""),
           text_total = paste0(auc_text, "\n", youden_text, youden_sesp_text, "\n",
                               dist_text, dist_sesp_text, sep="")) %>%
    ggplot() +
    geom_line(aes(x=1-sp,y=se,group=sim, colour=factor(R0)), size=1) +
    geom_text(aes(x=.6,y=.3,label=text_total),size=3) +
    geom_text(aes(x=1-sp, y=se, label=thresholds, 
                  colour=ifelse(youdan==youdan_cutoff, "2", ifelse(dist==distance_cutoff, "3","4")))) + 
    facet_grid(seed~p) +
    #scale_colour_gradient(low="green",high="tomato") +
    geom_segment(x=0,y=0,xend=1,yend=1,linetype=2,color="grey",size=.2) +
    coord_cartesian(xlim=c(0,1),ylim=c(0,1)) +
    scale_x_continuous(labels=function(x) paste0((1-x)*100,"%"),breaks=c(0,0.25,.5,.75,1)) +
    scale_y_continuous(labels=scales::percent) +
    scale_color_manual(values = c("forestgreen","red","orange","black"),guide=FALSE) +
    labs(x="Specificity",y="Sensitivity",colour="R")
}
  

