library(data.table)
library(tidyverse)

#registry_name <- c("ref", "AL", "PriorLasso", "RGCCA", "JIVE", "PCA")
registry_name <- c("ref", "AL", "PriorLasso", "RGCCA", "JIVE", "PCA")
#ncpus <- 1

data.table::setDTthreads(ncpus)

#bmrdt = fread(paste0(registry_name, "_results.csv"))
bmrdt = lapply(paste0("results/", registry_name, "/", registry_name, "_results.csv"), fread)
bmrdt = rbindlist(bmrdt, fill=TRUE)

# from stackoverflow
#bmrdt_comp = bmrdt[,.SD[.(iteration=1:50), on="iteration"], by=.(learner_id,
                                                                 #task_id)]
#bmrdt_comp[, rep:=iteration %/% 5]
#bmrdt_imp = copy(bmrdt_comp)
bmrdt = bmrdt[,
    surv.cindex:=nafill(surv.cindex, type="const",
                       fill=mean(surv.cindex, na.rm=T)),
    by=.(learner_id)
]
#bmrdt = bmrdt[
    #is.infinite(surv.graf),
    #surv.graf:=1
#]
#bmrdt = bmrdt[,
    #surv.graf:=nafill(surv.graf, type="const",
                       #fill=mean(surv.cindex, na.rm=T)),
    #by=.(learner_id)
#]
order_learner = c(
    "kaplan",
    "clin_ref",
    "glmnet_clin",
    "lasso",
    "ipflasso",
    "prioritylasso",
    "prioritylasso_clin_fav",
    "ranger",
    "blockforest",
    "rgcca_unsup_clin_fav_n1_tau0.1",
    "rgcca_unsup_clin_fav_n1_tau1",
    "rgcca_unsup_clin_fav_n3_tau0.1",
    "rgcca_unsup_clin_fav_n3_tau1",
    "rgcca_sup_clin_fav_n1_tau0.1",
    "rgcca_sup_clin_fav_n1_tau1",
    "rgcca_sup_clin_fav_n3_tau0.1",
    "rgcca_sup_clin_fav_n3_tau1",
    "jive_clin_fav_j1_a1",
    "jive_clin_fav_j3_a1",
    "jive_clin_fav_j1_a3",
    "jive_clin_fav_j3_a3",
    "jive_clin_fav_j1_a0",
    "jive_clin_fav_j3_a0",
    "jive_clin_fav_j0_a1",
    "jive_clin_fav_j0_a3"
)
order_task = c(
    "LAML",
    "ESCA",
    "PAAD",
    "SARC",
    "LIHC",
    "COAD",
    "KIRP",
    "OV",
    "KIRC",
    "SKCM",
    "STAD",
    "BLCA",
    "UCEC",
    "LGG",
    "HNSC",
    "LUSC",
    "LUAD",
    "BRCA"
)
#bmrdt_imp[,learner_id:=factor(learner_id, levels=order_id)]
bmrdt[,`:=`(learner_id=factor(learner_id, levels=order_learner),
            task_id=factor(task_id, levels=order_task))]
bmrdt[learner_id %in% c("kaplan", "clin_ref", "glmnet_clin"),
      type:="reference"]
bmrdt[learner_id %in% c("ranger", "lasso"),
      type:="anne-laure"]
bmrdt[learner_id %like% "rgcca_sup",
      type:="supervised rgcca"]
bmrdt[learner_id %like% "rgcca_unsup",
      type:="unsupervised rgcca"]
bmrdt[learner_id %like% "jive",
          type:="jive"]
bmrdt[learner_id %like% "[ja]0",
          type:="pca"]
bmrdt[learner_id %like% "lasso",
          type:="AL"]
bmrdt[learner_id %like% "ranger",
          type:="AL"]
bmrdt[learner_id %like% "blockforest",
          type:="AL"]

bmrdt[, 100*sum(is.infinite(surv.graf))/.N, by=learner_id]

#bmrdt[is.infinite(surv.graf), unique(paste0(task_id, "_", rep))]
tab1 = table(bmrdt[is.infinite(surv.graf), .(task_id, iteration)])
tab2 = table(bmrdt[,.(task_id, iteration)])
for (n in colnames(tab2)) {
    if (!(n %in% colnames(tab1))) {
        newcol = matrix(0, ncol=1, nrow=nrow(tab1))
        colnames(newcol) = n
        tab1 = cbind(tab1, newcol)
    }
}
tab1 = tab1[,order(as.numeric(colnames(tab1)))]
tab1/tab2

prop = round(100*tab1/tab2, 1)

table(bmrdt[is.infinite(surv.graf), .(task_id, iteration)]) /
    table(bmrdt[,.(task_id, iteration)])

bmrdt[error!="", round(100*.N/900, 1), learner_id]

bmrdt2 = bmrdt[is.infinite(surv.graf), surv.graf=0.25]

cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
              "#D55E00", "#CC79A7")

yline = median(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
yline2 = mean(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(type=head(type, 1), surv.cindex=mean(surv.cindex)) %>%
    ggplot(aes(x=learner_id, y=surv.cindex, fill=type)) +
    geom_boxplot() +
    #geom_jitter(alpha=0.5, size=0.5) +
    stat_summary(fun=mean, geom="point", colour="red") +
    geom_hline(yintercept=yline, linetype="dashed") +
    #geom_hline(yintercept=yline2, linetype="dashed", color="red") +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
    scale_fill_manual(values=cbPalette)
    ggsave(file="boxplotC.svg", width=16, height=8)

library(ggpubr)

yline = median(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
yline2 = mean(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(type=head(type, 1), surv.cindex=mean(surv.cindex)) %>%
    ggboxplot(x="learner_id", y="surv.cindex", fill="type", palette=cbPalette,
              notch=FALSE, xlab="Learner", ylab="Uno's C-index", shape="learner_id") +
    stat_compare_means(method="wilcox", paired=T, ref.group="clin_ref",
                       label="p.signif") +
    #geom_jitter(alpha=0.5, size=0.5) +
    #stat_summary(fun=mean, geom="point", colour="red") +
    geom_hline(yintercept=yline, linetype="dashed") +
    #geom_hline(yintercept=yline2, linetype="dashed", color="red") +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    ggsave(file="boxplotC.svg", width=16, height=8)

yline = median(
    bmrdt[learner_id=="clin_ref" & !is.infinite(surv.graf),
        mean(surv.graf), by="task_id"]$V1
)
yline2 = mean(
    bmrdt[learner_id=="clin_ref" & !is.infinite(surv.graf),
        mean(surv.graf), by="task_id"]$V1
)
bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(type=head(type, 1), surv.graf=mean(surv.graf)) %>%
    ggplot(aes(x=learner_id, y=surv.graf, fill=type)) +
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", colour="red") +
    geom_hline(yintercept=yline, linetype="dashed") +
    #geom_hline(yintercept=yline2, linetype="dashed", color="red") +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
    ggsave(file="boxplotI.svg", width=16, height=8)

bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(surv.cindex=mean(surv.cindex)) %>%
    group_by(learner_id) %>%
    summarize(mean.cindex=mean(surv.cindex), sd.cindex=sd(surv.cindex)) %>%
    print(n=nrow(.))

bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize_all(mean) %>%
    select(learner_id, task_id, surv.cindex, surv.graf) %>%
    filter(task_id=="BRCA")

bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(time_train=mean(time_train, na.rm=T), type=head(type, 1),
              time_predict=mean(time_predict, na.rm=T),
              time_both=mean(time_both, na.rm=T)) %>%
    ggplot(aes(x=task_id, y=time_train, color=learner_id, shape=type)) +
    geom_point() +
    geom_line(aes(group=learner_id, linetype=type)) +
    scale_y_log10()
    ggsave(file="time_all.svg", width=12, height=8)

bmrdt %>%
    mutate(type=ifelse(grepl("rgcca", type), "rgcca", type)) %>%
    group_by(learner_id, task_id) %>%
    summarize(time_train=mean(time_train, na.rm=T), type=head(type, 1),
              time_predict=mean(time_predict, na.rm=T),
              time_both=mean(time_both, na.rm=T)) %>%
    ggplot(aes(x=task_id, y=time_train, color=type)) +
    geom_point() +
    geom_line(aes(group=learner_id)) +
    scale_y_log10()
    ggsave(file="time_all.svg", width=12, height=8)

bmrdt %>%
    filter(type!="reference") %>%
    group_by(learner_id, task_id) %>%
    summarize(time_train=mean(time_train, na.rm=T), type=head(type, 1),
              time_predict=mean(time_predict, na.rm=T),
              time_both=mean(time_both, na.rm=T)) %>%
    ggplot(aes(x=task_id, y=time_predict, color=learner_id, shape=type)) +
    geom_point() +
    geom_line(aes(group=learner_id, linetype=type))
    ggsave(file="time_pred.svg", width=12, height=8)

bmrdt %>%
    filter(grepl("rgcca", learner_id)) %>%
    group_by(learner_id, task_id, type) %>%
    summarize(time_train=mean(time_train, na.rm=T)) %>%
    ggplot(aes(x=task_id, y=time_train, color=learner_id)) +
    geom_point() +
    geom_line(aes(group=learner_id))
    ggsave(file="time_rgcca.svg", width=12, height=8)

bmrdt %>%
    filter(grepl("jive", learner_id)) %>%
    group_by(learner_id, task_id, type) %>%
    summarize(time_train=mean(time_train, na.rm=T)) %>%
    ggplot(aes(x=task_id, y=time_train, color=learner_id)) +
    geom_point() +
    geom_line(aes(group=learner_id))
    ggsave(file="time_jive.svg", width=12, height=8)

bmrdt %>%
    filter(type=="AL") %>%
    group_by(learner_id, task_id, type) %>%
    summarize(time_train=mean(time_train, na.rm=T)) %>%
    ggplot(aes(x=task_id, y=time_train, color=learner_id)) +
    geom_point() +
    geom_line(aes(group=learner_id))
    ggsave(file="time_AL.svg", width=12, height=8)

bmrdt %>%
    group_by(learner_id) %>%
    summarize(time_train=mean(time_train, na.rm=T)/3600) %>%
    print(n=nrow(.))

sum(bmrdt$time_both, na.rm=T)/(3600) # hours
sum(bmrdt$time_both, na.rm=T)/(24*3600) # days

tot_time = sum(bmrdt$time_both, na.rm=T)
bmrdt[, .(prop_time=100*round(sum(time_both, na.rm=T)/tot_time, 3)), learner_id]

# ranks
bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(surv.cindex=mean(surv.cindex)) %>%
    arrange(task_id, desc(surv.cindex), desc=T) %>%
    group_by(task_id) %>% top_n(1)

bmrdt %>%
    select(task_id, learner_id, surv.cindex) %>%
    group_by(learner_id, task_id) %>%
    summarize(surv.cindex=mean(surv.cindex)) %>%
    arrange(task_id, desc(surv.cindex), desc=T) %>%
    group_by(task_id) %>%
    mutate(rank=row_number()) %>%
    filter(learner_id=="clin_ref")

bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(surv.cindex=mean(surv.cindex)) %>%
    arrange(task_id, desc(surv.cindex), desc=T) %>%
    group_by(task_id) %>%
    mutate(rank=row_number()) %>%
    group_by(learner_id) %>%
    summarize(rank=mean(rank), surv.cindex=mean(surv.cindex)) %>%
    arrange(rank)

bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(surv.cindex=mean(surv.cindex)) %>%
    arrange(task_id, desc(surv.cindex), desc=T) %>%
    group_by(task_id) %>%
    mutate(rank=row_number()) %>%
    group_by(learner_id) %>%
    summarize(rank=median(rank), surv.cindex=mean(surv.cindex)) %>%
    arrange(rank)

library(scmamp)

bmrdt %>%
    #filter(!learner_id %in% c("glmnet_clin", "clin_ref", "kaplan")) %>%
    group_by(learner_id, task_id) %>%
    summarize(surv.cindex=mean(surv.cindex)) %>%
    arrange(task_id, desc(surv.cindex), desc=T) %>%
    group_by(task_id) %>%
    mutate(rank=row_number()) %>%
    group_by(learner_id) %>%
    summarize(rank=mean(rank), surv.cindex=mean(surv.cindex)) %>%
    arrange(rank) %>%
    print(n=nrow(.))

comparisonMatrix = bmrdt %>%
    #filter(!learner_id %in% c("glmnet_clin", "clin_ref", "kaplan")) %>%
    filter(grepl("_clin_fav", learner_id) |
        learner_id %in% c("kaplan", "clin_ref", "glmnet_clin")) %>%
    filter(!is.na(learner_id)) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    mutate(surv.graf=-1*surv.graf) %>%
    group_by(learner_id, task_id) %>%
    summarize(surv.cindex=mean(surv.cindex), surv.graf=mean(surv.graf)) %>%
    select(task_id, learner_id, surv.cindex) %>%
    pivot_wider(names_from=learner_id, values_from=surv.cindex) %>%
    select(-task_id) %>%
    as.matrix()
test = nemenyiTest(comparisonMatrix, alpha=0.05)
plotCD(comparisonMatrix, alpha=0.05, cex=0.75)

comparisonMatrix = bmrdt %>%
    #filter(!learner_id %in% c("glmnet_clin", "clin_ref", "kaplan")) %>%
    filter(!grepl("_clin_fav", learner_id) |
        learner_id %in% c("kaplan", "clin_ref", "glmnet_clin")) %>%
    filter(!is.na(learner_id)) %>%
    mutate(surv.graf=-1*surv.graf) %>%
    group_by(learner_id, task_id) %>%
    summarize(surv.cindex=mean(surv.cindex), surv.graf=mean(surv.graf)) %>%
    select(task_id, learner_id, surv.cindex) %>%
    pivot_wider(names_from=learner_id, values_from=surv.cindex) %>%
    select(-task_id) %>%
    as.matrix()
test = nemenyiTest(comparisonMatrix, alpha=0.05)
plotCD(comparisonMatrix, alpha=0.05, cex=0.75)

plotDensities(comparisonMatrix[,-c(1)])

comparisonMatrix = bmrdt %>%
    mutate(surv.graf=-1*surv.graf) %>%
    select(task_id, iteration, learner_id, surv.graf) %>%
    pivot_wider(names_from=learner_id, values_from=surv.graf) %>%
    select(-c(task_id, iteration)) %>%
    as.matrix()
test = nemenyiTest(comparisonMatrix, alpha=0.05)
plotCD(comparisonMatrix, alpha=0.05, cex=1.25)

pv.matrix = quadePost(comparisonMatrix, control=NULL)
pv.matrix.adj = adjustShaffer(pv.matrix)

pv.matrix.adj %>%
    as.data.frame() %>%
    rownames_to_column("learner1") %>%
    pivot_longer(-c(learner1), names_to="learner2", values_to="pv") %>%
    mutate(pv = (pv<=0.05)) %>%
    ggplot(aes(x=learner1, y=learner2, fill=pv)) +
    geom_tile()

pv = quadePost(comparisonMatrix, control=2)

#bmrdt_imp %>%
    #select(task_id, learner_id, surv.cindex, rep) %>%
    #group_by(learner_id, task_id, rep) %>%
    #summarize_all(mean) %>%
    #group_by(learner_id, task_id) %>%
    #summarize(surv.cindex=mean(surv.cindex)) %>%
    #arrange(task_id, desc(surv.cindex), desc=T) %>%
    #group_by(task_id) %>%
    #mutate(rank=row_number()) %>%
    #group_by(learner_id) %>%
    #summarize(rank=mean(rank), surv.cindex=mean(surv.cindex)) %>%
    #arrange(rank)

registry_name <- c("ref", "AL_woClin", "PCA_woClin", "RGCCA_woClin", "JIVE_woClin")
#ncpus <- 1

data.table::setDTthreads(ncpus)

#bmrdt = fread(paste0(registry_name, "_results.csv"))
bmrdt = lapply(paste0("results/", registry_name, "/", registry_name,
                      "_results.csv"), fread)
bmrdt = rbindlist(bmrdt, fill=TRUE)

# from stackoverflow
#bmrdt_comp = bmrdt[,.SD[.(iteration=1:50), on="iteration"], by=.(learner_id,
                                                                 #task_id)]
#bmrdt_comp[, rep:=iteration %/% 5]
#bmrdt_imp = copy(bmrdt_comp)
bmrdt = bmrdt[,
    surv.cindex:=nafill(surv.cindex, type="const",
                       fill=mean(surv.cindex, na.rm=T)),
    by=.(learner_id)
]
#bmrdt = bmrdt[
    #is.infinite(surv.graf),
    #surv.graf:=1
#]
#bmrdt = bmrdt[,
    #surv.graf:=nafill(surv.graf, type="const",
                       #fill=mean(surv.cindex, na.rm=T)),
    #by=.(learner_id)
#]
order_learner = c(
    "kaplan",
    "clin_ref",
    "glmnet_clin",
    "lasso",
    "ranger",
    "rgcca_unsup_n1_tau0.1",
    "rgcca_unsup_n1_tau1",
    "rgcca_unsup_n3_tau0.1",
    "rgcca_unsup_n3_tau1",
    "rgcca_sup_n1_tau0.1",
    "rgcca_sup_n1_tau1",
    "rgcca_sup_n3_tau0.1",
    "rgcca_sup_n3_tau1",
    "jive_j1_a1",
    "jive_j3_a1",
    "jive_j1_a3",
    "jive_j3_a3",
    "jive_j1_a0",
    "jive_j3_a0",
    "jive_j0_a1",
    "jive_j0_a3"
)
order_task = c(
    "LAML",
    "ESCA",
    "PAAD",
    "SARC",
    "LIHC",
    "COAD",
    "KIRP",
    "OV",
    "KIRC",
    "SKCM",
    "STAD",
    "BLCA",
    "UCEC",
    "LGG",
    "HNSC",
    "LUSC",
    "LUAD",
    "BRCA"
)
#bmrdt_imp[,learner_id:=factor(learner_id, levels=order_id)]
bmrdt[,`:=`(learner_id=factor(learner_id, levels=order_learner),
            task_id=factor(task_id, levels=order_task))]
bmrdt[learner_id %in% c("kaplan", "clin_ref", "glmnet_clin"),
      type:="reference"]
bmrdt[learner_id %in% c("ranger", "lasso"),
      type:="anne-laure"]
bmrdt[learner_id %like% "rgcca_sup",
      type:="supervised rgcca"]
bmrdt[learner_id %like% "rgcca_unsup",
      type:="unsupervised rgcca"]
bmrdt[learner_id %like% "jive",
          type:="jive"]
bmrdt[learner_id %like% "[ja]0",
          type:="pca"]
bmrdt[learner_id %like% "lasso",
          type:="AL"]
bmrdt[learner_id %like% "ranger",
          type:="AL"]
bmrdt[learner_id %like% "blockforest",
          type:="AL"]

bmrdt[, 100*sum(is.infinite(surv.graf))/.N, by=learner_id]

#bmrdt[is.infinite(surv.graf), unique(paste0(task_id, "_", rep))]
tab1 = table(bmrdt[is.infinite(surv.graf), .(task_id, iteration)])
tab2 = table(bmrdt[,.(task_id, iteration)])
for (n in colnames(tab2)) {
    if (!(n %in% colnames(tab1))) {
        newcol = matrix(0, ncol=1, nrow=nrow(tab1))
        colnames(newcol) = n
        tab1 = cbind(tab1, newcol)
    }
}
tab1 = tab1[,order(as.numeric(colnames(tab1)))]
tab1/tab2

prop = round(100*tab1/tab2, 1)

table(bmrdt[is.infinite(surv.graf), .(task_id, iteration)]) /
    table(bmrdt[,.(task_id, iteration)])

bmrdt[error!="", round(100*.N/900, 1), learner_id]

bmrdt2 = bmrdt[is.infinite(surv.graf), surv.graf=0.25]

yline = median(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
yline2 = mean(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
bmrdt %>%
    filter(!is.na(learner_id)) %>%
    group_by(learner_id, task_id) %>%
    summarize(type=head(type, 1), surv.cindex=mean(surv.cindex)) %>%
    ggplot(aes(x=learner_id, y=surv.cindex, fill=type)) +
    geom_boxplot() +
    #geom_jitter(alpha=0.5, size=0.5) +
    stat_summary(fun=mean, geom="point", colour="red") +
    geom_hline(yintercept=yline, linetype="dashed") +
    #geom_hline(yintercept=yline2, linetype="dashed", color="red") +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
    ggsave(file="boxplotC.svg", width=16, height=8)

yline = median(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
yline2 = mean(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(type=head(type, 1), surv.cindex=mean(surv.cindex)) %>%
    ggplot(aes(x=learner_id, y=surv.cindex, fill=type)) +
    geom_boxplot() +
    #geom_jitter(alpha=0.5, size=0.5) +
    stat_summary(fun=mean, geom="point", colour="red") +
    geom_hline(yintercept=yline, linetype="dashed") +
    #geom_hline(yintercept=yline2, linetype="dashed", color="red") +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
    ggsave(file="boxplotC.svg", width=16, height=8)

yline = median(
    bmrdt[learner_id=="clin_ref" & !is.infinite(surv.graf),
        mean(surv.graf), by="task_id"]$V1
)
yline2 = mean(
    bmrdt[learner_id=="clin_ref" & !is.infinite(surv.graf),
        mean(surv.graf), by="task_id"]$V1
)
bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(type=head(type, 1), surv.graf=mean(surv.graf)) %>%
    ggplot(aes(x=learner_id, y=surv.graf, fill=type)) +
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", colour="red") +
    geom_hline(yintercept=yline, linetype="dashed") +
    #geom_hline(yintercept=yline2, linetype="dashed", color="red") +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
    ggsave(file="boxplotI.svg", width=16, height=8)

library(data.table)
library(tidyverse)

#registry_name <- c("ref", "AL", "PriorLasso", "RGCCA", "JIVE", "PCA")
registry_name <- c("ref", "PCA_woClin", "PCA", "RGCCA", "RGCCA_woClin")
#ncpus <- 1

data.table::setDTthreads(ncpus)

#bmrdt = fread(paste0(registry_name, "_results.csv"))
bmrdt = lapply(paste0("results/", registry_name, "/", registry_name, "_results.csv"), fread)
bmrdt = rbindlist(bmrdt, fill=TRUE)

# from stackoverflow
#bmrdt_comp = bmrdt[,.SD[.(iteration=1:50), on="iteration"], by=.(learner_id,
                                                                 #task_id)]
#bmrdt_comp[, rep:=iteration %/% 5]
#bmrdt_imp = copy(bmrdt_comp)
bmrdt = bmrdt[,
    surv.cindex:=nafill(surv.cindex, type="const",
                       fill=mean(surv.cindex, na.rm=T)),
    by=.(learner_id)
]
#bmrdt = bmrdt[
    #is.infinite(surv.graf),
    #surv.graf:=1
#]
#bmrdt = bmrdt[,
    #surv.graf:=nafill(surv.graf, type="const",
                       #fill=mean(surv.cindex, na.rm=T)),
    #by=.(learner_id)
#]
order_learner = c(
    "kaplan",
    "clin_ref",
    "glmnet_clin",
    "lasso",
    "ipflasso",
    "prioritylasso",
    "prioritylasso_clin_fav",
    "ranger",
    "blockforest",
    "rgcca_unsup_clin_fav_n1_tau0.1",
    "rgcca_unsup_clin_fav_n1_tau1",
    "rgcca_unsup_clin_fav_n3_tau0.1",
    "rgcca_unsup_clin_fav_n3_tau1",
    "rgcca_sup_clin_fav_n1_tau0.1",
    "rgcca_sup_clin_fav_n1_tau1",
    "rgcca_sup_clin_fav_n3_tau0.1",
    "rgcca_sup_clin_fav_n3_tau1",
    "jive_clin_fav_j1_a1",
    "jive_clin_fav_j3_a1",
    "jive_clin_fav_j1_a3",
    "jive_clin_fav_j3_a3",
    "jive_clin_fav_j1_a0",
    "jive_clin_fav_j3_a0",
    "jive_clin_fav_j0_a1",
    "jive_clin_fav_j0_a3",
    "rgcca_unsup_n1_tau0.1",
    "rgcca_unsup_n1_tau1",
    "rgcca_unsup_n3_tau0.1",
    "rgcca_unsup_n3_tau1",
    "rgcca_sup_n1_tau0.1",
    "rgcca_sup_n1_tau1",
    "rgcca_sup_n3_tau0.1",
    "rgcca_sup_n3_tau1",
    "jive_j1_a1",
    "jive_j3_a1",
    "jive_j1_a3",
    "jive_j3_a3",
    "jive_j1_a0",
    "jive_j3_a0",
    "jive_j0_a1",
    "jive_j0_a3"
)
order_task = c(
    "LAML",
    "ESCA",
    "PAAD",
    "SARC",
    "LIHC",
    "COAD",
    "KIRP",
    "OV",
    "KIRC",
    "SKCM",
    "STAD",
    "BLCA",
    "UCEC",
    "LGG",
    "HNSC",
    "LUSC",
    "LUAD",
    "BRCA"
)
#bmrdt_imp[,learner_id:=factor(learner_id, levels=order_id)]
bmrdt[,`:=`(learner_id=factor(learner_id, levels=order_learner),
            task_id=factor(task_id, levels=order_task))]
bmrdt[learner_id %in% c("kaplan", "clin_ref", "glmnet_clin"),
      type:="reference"]
bmrdt[learner_id %in% c("ranger", "lasso"),
      type:="anne-laure"]
bmrdt[learner_id %like% "rgcca_sup",
      type:="supervised rgcca"]
bmrdt[learner_id %like% "rgcca_unsup",
      type:="unsupervised rgcca"]
bmrdt[learner_id %like% "jive",
          type:="jive"]
bmrdt[learner_id %like% "[ja]0",
          type:="pca"]
bmrdt[learner_id %like% "lasso",
          type:="AL"]
bmrdt[learner_id %like% "ranger",
          type:="AL"]
bmrdt[learner_id %like% "blockforest",
          type:="AL"]

bmrdt[, 100*sum(is.infinite(surv.graf))/.N, by=learner_id]

#bmrdt[is.infinite(surv.graf), unique(paste0(task_id, "_", rep))]
tab1 = table(bmrdt[is.infinite(surv.graf), .(task_id, iteration)])
tab2 = table(bmrdt[,.(task_id, iteration)])
for (n in colnames(tab2)) {
    if (!(n %in% colnames(tab1))) {
        newcol = matrix(0, ncol=1, nrow=nrow(tab1))
        colnames(newcol) = n
        tab1 = cbind(tab1, newcol)
    }
}
tab1 = tab1[,order(as.numeric(colnames(tab1)))]
tab1/tab2

prop = round(100*tab1/tab2, 1)

table(bmrdt[is.infinite(surv.graf), .(task_id, iteration)]) /
    table(bmrdt[,.(task_id, iteration)])

bmrdt[error!="", round(100*.N/900, 1), learner_id]

bmrdt2 = bmrdt[is.infinite(surv.graf), surv.graf=0.25]

cbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
              "#D55E00", "#CC79A7")

yline = median(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
yline2 = mean(
    bmrdt[learner_id=="clin_ref", mean(surv.cindex), by="task_id"]$V1
)
bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(type=head(type, 1), surv.cindex=mean(surv.cindex)) %>%
    ggboxplot(x="learner_id", y="surv.cindex", fill="type", palette=cbPalette,
              notch=FALSE, xlab="Learner", ylab="Uno's C-index") +
    stat_compare_means(method="wilcox", paired=T, ref.group="clin_ref",
                       label="p.signif") +
    #stat_summary(fun=mean, geom="point", colour="red") +
    geom_hline(yintercept=yline, linetype="dashed") +
    #geom_hline(yintercept=yline2, linetype="dashed", color="red") +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

bmrdt %>%
    group_by(learner_id, task_id) %>%
    summarize(type=head(type, 1), surv.rcll=mean(surv.rcll)) %>%
    ggplot(aes(x=learner_id, y=surv.rcll, fill=type)) +
    geom_boxplot() +
    #geom_jitter(alpha=0.5, size=0.5) +
    stat_summary(fun=mean, geom="point", colour="red") +
    #geom_hline(yintercept=yline, linetype="dashed") +
    #geom_hline(yintercept=yline2, linetype="dashed", color="red") +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
    scale_fill_manual(values=cbPalette)
