library(tidyverse)
library(ggthemes)
library(ggrepel)
library(gt)
library(ggthemes)
library(ggpattern)
library(patchwork)
library(ggpmisc)
library(ggdendro)
library(Cairo)
library(paletteer)
library(ggokabeito)

measures = c("cindex_p0.9", "graf_p0.9")
aggr = median

method_order = c(
    "Kaplan-Meier",
    "Cox clin.",
    "Cox L2 clin.",
    "LASSO",
    "ranger",
    "IPF-LASSO",
    "Priority-LASSO",
    "Priority-LASSO clin. fav.",
    "BlockForest",
    "EI-PCA",
    "LI-PCA",
    "JIVE",
    "sJIVE",
    "IntNMF",
    "iCluster",
    "MOFA+MOTL",
    "tPCA",
    "tPCA+tFOBI",
    "RGCCA Unsup.",
    "SGCCA Unsup.",
    "RGCCA Sup.",
    "SGCCA Sup.",
    "MCIA")
type_order = c("Reference",
  "PS - Naive",
  "PS - Structured",
  "PCA",
  "JDR - MF",
  "JDR - TF",
  "JDR - Cov")
cancer_order = c(
"LAML", "ESCA", "PAAD", "SARC", "LIHC", "KIRP", "COAD", "OV", "KIRC", "SKCM",
"STAD", "BLCA", "UCEC", "LUSC", "LGG", "LUAD", "HNSC", "BRCA"
)
cancer_info = data.frame(
    n_patients=c(382,735,191,106,443,249,167,35,419,159,426,418,219,124,126,249,295,405),
    task_id=c("BLCA","BRCA","COAD","ESCA","HNSC","KIRC","KIRP","LAML","LGG","LIHC","LUAD","LUSC","OV","PAAD","SARC","SKCM","STAD","UCEC"),
    n_death=c(103,72,17,37,152,62,20,14,77,35,101,132,109,52,38,87,62,38)
) %>%
    mutate(n_cens=n_patients-n_death) %>%
    mutate(p_cens=n_cens/n_patients)

cbPalette <- c(
  Reference="#999999",
  `PS - Naive`="#CC79A7",
  `PS - Structured`="#D55E00",
  PCA="#56B4E9",
  `JDR - MF`="#009E73",
  `JDR - TF`="#F0E442",
  `JDR - Cov`="#0072B2"
)

coef = read_csv("results/coef.csv")
bmr = read_csv("results/results_cv_clean.csv") %>%
  filter(method_name != "MCIA Ntest") %>% # select the best perform MCIA methods
  mutate(method_name=ifelse(method_name=="MCIA Ntrain", "MCIA", method_name)) %>% # rename it simply MCIA
  mutate(method_name=ifelse(method_name=="PCA Joint", "EI-PCA", method_name)) %>%
  mutate(method_name=ifelse(method_name=="PCA Indiv.", "LI-PCA", method_name)) %>%
  mutate(method_type=case_when(
    method_type=="Previous study - Naive" ~ "PS - Naive",
    method_type=="Previous study - Structured" ~ "PS - Structured" ,
    method_type=="JDR - Matrix Factorization" ~ "JDR - MF" ,
    method_type=="JDR - Tensor Factorization" ~ "JDR - TF" ,
    method_type=="JDR - Covariance Based" ~ "JDR - Cov" ,
    .default=method_type
  )) %>%
  mutate(method_type=factor(method_type)) %>%
  mutate(method_type=forcats::fct_relevel(method_type, type_order)) %>%
  mutate(method_name=factor(method_name)) %>%
  mutate(method_name=forcats::fct_relevel(method_name, method_order)) %>%
  mutate(task_id=task_id) %>%
  mutate(task_id=forcats::fct_relevel(task_id, cancer_order))

bmr_perf = bmr %>%
  pivot_longer(all_of(measures), names_to="measure", values_to="value") %>%
  select(clin, task_id, iteration, cv_id, method_name, method_type, measure, value)

# loading computing times
bmr_time = read_csv("results/results_aggregated_imputed.csv") %>%
  filter(method_name != "MCIA Ntest") %>%
  mutate(method_name=ifelse(method_name=="MCIA Ntrain", "MCIA", method_name)) %>%
  mutate(method_name=ifelse(method_name=="PCA Joint", "EI-PCA", method_name)) %>%
  mutate(method_name=ifelse(method_name=="PCA Indiv.", "LI-PCA", method_name)) %>%
  select(clin, task_id, iteration, learner_id, method_name, method_type, time_both) %>%
  mutate(method_type=case_when(
    method_type=="Previous study - Naive" ~ "PS - Naive",
    method_type=="Previous study - Structured" ~ "PS - Structured" ,
    method_type=="JDR - Matrix Factorization" ~ "JDR - MF" ,
    method_type=="JDR - Tensor Factorization" ~ "JDR - TF" ,
    method_type=="JDR - Covariance Based" ~ "JDR - Cov" ,
    .default=method_type
  )) %>%
  mutate(method_type=factor(method_type)) %>%
  mutate(method_type=forcats::fct_relevel(method_type, type_order)) %>%
  mutate(method_name=factor(method_name)) %>%
  mutate(method_name=forcats::fct_relevel(method_name, method_order)) %>%
  mutate(task_id=task_id) %>%
  mutate(task_id=forcats::fct_relevel(task_id, cancer_order))

ref = "cox_ref"
# computing ref performance
bmr_for_test = bmr_perf %>%
  left_join(bmr_perf %>%
    filter(cv_id==ref) %>%
    select(task_id, iteration, measure, value),
    by=c("task_id", "iteration", "measure"),
    suffix=c("", "_ref"))

test_func = wilcox.test # function for testing perf difference
pval_by_learner = bmr_for_test %>%
  group_by(cv_id, task_id, measure) %>%
  summarise(across(starts_with("value"), \(x) aggr(x, na.rm=TRUE))) %>%
  ungroup() %>%
  group_by(cv_id, measure) %>%
  summarise(
    data.frame(test_func(value, value_ref, paired=TRUE)[c("statistic", "p.value", "method")], n=n())
  ) %>%
  ungroup() %>%
  mutate(expected=case_when(
    grepl("Wilcoxon", method) ~ n*(n+1)/4,
    .default=0
  )) %>%
  mutate(statistic=statistic-expected) %>%
  mutate(p_adj=p.adjust(p.value, method="BH"))

# the big boxplot 
measure.labs = c("Uno's C-Index", "Integrated Brier Score (IBS)")
names(measure.labs) = measures
clin.labs = c("Reference", "With clinical data", "Without clinical data")
names(clin.labs) = c("Reference", "With clinical data", "Without clinical data")
pval_height = data.frame(measure=measures, pval_height=c(0.83, 0.265))
bmr_for_boxplot = bmr_perf %>%
  group_by(cv_id, clin, task_id, method_name, method_type, measure) %>%
  summarise(value=aggr(value, na.rm=FALSE)) %>%
  ungroup() %>%
  left_join(pval_by_learner %>%
    #mutate(signif=ifelse(p_adj<=0.05, "*", "")) %>%
    mutate(signif=case_when(
      p_adj<=0.05 & statistic>0 ~ "\u2191",
      p_adj<=0.05 & statistic<0 ~ "\u2193",
      .default=""
    )) %>%
    mutate(statistic=ifelse(grepl("graf", measure), -1*statistic, statistic)) %>%
    mutate(dir=ifelse(statistic>0, "Better than reference", "Worse than reference")) %>%
    select(cv_id, measure, signif, dir)) %>%
  mutate(supervised=case_when(
    grepl("Sup|sJIVE", method_name) ~ "supervised",
    .default="unsupervised"
  ))
yline = bmr_for_boxplot %>%
  filter(cv_id=="cox_ref") %>%
  group_by(measure) %>%
  summarise(yline=median(value))
bmr_for_boxplot %>%
  left_join(yline) %>%
  left_join(pval_height) %>%
  ggplot(aes(x=method_name, y=value)) +
  geom_boxplot(aes(fill=method_type)) +
  geom_boxplot_pattern(
    aes(pattern=supervised),
    filter(bmr_for_boxplot, supervised=="supervised"),
    fill=NA,
    pattern_fill="black",
    pattern_density=0.15,
    pattern_spacing=0.03
  ) +
  geom_hline(aes(yintercept=yline), linetype="dashed") +
  geom_text(aes(y=pval_height, label=signif, color=factor(dir)),
      size=4) +
  facet_grid(measure~clin, scales="free", space="free_x",
      labeller=labeller(measure=measure.labs, clin=clin.labs)) +
  scale_x_discrete(guide=guide_axis(angle=45)) +
  scale_fill_manual(values=cbPalette, guide=guide_legend(nrow=1)) +
  scale_color_manual(values=c("Better than reference"="forestgreen",
      "Worse than reference"="red"), guide="none") +
  scale_pattern_manual(values=c("circle"), guide="none") +
  theme_bw(base_size=9) +
  theme(
    legend.position="top",
  ) +
  xlab("Method name") + ylab("Metric") + labs(fill="Method type")

ggsave("1_aggr_perf.pdf", width=180, height=150, units="mm",
  device=cairo_pdf)

# boxplot for each cancer, non aggregated
for (c in cancer_order) {
  message(c)
  bmr_tmp = bmr_perf %>%
    filter(task_id==c) %>%
    mutate(supervised=case_when(
      grepl("Sup|sJIVE", method_name) ~ "supervised",
      .default="unsupervised"
    ))
  yline = bmr_tmp %>%
    filter(cv_id=="cox_ref") %>%
    group_by(measure) %>%
    summarise(yline=median(value))
  bxplt_tmp = bmr_tmp %>%
    left_join(yline) %>%
    ggplot(aes(x=method_name, y=value)) +
    geom_boxplot(aes(fill=method_type)) +
    geom_boxplot_pattern(
      aes(pattern=supervised),
      filter(bmr_for_boxplot, supervised=="supervised"),
      fill=NA,
      pattern_fill="black",
      pattern_density=0.15,
      pattern_spacing=0.03
    ) +
    geom_hline(aes(yintercept=yline), linetype="dashed") +
    facet_grid(measure~clin, scales="free", space="free_x",
        labeller=labeller(measure=measure.labs, clin=clin.labs)) +
    scale_x_discrete(guide=guide_axis(angle=45)) +
    scale_fill_manual(values=cbPalette, guide=guide_legend(nrow=1)) +
    scale_pattern_manual(values=c("circle"), guide="none") +
    theme_bw(base_size=9) +
    theme(
      legend.position="top",
    ) +
    xlab("Method name") + ylab("Metric") + labs(fill="Method type")
    if (c %in% c("ESCA", "PAAD", "SARC")) # effect is hard to see due to outliers for these cancers
      bxplt_tmp = bxplt_tmp + scale_y_log10() + ylab("Metric (log scale)")
    ggsave(sprintf("plots/%02d_%s_boxplot.pdf", which(cancer_order==c), c),
      bxplt_tmp,
      width=180, height=150, units="mm", device=cairo_pdf)
}


# heatmap of relative difference
combs = data.frame(
  measure=paste0(rep(c("cindex", "graf"), each=2), "_p0.9"),
  clin=rep(c("With clinical data", "Without clinical data"), 2)
)
bmr_for_heatmap = bmr_for_boxplot %>% # difference of median
  filter(!grepl("ref", cv_id)) %>%
  filter(method_name!="IPF-LASSO") %>%
  select(clin, task_id, method_name, method_type, measure, value) %>%
  left_join(bmr_for_boxplot %>%
    filter(method_name=="Cox clin.") %>%
    select(task_id, measure, value) %>%
    rename(value_ref=value)
  ) %>%
  mutate(diff=(value-value_ref)/value_ref) %>%
  select(clin, task_id, measure, method_name, diff)
limits_df = bmr_for_heatmap %>%
  group_by(measure) %>%
  summarize(inf=min(diff), sup=max(diff)) %>%
  mutate(lim=purrr::map2_dbl(abs(inf), sup, max)) %>%
  ungroup() %>%
  select(-inf, -sup)
limits_vec = pull(limits_df, lim)
names(limits_vec) = pull(limits_df, measure)
htmps = list()
dendro_method = list()
dendro_cancer = list()
ann_method = list()
composites=list()
font = "sans"
dist_method="euclidean"
library(ggdendro)
for(i in 1:nrow(combs)) {
  bmr_tmp = bmr_for_heatmap %>%
    filter(measure==combs[i,]$measure, clin==combs[i,]$clin)
  limits = limits_vec[combs[i,]$measure] * c(-1, 1)
  # Clustering of methods
  mat = bmr_tmp %>%
    select(-clin, -measure) %>%
    pivot_wider(names_from="task_id", values_from="diff") 
  methods = mat$method_name
  mat = as.matrix(select(mat, -method_name))
  rownames(mat) = methods
  n = length(methods)
  clust_method = hclust(dist(mat, method="euclidean"), method="ward.D2")
  ddata_method = ggdendro::dendro_data(as.dendrogram(clust_method, type="rectangle"))
  dendro_method[[i]] = ggplot() +
    geom_segment(
      data=ggdendro::segment(ddata_method),
      aes(x=x, y=y, xend=xend, yend=yend)
    ) +
    #geom_text(
    #  data=ggdendro::label(ddata_method),
    #  aes(x=x, y=0, label=label),
    #  size=4, family=font, color="#444444", vjust=0.5, angle=90, hjust=1
    #) +
    scale_x_continuous(limits=c(0, n+1), expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    ggdendro::theme_dendro() +
    theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      plot.margin=unit(c(0, 0, 0.5, 0), "mm")
    ) +
    coord_cartesian(clip="off")
  # Clustering of cancers
  clust_cancer = hclust(dist(t(mat), method="euclidean"), method="ward.D2")
  ddata_cancer = ggdendro::dendro_data(as.dendrogram(clust_cancer, type="rectangle"))
  dendro_cancer[[i]] = ggplot() +
    geom_segment(
      data=ggdendro::segment(ddata_cancer),
      aes(x=x, y=y, xend=xend, yend=yend),
      position=position_nudge(x=-0.5)
    ) +
    coord_flip(clip="off") +
    scale_y_reverse() +
    scale_x_continuous(limits=c(0, ncol(mat)), expand=c(0, 0)) +
    theme_dendro() +
    theme(plot.margin=unit(c(0, 0, 0, 0), "mm"))
  # annotation for methods
  ann_method[[i]] = bmr_for_boxplot %>%
    filter(measure==combs[i,]$measure, clin==combs[i,]$clin) %>%
    select(method_name, method_type) %>%
    unique() %>%
    filter(method_name %in% unique(bmr_for_heatmap$method_name)) %>%
    mutate(method_name=forcats::fct_relevel(method_name, clust_method$labels[clust_method$order])) %>%
    ggplot(aes(x=method_name, y=0)) +
    geom_tile(aes(fill=method_type), color="white") +
    xlab("") + ylab("") +
    scale_x_discrete(continuous.limits=c(0, n+1), expand=c(0, 0)) +
    scale_fill_manual(values=cbPalette) +
    theme_void() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "mm"),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
    )
  # Heatmap
  htmps[[i]] = bmr_tmp %>%
    mutate(task_id=forcats::fct_relevel(task_id, clust_cancer$labels[clust_cancer$order])) %>%
    mutate(method_name=forcats::fct_relevel(method_name, clust_method$labels[clust_method$order])) %>%
    ggplot(aes(x=method_name, y=task_id)) +
    geom_tile(aes(fill=diff), color="grey") +
    scale_x_discrete(continuous.limits=c(0, n+1), expand=c(0, 0)) +
    paletteer::scale_fill_paletteer_c("scico::vik", limit=limits,
      direction=ifelse(combs[i,]$measure=="graf_p0.9", -1, 1)) +
    theme_void() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "mm"),
      axis.text.y = element_text(hjust = 0, family = font, size = 7),
      axis.text.x = element_text(hjust = 0, family = font, size = 7),
      legend.title = element_text(family = font, size = 9, color = "#444444"),
      legend.text = element_text(family = font, size = 7, color = "#444444"),
      #legend.key.height = unit(3, "mm"),
      legend.position = ifelse(combs[i,]$clin=="With clinical data", "none", "right")
    ) +
    guides(
      fill=guide_colorbar(
        title.position="top",
        title=""
      ),
      x=guide_axis(angle=45, position="bottom")
    )
}

layout = "
  QAAAAAAAREEEEEEE
  #BBBBBBB#FFFFFFF
  CDDDDDDDGHHHHHHH
  SIIIIIIITMMMMMMM
  #JJJJJJJ#NNNNNNN
  KLLLLLLLOPPPPPPP
"
annot_size=6
A = ggplot() + annotate("text", x=0, y=0, label="A", fontface=2, size=annot_size) + theme_void()
B = ggplot() + annotate("text", x=0, y=0, label="B", fontface=2, size=annot_size) + theme_void()
C = ggplot() + annotate("text", x=0, y=0, label="C", fontface=2, size=annot_size) + theme_void()
D = ggplot() + annotate("text", x=0, y=0, label="D", fontface=2, size=annot_size) + theme_void()
dendro_method[[1]] + ann_method[[1]] + dendro_cancer[[1]] + htmps[[1]] +
dendro_method[[2]] + ann_method[[2]] + dendro_cancer[[2]] + htmps[[2]] +
dendro_method[[3]] + ann_method[[3]] + dendro_cancer[[3]] + htmps[[3]] +
dendro_method[[4]] + ann_method[[4]] + dendro_cancer[[4]] + htmps[[4]] +
A + B + C + D +
  plot_layout(design=layout, heights=c(0.2, 0.05, 1, 0.2, 0.05, 1)) 
ggsave("4_test.pdf", width=190, height=180, units="mm",
  device=cairo_pdf)

library(patchwork)

# for table of best methods per cancer
best = bmr_perf %>%
  #filter(clin!="Reference") %>%
  filter(!method_name %in% c("Cox L2 clin.", "Kaplan-Meier")) %>%
  group_by(cv_id, clin, task_id, method_name, method_type, measure) %>%
  summarise(value=aggr(value, na.rm=FALSE)) %>%
  ungroup() %>%
  mutate(value=ifelse(measure=="cindex_p0.9", -value, value)) %>%
  group_by(task_id, clin, measure) %>%
  slice_min(value, with_ties=FALSE) %>%
  ungroup() %>%
  mutate(value=ifelse(measure=="cindex_p0.9", -value, value)) %>%
  select(-method_type, -cv_id) %>%
  mutate(clin=sub(" .*$", "", clin)) %>%
  mutate(measure=sub("_p0.9", "", measure)) %>%
  rename(name=method_name) %>%
  pivot_wider(names_from=c(measure, clin), values_from=c(name, value))

# checking %age of improvement
best %>%
  mutate(value_cindex_With=100*(value_cindex_With-value_cindex_Reference)/value_cindex_Reference) %>%
  mutate(value_graf_With=-100*(value_graf_With-value_graf_Reference)/value_graf_Reference) %>%
  mutate(value_cindex_Without=100*(value_cindex_Without-value_cindex_Reference)/value_cindex_Reference) %>%
  mutate(value_graf_Without=-100*(value_graf_Without-value_graf_Reference)/value_graf_Reference) %>%
  select(
    task_id,
    name_cindex_With, 
    value_cindex_With,
    name_graf_With, 
    value_graf_With,
    name_cindex_Without, 
    value_cindex_Without,
    name_graf_Without, 
    value_graf_Without
  )

# Assessing the tie in best methods
bmr_perf %>%
  filter(clin!="Reference") %>%
  group_by(cv_id, clin, task_id, method_name, method_type, measure) %>%
  summarise(value=aggr(value, na.rm=FALSE)) %>%
  ungroup() %>%
  mutate(value=ifelse(measure=="cindex_p0.9", -value, value)) %>%
  group_by(task_id, clin, measure) %>%
  slice_min(value, with_ties=TRUE) %>%
  summarize(n=n()) %>%
  filter(n>1)
# there's only one tie on ESCA, models are so regularized they are equivalent to a Kaplan Meier
# beta have typical values around 1e-36 on ESCA

bmr_perf %>%
  filter(clin!="Reference") %>%
  group_by(cv_id, clin, task_id, method_name, method_type, measure) %>%
  summarise(value=aggr(value, na.rm=FALSE)) %>%
  ungroup() %>%
  mutate(value=ifelse(measure=="cindex_p0.9", -value, value)) %>%
  group_by(task_id, clin, measure) %>%
  slice_min(value, with_ties=TRUE) %>%
  filter(task_id=="ESCA", clin=="Without clinical data", measure=="graf_p0.9")

bmr_perf %>%
  filter(clin!="Reference") %>%
  group_by(cv_id, clin, task_id, method_name, method_type, measure) %>%
  summarise(value=aggr(value, na.rm=FALSE)) %>%
  ungroup() %>%
  mutate(value=ifelse(measure=="cindex_p0.9", -value, value)) %>%
  group_by(task_id, clin, measure) %>%
  slice_min(value, with_ties=TRUE) %>%
  filter(task_id=="ESCA", clin=="Without clinical data", measure=="graf_p0.9") %>%
  pull(value) %>%
  diff()

# folds are perfectly correlated on ESCA - graf
bmr_perf %>%
  filter(
    method_name %in% c("EI-PCA", "MOFA+MOTL"),
    task_id=="ESCA",
    clin=="Without clinical data",
    #measure=="cindex_p0.9"
  ) %>%
  mutate(method_name = ifelse(method_type=="PCA", "jPCA", "MOFA")) %>%
  select(iteration, measure, method_name, value) %>%
  pivot_wider(id_cols=c(iteration, measure) , names_from=method_name, values_from=value) %>%
  ggplot(aes(x=MOFA, y=jPCA)) +
  geom_point() +
  facet_wrap(.~measure, scale="free")

# selected MOFA models
bmr %>%
    filter(grepl("mofa", learner_id)) %>%
    filter(task_id=="ESCA", clin=="Without clinical data") %>%
    select(iteration, learner_id)

# selected jpca models
bmr %>%
    filter(grepl("jpca", learner_id)) %>%
    filter(task_id=="ESCA", clin=="Without clinical data") %>%
    select(iteration, learner_id)

# values of beta for two models
read_csv("results/coef.csv") %>%
  filter(task_id=="ESCA") %>%
  filter(iteration==2, learner_id %in% c("mofa_f5", "jpca_j5")) %>%
  select(task_id, iteration, learner_id, comp, beta)

# Table of best methods per cancer
great_table = best %>%
  select(
    task_id,
    value_cindex_Reference,
    value_graf_Reference,
    name_cindex_With,
    value_cindex_With,
    name_graf_With,
    value_graf_With,
    name_cindex_Without,
    value_cindex_Without,
    name_graf_Without,
    value_graf_Without,
  ) %>%
  gt(rowname_col="task_id") %>%
  tab_stubhead(label="Cancer") %>%
  tab_spanner(label="Uno's C-Index", id="uno_ref",
    columns=c(value_cindex_Reference)) %>%
  tab_spanner(label="Integrated Brier Score", id="graf_ref",
    columns=c(value_graf_Reference)) %>%
  tab_spanner(label="Uno's C-Index", id="uno_with",
    columns=c(name_cindex_With, value_cindex_With)) %>%
  tab_spanner(label="Integrated Brier Score", id="graf_with",
    columns=c(name_graf_With, value_graf_With)) %>%
  tab_spanner(label="Uno's C-Index", id="uno_without",
    columns=c(name_cindex_Without, value_cindex_Without)) %>%
  tab_spanner(label="Integrated Brier Score", id="graf_without",
    columns=c(name_graf_Without, value_graf_Without)) %>%
  tab_spanner(label="Reference",
    columns=c(value_cindex_Reference, value_graf_Reference)) %>%
  tab_spanner(label="With clinical data",
    columns=c(name_cindex_With, value_cindex_With, name_graf_With, value_graf_With)) %>%
  tab_spanner(label="Without clinical data",
    columns=c(name_cindex_Without, value_cindex_Without, name_graf_Without, value_graf_Without)) %>%
  cols_label(
    value_cindex_Reference="Value",
    value_graf_Reference="Value",
    name_cindex_With="Method",
    value_cindex_With="Value",
    name_graf_With="Method",
    value_graf_With="Value",
    name_cindex_Without="Method",
    value_cindex_Without="Value",
    name_graf_Without="Method",
    value_graf_Without="Value",
  ) %>%
  fmt_number(
    n_sigfig=3,
    columns=c(
    value_cindex_Reference,
    value_graf_Reference,
    value_cindex_With,
    value_graf_With,
    value_cindex_Without,
    value_graf_Without
  )) %>%
  cols_align(align="left", columns=c(
    name_cindex_With,
    name_graf_With,
    name_cindex_Without,
    name_graf_Without
  )) %>%
  opt_table_lines(extent="none") %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_body(
      columns=name_cindex_With,
      rows=sub(" \\*", "", name_cindex_With) %in% method_order[12:23]
    )
  ) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_body(
      columns=name_graf_With,
      rows=sub(" \\*", "", name_graf_With) %in% method_order[12:23]
    )
  ) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_body(
      columns=name_cindex_Without,
      rows=sub(" \\*", "", name_cindex_Without) %in% method_order[12:23]
    )
  ) %>%
  tab_style(
    style=cell_text(weight="bold"),
    locations=cells_body(
      columns=name_graf_Without,
      rows=sub(" \\*", "", name_graf_Without) %in% method_order[12:23]
    )
  ) 
gtsave(great_table, "perf_by_cancer.tex")

# slow
bmr_for_time = bmr_time %>%
  filter(grepl("JDR", method_type) | method_type=="PCA") %>%
  rbind(bmr_time %>%
    filter(method_name=="iCluster") %>%
    mutate(time_both=2.75*time_both) %>%
    mutate(method_name="iCluster (estimate)")) %>%
  mutate(supervised=case_when(
    grepl("Sup|sJIVE", method_name) ~ "supervised",
    .default="unsupervised"
  )) %>%
  mutate(time_both=time_both/3600) %>%
  group_by(clin, task_id, iteration, method_name, method_type, supervised) %>%
  group_modify(\(.x, ...) bind_rows(.x, summarize(.x, time_both=sum(time_both, na.rm=TRUE), category="total"))) %>%
  ungroup() %>%
  mutate(category=ifelse(is.na(category), "JDR - One hyperparameter combination", "JDR - All hyperparameter combinations")) %>%
  mutate(method_name=forcats::fct_relevel(
    method_name,
    c(method_order[1:15], "iCluster (estimate)", method_order[16:length(method_order)]
  ))) %>%
  select(-learner_id) %>%
  rbind(bmr_time %>%
    filter(method_type=="Reference" | grepl("PS", method_type)) %>%
    mutate(category="Reference & Previous study") %>%
    mutate(supervised=case_when(
      grepl("Sup|sJIVE", method_name) ~ "supervised",
      .default="unsupervised"
    )) %>%
    mutate(time_both=time_both/3600) %>%
    select(clin, task_id, iteration, method_name, method_type, supervised, category, time_both)
    ) %>%
  mutate(category=forcats::fct_relevel(category,
    "Reference & Previous study", "JDR - One hyperparameter combination",
    "JDR - All hyperparameter combinations")) 
bmr_for_time %>%
  ggplot(aes(x=method_name, y=time_both)) +
  geom_boxplot(aes(fill=method_type)) +
  geom_boxplot_pattern(
    aes(pattern=supervised),
    filter(bmr_for_time, supervised=="supervised"),
    fill=NA,
    pattern_fill="black",
    pattern_density=0.15,
    pattern_spacing=0.03
  ) +
  geom_hline(yintercept=c(1/3600, 1/60, 1, 24), linetype="dashed") +
  scale_y_log10() +
  scale_x_discrete(guide=guide_axis(angle=45)) +
  scale_fill_manual(values=cbPalette, guide=guide_legend(nrow=1)) +
  scale_pattern_manual(values="circle", guide="none") +
  theme_bw(base_size=9) +
  facet_grid(.~category, scales="free", space="free") +
  theme(legend.position="top") +
  xlab("Method name") + ylab("Time (in hours, log scale)") + labs(fill="Method type")
ggsave("5_time.pdf", width=180, height=135, units="mm",
  device=cairo_pdf)

sum(bmr_time$time_both, na.rm=TRUE)/3600 # total time of the benchmark in hours

coef_for_plot = bmr %>%
  filter(clin=="With clinical data") %>%
  select(task_id, cv_id, learner_id, iteration, method_name, method_type) %>%
  inner_join(coef %>% filter(var_type=="omics")) %>%
  group_by(task_id, method_type, method_name, iteration) %>%
  summarise(beta=sum(beta_norm2)) %>%
  group_by(task_id, method_name, method_type) %>%
  summarise(beta=mean(beta)) %>%
  ungroup() %>%
  mutate(supervised=case_when(
    grepl("Sup|sJIVE", method_name) ~ "supervised",
    .default="unsupervised"
  )) %>%
  mutate(task_id=forcats::fct_relevel(factor(task_id), cancer_order))
boxplot = coef_for_plot %>%
  ggplot(aes(x=method_name, y=beta, fill=method_type)) +
  geom_boxplot() +
  geom_boxplot_pattern(
    aes(pattern=supervised),
    filter(coef_for_plot, supervised=="supervised"),
    fill=NA,
    pattern_fill="black",
    pattern_density=0.15,
    pattern_spacing=0.03
  ) +
  #scale_x_discrete(guide=guide_axis(angle=45)) +
  scale_fill_manual(values=cbPalette) +
  scale_pattern_manual(values=c("circle"), guide="none") +
  xlab("") + ylab("Omics contribution") +
  labs(pattern="Supervision", fill="Method type") +
  theme_bw(base_size=9) +
  theme(
    #legend.position="top",
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )
heatmap = coef_for_plot %>%
  ggplot(aes(y=task_id, x=method_name)) +
  geom_tile(aes(fill=beta)) +
  paletteer::scale_fill_paletteer_c("scico::lapaz") +
  scale_x_discrete(guide=guide_axis(angle=45)) +
  xlab("Method name") + ylab("Cancer") +
  labs(fill="Omics contribution") +
  theme_bw(base_size=9) #+
  #theme(legend.position="top")

boxplot + heatmap + plot_layout(ncol=1, heights=c(0.5, 1), guides="collect") & theme(legend.position="right")
ggsave("3_coef.pdf", width=180, height=135, units="mm",
  device=cairo_pdf)

# median perf of methods vs number of patients - with linear models
bmr_for_lm = bmr_perf %>%
  #filter(clin!="Reference") %>%
  mutate(JDR=case_when(
    grepl("JDR", method_type) ~ "JDR method",
    clin=="Reference" ~ "Reference",
    .default="non-JDR method"
  )) %>%
  group_by(clin, task_id, measure, method_name, JDR) %>%
  summarize(value=median(value)) %>%
  ungroup() %>%
  group_by(clin, task_id, measure, JDR) %>%
  summarize(value=median(value)) %>%
  ungroup() %>%
  left_join(cancer_info)
combs = data.frame(
  measure=paste0(rep(c("cindex", "graf"), each=3), "_p0.9"),
  clin=rep(c("With clinical data", "Without clinical data", "Reference"), 2)
)
pvals_lst=list()
for (i in 1:nrow(combs)) {
  if (combs[i,]$clin=="Reference") {
    lm_tmp = lm(value ~ n_patients, data=subset(bmr_for_lm,
        measure==combs[i,]$measure & clin==combs[i,]$clin))
  } else {
    lm_tmp = lm(value ~ n_patients*JDR, data=subset(bmr_for_lm,
        measure==combs[i,]$measure & clin==combs[i,]$clin))
  }
  pvals_lst[[i]] = data.frame(
    clin=combs[i,]$clin,
    measure=combs[i,]$measure,
    summary(lm_tmp)$coefficients[,4] |> as.data.frame() |> t()
  )
  rownames(pvals_lst[[i]]) = NULL
  if (combs[i,]$clin=="Reference") {
    pvals_lst[[i]]["JDRnon.JDR.method"]=NA
    pvals_lst[[i]]["n_patients.JDRnon.JDR.method"]=NA
  }
  colnames(pvals_lst[[i]]) = c(
    "clin",
    "measure",
    "p_intercept",
    "p_n",
    "p_JDR",
    "p_n_JDR"
  )
}
pvals = Reduce(rbind, pvals_lst) %>%
  #mutate(label=paste0(
  #  "P[n] = " , round(p_n, 3), "\n",
  #  ifelse(clin!="Reference", paste0("P[JDR] = " , round(p_JDR, 3), "\n"), ""),
  #  ifelse(clin!="Reference", paste0("P[n*JDR] = " , round(p_n_JDR, 3), "\n"), "")
  #)) %>%
  mutate(label_p_n=paste0("P[n]==", round(p_n, 3))) %>%
  mutate(label_p_jdr=ifelse(clin!="Reference", paste0("P[JDR]==" , round(p_JDR, 3)), "")) %>%
  mutate(label_p_n_jdr=ifelse(clin!="Reference", paste0("P[n*plain('*')*JDR]==" , round(p_n_JDR, 3)), "")) %>%
  select(-p_n, -p_JDR, -p_n_JDR) %>%
  mutate(x=max(bmr_for_lm$n_patients)-200) %>%
  left_join(
    bmr_for_lm %>%
      group_by(measure) %>%
      summarize(y=max(value)) %>%
      select(measure, y)
  ) %>%
  mutate(y=ifelse(measure=="cindex_p0.9", y-0.02, y-0.01)) %>%
  pivot_longer(starts_with("label"), names_to="p", values_to="label",
    names_prefix="label_") %>%
  mutate(y=case_when(
    p=="p_n" ~ y,
    p=="p_jdr" & measure=="graf_p0.9" ~ 0.97*y,
    p=="p_n_jdr" & measure=="graf_p0.9"  ~ 0.94*y,
    p=="p_jdr" & measure=="cindex_p0.9" ~ 0.98*y,
    p=="p_n_jdr" & measure=="cindex_p0.9"  ~ 0.96*y,
    .default=y
  ))
bmr_for_lm %>%
    ggplot() +
    geom_point(aes(x=n_patients, y=value, color=JDR, shape=JDR)) +
    facet_grid(measure~clin, scales="free", labeller=labeller(measure=measure.labs)) +
    geom_smooth(aes(x=n_patients, y=value, color=JDR, linetype=JDR),
      se=FALSE, method="lm") +
    geom_text(data=pvals, aes(x=x, y=y, label=label), size=2.5, hjust=0, parse=TRUE) +
    ggokabeito::scale_color_okabe_ito() +
    theme_bw(base_size=9) +
    theme(legend.position="top") +
    xlab("Number of patients") + ylab("Metric") +
    labs(color="", shape="", linetype="")
ggsave("2_perf_n.pdf", width=180, height=135, units="mm",
  device=cairo_pdf)

# starting lots of plot for hyperaparameter combinations
bmr %>%
    filter(!grepl("sjive", learner_id)) %>%
    filter(grepl("jive", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("jive",
                   "_j", j=".*",
                   "_a", a=".*"
    ), too_few="debug") %>%
    group_by(clin, j, a) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 3)) %>%
    mutate(prop_label=paste(100*prop, "%")) %>%
    ungroup() %>%
    filter(j!="total" | a!="total") %>%
    mutate(across(c(j, a), forcats::fct_inseq)) %>%
    ggplot(aes(x=j, y=a, fill=prop)) +
    geom_tile() +
    geom_text(aes(label=prop_label), colour="white") +
    facet_grid(.~clin) +
    xlab("Number of common components") +
    ylab("Number of distincts components") +
    labs(fill="Proportion") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("jive_params_prop.pdf", width=180, height=120, units="mm",
  device=cairo_pdf)

labels = c("eta = 0.1", "eta = 0.5", "eta = 0.9")
names(labels) = c(0.1, 0.5, 0.9)
bmr %>%
    filter(grepl("sjive", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("sjive",
                   "_j", j=".*",
                   "_a", a=".*",
                   "_eta", eta=".*"
    ), too_few="debug") %>%
    group_by(clin, j, a, eta) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 3)) %>%
    mutate(prop_label=paste(100*prop, "%")) %>%
    ungroup() %>%
    filter(j!="total" | a!="total") %>%
    mutate(across(c(j, a, eta), forcats::fct_inseq)) %>%
    ggplot(aes(x=j, y=a, fill=prop)) +
    geom_tile() +
    geom_text(aes(label=prop_label), colour="white") +
    facet_grid(eta~clin, labeller=labeller(eta=labels)) +
    xlab("Number of common components") +
    ylab("Number of distincts components") +
    labs(fill="Proportion") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("sjive_params_prop.pdf", width=180, height=180, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("rgcca_unsup", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("rgcca_", sup=".*",
                   "_n", n=".*",
                   "_tau", tau=".*"
    )) %>%
    group_by(clin, sup, n, tau) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin, sup) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    mutate(prop_label=paste(100*prop, "%")) %>%
    ungroup() %>%
    mutate(across(c(n, tau), forcats::fct_inseq)) %>%
    ggplot(aes(x=n, y=tau, fill=prop)) +
    geom_tile() +
    geom_text(aes(label=prop_label), colour="white", size=20, size.unit="pt") +
    facet_grid(.~clin) +
    labs(fill="Proportion") +
    xlab("Number of components") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("rgcca_unsup_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("rgcca_sup", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("rgcca_", sup=".*",
                   "_n", n=".*",
                   "_tau", tau=".*"
    )) %>%
    group_by(clin, sup, n, tau) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin, sup) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    mutate(prop_label=paste(100*prop, "%")) %>%
    ungroup() %>%
    mutate(across(c(n, tau), forcats::fct_inseq)) %>%
    ggplot(aes(x=n, y=tau, fill=prop)) +
    geom_tile() +
    geom_text(aes(label=prop_label), colour="white", size=20, size.unit="pt") +
    facet_grid(.~clin) +
    labs(fill="Proportion") +
    xlab("Number of components") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("rgcca_sup_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("sgcca_sup", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("sgcca_", sup=".*",
                   "_n", n=".*",
                   "_s", s=".*"
    )) %>%
    group_by(clin, sup, n, s) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin, sup) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    mutate(prop_label=paste(100*prop, "%")) %>%
    ungroup() %>%
    mutate(across(c(n, s), forcats::fct_inseq)) %>%
    ggplot(aes(x=n, y=s, fill=prop)) +
    geom_tile() +
    geom_text(aes(label=prop_label), colour="white", size=20, size.unit="pt") +
    facet_grid(.~clin) +
    labs(fill="Proportion") +
    xlab("Number of components") +
    ylab("Sparsity") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("sgcca_sup_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("sgcca_unsup", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("sgcca_", sup=".*",
                   "_n", n=".*",
                   "_s", s=".*"
    )) %>%
    group_by(clin, sup, n, s) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin, sup) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    mutate(prop_label=paste(100*prop, "%")) %>%
    ungroup() %>%
    mutate(across(c(n, s), forcats::fct_inseq)) %>%
    ggplot(aes(x=n, y=s, fill=prop)) +
    geom_tile() +
    geom_text(aes(label=prop_label), colour="white", size=20, size.unit="pt") +
    facet_grid(.~clin) +
    labs(fill="Proportion") +
    xlab("Number of components") +
    ylab("Sparsity") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("sgcca_unsup_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("intnmf", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("intnmf",
                   "_k", k=".*",
                   "_frob"),
        too_few="debug") %>%
    group_by(clin, k) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    ungroup()  %>%
    mutate(across(c(k), forcats::fct_inseq)) %>%
    ggplot(aes(x=k, y=count)) +
    geom_bar(stat="identity", color="black", fill="white") +
    facet_grid(.~clin) +
    xlab("Number of components") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("intnmf_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("mofa", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("mofa",
                   "_f", f=".*"),
        too_few="debug") %>%
    group_by(clin, f) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    ungroup()  %>%
    mutate(across(c(f), forcats::fct_inseq)) %>%
    ggplot(aes(x=f, y=count)) +
    geom_bar(stat="identity", fill="white", color="black") +
    facet_grid(.~clin) +
    xlab("Number of components") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("mofa_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("icluster", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("icluster",
                   "__K", k=".*",
                  "_s", s=".*"),
        too_few="debug") %>%
    group_by(clin, k, s) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    mutate(prop_label=paste(100*prop, "%")) %>%
    ungroup() %>%
    mutate(k=as.character(as.numeric(k)-1)) %>%
    mutate(across(c(k, s), forcats::fct_inseq)) %>%
    ggplot(aes(x=k, y=s, fill=prop)) +
    geom_tile() +
    geom_text(aes(label=prop_label), colour="white", size=20, size.unit="pt") +
    facet_grid(.~clin) +
    labs(fill="Proportion") +
    xlab("Number of components") +
    ylab("Sparsity") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("icluster_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("MCIA_Ntrain", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("MCIA_Ntrain",
                   "_r", r=".*"),
        too_few="debug") %>%
    group_by(clin, r) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    ungroup()  %>%
    mutate(across(c(r), forcats::fct_inseq)) %>%
    ggplot(aes(x=r, y=count)) +
    geom_bar(stat="identity", fill="white", color="black") +
    facet_grid(.~clin) +
    xlab("Number of components") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("mcia_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("tpca", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("tpca",
                   "_r", r=".*"),
        too_few="debug") %>%
    group_by(clin, r) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    ungroup()  %>%
    mutate(across(c(r), forcats::fct_inseq)) %>%
    ggplot(aes(x=r, y=count)) +
    geom_bar(stat="identity", fill="white", color="black") +
    facet_grid(.~clin) +
    xlab("Number of components") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("tpca_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("tfobi", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("tfobi",
                   "_r", r=".*"),
        too_few="debug") %>%
    group_by(clin, r) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    ungroup()  %>%
    mutate(across(c(r), forcats::fct_inseq)) %>%
    ggplot(aes(x=r, y=count)) +
    geom_bar(stat="identity", fill="white", color="black") +
    facet_grid(.~clin) +
    xlab("Number of components") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("tfobi_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("ipca", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("ipca",
                   "_a", r=".*"),
        too_few="debug") %>%
    group_by(clin, r) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    ungroup()  %>%
    mutate(across(c(r), forcats::fct_inseq)) %>%
    ggplot(aes(x=r, y=count)) +
    geom_bar(stat="identity", fill="white", color="black") +
    facet_grid(.~clin) +
    xlab("Number of components") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("ipca_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)

bmr %>%
    filter(grepl("jpca", learner_id)) %>%
    select(clin, learner_id) %>%
    mutate(learner_id=gsub("_clin_fav", "", learner_id)) %>%
    separate_wider_regex(learner_id,
        patterns=c("jpca",
                   "_j", r=".*"),
        too_few="debug") %>%
    group_by(clin, r) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    group_by(clin) %>%
    mutate(prop=round(count/sum(count), 2)) %>%
    ungroup()  %>%
    mutate(across(c(r), forcats::fct_inseq)) %>%
    ggplot(aes(x=r, y=count)) +
    geom_bar(stat="identity", fill="white", color="black") +
    facet_grid(.~clin) +
    xlab("Number of components") +
    theme_bw(base_size=12) +
    theme(legend.position="right")
ggsave("jpca_params_prop.pdf", width=180, height=100, units="mm",
  device=cairo_pdf)


bmr_perf %>%
  filter(method_name=="sJIVE",
    task_id=="KIRP",
    clin=="Without clinical data",
    measure=="cindex_p0.9") %>%
  pull(value) %>% median()

bmr_perf %>%
  filter(method_name=="Cox clin.",
    task_id=="KIRP",
    measure=="cindex_p0.9") %>%
  pull(value) %>% median()

# Creating dataframe with error rates
bmr_error = read_csv("data/benchmark/results_aggregated_raw.csv") %>%
  mutate(clin=case_when(
    grepl("clin", learner_id) ~ "With clinical data",
    grepl("ref", learner_id) ~ "Reference",
    .default="Without clinical data"
  )) %>%
  mutate(method_name = case_when(
    grepl("rgcca_unsup", learner_id) ~ "RGCCA Unsup.",
    grepl("rgcca_sup", learner_id) ~ "RGCCA Sup.",
    grepl("sgcca_unsup", learner_id) ~ "SGCCA Unsup.",
    grepl("sgcca_sup", learner_id) ~ "SGCCA Sup.",
    grepl("sjive", learner_id) ~ "sJIVE", # always before JIVE
    grepl("jive", learner_id) ~ "JIVE",
    grepl("ipca", learner_id) ~ "LI-PCA",
    grepl("jpca", learner_id) ~ "EI-PCA",
    grepl("intnmf", learner_id) ~ "IntNMF",
    grepl("MCIA_Ntest", learner_id) ~ "MCIA Ntest",
    grepl("MCIA_Ntrain", learner_id) ~ "MCIA Ntrain",
    grepl("tpca", learner_id) ~ "tPCA",
    grepl("tfobi", learner_id) ~ "tPCA+tFOBI",
    grepl("tpca", learner_id) ~ "tPCA",
    grepl("mofa", learner_id) ~ "MOFA+MOTL",
    grepl("icluster", learner_id) ~ "iCluster",
    learner_id == "cox_ref" ~ "Cox clin.",
    learner_id == "glmnet_ref" ~ "Cox L2 clin.",
    learner_id == "kaplan_ref" ~ "Kaplan-Meier",
    grepl("blockforest", learner_id) ~ "BlockForest",
    learner_id == "prioritylasso_clin_fav" ~ "Priority-LASSO clin. fav.",
    grepl("prioritylasso", learner_id) ~ "Priority-LASSO",
    grepl("ipflasso", learner_id) ~ "IPF-LASSO",
    grepl("lasso", learner_id) ~ "LASSO",
    grepl("ranger", learner_id) ~ "ranger",
    .default=learner_id
  )) %>%
  mutate(method_type = case_when(
    grepl("gcca", learner_id) ~ "JDR - Cov",
    grepl("jive", learner_id) ~ "JDR - MF",
    grepl("tfobi", learner_id) ~ "JDR - TF",
    grepl("tpca", learner_id) ~ "JDR - TF",
    grepl("pca", learner_id) ~ "PCA",
    grepl("intnmf", learner_id) ~ "JDR - Matrix Factorization",
    grepl("MCIA", learner_id) ~ "JDR - Covariance Based",
    grepl("mofa", learner_id) ~ "JDR - Matrix Factorization",
    grepl("icluster", learner_id) ~ "JDR - Matrix Factorization",
    clin == "Reference" ~ "Reference",
    .default="Previous study"
  )) %>%
  filter(method_name != "MCIA Ntest") %>%
  mutate(method_name=ifelse(method_name=="MCIA Ntrain", "MCIA", method_name)) %>%
  mutate(method_type=factor(method_type)) %>%
  mutate(method_type=forcats::fct_relevel(method_type, type_order)) %>%
  mutate(method_name=factor(method_name)) %>%
  mutate(method_name=forcats::fct_relevel(method_name, method_order)) %>%
  mutate(task_id=task_id) %>%
  mutate(task_id=forcats::fct_relevel(task_id, cancer_order)) %>%
  mutate(error=!is.na(error)) %>%
  group_by(method_name, method_type) %>%
  summarize(error=sum(error), n=n()) %>%
  mutate(error_rate=error/n) %>%
  ungroup()

# error table for the paper
table = bmr_error %>%
  gt() %>%
  fmt_integer(columns=c(error, n)) %>%
  fmt_percent(error_rate)

gtsave(table, "errors.tex")

# reproducing Herrmann table for dataset description
cancer_info = read_csv("data/cancer_info.csv")

cancer_names = 
  data.frame(
    dataset=c("BLCA","BRCA","COAD","ESCA","HNSC","KIRC","KIRP","LAML","LGG","LIHC","LUAD","LUSC","OV","PAAD","SARC","SKCM","STAD","UCEC"),
    cancer=c(
      "Bladder urothelial",
      "Breast invasive C.",
      "Colon adenocarcinoma",
      "Esophageal C.",
      "Head–neck squamous CC.",
      "Kidney renal clear CC.",
      "Cervical kidney RP. CC.",
      "Acute myeloid leukemia",
      "Low grade glioma",
      "Liver hepatocellular C.",
      "Lung adenocarcinoma",
      "Lung squamous CC.",
      "Ovarian cancer",
      "Pancreatic AC.",
      "Sarcoma",
      "Skin cutaneous M.",
      "Stomach AC.",
      "Uterine corpus EC."
    )
  )

cancer_info %>%
  mutate(p=mutation+cnv+rna+mirna+clinical) %>%
  mutate(across(c(mutation, rna, mirna, cnv), \(x) x/p)) %>%
  summarize(across(c(mutation, rna, cnv, mirna), mean))

table = cancer_info %>%
  left_join(cancer_names) %>%
  mutate(p=mutation+cnv+rna+mirna+clinical) %>%
  mutate(r_e=n_e/n) %>%
  arrange(n) %>%
  select(dataset, cancer, n, n_e, r_e, mutation, rna, mirna, cnv, p) %>%
  gt() %>%
  fmt_integer(columns=c(n, n_e, p, mutation, rna, mirna, cnv)) %>%
  fmt_number(columns=r_e)

gtsave(table, "cancer_info.tex")
