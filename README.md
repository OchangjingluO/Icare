
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Icare

<!-- badges: start -->
<!-- badges: end -->

Icare R包是一款专为医学检验组学数据分析设计的智能化工具集，旨在帮助医学检验人员高效、准确地处理与分析高维度、多样性的临床检验数据。
该包提供从数据预处理、疾病预测、亚型分型到预后建模的一站式分析流程，严格遵循国际标准），并整合了多种统计方法与机器学习算法。
通过用户友好的Shiny界面，Icare R包显著降低了使用门槛，提升了数据分析的透明性与可重复性。
其模块化设计与高效的数据处理能力，为精准医疗和个性化治疗提供了强有力的技术支持，有望推动医学检验数据分析领域的标准化与智能化发展。

It offers four main applications:

- ​数据预处理模块：通过多种方法（如缺失值填补、异常值处理、标准化）和可视化工具，确保数据质量与一致性，为后续分析奠定基础。
- 疾病预测模块：整合传统统计与机器学习算法，构建稳健的预测模型，支持特征筛选、模型评估与可解释性分析，助力疾病风险评估与诊断。
- 亚型分型模块：采用聚类与降维技术，识别疾病潜在亚型，结合可视化工具与分型评估方法，为个性化治疗方案提供科学依据。
- ​预后建模与生存分析模块：通过Cox回归、随机生存森林等方法，构建个性化预后模型，结合Kaplan-Meier曲线与森林图，精确预测患者生存风险并支持临床决策

(--------放一张总的图-----------)
## 1.Install

You can install the development version of Icare like so:

``` r

library(Icare)
```

## 2.Quick Start

This is a basic example which shows you how to use `Icare`:

``` r
##这里添加示例数据的调用
```
### 1.数据预处理模块
#### 1.1 数据准备与变量类型注释
加载对象`Stat`
``` r
object_stat <- CreateStatObject(
  raw.data = df,   ###原始数据
  #clean.data = clean,  ###传入清洗后的数据
  info.data = info,  ###临床信息
  group_col = "group" ###标签信息、假设不存在group_col=NULL 
)
```

注释变量的类型（数值型、分类型、需要独热编码的变量）。
如果一个分类变量的唯一值数量`max_unique_values = 5`超过 5，则认为其需要进行独热编码处理。
如果输入是 Stat 对象，函数将注信息存储在`variable.types`槽中
``` r
# 注释变量类型
object_stat <- stat_diagnose_variable_type(object_stat)
```

#### 1.2 缺失值处理
变量缺失值的分布：
如果输入是 Stat 对象，函数将缺失值信息存储在`processe.info`槽中
``` r
object_stat<-state_plot_missing_data(object_stat)
```
对缺失值进行处理，支持以下两种方法：
- **mice**：多重插补法，适合数据缺失机制复杂的情况。
- **median_mode**：中位数/众数填补法，适合数据缺失机制简单的情况。<br>
默认使用 mice 方法（impute_method = "mice"）。<br>
如果样本或特征的缺失值比例超过 20%（miss_threshold = 20），则自动删除该样本或特征。<br>
如果输入是 Stat 对象，清洗后的数据将在`clean.data`槽中更新
``` r
object_stat<-stat_miss_processed(object_stat,
                                 impute_method ="mice",
                                 miss_threshold = 20)
```

#### 1.3 异常值处理
异常值检测
使用 `stat_detect_and_mark_outliers` 函数检测数据中的异常值，并对其进行标记。
如果输入是 Stat 对象，函数将异常值信息存储在`outlier.marked`槽中
``` r
# 检测并标记异常值
object_stat <- stat_detect_and_mark_outliers(object_stat)
```
异常值处理
检测到异常值后，支持以下四种处理方式：
- ​**replace**：用中位数或四分位距（IQR）范围内的值替换异常值。
- ​**remove**：删除包含异常值的样本。
- **keep**：保留异常值，不做处理。
- ​**capping**：将异常值限制在指定范围内（如 IQR 的上下限）

默认使用`handle_method = "replace"`
``` r
# 处理异常值
object_stat <- stat_handle_outliers(object_stat, handle_method = "replace")

```

#### 1.4 生成基线表
如果输入是 Stat 对象，函数将基线表信息存储在`baseline.table`槽中
``` r
##生成基线表格
object_stat<-stat_gaze_analysis(object_stat)
```


#### 1.5 描述性统计分析
`stat_compute_descriptive` 函数用于计算数据的描述性统计信息，包括数值型变量的均值、中位数、标准差等，以及分类变量的频数统计。
此外，它还会对数值型变量进行正态性检验，并根据检验结果分别计算正态和非正态变量的统计量。.
如果输入是 Stat 对象，函数将基线表信息存储在`compute.descriptive`槽中
``` r
object_stat <- stat_compute_descriptive(object_stat)


#> object_stat@compute.descriptive[["Group_Counts"]
#  0   1 
# 200 260 
#> object_stat@compute.descriptive[["Count_Results"]]
#$SEX
#Female   Male
#    72    388 
#$ABO
#A  AB   B   O 
#134  37 125 164
#> object_stat@compute.descriptive[["Num_Results"]][["Normal"]]
#           AGE        TT       MCV      MCHC
#Mean 55.486957 16.651739 40.911087 30.392174
#SD    9.485561  1.724771  4.279162  1.733919
#> object_stat@compute.descriptive[["Num_Results"]][["Non_Normal"]]
#                  HBcAb        HCVAg
#AD_p_value 5.930567e-24 6.577279e-21
#Median     6.700000e-01 3.300000e-01
#IQR        2.100250e+00 6.000000e-02
#> object_stat@compute.descriptive[["Normality_Test"]][["AGE"]]
#$p_value
#[1] 0.09958748
#$is_normal
#[1] TRUE
```
**对分类变量可视化**
``` r
object_stat<-plot_categorical_descriptive(object_stat)
```
- ​小提琴图：展示数值型变量的分布密度，适合观察数据的整体分布和集中趋势。
- ​山脊图：按组展示数值型变量的分布密度，适合比较不同组之间的分布差异。
plot_numeric_descriptive 函数用于对数值型变量进行可视化展示，支持两种展示形式："violin"（小提琴图）和 "ridge"（山脊图）。
默认使用` plot_type = "violin"`
``` r
object_stat <- plot_numeric_descriptive(object_stat)
```
#### 1.6 数据类型转换与独热编码
数据类型转换
`stat_convert_variables `函数用于将数据中的变量转换为正确的数据类型。例如，将字符型变量转换为因子型，或将数值型变量保留为数值型。

``` r
# 将变量转换为正确的数据类型
object_clean <- stat_convert_variables(object_stat)
```
 `stat_onehot_encode`函数用于对分类变量进行独热编码（One-Hot Encoding），将其转换为二进制向量形式
``` r
# 对分类变量进行独热编码
object_clean <- stat_onehot_encode(object_clean)
```

#### 1.7 数据标准化
​多种标准化方法：支持对数变换 (log_transform)、最小-最大缩放 (min_max_scale)、Z 分数标准化 (z_score_standardize)、中心化 (center_data) 和缩放 (scale_data)。
默认为 `normalize_method = "log_transform"`
如果输入是 Stat 对象，函数会自动更新对象的 scale.data 槽位，存储标准化后的数据。
``` r
# 使用对数变换标准化数据
object_clean <- stat_normalize_process(object_clean, normalize_method = "log_transform")

# 使用最小-最大缩放标准化数据
object_clean <- stat_normalize_process(object_clean, normalize_method = "min_max_scale")
```
- `cal_auc_pre.prog.sig()` will calculate the AUC based on the signatures from previous papers like the fuction `cal_RS_pre.prog.sig()`.
- `AUC_time` is like the requirement by `cal_AUC_ml_res()`.

Compare the AUC of specific model with previously published models:
``` r
auc_comp(auc.glioma.lgg.gbm.1,
         all.auc.1y,
         model_name="StepCox[forward] + plsRcox",
         dataset=names(list_train_vali_Data))
```
![Screenshot](https://github.com/l-magnificence/Mime/blob/main/fig/auc_comp_1y.png)

#### 1.5 Immune infiltration analysis
After completing the risk grouping, users can perform downstream analysis on the grouped data. Here, we combine Mime with the R package immunedeconv to assist users in quickly previewing immune infiltration. If users require a more precise immune infiltration analysis, they can fine-tune the parameters themselves.
``` r
devo <- TME_deconvolution_all(list_train_vali_Data)
```
- If you want to use this function, you should install package `immunedeconv` ahead.
- `TME_deconvolution_all()` includes 10 deconvolution methods ("quantiseq", "xcell", "epic", "abis", "mcp_counter", "estimate", "cibersort", "cibersort_abs", "timer", "consensus_tme") from `immunedeconv::deconvolution_methods`. By default, deconvolution methods are set as ("xcell", "epic", "abis", "estimate", "cibersort", "cibersort_abs").

Show the results:
``` r
immuno_heatmap(res,
               devo,
               model_name="StepCox[backward] + plsRcox",
               dataset="Dataset1")
```
![Screenshot](https://github.com/l-magnificence/Mime/blob/main/fig/immune_heatmap_Mime_dataset1.png)

### 2. Construct predicting models for response
``` r
load("./Example.ici.Rdata")
load("./genelist.Rdata")
res.ici <- ML.Dev.Pred.Category.Sig(train_data = list_train_vali_Data$training,
                                      list_train_vali_Data = list_train_vali_Data,
                                      candidate_genes = genelist,
                                      methods = c('nb','svmRadialWeights','rf','kknn','adaboost','LogitBoost','cancerclass'),
                                      seed = 5201314,
                                      cores_for_parallel = 60
)
```
- `ML.Dev.Pred.Category.Sig()` develop the predictive model for the binary variables with machine learning algorithms.

Plot AUC of different methods among different datasets:
``` r
auc_vis_category_all(res.ici,dataset = c("training","validation"),
                     order= c("training","validation"))
```
![Screenshot](https://github.com/l-magnificence/Mime/blob/main/fig/ICI_response_auc_all.png)

Plot ROC of specific method among different datasets:
``` r
plot_list<-list()
methods <- c('nb','svmRadialWeights','rf','kknn','adaboost','LogitBoost','cancerclass')
for (i in methods) {
  plot_list[[i]]<-roc_vis_category(res.ici,model_name = i,dataset = c("training","validation"),
                                   order= c("training","validation"),
                                   anno_position=c(0.4,0.25))
}
aplot::plot_list(gglist=plot_list,ncol=3)
```
![Screenshot](https://github.com/l-magnificence/Mime/blob/main/fig/ICI_response_roc_all.png)

Compared AUC with other published models associated with immunotherapy response:
``` r
auc.other.pre <- cal_auc_previous_sig(list_train_vali_Data = list_train_vali_Data,seed = 5201314,
                                      train_data = list_train_vali_Data$training,
                                      cores_for_parallel = 32)
```
- `cal_auc_previous_sig()` will calculate the AUC based on the signatures from previous papers for immunotherapy response.
- `cores_for_parallel` means the cores you can choose for parallel operation. If multi-cores condition is error, please set `cores_for_parallel` as 1.

Plot comparison results of specific model:
``` r
auc_category_comp(res.ici,
                  auc.other.pre,
                  model_name="svmRadialWeights",
                  dataset=names(list_train_vali_Data))
```
![Screenshot](https://github.com/l-magnificence/Mime/blob/main/fig/ICI_response_auc_comp.png)

### 3. Core feature selection

``` r
load("./Example.cohort.Rdata")
load("./genelist.Rdata")
res.feature.all <- ML.Corefeature.Prog.Screen(InputMatrix = list_train_vali_Data$Dataset1,
                                            candidate_genes = genelist,
                                            mode = "all",nodesize =5,seed = 5201314 )
```
- `ML.Corefeature.Prog.Screen()` provides three modes including `all`, `single`, and `all_without_SVM`. `all` mode means using all eight methods ("RSF", "Enet", "Boruta", "Xgboost", "SVM-REF", "Lasso", "CoxBoost', "StepCox") for selecting. `single` mode means using only one method for running. If you use `single` mode, `single_ml` should be specified among eight methods. Since SVM takes too long time, we define other seven methods used for selecting as `all_without_SVM` mode.
- The output genes are closely associated with patient outcome and higher frequence of screening means more critical.

Upset plot of genes filtered by different methods:
``` r
core_feature_select(res.feature.all)
```
![Screenshot](https://github.com/l-magnificence/Mime/blob/main/fig/core_feature_intersect.png)

Plot the rank of genes filtered by different methods:
``` r
core_feature_rank(res.feature.all, top=20)
```
![Screenshot](https://github.com/l-magnificence/Mime/blob/main/fig/core_feature_intersect_rank.png)

Here, we randomly select top two genes to analyze their correlation:
``` r
dataset_col<-c("#3182BDFF","#E6550DFF")
corplot <- list()
for (i in c(1:2)) {
  print(corplot[[i]]<-cor_plot(list_train_vali_Data[[i]],
                               dataset=names(list_train_vali_Data)[i],
                               color = dataset_col[i],
                               feature1="PSEN2",
                               feature2="WNT5B",
                               method="pearson"))
}
aplot::plot_list(gglist=corplot,ncol=2)
```
![Screenshot](https://github.com/l-magnificence/Mime/blob/main/fig/gene_cor.png)

Plot survival curve of patients according to median expression level of specific gene among different datasets:
``` r
survplot <- vector("list",2) 
for (i in c(1:2)) {
  print(survplot[[i]]<-core_feature_sur("PSEN2", 
                                        InputMatrix=list_train_vali_Data[[i]],
                                        dataset = names(list_train_vali_Data)[i],
                                        #color=c("blue","green"),
                                        median.line = "hv",
                                        cutoff = 0.5,
                                        conf.int = T,
                                        xlab="Day",pval.coord=c(1000,0.9)))
}
aplot::plot_list(gglist=survplot,ncol=2)
```
![Screenshot](https://github.com/l-magnificence/Mime/blob/main/fig/gene_km.png)

## Citations
If you use **_Mime_** in the study, please cite the following publication:
- Liu H, Zhang W, Zhang Y, et al. Mime: A flexible machine-learning framework to construct and visualize models for clinical characteristics prediction and feature selection. Comput Struct Biotechnol J. 2024.

## Contact
Any technical question please list in Issues section.

  
