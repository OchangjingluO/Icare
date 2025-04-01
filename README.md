
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Icare

<!-- badges: start -->
<!-- badges: end -->

🔍 Icare R包是一款专为医学检验组学数据分析设计的智能化工具集，旨在帮助医学检验人员高效、准确地处理与分析高维度、多样性的临床检验数据。
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
最好新创建一个新的R-project
所有可视化函数均支持以下标准化参数配置，确保输出风格一致
  palette_name = "AsteroidCity1",  # 使用wesanderson调色板
  save_plot = TRUE,                # 自动保存图表
  save_dir = here("PrognosiX", "results"),  # 统一保存路径
  plot_width = 5,                  # 图表宽度(英寸)
  plot_height = 5,                 # 图表高度(英寸) 
  base_size = 14                   # 基础字体大小

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

注释变量的类型（数值型、分类型、需要独热编码的变量）。<br>
如果一个分类变量的唯一值数量`max_unique_values = 5`超过 5，则认为其需要进行独热编码处理。<br>
如果输入是 Stat 对象，函数将注信息存储在`variable.types`槽中
``` r
# 注释变量类型
object_stat <- stat_diagnose_variable_type(object_stat)
```

#### 1.2 缺失值处理
**变量缺失值的分布**<br>

如果输入是 Stat 对象，函数将缺失值信息存储在`processe.info`槽中
``` r
object_stat<-state_plot_missing_data(object_stat)
```
<div align="center">
  <img src="https://github.com/OchangjingluO/Icare/blob/master/fig/combined_missing_data_plot.png" alt="Screenshot" width="500">
</div>

**缺失值处理**
对缺失值进行处理，支持以下两种方法：<br>
- **mice**：多重插补法，适合数据缺失机制复杂的情况。<br>
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
**异常值检测**<br>
使用 `stat_detect_and_mark_outliers` 函数检测数据中的异常值，并对其进行标记并可视化。<br>
如果输入是 Stat 对象，函数将异常值信息存储在`outlier.marked`槽中
``` r
# 检测并标记异常值
object_stat <- stat_detect_and_mark_outliers(object_stat)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/boxplot_outliers_batch_60.png" alt="Screenshot" width="500">
</div>



**异常值处理**<br>
检测到异常值后，支持以下四种处理方式：
- ​**replace**：用中位数或四分位距（IQR）范围内的值替换异常值。
- ​**remove**：删除包含异常值的样本。
- **keep**：保留异常值，不做处理。
- ​**capping**：将异常值限制在指定范围内（如 IQR 的上下限）

默认使用`handle_method = "replace"`
如果输入是 Stat 对象，函数将清洗后的数据存储在`outlier.marked`槽中，并且对`clean.data`进行更新
``` r
# 处理异常值
object_stat <- stat_handle_outliers(object_stat, handle_method = "replace")

```

#### 1.4 生成基线表
您可以通过`gaze_method`参数选择组间均值比较的统计方法,默认`gaze_method=3`
- 1 forces analysis as normal-distributed
- 2 forces analysis as continuous non-normal
- 3 performs a Shapiro-Wilk test or nortest::ad.test to decide between normal or non-normal <br>

允许用户自定义公式 `formula` 或基于 `group_cols` 自动生成分析公式。<br>
分析结果可按 `digits`指定的小数位数进行四舍五入，并可选择是否显示 p 值 (show.p = TRUE/FALSE)。<br>
如果输入是 `Stat` 对象，函数将基线表信息存储在`baseline.table`槽中<br>
``` r
##生成基线表格
object_stat<-stat_gaze_analysis(object_stat,
                                show.p = TRUE,
                                gaze_method = 3)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/gaze_analysis.png" alt="Screenshot" width="500">
</div>


#### 1.5 描述性统计分析

**统计分析**<br>
`stat_compute_descriptive` 函数用于计算数据的描述性统计信息，包括数值型变量的均值、中位数、标准差等，以及分类变量的频数统计。<br>
此外，它还会对数值型变量进行正态性检验，并根据检验结果分别计算正态和非正态变量的统计量。<br>
如果输入是 Stat 对象，函数将统计结果存储在`compute.descriptive`槽中
``` r
object_stat <- stat_compute_descriptive(object_stat)

#> object_stat@compute.descriptive[["Group_Counts"]]
#  0   1 
#  200 260 
#> object_stat@compute.descriptive[["Count_Results"]][["ABO"]]
#  A  AB   B   O 
#134  37 125 164 
#> object_stat@compute.descriptive[["Num_Results"]][["Normal"]]
#           AGE        TT       MCV      MCHC     RDWSD       rct
#Mean 55.486957 16.651739 40.911087 30.392174 43.834565 4.3802391
#SD    9.485561  1.724771  4.279162  1.733919  3.160162 0.4429685
#> object_stat@compute.descriptive[["Num_Results"]][["Non_Normal"]]
#                 HBcAb        HCVAg PreS1antigenofHBV
#AD_p_value 5.930567e-24 6.577279e-21      1.412496e-24
#Median     6.700000e-01 3.300000e-01      1.700000e-01
#IQR        2.100250e+00 6.000000e-02      3.025000e-01
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
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/ABO_plot.png" alt="Screenshot" width="500">
</div>

**对数值型变量可视化**
- ​小提琴图：展示数值型变量的分布密度，适合观察数据的整体分布和集中趋势。
- ​山脊图：按组展示数值型变量的分布密度，适合比较不同组之间的分布差异。
plot_numeric_descriptive 函数用于对数值型变量进行可视化展示，支持两种`plot_type`展示形式："violin"（小提琴图）和 "ridge"（山脊图）。
默认使用`plot_type = "violin"`
``` r
###山脊图绘制
object_stat <- plot_numeric_descriptive(object_stat，plot_type = "ridge")
###小提琴图绘制
object_stat <- plot_numeric_descriptive(object_stat，plot_type = "violin")

```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/density_ridge_plot_part_50.png" alt="Screenshot" width="500">
</div>

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/violin_plot_part_5.png" alt="Screenshot" width="500">
</div>


#### 1.6数据类型转换与独热编码
**数据类型转换**<br>
`stat_convert_variables`函数用于将数据中的变量转换为正确的数据类型。例如，将字符型变量转换为因子型，或将数值型变量保留为数值型。

``` r
# 将变量转换为正确的数据类型
object_stat <- stat_convert_variables(object_stat)
```
**独热编码**<br>
 `stat_onehot_encode`函数用于对分类变量进行独热编码（One-Hot Encoding），将其转换为二进制向量形式
``` r
# 对分类变量进行独热编码
object_stat <- stat_onehot_encode(object_stat)
```

#### 1.7 数据标准化
​多种标准化方法：支持对数变换 (log_transform)、最小-最大缩放 (min_max_scale)、Z 分数标准化 (z_score_standardize)、中心化 (center_data) 和缩放 (scale_data)。 <br>
默认 `normalize_method = "log_transform"` <br>
如果输入是 Stat 对象，函数会自动更新对象的 scale.data 槽位，存储标准化后的数据。
``` r
# 使用对数变换标准化数据
object_stat <- stat_normalize_process(object_stat, normalize_method = "log_transform")

# 使用最小-最大缩放标准化数据
object_stat <- stat_normalize_process(object_stat, normalize_method = "min_max_scale")
```
#### 1.8 相关性分析与交叉变量分析
**相关性分析**<br>
`stat_correlated_features` 函数用于分析数据集中的特征相关性，主要提供以下功能：
1.相关性计算：
- 计算变量间的相关性矩阵
- 识别高相关变量对（默认阈值correlation_threshold = 0.95）
- 按相关性值降序排序并输出前5组最高相关变量对
2.检测高度相关特征：
- 根据指定的相关性阈值（correlation_threshold）默认为 0.95，高于该阈值的特征对将被视为高度相关。<br>
- ​生成相关性热图：可视化特征之间的相关性矩阵<br>
- `data_type`数据类型，可选 "clean" 或 "scale"，默认为 "scale"<br>
如果输入是 Stat 对象，函数会自动更新对象的 corr.result槽位，存储相关性分析结果。
``` r
# 检测高度相关特征并生成相关性热图
object_stat <- stat_correlated_features(object_stat, data_type="scale,correlation_threshold = 0.95)

#> object_stat@corr.result[["top5_pairs"]]
#         drop_feature        corr_feature corr_value
#4           pre_RET_r Ret_mid_F_Intensity  0.9961969
#5 Ret_mid_F_Intensity           pre_RET_r  0.9961969
#3                 MCV                  hb  0.9624690
#6                  hb                 MCV  0.9624690
#1                  PT                 INR  0.9579065
```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/correlation_heatmap.png" alt="Screenshot" width="500">
</div>


**相关性热图（Top N 特征）** <br>
`cor_top_correlations `函数用于生成相关性热图，展示前 N 个最相关的特征。

``` r
# 检测高度相关特征并生成相关性热图
# 生成前 15 个最相关特征的相关性热图
object_stat <- cor_top_correlations(object_stat,top_n = 15)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/top_correlations_plot.png" alt="Screenshot" width="500">
</div>

**交叉变量分析** <br>
`cross_plot_analysis` 函数用于对两个变量进行交叉分析，生成散点图并计算相关性。
``` r
# 对两个变量（如cl和cr）进行交叉分析
cross_plot_analysis(object_stat, vars = c("cl", "cr"))
```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/scatter_plot_cl_vs_cr.png" alt="Screenshot" width="500">
</div>

#### 1.9 差异分析与可视化
`stat_var_feature` 函数用于对数据进行差异分析，识别在不同组别之间显著变化的特征。<br>
⚠️ 注意 | 关键前提:
本功能必须在数据包含`group_col`指定的有效分组列时才能正常运行！使用前请确保：
如果输入是 Stat 对象，函数会自动更新对象的 var.result 槽位，存储差异分析结果。<br>
- Wilcoxon 检验：使用 Wilcoxon 秩和检验比较两组之间的差异。
​- 多重检验校正：对 p 值进行 Bonferroni 校正，控制假阳性率。
- ​差异特征筛选：根据 logFC 和 p 值筛选显著变化的特征。
- ​生物标志物筛选：差异分析可用于识别潜在的生物标志物。
- ​数据探索：可视化工具（如火山图、小提琴图）可用于探索数据分布和差异。
- 模型评估：ROC 曲线图可用于评估特征的分类性能。

**进行差异分析**<br>
  
``` r
# 进行差异分析
object_stat <- stat_var_feature(object_stat, data_type = "clean")
#> object_stat@var.result[["last_test_sig"]]
#                  id       W            p       mean_x     mean_y median_x median_y
#2                alp 48762.0 2.318873e-58 1.382423e+02  76.820000  129.000   74.000
#14               PLT 46322.5 7.060287e-47 3.243819e+02 175.544000  310.500  159.600
#1                alb 18289.0 4.879498e-08 3.886615e+01  41.236500   39.400   41.500
#11               L_r 19655.5 7.168725e-06 2.035346e+01  23.748000   19.550   23.400
#10               L_c 20400.0 7.431591e-05 1.434423e+00   1.622850    1.410    1.505
#       p.adjust       sd_0       sd_1        logFC change
#2  4.405858e-57   22.88034   38.45866  0.847645337     Up
#14 1.341455e-45   71.13009  106.62522  0.885860738     Up
#1  9.271046e-07   3.772383   4.823576 -0.085407540 Stable
#11 1.362058e-04   7.815845   7.917520 -0.222531842 Stable
#10 1.412002e-03  0.4801972  0.4426622 -0.178059054 Stable  

```
**雷达图可视化** <br>
`VarFeature_radarchart` 函数用于生成雷达图，展示显著变化特征的分布。
``` r
# 生成雷达图
object_var <- VarFeature_radarchart(object_var)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/radar_chart.png" alt="Screenshot" width="500">
</div>

**火山图可视化** <br>
`VarFeature_volcano`函数用于生成火山图，展示差异分析结果。
``` r
# 生成火山图
object_stat <- VarFeature_volcano(object_stat)
```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/volcano_plot.png" alt="Screenshot" width="500">
</div>


**小提琴图可视化** <br>
`VarFeature_violinplot` 函数用于生成小提琴图，展示显著变化特征的分布。
``` r
# 生成小提琴图
object_stat <- VarFeature_violinplot(object_stat)
```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/violinplot.png" alt="Screenshot" width="500">
</div>

**ROC 曲线图可视化** <br>
`VarFeature_ROC` 函数用于生成 ROC 曲线图，评估显著变化特征的分类性能。
``` r
# 生成 ROC 曲线图
object_stat <- VarFeature_ROC(object_stat)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/roc_plot.png" alt="Screenshot" width="500">
</div>


#### 1.10 保存结果 
保存清洗后的 Stat 对象<br>
在完成数据清洗后，清洗后的 `Stat` 对象可以保存为 .RData 文件。您可以使用`readRDS`函数加载该对象，以便在后续分析中使用。
``` r
###保存S4对象
saveRDS(object_stat, file = "object_stat.RDS")
###读取S4对象
object_stat <-readRDS(file = "object_stat.RData")
```

### 2.疾病预测模块
#### 2.1 数据加载与简单清洗
在疾病预测模块中，您可以使用 `Stat` 对象或清洗后的数据来创建 `Model` 对象。<br>
`PrepareData`函数用于对数据进行简单处理，特别是将因子变量（factor 或 character 类型）转换为二进制或哑变量（dummy variables），以便数据符合模型的输入要求。
``` r
# 使用 Stat 对象创建 Model 对象
object_model <- CreateModelObject(
  object = object_stat,  # 加载的 Stat 对象
  group_col = "group"    # 分组列名
)


# 使用清洗后的数据创建 Model 对象
object_model <- CreateModelObject(
  clean.data = data,  # 清洗后的数据框
  group_col = "group" # 分组列名
)

# 使用 Model_data 对象进行数据处理
object_model <- PrepareData(object_model)
```

#### 2.2 数据平衡处理
`BalanceData`函数用于处理数据中的类别不平衡问题，支持过采样（over）、欠采样（under）或两者结合（both）的方法。默认使用 both 方法`method = "both"`。<br>
该函数根据类别不平衡情况自动选择是否进行平衡处理，并提供了可视化功能，用于展示平衡前后的类别分布。<br>
如果类别不平衡比例低于 默认`imbalance_threshold=0.15` 或样本大小超过 默认`sample_size_threshold=1500`，则不会进行平衡处理，除非`force_balance=TRUE`<br>
如果输入是 `Model_data` 对象，函数会自动更新对象的`clean.data`槽位，存储相关性分析结果。

``` r
# 使用 Model_data 对象进行数据平衡处理
object_model <- BalanceData(object_model,
                         imbalance_threshold = 0.15,
                         sample_size_threshold = 1500,
                         force_balance = FALSE,
                         method = "both")

####采取强制平衡处理
object_model <- BalanceData(object_model,
                        force_balance =T)

```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/class_distribution_balance.png" alt="Screenshot" width="500">
</div>

#### 2.2 数据集划分
`SplitDatModel`函数用于将数据按比例拆分为训练集和测试集。基于分组列`group_col`进行分层拆分，以确保训练集和测试集中的类别分布一致。<br>
默认训练集`train_ratio=0.7`测试集`test_ratio=0.3`<br>
如果输入是 `Model_data 对象`，函数会自动更新对象的`split.data`槽位，存储数据划分结果。<br>

``` r
# 将数据拆分为训练集和测试集
object_model<-SplitDatModel(object_model,
                         train_ratio = 0.7,
                         test_ratio = 0.3)
```

#### 2.3 数据标准化
`NormalizeData` 函数对数据进行标准化处理，并将标准化方法应用于测试集。支持多种标准化方法，如对数变换、最小-最大缩放、Z 分数标准化等默认`normalize_method = "min_max_scale"`<br>
如果输入是 `Model_data 对象`，函数会自动更新对象的`split.sacle.data`槽位，存储数据标准化处理结果。<br>

``` r
# 对数据进行标准化处理
object_model<-NormalizeData(object_model,
                         normalize_method = "min_max_scale")
```

#### 2.4 特征筛选与特征子集过滤
`SelFeatureSet` 函数用于从数据集中选择最优特征子集，支持基于以下方法的特征选择：
- 信息值（IV）​：评估特征与目标变量之间的关联强度。<br>
- ​最大信息系数（MIC）​：衡量特征与目标变量之间的非线性关系。<br>
- ​互信息（MI）​：量化特征与目标变量之间的信息共享程度。<br>

该函数通过计算不同特征数量下的 AUC（Area Under Curve）值，选择最优特征子集，并可视化 AUC 随特征数量的变化趋势。<br>
- `AUC_change_threshold `是用于判断特征选择过程中 AUC（Area Under Curve）值变化的阈值。当增加特征数量时，如果 AUC 的提升幅度小于该阈值，则认为继续增加特征数量对模型性能的提升不再显著，从而停止特征选择。默认值`AUC_change_threshold=0.01`，即 AUC 变化小于 1% 时，选择当前特征数量为最优。<br>
- `max_feature`为筛选得到最大特征子集数量，默认`max_feature=NULL`，即使用所有特征。<br>
- data_type用于指定进行特征筛选的数据类型，可选值为 "clean"（清洗后的数据）或 "scale"（标准化后的数据）默认`data_type = "clean"`<br>

`FilterDataFeatures` 函数用于根据特征选择结果或直接使用完整数据集，过滤训练集和测试集，保留最优特征子集或全部特征。<br>
该函数支持从清洗后或标准化后的数据中进行过滤，并更新 `Model_data` 对象的 `filtered.set` 槽位。<br>

``` r
# 从数据集中选择最优特征子集
object_model <- SelFeatureSet(object_model,
                           AUC_change_threshold=0.01,
                           max_feature=NULL,
                           data_type = "clean")
#> object_model@feature.result[["best_features_subset"]]
#[1] "HGB" "RBC" "WBC" "PLT" "MCV"

# 过滤特征子集
#也可以不经过特征筛选直接执行下面这一步,对数据进行过滤
object_model <- FilterDataFeatures(object_model)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/Combined_AUC_vs_Features.png" alt="Screenshot" width="500">
</div>


#### 2.5 模型训练与分析

**训练模型**<br>


`ModelTrainAnalysis` 函数用于训练多个机器学习模型，评估其性能，并生成 ROC 曲线和性能指标。支持多种模型（如 GBM、随机森林、SVM 等），并允许自定义超参数调优和交叉验证设置。<br>
`methods`:模型名称列表，默认为 `methods=c("gbm", "rf", "svmLinear", "svmRadial", "glmnet")`<br>
`tune_grids`:模型超参数调优网格，默认为预定义的调优网格<br>
`classProbs`：是否计算类别概率，默认为 TRUE。<br>
`verboseIter`：是否显示训练过程的详细信息，默认为 FALSE。<br>
`allowParallel`：是否启用并行计算，默认为 TRUE。<br>
如果输入是 Model_data 对象，函数会自动更新其 all.results 槽位（用于存储所有模型在训练集上的性能分析结果）和 train.models 槽位（用于存储训练完成的所有模型），并返回更新后的对象。<br>


``` r
# 训练模型并分析性能
object_model<-ModelTrainAnalysis(object_model,
                                 methods = c("gbm", "rf", "svmLinear", "svmRadial", "glmnet"),
                                 control = list(method = "repeatedcv", number = 10, 
                                                repeats = 5),
                                 tune_grids = list(
                                   gbm = expand.grid(
                                     n.trees = c(50, 100),
                                     interaction.depth = c(2, 3),
                                     shrinkage = c(0.001, 0.01),
                                     n.minobsinnode = c(10, 20)
                                   ),
                                   rf = expand.grid(
                                     mtry = c(2, 3, 4, 5)
                                   ),
                                   svmLinear = expand.grid(
                                     C = c(0.01, 0.1, 1)
                                   ),
                                   svmRadial = expand.grid(
                                     sigma = c(0.01, 0.05, 0.1),
                                     C = c(0.1, 1)
                                   ),
                                   glmnet = expand.grid(
                                     alpha = c(0.1, 0.5, 0.9),
                                     lambda = 10^seq(-4, -1, 1)
                                   )
                                 ),
                                 classProbs = TRUE, 
                                 verboseIter = FALSE,
                                 allowParallel = TRUE)

#> object_model@all.results
#              Model Sensitivity Specificity Positive_predictive_value
#rf               rf   1.0000000   1.0000000                 100.00000
#svmRadial svmRadial   0.9863946   0.9942857                  99.31507
#svmLinear svmLinear   0.9795918   0.9714286                  96.64430
#glmnet       glmnet   0.9659864   0.9714286                  96.59864
#gbm             gbm   0.8843537   0.9542857                  94.20290
#          Negative_predictive_value accuracy_score Precision f1_score recall_score       auc
#rf                        100.00000      100.00000 100.00000 1.980198    100.00000 1.0000000
#svmRadial                  98.86364       99.06832  99.31507 1.953388     98.63946 0.9987949
#svmLinear                  98.26590       97.51553  96.64430 1.939525     97.95918 0.9919534
#glmnet                     97.14286       96.89441  96.59864 1.912844     96.59864 0.9913314
#gbm                        90.76087       92.23602  94.20290 1.752258     88.43537 0.9860447

```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/roc_curves.png" alt="Screenshot" width=800">
</div>

**提取最佳模型结果**<br>

`ModelBestRoc` 函数用于从 `Model_data` 对象中提取性能最佳模型，并在训练集和测试集上绘制 ROC 曲线。支持自定义性能指标（如 AUC、准确率等）作为最佳模型选择指标默认`metric="auc"`<br>
如果输入是 Model_data 对象，函数会自动更新其 best.model.result 槽位存储最佳模型及其在测试集上的性能分析结果<br>

``` r
# 绘制最佳模型的 ROC 曲线
object_model <- ModelBestRoc(object_model,
                          metric="auc")
#> object_model@best.model.result[["test_result"]]
#  Model Sensitivity Specificity Positive_predictive_value Negative_predictive_value accuracy_score
#1    rf   0.9056604   0.9529412                  92.30769                  94.18605       93.47826
#  Precision f1_score recall_score       auc
#1  92.30769 1.793722     90.56604 0.9610433

```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/best_model_roc_plot.png" alt="Screenshot" width=500">
</div>

**最佳模型混淆矩阵生成**<br>
`ModelBestCM`函数用于从 `Model_data`对象中提取性能最佳模型，并在测试集上生成混淆矩阵及其可视化图表。<br>
如果输入是 Model_data 对象，函数会自动更新其 best.model.result 槽位<br>

``` r
# 生成最佳模型的混淆矩阵
object_model <- ModelBestCM(object_model)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/confusion_matrix_plot.png" alt="Screenshot" width=500">
</div>

**特征重要性分析**<br>
`FeatureImportance` 函数用于从 `Model_data` 对象中提取最佳模型，并计算其特征重要性。<br>
支持自定义显示前 top_n 个重要特征默`top_n = 15`，并生成可视化图表。<br>
如果输入是 Model_data 对象，函数会自动更新其 best.model.result 槽位<br>

``` r
# 计算特征重要性并生成可视化图表
object_model<-FeatureImportance(object_model,
                             top_n =15)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/Feature_Importance.png" alt="Screenshot" width=500">
</div>

**SHAP 值分析模块**<br>
`ModelShap` 函数用于从` Model_data `对象中提取最佳模型，并生成 SHAP（SHapley Additive exPlanations）值分析的可视化图表。<br>
支持生成 Beeswarm 图、Force 图和 Waterfall 图<br>
如果输入是 Model_data 对象，函数会自动更新其 shap.result 槽位<br>

``` r
# 生成 SHAP 值分析的可视化图表
object_model <- ModelShap(object_model)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/shap_beeswarm_plot.png" alt="Screenshot" width=500">
</div>

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/shap_force_plot.png" alt="Screenshot" width=500">
</div>

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/shap_waterfall_plot.png" alt="Screenshot" width=500">
</div>

**添加外部验证集**<br>
`Extract_external_validata` 函数用于将外部验证数据集添加到 Model_data 对象中。<br>
支持从 Stat 对象或直接提供的数据框中提取验证数据<br>
如果输入是 Model_data 对象，函数会自动更新其filtered.set槽位存储外部验证集<br>

``` r
# 从 `Stat` 对象中提取验证数据并更新 `Model_data` 对象
object_model <- Extract_external_validata(object_stats = object_val, object_model = object_model)

# 直接提供验证数据并更新 `Model_data` 对象
object_model <- Extract_external_validata(data = val_data, object_model = object_model)

```
**模型外部验证**<br>
`ModelValidation`函数用于对 `Model_data` 对象中的最佳模型进行外部验证。<br>
支持在独立验证集上评估模型性能，生成 ROC 曲线<br>
如果输入是 Model_data 对象，函数会自动更新其 best.model.result 槽位<br>

``` r
# 进行模型外部验证
object_model <- ModelValidation(object_model)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/validation_roc_plot.png" alt="Screenshot" width=500">
</div>

#### 2.5 模型临床应用 

**模型阈值选择**
`ModelThreshold` 函数用于从 `Model_data` 对象中提取最佳模型，并在测试集上选择最佳阈值。<br>
支持基于最大准确率、最接近 0.95 的 PPV 或 NPV 来选择阈值，并生成相应的可视化图表。<br>
如果输入是 Model_data 对象，函数会自动更新其best.model.result槽位存储最优阈值<br>


``` r
# 选择最佳阈值并生成可视化图表
object_model<-ModelThreshold(object_model,
                          method_threshold="max_accuracy")
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/accuracy_vs_threshold_curve.png" alt="Screenshot" width=500">
</div>

**临床预测**<br>
`ModelClinicalPrediction`函数用于使用`Model_data`对象中的最佳模型对新临床数据进行预测。<br>
支持基于最佳阈值生成预测结果，并可视化预测概率和分类。<br>
可根据上述函数`ModelThreshold`得到的结果也可以自定义，这里选取通用的0.5

``` r
# 对新临床数据进行预测
# 使用 ModelClinicalPrediction 进行预测（自动提取最佳阈值）
Clinical_results <- ModelClinicalPrediction(object = object_model, new_data = new_data)

# 使用自定义阈值进行预测
Clinical_results <- ModelClinicalPrediction(object = object_model, new_data = new_data, best_threshold = 0.5)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/prediction_visualization.png" alt="Screenshot" width=500">
</div>

### 3.亚型分型模块
#### 3.1 数据加载
在亚型分型模块中，您可以使用可选的现有对象(Stat/Subtyping/PrognosiX/Model_data)或清洗后的数据来创建 `Subtyping` 对象。<br>

``` r
# 使用 Stat 对象创建 Subtyping 对象
object_sub <- CreateSubtypingObject(object=object_stat)

# 使用 PrognosiX 对象创建 Subtyping 对象
object_sub <- CreateSubtypingObject(object=object_pn)

# 使用 Model_data 对象创建 Subtyping 对象
object_sub <- CreateSubtypingObject(object=object_model)

# 使用清洗后的数据创建 Subtyping 对象
object_sub <- CreateSubtypingObject(data=clean_data)
```

#### 3.2 数据标准化
`Sub_normalize_process` 函数对数据进行标准化处理。支持多种标准化方法，如对数变换、最小-最大缩放、Z 分数标准化等默认`normalize_method = "min_max_scale"`<br>
如果输入是 Subtyping 对象，函数会自动更新对象的`scale.data`槽位，存储数据标准化处理结果。<br>

``` r
# 对数据进行标准化处理
object_sub<-Sub_normalize_process(object_sub,
                              normalize_method ="min_max_scale")
```

#### 3.3分型分析-基于非负矩阵分解(NMF)

**NMF分解与初步分析**
`Sub_nmf_estimate`函数用于执行非负矩阵分解(NMF)分析。该函数通过对输入数据进行NMF分解，能够有效识别数据中的潜在模式，并自动生成以下关键可视化结果：<br>
1.NMF拟合评估图 - 展示不同rank值下的模型拟合优度<br>
2.共识矩阵图 - 直观呈现样本聚类稳定性<br>

在使用时需注意以下参数设置：<br>
`rank_range`参数控制测试的聚类数目范围，默认`rank_range = 2:4`，范围扩大虽能探索更多可能性但会显著增加计算时间 <br>
`nrun`参数决定每个rank值的重复计算次数，适当增加可提高结果稳定性，但需权衡计算成本 <br>
默认采用`method = "brunet"`分解算法，该方法是NMF的标准实现，在多数数据集上表现稳健 <br>
如果输入是 Subtyping 对象，函数会自动更新对象的`cluster.results`槽位，在`nmf.result`中存储nmf初步分析的结果<br>

``` r
object_sub<-Sub_nmf_estimate(object_sub,
                                  rank_range = 2:4,
                                  nrun = 10,
                                  method = "brunet")
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/consensus_map.png" alt="Screenshot" width=500">
</div>
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/nmf_plot.png" alt="Screenshot" width=500">
</div>

**最优秩选择**<br>
`Sub_best_rank_analysis`函数通过计算cophenetic相关系数自动确定最优rank值，并使用该最优rank重新运行NMF分解生成基矩阵热图，同时将最佳rank值返回。<br>
如果输入是 Subtyping 对象，函数会自动更新对象的`cluster.results`槽位，在`nmf.result`中存储最优秩的结果，`Optimal.cluster`更新最佳亚组数量<br>

``` r
object_sub<-Sub_best_rank_analysis(object_sub,
                                  nrun = 10,
                                  method = "brunet")
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/basismap_plot.png" alt="Screenshot" width=500">
</div>

**样本分组**<br>
`Sub_assign_groups`函数用于根据最优秩的NMF分解结果将样本分配到不同的亚型组<br>
如果输入是 `Subtyping 对象`，函数会自动更新对象的`clustered.data`槽位，保存分组后数据<br>

``` r
object_sub<-Sub_assign_groups(object_sub)
```
**可视化**<br>
提供t-SNE和UMAP两种降维可视化方法，用于直观展示NMF聚类结果。<br>
如果输入是 `Subtyping 对象`，函数会自动更新对象的`visualization.results`槽位，保存分组后数据<br>

``` r
# 完整可视化流程
object_sub <- Sub_tsne_analyse(object_sub)  # t-SNE计算
object_sub <- Sub_plot_tsne(object_sub)      # t-SNE绘图

object_sub <- Sub_umap_analyse(object_sub)  # UMAP计算
object_sub <- Sub_plot_umap(object_sub)      # UMAP绘图
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/tsne_clustering_plot-1.png" alt="Screenshot" width=500">
</div>
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/umap_clustering_plot-1.png" alt="Screenshot" width=500">
</div>

#### 3.4分型分析-基于K-means聚类

**K-means聚类分析**
`Sub_kmeans_with_optimal_k`函数能基于轮廓系数法自动确定最佳聚类数(K值)并执行稳健的K-means聚类分析(nstart=25)，同时支持结果可视化。<br>
如果输入是 `Subtyping 对象`，函数会自动更新对象的`cluster.results`槽位，存储完整的聚类分析结果<br>
`Optimal.cluster`记录确定的最佳亚组数量，并且更新槽位`clustered.data`保存带有分组标签的分析数据<br>

``` r
object_sub<-Sub_kmeans_with_optimal_k(object_sub)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/silhouette_plot.png" alt="Screenshot" width=500">
</div>

**可视化**<br>
与nmf分析流程相似K-means聚类分析也可以用t-SNE和UMAP两种降维可视化方法，用于直观展示K-means聚类结果。<br>
如果输入是 `Subtyping 对象`，函数会自动更新对象的`visualization.results`槽位，保存分组后数据<br>

``` r
# 完整可视化流程
object_sub <- Sub_tsne_analyse(object_sub)  # t-SNE计算
object_sub <- Sub_plot_tsne(object_sub)      # t-SNE绘图
object_sub <- Sub_umap_analyse(object_sub)  # UMAP计算
object_sub <- Sub_plot_umap(object_sub)      # UMAP绘图
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/tsne_clustering_plot.png" alt="Screenshot" width=500">
</div>
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/umap_clustering_plot.png" alt="Screenshot" width=500">
</div>

### 4.预后建模与生存分析模块
#### 4.1 数据加载
在预后建模与生存分析模块中，您可以使用可选的现有对象(Stat/Subtyping/PrognosiX)或清洗后的数据来创建 `PrognosiX` 对象。<br>
​默认列名设置：当用户未明确指定时，默认使用生存时间列`time_col = "time"`,生存状态列`status_col = "status"`<br>
自我转化机制：无论输入数据中原始列名为何，在对象创建过程中，系统会自动将指定的时间/状态列重命名为标准名称`"time"`以及`"status"`

``` r
# 使用 Stat 对象创建 Subtyping 对象
object_pn<-CreatePrognosiXObject(object=object_stat,
                             time_col ="time",
                             status_col = "event")

# 使用 PrognosiX 对象创建 Subtyping 对象
object_pn <- CreatePrognosiXObject(object=object_pn,
                             time_col ="time",
                             status_col = "event")

# 使用清洗后的数据创建 Subtyping 对象
object_sub <- CreatePrognosiXObject(data=clean_data,
                             time_col ="time",
                             status_col = "event")

```

#### 4.2 变量分箱
`perform_subgroup_analysis`函数是将连续变量转换为分类变量，便于后续的亚组生存分析。
该函数提供两种最优分割方法:
- ROC曲线法：基于二元状态变量的最佳区分点
- MaxStat法：基于生存时间的最优分割点

如果输入是 PrognosiX 对象，函数会自动更新对象的`sub.data`槽位，保存分箱后的数据。

``` r
object_pn<-perform_subgroup_analysis(object_pn,
                                     methods = "roc")
```

#### 4.3 生成基线表
`Prognos_gaze_analysis` 用于生成临床基线特征表的函数<br>
通过`use_subgroup_data`参数可灵活选择使用原始数据或分箱后的亚组数据进行计算，并支持通过`response_var`参数指定分组变量（如治疗方案、生存状态等）进行组间差异比较，最终生成规范化基线特征表。<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`baseline.table`槽位，保存基线信息。

``` r
object_pn <- Prognos_gaze_analysis(
  object_pn,
  use_subgroup_data =T,
  response_var = "group")
```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/gaze_analysis-pn.png" alt="Screenshot" width=500">
</div>

#### 4.4  数据简单清洗
`PrepareDataForPrognosiX` 函数数据类型简单转化符合后续分析
如果输入是 PrognosiX 对象，函数会自动更新对象的`sub.data`和`survival.data`槽位,保存清洗后的数据
``` r
object_pn <- Prognos_gaze_analysis(object_pn)
```

#### 4.5  单变量回归分析

**单变量Cox回归分析**<br>
`Prognos_cox_univariate_analysis`函数能够执行单变量Cox比例风险回归分析，评估各特征与生存结局的独立关联性。<br>
该函数支持两种数据输入方式：<br>
- 可直接分析PrognosiX对象中的生存数据（survival.data）或亚组分析数据（sub.data）由参数use_subgroup_data调节 。<br>
- 用户也可指定待分析的变量列表（selected_vars）<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的univariate.analysis槽位保存单因素分析结果<br>
``` r
##单因素分析
object_pn<-Prognos_cox_univariate_analysis(object_pn,
                                               selected_vars =NULL)
#> object_pn@univariate.analysis[["all_univariate_results"]][["significant_results"]]
#            Variable        HR  CI_lower  CI_upper      P_value          HR_95CI         se
#4  PreS1antigenofHBV 0.7811950 0.6723037 0.9077232 1.263983e-03 0.78 (0.67-0.91) 0.07658912
#5               TPAb 0.7963130 0.6953908 0.9118820 9.874696e-04   0.8 (0.7-0.91) 0.06914200
#11               INR 1.1475945 1.0144955 1.2981556 2.861338e-02  1.15 (1.01-1.3) 0.06289619
#13                TT 1.1406791 1.0073308 1.2916799 3.797581e-02 1.14 (1.01-1.29) 0.06342846
#14        Fibrinogen 1.2766036 1.1294602 1.4429167 9.292968e-05 1.28 (1.13-1.44) 0.06248131
#15               FDP 1.2231168 1.0976850 1.3628816 2.639883e-04  1.22 (1.1-1.36) 0.05520354
```


**森林图**<br>
`forest_plot_univariate_analysis`函数能够将单变量Cox回归结果以森林图形式直观呈现。
通过`result_type`参数可选择展示"all"（全部结果）或"significant"（仅p<0.05的显著结果）。

``` r
###森林图绘制
object_pn<-forest_plot_univariate_analysis(object_pn,
                                        result_type = "significant")
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/forest_plot.png" alt="Screenshot" width=500">
</div>

**生存分析**<br>
`plot_var_kaplan_meier`函数能够根据指定分组变量生成Kaplan-Meier生存曲线及风险表。`var_col`参数指定分组变量。
如果输入是 `PrognosiX 对象`，函数会自动更新对象的univariate.analysis槽位
``` r
var_col="group"
time_col = "time"
status_col = "status"
object_object_pn<-plot_var_kaplan_meier(object_pn, var_col = "group")

```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/survival_time_distribution_by_group.png" alt="Screenshot" width=500">
</div>

**生存时间差异**<br>
`plot_var_survival_time`函数用于比较不同分类变量间的生存时间分布特征。通过`var_col`参数指定分组变量<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的univariate.analysis槽位<br>

``` r
object_pn<-plot_var_survival_time(object_pn,
                               var_col = "group")
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/survival_distribution_by_group.png" alt="Screenshot" width=500">
</div>

**非线性回归分析**<br>
`Pron_univariate_regression`函数用于非线性关联分析，通过限制性立方样条(RCS)方法精确建模连续变量与临床结局的复杂关系。<br>
基于AIC准则选择最优节点数`knots`构建RCS模型，支持三种回归模型类型`method = c("linear", "logistic", "cox")`默认采用"Cox"模型<br>
`y_label`参数用于指定结局变量（如生存状态），当选择Cox模型时还需通过`time_col`参数设置生存时间变量<br>
`x_label`参数则用于指定要分析的主要自变量（如"age"或"tumor_size"等连续变量）。<br>
在模型构建方面，knots参数（默认值3，建议范围3-5）控制RCS曲线的灵活度，节点数越多曲线拟合越复杂<br>
adjust_vars参数允许用户输入需要调整的混杂因素变量名（如c("age","gender")），以控制潜在混杂效应；<br>
prob参数（默认0.1）则用于设置剂量反应曲线的参考百分位点，通常取第10百分位数作为参考基准。<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的univariate.analysis槽位。<br>
``` r
object_pn<-Pron_univariate_regression(object_pn,
                                      method = "cox",
                                      x_label = "cl",
                                      knots = 3,  
                                      adjust_vars = NULL,  
                                      prob = 0.1)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/fig.cox_lshapall.png" alt="Screenshot" width=500">
</div>
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/fig.cox_nshapall.png" alt="Screenshot" width=500">
</div>
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/fig.cox_proball.png" alt="Screenshot" width=500">
</div>
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/fig.cox_ushapall.png" alt="Screenshot" width=500">
</div>

**时间依赖性ROC分析**<br>
`plot_var_roc_plot`函数用时间依赖性ROC曲线分析,用于评估生物标志物或临床变量在不同时间点的预测效能。通过`var_col`参数指定分组变量
如果输入是 PrognosiX 对象，函数会自动更新对象的univariate.analysis槽位

``` r
object_pn <- plot_var_roc_plot(object_pn, var_col = "cl")
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/time_dependent_roc_by_cl.png" alt="Screenshot" width=500">
</div>

#### 4.6 数据集划分
`SplitDataPrognosiX`函数用于数据分割，能够将数据集智能划分为训练集和测试集<br>
该函数通过`train_ratio`和`test_ratio`参数（默认采用7:3比例），通过`unwanted_var`参数设置去除指定变量<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的split.data槽位,保存划分后的数据集<br>
``` r
###自动排除名为"group"的列
object_pn<-SplitDataPrognosiX(object_pn,
                           unwanted_var ="group",
                           train_ratio = 0.7,
                           test_ratio = 0.3)
```

#### 4.7 特征筛选

`run_lasso_feature_selection`函数通过LASSO回归（最小绝对收缩和选择算子）自动筛选与生存结局最相关的预测变量。<br>
该模块首先使用交叉验证确定最优正则化参数lambda（`lambda.1se`标准），然后基于此筛选非零系数变量，最终生成两个专业可视化结果：
1) 交叉验证曲线图（`lasso_cv_visualization`）展示不同lambda值下的模型误差，标注最优lambda位置；
2) 特征重要性图（`lasso_feature_importance`）以系数大小排序显示各变量的贡献度。
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`feature.result`槽位保存特征筛选过程
``` r
###执行LASSO特征选择分析
object_pn <- run_lasso_feature_selection(object_pn)
#object_pn@feature.result[["important_vars"]]
#[1] "TT"  "PLT" "PCT" "rct" "alp" "k"   "tp" 

### LASSO交叉验证可视化
object_pn <- lasso_cv_visualization(object_pn)

### LASSO特征重要性可视化
object_pn <- lasso_feature_importance(object_pn)
```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/lasso_path_visualization.png" alt="Screenshot" width=500">
</div>
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/lasso_feature_importance.png" alt="Screenshot" width=500">
</div>

#### 4.7 特征过滤
`Prognos_filter_features`函数根据分析结果过滤特征子集数据<br>
`use_filtered_features`默认（use_filtered_features=T）由于本次LASSO分析筛选得到的显著特征数量较少，为保留更全面的预后信息，本次分析将不应用特征筛选结果，而继续使用原始特征集进行分析。<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`filtered.set`槽位。
``` r
###保留所有原始特征（不应用LASSO筛选结果）
object_pn <- Prognos_filter_features(object_pn, use_filtered_features=FALSE)
```

#### 4.8预后模型构建

**多模型预后分析**<br>
`train_all_models`是预后分析集成建模函数，支持7种主流预后模型的训练与评估：Ridge回归、Lasso回归、偏最小二乘(PLS)、CoxBoost、Cox比例风险模型(CoxPH)、超级主成分分析(SuperPC)和随机生存森林(RSF)。<br>
该模块通过`model_list = c("ridge", "lasso", "pls", "coxboost", "coxph", "superpc", "rsf")`参数灵活控制需要训练的模型类型<br>
执行完整的分析流程：<br>
✅ ​自动训练与交叉验证<br>
✅ ​ROC曲线与C-index评估<br>
✅ ​Kaplan-Meier生存分析<br>
✅ ​风险比（HR）计算<br>
✅ 森林图可视化<br>

如果输入是 PrognosiX 对象，函数会自动更新对象的`survival.model`槽位保存模型分析结果

**快速开始**
执行了这一步下面的模块可以不执行
``` r
###提取训练集和测试集
data_sets <- pn_filtered_set(object_pn)
train_data <- data_sets$training
test_data <- data_sets$testing
###预后模型分析
object_pn <- train_all_models(object_pn,
                               model_list = c("ridge", "lasso", "pls", "coxboost", "coxph", "superpc", "rsf"))

```

**coxph回归预后模型**<br>
支持自动交叉验证的Cox比例风险模型训练功能，通过分层抽样评估模型性能(C-index指标)，支持数据预处理核函数和可视化分析
模型性能评估包括：
1) ROC曲线分析（evaluate_roc_coxph_model）计算训练集和测试集的AUC值与C-index；
2) 生存分析验证（evaluate_km_coxph_model）基于风险评分中位数划分高低风险组，输出Kaplan-Meier曲线和风险比(HR)
3) 风险比计算（coxph_compute_hr_and_ci）提供各变量的HR及95%置信区间；
4) 森林图可视化（forest_plot_coxph_model）展示各预测因子的效应大小。
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`survival.model`槽位保存模型coxph_model分析结果
``` r
###训练coxph回归预后模型
object_pn<-train_coxph_model(object_pn,
                             nfolds = 10)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/cv_coxph_plot.png" alt="Screenshot" width=500">
</div>

``` r
###计算训练集和测试集的AUC和C-index并生成ROC曲线图
object_pn <- evaluate_roc_coxph_model(object_pn)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/ROC_curves-coxph.png" alt="Screenshot" width=500">
</div>

``` r
###执行Kaplan-Meier生存分析验证
###基于风险评分中位数划分高低风险组
###计算HR及95%CI，生成生存曲线图
object_pn <- evaluate_km_coxph_model(object_pn)
```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/coxph_time_distribution_test.png" alt="Screenshot" width=500">
</div>

``` r
###计算各变量的风险比(HR)及置信区间
object_pn <- coxph_compute_hr_and_ci(object_pn)

###生成森林图
object_pn <- forest_plot_coxph_model(object_pn)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/forest_plot-coxph.png" alt="Screenshot" width=500">
</div>


**Ridge回归预后模型**<br>
`train_ridge_model`函数实现了完整的Ridge回归预后模型分析流程，通过L2正则化处理高维临床数据中的共线性问题。<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`survival.model`槽位保存模型ridge_model分析结果<br>
``` r
###训练Ridge回归预后模型
object_pn<-train_ridge_model(object_pn,
                             nfolds = 10)
###计算训练集和测试集的AUC和C-index并生成ROC曲线图
object_pn <- evaluate_roc_ridge_model(object_pn)
###执行Kaplan-Meier生存分析验证
###基于风险评分中位数划分高低风险组
###计算HR及95%CI，生成生存曲线图
object_pn <- evaluate_km_ridge_model(object_pn)
###计算各变量的风险比(HR)及置信区间
object_pn <- ridge_compute_hr_and_ci(object_pn)
###生成森林图
object_pn <- forest_plot_ridge_model(object_pn)
```


**LASSO回归预后模型**
实现了完整的LASSO回归预后分析流程，通过L1正则化处理高维临床数据中的特征选择问题<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`survival.model`槽位保存模型lasso_model分析结果<br>
``` r
###训练Lasso回归预后模型
object_pn <- train_lasso_model(object_pn, nfolds = 10)
###计算训练集和测试集的AUC和C-index并生成ROC曲线图
object_pn <- evaluate_roc_lasso_model(object_pn)
###执行Kaplan-Meier生存分析验证
###基于风险评分中位数划分高低风险组
###计算HR及95%CI，生成生存曲线图
object_pn <- evaluate_km_lasso_model(object_pn)
###计算各变量的风险比(HR)及置信区间
object_pn <- lasso_compute_hr_and_ci(object_pn)
###生成森林图
object_pn <- forest_plot_lasso_model(object_pn)
``` 


**PLS回归预后模型**<br>
本模块提供用于训练偏最小二乘(PLS)生存分析模型的函数，专门针对右删失数据。实现包含交叉验证用于最优参数选择。<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`survival.model`槽位保存模型pls_model分析结果<br>


``` r
###训练pls回归预后模型
object_pn <- train_pls_model(object_pn,
                           nfolds = 10,
                           method = "efron",
                           se = TRUE,
                           scaleX = TRUE)

###计算训练集和测试集的AUC和C-index并生成ROC曲线图
object_pn <- evaluate_roc_pls_model(object_pn)

###执行Kaplan-Meier生存分析验证
###基于风险评分中位数划分高低风险组
###计算HR及95%CI，生成生存曲线图
object_pn <- evaluate_km_pls_model(object_pn)
###计算各变量的风险比(HR)及置信区间
object_pn <- pls_compute_hr_and_ci(object_pn)
###生成森林图
object_pn <- forest_plot_pls_model(object_pn)
```


**RSF回归预后模型**<br>
提供支持自动交叉验证的随机生存森林(RSF)预后建模功能，通过调节树数量(ntree)、节点大小(nodesize)和分割规则(splitrule)等参数优化模型，并支持训练过程监控和性能评估。<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`survival.model`槽位保存模型rsf_model分析结果<br>
``` r
###训练rsf回归预后模型
object_pn<-train_rsf_model(object_pn,
                           nfolds = 10,
                           ntree = 1000,
                           nodesize = 10,
                           splitrule = "logrank")
###计算训练集和测试集的AUC和C-index并生成ROC曲线图
object_pn <- evaluate_roc_rsf_model(object_pn)
###执行Kaplan-Meier生存分析验证
###基于风险评分中位数划分高低风险组
###计算HR及95%CI，生成生存曲线图
object_pn <- evaluate_km_rsf_model(object_pn)
###计算各变量的风险比(HR)及置信区间
object_pn <- rsf_compute_hr_and_ci(object_pn)
###生成森林图
object_pn <- forest_plot_rsf_model(object_pn)
```

**Coxboost回归预后模型**<br>
提供支持自动参数优化的CoxBoost生存分析模型训练功能，通过交叉验证确定最佳迭代步数(stepno)和惩罚参数(penalty)，支持并行计算和可视化输出<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`survival.model`槽位保存模型coxboost_model分析结果<br>

``` r
###训练coxboost回归预后模型
object_pn<-train_coxboost_model(object_pn,
                                trace = TRUE,
                                parallel = FALSE,
                                stepno = 100,
                                maxstepno = 500,
                                K = 10,
                                type = "verweij",
                                penalty = NULL,
                                multicore = 1)
###计算训练集和测试集的AUC和C-index并生成ROC曲线图
object_pn <- evaluate_roc_coxboost_model(object_pn)
###执行Kaplan-Meier生存分析验证
###基于风险评分中位数划分高低风险组
###计算HR及95%CI，生成生存曲线图
object_pn <- evaluate_km_coxboost_model(object_pn)
###计算各变量的风险比(HR)及置信区间
object_pn <- coxboost_compute_hr_and_ci(object_pn)
###生成森林图
object_pn <- forest_plot_coxboost_model(object_pn)
```

#### 4.9 最佳模型提取与外部验证

**最佳模型提取**

`extract_model_results`函数则基于指定指标`metric = "C_index"/"ROC_AUC"`从训练好的生存分析模型提取最优模型<br>
如果输入是 `PrognosiX 对象`，函数会自动更新对象的`best.model`槽位保存最优模型结果<br>

``` r
object_pn<-extract_model_results(object_pn,
                                 metric = "C_index")
#> object_pn@best.model[["best_model_results"]]
#   Dataset   C_index   ROC_AUC       Model
#9    Train 0.9770971 0.8532556 coxph_model
#10    Test 0.9728736 0.8299667 coxph_model
>
```

**决策曲线分析(DCA)**
提供决策曲线分析(Decision Curve Analysis, DCA)功能，用于评估临床预测模型的有用性<br>
``` r
object_pn <- plot_dca_best_model(
  object = object_pn,
``` 
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/dca-coxph.png" alt="Screenshot" width=500">
</div>

**外部验证**<br>
验证数据集整合（支持Stats对象或数据框两种输入格式）支持多种生存模型在独立验证集上的自动化测试(ROC曲线、KM生存分析）<br>

``` r
###输入清洗后的 Stats 对象
object_pn <- ExtractPrognosiXValidata(
  object_stats = object_val,  # 清洗后的 Stats 对象
  object_prognosix = object_pn,  # 目标 PrognosiX 对象
  time_col = "time",  # 指定生存时间列名
  status_col = "event"  # 指定事件状态列名
)
###输入清洗后的数据框
object_pn <- ExtractPrognosiXValidata(
  data = val_data,  # 清洗后的数据框
  object_prognosix = object_pn,  # 目标 PrognosiX 对象
  time_col = "time",  # 指定生存时间列名
  status_col = "event"  # 指定事件状态列名
)

# 2. 验证模型性能
object_pn <- evaluate_best_model_val(object_pn)
```
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/ROC_curves-val.png" alt="Screenshot" width=500">
</div>
<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/coxph_time_distribution_val.png" alt="Screenshot" width=500">
</div>


#### 4.10 临床应用
`PrognosiXClinicalPrediction` 函数用于对新临床样本进行风险预测和可视化<br>


``` r
# 基本调用方式
pred_results <- PrognosiXClinicalPrediction(
  object = object_pn,      # 训练好的PrognosiX模型对象
  new_data = val_data      # 新临床样本数据
)
```

<div align="center">
<img src="https://github.com/OchangjingluO/Icare/blob/master/fig/risk_group_prediction.png" alt="Screenshot" width=500">
</div>


