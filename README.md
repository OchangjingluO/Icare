
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

注释变量的类型（数值型、分类型、需要独热编码的变量）。<br>
如果一个分类变量的唯一值数量`max_unique_values = 5`超过 5，则认为其需要进行独热编码处理。<br>
如果输入是 Stat 对象，函数将注信息存储在`variable.types`槽中
``` r
# 注释变量类型
object_stat <- stat_diagnose_variable_type(object_stat)
```

#### 1.2 缺失值处理
变量缺失值的分布 <br>
如果输入是 Stat 对象，函数将缺失值信息存储在`processe.info`槽中
``` r
object_stat<-state_plot_missing_data(object_stat)
```
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
使用 `stat_detect_and_mark_outliers` 函数检测数据中的异常值，并对其进行标记。<br>
如果输入是 Stat 对象，函数将异常值信息存储在`outlier.marked`槽中
``` r
# 检测并标记异常值
object_stat <- stat_detect_and_mark_outliers(object_stat)
```
**异常值处理**
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
`stat_compute_descriptive` 函数用于计算数据的描述性统计信息，包括数值型变量的均值、中位数、标准差等，以及分类变量的频数统计。<br>
此外，它还会对数值型变量进行正态性检验，并根据检验结果分别计算正态和非正态变量的统计量。<br>
如果输入是 Stat 对象，函数将统计结果存储在`compute.descriptive`槽中
``` r
object_stat <- stat_compute_descriptive(object_stat)

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
**数据类型转换**<br>
`stat_convert_variables `函数用于将数据中的变量转换为正确的数据类型。例如，将字符型变量转换为因子型，或将数值型变量保留为数值型。

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
默认为 `normalize_method = "log_transform"` <br>
如果输入是 Stat 对象，函数会自动更新对象的 scale.data 槽位，存储标准化后的数据。
``` r
# 使用对数变换标准化数据
object_stat <- stat_normalize_process(object_stat, normalize_method = "log_transform")

# 使用最小-最大缩放标准化数据
object_stat <- stat_normalize_process(object_stat, normalize_method = "min_max_scale")
```
#### 1.8 相关性分析与交叉变量分析
**相关性分析**<br>
- 检测高度相关特征：根据指定的相关性阈值（correlation_threshold）默认为 0.95，高于该阈值的特征对将被视为高度相关。<br>
​生成相关性热图：可视化特征之间的相关性矩阵，支持自定义调色板。<br>
`data_type`数据类型，可选 "clean" 或 "scale"，默认为 "scale"<br>
如果输入是 Stat 对象，函数会自动更新对象的 corr.result槽位，存储相关性分析结果。
``` r
# 检测高度相关特征并生成相关性热图
object_stat <- stat_correlated_features(object_stat, data_type="scale,correlation_threshold = 0.95)
```
**相关性热图（Top N 特征）** <br>
`cor_top_correlations `函数用于生成相关性热图，展示前 N 个最相关的特征。

``` r
# 检测高度相关特征并生成相关性热图
# 生成前 15 个最相关特征的相关性热图
object_stat <- cor_top_correlations(object_stat,top_n = 15)
```
**交叉变量分析** <br>
`cross_plot_analysis` 函数用于对两个变量进行交叉分析，生成散点图并计算相关性。
``` r
# 对两个变量（如cl和cr）进行交叉分析
cross_plot_analysis(object_stat, vars = c("cl", "cr"))
```
#### 1.9 差异分析与可视化
`stat_var_feature` 函数用于对数据进行差异分析，识别在不同组别之间显著变化的特征。<br>
注意本功能只在group_col存在的情况下进行使用<br>
如果输入是 Stat 对象，函数会自动更新对象的 var.result 槽位，存储差异分析结果。<br>
- Wilcoxon 检验：使用 Wilcoxon 秩和检验比较两组之间的差异。
​- 多重检验校正：对 p 值进行 Bonferroni 校正，控制假阳性率。
- ​差异特征筛选：根据 logFC 和 p 值筛选显著变化的特征。
- ​生物标志物筛选：差异分析可用于识别潜在的生物标志物。
- ​数据探索：可视化工具（如火山图、小提琴图）可用于探索数据分布和差异。
- 模型评估：ROC 曲线图可用于评估特征的分类性能。
**进行差异分析**
``` r
# 进行差异分析
object_stat <- stat_var_feature(object_stat, data_type = "scale")
#> object_stat@var.result[["last_test_sig"]]
#                  id       W            p       mean_x
#1                alb 18289.0 4.879498e-08 3.886615e+01
#10               L_r 19655.5 7.168725e-06 2.035346e+01
#9                L_c 20400.0 7.431591e-05 1.434423e+00
#12               N_r 31538.5 8.916802e-05 6.961962e+01
#6         Fibrinogen 31503.0 9.891832e-05 4.241808e+00

```
**火山图可视化** <br>
`VarFeature_volcano`函数用于生成火山图，展示差异分析结果。
``` r
# 生成火山图
object_stat <- VarFeature_volcano(object_stat)
```
**小提琴图可视化** <br>
`VarFeature_violinplot` 函数用于生成小提琴图，展示显著变化特征的分布。
``` r
# 生成小提琴图
object_stat <- VarFeature_violinplot(object_stat)
```
**ROC 曲线图可视化** <br>
`VarFeature_ROC` 函数用于生成 ROC 曲线图，评估显著变化特征的分类性能。
``` r
# 生成 ROC 曲线图
object_stat <- VarFeature_ROC(object_stat)
```
