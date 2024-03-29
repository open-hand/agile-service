# 更新日志
这个文件记录agile-service所有版本的重大变动。

## [1.1.0] - 2021-11-05

### 新增

- 增加工作日历功能。
- 甘特图支持按史诗视图查看。
- 甘特图支持自定义任务顺序。
- 甘特图支持自定义列字段。
- 甘特图支持快速创建工作项及子工作项。
- 针对工作项详情增加系统字段“实际开始时间”、“实际结束时间”。
- 甘特图右侧横道图展示支持按照“实际开始时间、实际结束时间”字段渲染延期情况。
- 组织层增加甘特图。
- 针对工作项详情增加系统字段“预估时间”。
- 增加工作日历功能。
- 新增工时日志功能。


## [1.1.0-alpha] - 2021-09-18

### 新增

- 支持自定义协作图表。
- 项目报告支持插入自定义图表。
- 项目概览支持添加自定义图表。
- 甘特图支持查看多个迭代的任务。
- 甘特图支持按冲刺视图查看。
- 甘特图支持按照经办人、预计开始时间排序。
- 通知配置支持自定义人员字段。
- 通知配置支持选择主要负责人。
- issue通知增加关注人。
- 支持发送每日工作提醒。
- 页面支持按照角色配置字段显示。
- 页面支持字段级联设置。
- 复制issue支持必输字段校验。
- 开启迭代支持检查是否预估故事点和工时。
- 所有问题列表支持字段升降序排序。
- 所有问题列表支持将问题字段值置空。
- 状态机支持子任务已完成时可拖动父级任务的条件验证。

### 改变

- issue日志详情支持显示史诗概要。
- issue详情支持显示评论数量。
- issue日志详情优化记录花费时间。
- issue日志增加环境、主要负责人。
- 优化issue评论回复内容展示。
- 优化待办事项，默认只展开当前冲刺。
- 导出问题筛选增加按概要筛选。
- 导出问题样式优化。
- 工作台卡片样式优化。
- 优化工作台点击issue详情为新开标签页。
- 所有问题增加常用筛选“我经手的”。
- 优化待办事项issue可以拖拽到未展开的迭代内。
- 优化敏捷消息通知内容。
- 优化界面loading动效。

### 修复

- 修复快捷创建issue时未显示概要默认值的问题。
- 修复富文本编辑器若干编辑缺陷。
- 修复甘特图左右滚动白页的问题。
- 修复创建issue时问题链接链入链出方向错误的问题。
- 修复项目报告添加静态列表table字段显示问题。
- 修复个人筛选重名校验的问题。



## [1.0.0] - 2021-06-18

### 新增

- 增加输入提示。
- 子任务支持问题类型转换。
- 增加每人每日工作量图表。
- 增加个人工作量统计。
- 所有问题支持自定义列表字段显示、顺序以及列宽度。
- 新增复制问题时支持字段可选复制。
- 新增快速创建问题时支持选择经办人。
- 项目报告增加一键折叠。
- 问题详情新增支持快速创建缺陷。
- 新增测试执行状态变更触发问题状态变更。
- 新增分支合并触发关联问题状态变更。
- 模块列表增加顺序。
- 兼容Safari浏览器。


### 改变

- 配置看板时支持切换看板。
- 优化问题详情关联问题。
- 优化评论问题默认倒序显示。
- 快速创建必填项提示优化。
- 优化快速创建子任务后自动打开详情。
- 优化问题详情更多按钮顺序。
- 优化问题评论发送消息。
- 优化状态机页面。
- 优化时间显示格式。
- 优化自定义问题类型值集过多时加载缓慢。

### 修复

- 修复UI/UX文件上传后无法预览的问题。
- 修复甘特图左右移动时间周期边长的问题。
- 修复状态自定义排序偶发乱序的问题。



## [0.25.0] - 2021-04-09

### 新增

- 问题详情支持上传UI&UX压缩文件并支持在线查看（支持zip、rar4、tar、tar.gz）。
- 个人工作台增加"向我报告的"。
- 个人工作台增加"我经手的"。
- 移动问题支持转交项目。
- 问题列表支持平铺列表查看。
- 问题关联分支可查看来源分支。
- 问题详情开发项支持查看关联分支及其commit。  
- 自定义字段增加多选人员控件。
- 平台项目类型支持根据后台服务进行关联限制。
- 自定义与预定义字段支持维护默认值，并支持默认值同步。
- 需求/issue/特性支持回复，并且向相关方发送通知。
- 导入导出支持保存常用模板。
- 项目报告新增devops报表。
- issue状态支持排序。
- 定时通知相关人员问题/冲刺逾期信息。
- issue导入支持导入自定义字段。
- 项目概览增加"操作动态"。
- 菜单访问API支持菜单维度控制。
- 增加组织层看板模板。
- 增加组织层状态机模板。
- 所有人员类型字段支持拼音搜索匹配。

### 改变

- 模块设置页可以调整排序及加载更多。
- 自定义字段单选、多选值列表支持搜索。
- 附件上传支持分片上传。
- 新建问题支持设置状态。
- 迭代计划看板增加多筛选字段。
- 发送webhook通知时通知对象非必填。
- 富文本编辑器调整增加支持表格操作。  
- 部分页面样式优化。
- 部分报表优化。

### 修复

- 修复待办事项快捷拖动issue数据不一致的问题。
- 修复状态机初始状态设置自定义流转不生效的问题。
- 修复看板仅拖动问题并未更改状态时触发通知和邮件。
- 修复快速创建问题时快速点击导致创建多条重复的问题项。
- 修复甘特图问题状态变更，延期信息错误。


## [0.24.0] - 2020-12-24

### 新增

- 工作台-我的事项增加我的关注。
- 支持关注问题项。
- 新增甘特图功能，以便项目管理员进行任务排期。
- 工作列表问题项支持批量删除。
- 问题项支持关联或新建测试用例。
- 故事地图新增冲刺泳道。
- 新增绩效功能。
- 新增项目报告。
- 新增触发器，可设置自动变更时自动变更经办人，发消息通知。
- 看板支持问题延期提醒。


### 改变

- 敏捷状态机支持自定义状态流转、状态变更通知、流转条件限制、父子级问题状态联动、状态流转后自动变更属性等。
- 优化页面字段配置。
- 优化故事地图样式。
- 部分页面样式优化。
- 部分报表优化。


### 修复

- 修复分支管理筛选报错。
- 修复待办事项按经办人筛选失效。
- 修复批量修改修复的版本报错。
- 修复配置看板移动列报错。
- 修复工作列表-所有问题保存筛选报错。




## [0.23.0] - 2020-09-30

### 新增

- 新增项目概览页，以便项目成员快速了解迭代研发进度。

### 改变

- 优化所有问题一键展开。
- 优化工作列表筛选。

### 修复

- 修复issue创建、分配和完成时站内信与邮件模版不正确的问题


## [0.22.0] - 2020-08-01

### 新增

- 工作列表-所有问题按issue层级展示。
- 工作列表-所有问题支持批量修改issue。
- 工作列表-所有问题支持全部字段筛选，包括全部预定义字段、自定义字段。
- 敏捷看板支持全屏显示。
- 敏捷看板支持查看多个迭代。
- 导入问题支持按照任务-子任务、故事-子任务父子层级导入。
- 敏捷看板支持自定义状态顺序。
- 敏捷消息添加预置钉钉、企业微信webhook模板。

### 改变

- 待办事项史诗侧栏优化为不显示已完成史诗。
- 优化子任务详情页：可以直接看到父级任务的概要。
- 优化敏捷服务权限。
- 优化导出问题性能。
- 优化待办事项团队成员工作量显示。
- 部分页面样式优化。
- 部分报表优化。

### 修复

- 修复issue详情副文本框图片粘贴重复显示。
- 修复创建状态接口权限校验错误。


## [0.21.0] - 2020-03-06

### 新增

- 配置看板支持删除状态。
- 导入问题支持导入父子级关系，用户可以在导入故事或任务时同时导入子任务。
- 敏捷消息通知支持邮件方式。

### 改变

- 优化敏捷看板性能。
- 优化工作列表性能。
- 优化配置看板状态设置为已完成保存不生效的问题。
- 优化配置看板自定义状态顺序。
- 优化待办事项批量拖拽问题数量显示。
- 优化问题链接页面样式。
- 优化自定义字段页面样式。

### 修复

- 修复故事地图全屏显示菜单栏的问题。
- 修复迭代计划工作台刷新后无数据的问题。
- 修复问题详情剩余预估时间名称显示错误的问题。
- 修复故事地图史诗特定情况无法查看评论的问题。
- 修复设置敏捷模块负责人显示undefined的问题。


## [0.20.0] - 2019-12-20

### 新增

- 支持通过点击待办事项上的经办人进行筛选。
- 故事地图支持在需求池按无版本筛选。
- 筛选问题支持选择历史版本和历史冲刺。

### 改变

- 子任务支持关联知识。
- 看板默认泳道配置调整为经办人。
- 优化配置看板在无冲刺时也可以配置。
- 去除侧栏顶部文字说明。
- 字段解释icon、说明样式统一。
- 统一字体颜色以及字号大小。

### 修复

- 修复问题详情知识链接跳转问题。
- 修复问题列表的优先级字段缺失。
- 修复问题详情快速创建子任务多次点击会重复创建。


## [0.19.0] - 2019-10-18

### 新增

- 新增在用户故事泳道中点击名称查看详情。
- 合并issue服务，状态机服务，基础服务到agile服务。
- 新增对故事类型转换的限制，如果有子任务，只能转成任务。
- 新增附件图片、pdf、word、excel等文件的预览。
- 创建bug类型的模板，给出初始的模板。

### 改变

- 全局样式调整。
- 取消面包屑最后一级，只留到菜单级别。
- 优化下拉选择时数据的加载。
- 故事地图重构后操作优化。
- 创建问题自定义字段加载优化。
- 工作列表的待办事项UI优化。
- 问题管理列表搜索重构。

### 修复

- 修复修改父任务同时修改冲刺。
- 修复当成员任务全部为子任务时，未正确返回人员信息。
- 修复问题中修改问题模块，模块复选框选中状态还是上一次的状态。
- 修复模块管理中删除模块时移动问题无法获取其他模块。
- 修复问题设置状态页面筛选状态列表为空时不能创建状态。
- 修复详情页修改了优先级，列表页没有更新。

## [0.18.0] - 2019-06-21

### 新增

- 新添加一个问题类型feature。
- 故事地图重构,主要包括：支持新问题类型feature的规划；支持规划所有的故事；只显示无泳道与版本泳道。
- 在故事下可以快速创建子任务。
- 新增人员类型的自定义字段。
- 新增自定义字段修改的活动日志。

### 改变

- 优化部分接口性能。
- 对于平台中已经停用的用户，人员列表不再显示。
- 父任务可以看到所有子任务状态的进度条。
- 部分页面样式优化。
- 部分报表优化。

### 修复

- 修复问题导入描述字段特殊字符报错。
- 修复版本名称为空的版本也可以创建的BUG。

## [0.17.0] - 2019-05-24

### 新增

- 项目群完成PI时，将自动完成PI下的sprint，team前端接受到提示。
- 项目群-项目设置可以查看项目信息。
- 项目群feature管理列表功能升级、可以进行高级搜索。
- PM可以在项目群路线图查看3个PI的feature待办事项。
- 项目群可以根据工作日历查看工作时间。
- 用户可以在故事中直接创建缺陷。
- 项目群成员可以通过公告板查看各个团队以及迭代之间的依赖关系。
- 特性查询模式下支持排序。
- 项目群看板添加快速搜索。

### 改变

- 问题关联关系展示关联的测试用例。
- issue导入模板增加模块、冲刺等字段。
- issue详情页面的宽窄样式优化。
- 自定义字段优化相关优化。
- 部分页面样式优化。
- 部分报表优化。

### 修复

- 修复史诗筛选的PI显示BUG。
- 修复ART列表时间显示BUG。


## [0.16.0] - 2019-04-19

### 新增

- 项目群ART设置，支持创建、修改、开启、停用ART，以及ART下的PI列表的展示。
- 项目群特性列表，包括计划模式和查询两种模式，并且支持创建特性。
- 项目群看板，支持特性的移动、展示等。
- 项目群看板的配置，包括列与状态的配置。
- 项目群项目设置，支持修改项目编码。
- 项目群ART日历，支持查看正在进行中的ART的PI规划以及PI下的冲刺规划。
- 项目群PI目标，包括列表和卡片两种模式，支持创建、修改、删除、查询PI目标。
- 项目群的team中的story可以关联待处理或处理中状态的feature。
- 项目群中开启PI后，为项目群中的每个team同步生成sprint，同时不允许删除、创建新的sprint。
- 创建问题/编辑问题页面，加载自定义字段列表，适应宽窄样式，保存值修改值。

### 改变

- 项目成员可以在项目首页查看未分配的任务，支持分页。
- 当一个故事下的子任务被移动到下一个冲刺中，记住之前的状态。
- 部分页面样式优化。
- 部分报表优化。

### 修复

- 史诗报告中不同维度下数据的展示。
- 问题管理中根据名称搜索不准确。
- 修复5.1节假日调整

## [0.15.0] - 2019-03-22

### 新增

- 组织层中，优先级自定义以及按照实际的顺序排列。
- 问题管理的高级搜索功能。
- 问题的导入、导出。
- 活跃冲刺在制品限制。
- DEMO功能添加。

### 改变

- 搜索查询时，对于有值列表的字段进行字段的显示。
- 问题管理支持自定义筛选显示的字段。
- 待办事项的排列以及显示性能优化。
- 在故事中创建子任务时，页面中显示故事信息。
- 问题详情页，登记工作日志的必填项提示。
- 只允许自己和项目所有者才能修改报告人。
- 部分页面样式优化。
- 部分报表优化。

### 修复
- 无泳道看板，取消收起功能。
- 快速搜索页面，搜索时后端sql报错。
- 问题详情页，问题链接同一关系可以关联同一问题多次。
- 燃尽图和待办事项中的剩余问题数/剩余时间数不相符。
- 待办事项中，版本的问题计数bug。


## [0.14.0] - 2019-02-22

### 新增

- 看板中卡片停留时间预警提示。
- 冲刺名称重复提示。
- 看板成员权限限制。
- 创建问题时，支持关联关系。
- 故事点、任务时间支持0.5小数点。

### 改变

- 对待办事项中已完成的issue进行展示，将编号进行中线划掉。
- 看板中的故事泳道下，对于子任务全部完成的故事进行排序。
- 活动日志的描述优化。
- 问题链接、问题管理、版本详情、发布版本、模块、快速搜索筛选添加或修改。
- 史诗泳道下，板上支持查看故事，并体现故事与子任务的关联。
- 任务卡片在不同泳道进行筛选后的显示优化。
- 创建模块相关逻辑问题优化修改。
- 影响的版本可以选到全部版本。
- 部分页面样式优化。
- 部分报表优化。

### 修复

- 副文本粘贴网络图片时数量错误的问题。
- 修复关闭冲刺的时候,燃尽图问题数量重复计算的问题。
- 修改了史诗名称，待办列表中issue未同步刷新。
- 统计图版本迭代为空时报错。
- 工作日历非节假日计算bug。
- 故事点统计的状态颜色错误。


## [0.13.0] - 2019-01-11

### 新增

- 统计图增加标签维度，同时增加冲刺、版本、时间过滤条件。
- 面板设置中新增修改面板名称功能，同时增加重名校验。
- 问题详情窄样式增加日志信息。
- 问题在创建的时候支持填入工时、故事点。
- 增加史诗、模块、版本重名校验。

### 改变  

- 待办事项中选中问题用户动作监听优化。
- 部分页面样式优化。
- 优化燃尽图计算逻辑。

### 修复

- 待办事项中计划中冲刺人员信息统计重复。
- 累积流图脏数据修复，需要手动调用进行修复。
- 创建状态、删除状态导致状态机草稿配置表产生脏数据后发布不可用。
- 日期选择器的节假日显示错误。


## [0.12.0] - 2018-12-14

### 新增

- 版本详情筛选功能：版本详情支持高级过滤筛选。
- 活跃冲刺问题拖动排序功能：活跃冲刺面板上的问题支持拖动排序。
- 活跃冲刺成员筛选过滤功能：活跃冲刺支持团队成员筛选过滤功能。
- 工作日历新增2019年法定节假日数据。
- 版本详情新增问题链接。
- 发布版本新增预计发布日期。

### 改变  

- 问题创建人可以删除自己创建的问题。
- 任务转化为子任务后状态自动修改为默认状态。
- 完成冲刺时，未完成的子任务随父任务一并移动到下个冲刺。
- 创建快速搜索的关系字段显示名称改为中文。
- 待办事项中冲刺的经办人工作量修改为问题总数、问题剩余数、总任务工时、剩余任务工时。
- 创建版本时结束日期修改为预计发布日期。
- 发布版本的时候需要输入实际发布时间。
- 面板设置列约束只允许项目所有者修改。
- 合并版本只能选择规划中的版本。
- 版本详情创建日期修改为开始日期。
- 部分页面样式优化。

### 修复

- 版本统计未完成问题计数错误。
- 问题详情优先级下拉列表显示不全。
- 版本名称为中文时创建失败。

## [0.11.0] - 2018-11-16

### 新增

- 问题类型自定义功能：新增问题服务，问题服务支持自定义问题类型，支持问题类型图标自定义，自定义的问题类型将会在敏捷服务应用。
- 问题优先级自定义功能：问题服务支持自定义问题优先级，自定义的问题优先级将会在敏捷服务应用。
- 问题状态机功能：新增状态机服务，问题状态更新、创建、删除由状态机服务控制。

### 改变

- 问题管理新增字段展示、字段搜索、字段排序。
- 问题管理支持自定义筛选。
- 问题管理界面子任务也显示在列表中。
- 活跃冲刺中的问题拖到其他位置，问题及其子任务全部还原到状态机初始状态。
- 问题详情表单页面优化。
- 产品全局插画优化。
- 活跃冲刺界面展示优化。
- 待办事项史诗计数详情优化。
- 日历样式与操作优化。
- 日历工作日与节假日按年份返回当前年份及下一年份数据。
- 问题链接列表显示经办人信息。
- 迭代速度图未开启的冲刺不统计。

### 修复

- 问题管理中工作日志时间登记后页面数据没有更新。
- 待办事项版本、史诗排序错误。
- 活跃冲刺及迭代工作台剩余时间计算错误。
- 版本报告缓存没有及时更新。
- 活跃冲刺中同列多个状态拖动白屏。

## [0.10.5] - 2018-10-22

### 新增

- 站内信通知功能：用户可以在组织层对问题创建、问题分配、问题解决3个事件分配对应的通知对象。
- 时区日历功能：用户可以在组织层设置时区、节假日、工作日并应用在敏捷管理中。
- 版本管理搜索功能：版本管理列表增加字段搜索功能。
- 模块管理搜索功能：模块管理列表增加字段搜索功能。
- 冲刺工作日功能：用户可以在开启冲刺选择日期的时候，勾选当前冲刺期间的工作日与非工作日。
- 燃尽图期望值工作日筛选功能：用户可以在查看燃尽图的时候对期望值进行工作日与非工作日展示。

### 改变

- 修改问题信息的时候，可以通过回车或者点击空白处进行保存输入的数据。
- 创建冲刺后页面增加引导性提示。
- 故事、任务、史诗、子任务和缺陷图标修改。
- 活跃冲刺剩余时间运用日历设置。
- 活跃冲刺切换工作台按钮样式修改。
- 故事地图中移除问题添加验证。
- 优化待办事项创建问题请求。

### 修复

- 问题详情中不输入值可以直接创建问题链接。
- 问题转换为子任务状态颜色不正确。
- 问题详情页面组件没有对齐。
- 发布版本跳转未解决问题列表筛选错误。
- 链接地址中文未做转码处理导致请求重复。
- 发布版本的问题列表中子任务图标错误。
- 活跃冲刺的问题卡片无史诗样式问题。
- 燃尽图报告点击子任务进入的是父任务详情。
- 史诗和版本燃耗图中链接到问题管理，返回页面404。
- 迭代工作台冲刺详情的表格一次加载全部数据，点击分页时重复加载数据。
- 发布版本时统计未完成数量不正确。

## [0.10.0] - 2018-09-24

### 新增

- 史诗燃耗图功能：用户可以在报表界面中选择史诗燃耗图，图表和报告展示了团队在不同史诗中取得的工作进展，并预估未来冲刺完成趋势。
- 版本燃耗图功能：用户可以在报表界面中选择版本燃耗图，图表和报告展示了团队在不同版本中取得的工作进展，并预估未来冲刺完成趋势。
- 冲刺工作台功能：用户可以在冲刺工作台中查看问题的状态、优先级、经办人、类型分布，冲刺的简要信息、燃尽图、问题列表详情。
- 报告工作台功能：用户可以在报告工作台中查看累积流图、经办人分布图等图表的实时数据。
- 故事地图导出图片功能。
- 故事地图全屏操作功能。

### 改变

- 故事地图的滑动功能可以更加流畅。
- 故事地图在移动问题时可以记录其位置。
- 故事地图在需求池拖动问题的时候可以记录其位置。
- 部分页面内存优化。
- 报告中的燃尽图、冲刺报告可以建立缓存保留上一次的选择的冲刺及其单位。
- 修改看板配置中添加状态的样式。
- 累积流量图获取时间函数优化。
- 仪表盘中的版本进度过滤掉归档版本。
- 设置中创建问题链接增加了重名校验。
- 故事地图各种泳道支持上下拖动排序。

### 修复

- 活跃冲刺中拖动到有多种状态的一列处，会出现页面堆叠现象。
- 活跃冲刺界面拖动问题排序，页面数据延迟问题。
- 待办事项创建问题执行时间过长。
- 问题管理导出Excel中问题的描述带有格式。
- 冲刺燃尽图根据问题数量统计在一部分条件下加载失败。

### 移除

- 分页功能，若低于10条则不显示分页工具栏。

## [0.9.5] - 2018-08-31

### 新增

- 仪表板功能：用户可以在主页中自定义配置仪表板，仪表板包含：燃尽图、版本进度、史诗进度、我的未完成问题等。
- 用户故事地图功能：用户故事地图是以史诗为基础，根据版本、冲刺维度对问题进行规划管理。
- 用户故事地图泳道功能：用户可以选择无、版本、冲刺对问题进行泳道划分并记录用户选择泳道。
- 用户故事地图需求池功能：用户可以在用户故事地图中的需求池过滤查看所有未分配史诗的问题。
- 用户故事地图问题拖动功能：用户可以将问题在不同史诗、版本、冲刺之间拖动，也可以在需求池中拖动到地图面板上。
- 报告图表缓存功能：对报告中的图表通过Redis进行缓存。
- 统计图中的问题可以跳转到问题管理(不包含子任务)。
- 待办事项、活跃冲刺、发布版本、模块管理的单元测试。
- 记录用户活跃冲刺对应看板的泳道设置。
- 问题管理导出Excel包含子任务。

### 改变

- 部分页面样式修改。
- 部分页面内存优化。
- 累积流图查询优化。

### 修复

- 待办事项拖动问题重复生成日志。
- 待办事项无问题导致冲刺数据不返回。
- 待办事项问题拖动到未对应版本失败。
- 活跃冲刺面板配置中有列约束时，通过修改待办事项的状态可以直接忽略约束。
- 版本管理删除版本失败。

## [0.9.0] - 2018-08-17

### 新增

- 版本拖动排序功能：用户可以在版本管理界面、待办事项界面对版本进行拖动排序。
- 史诗拖动排序功能：用户可以在待办事项界面对史诗进行拖动排序。
- 快速搜索排序功能：用户可以在设置中的快速搜索界面对快速搜索进行拖动排序。
- 迭代速度图功能：用户可以通过选择故事点、问题计数、剩余时间查看不同冲刺对应的问题完成比例柱状图。
- 史诗报告功能：用户可以选择不同史诗通过故事点、问题计数、剩余时间查看当前冲刺的已完成、未完成、未完成未预估的问题，同时还可以查看对应的汇总数据。
- 问题统计图功能：用户可以根据经办人、模块、问题类型、修复版本、优先级、状态、冲刺、史诗、解决结果查看项目下的问题统计饼图。
- 问题详情返回功能：用户在任意界面点击问题详情查看后，都可以退回到原页面。
- 新增agile-service基于Spock编写的单元测试。
- 问题详情中操作添加创建分支功能。
- 修改状态为已完成时生成该状态下的问题解决日志。
- 修改冲刺名称增加长度限制。

### 改变

- agile-service消息机制由Kafka修改为Saga。
- 对版本报告图请求时间过长进行优化。
- 对燃尽图请求时间过长进行优化。
- 待办事项界面样式调整。
- 优化待办事项界面中史诗和版本加载过慢问题。
- 日志处理逻辑重构。
- 版本状态样式修改。

### 修复

- 待办事项界面内存溢出问题。
- 燃尽图数据不一致。
- 累积流图数据不一致。
- 模块管理创建模块后数据展示不一致。


### 移除

- 问题详情左侧窄栏工作日志、活动日志的显示。
- 对史诗及其子任务的累积流图统计信息。

## [0.8.0] - 2018-07-20

### 新增

- 问题分支管理功能：用户可以在问题详情中操作与问题关联的Gitlab远程仓库分支，包括创建、合并、查看分支信息。
- 版本报告功能：用版本报告显示了您的团队在完成版本方面的进展，版本报告可根据：剩余预估时间、故事点、问题计数进行筛选，版本报告还会根据您的团队自版本开始以来的平均进度（速度）以及估计的剩余工作量向您显示预测的发布日期。
- 累积流程图功能：累积流程图是一个区域图，显示应用程序、版本、sprint的各种工作项状态。水平x轴表示时间，垂直y轴表示问题计数，图表的每个彩色区域等同于面板上列的问题变化，累积流程图可用于识别瓶颈，如果您的图表包含随时间垂直加宽的区域，则等于加宽区域的列通常会成为瓶颈。
- 测试类型问题功能：问题类型新增测试类型。测试类型的问题用于“测试管理”模块之中，用户可以从该模块中创建测试类型问题，用于管理测试用例。
- 项目默认设置功能：项目管理员可以设置项目的默认经办人、问题默认优先级，若用户未设置默认经办人、优先级，则系统根据项目设置的默认经办人、优先级创建，特别的是，项目默认经办人优先级低于模块默认经办人。
- 用户默认面板功能：用户选择面板后会记录在系统中，用户再次点击进入面板中，将会展示用户选择后的面板。
- 问题导出Excel功能：用户可以根据选择的条件过滤出问题并导出到表格中。
- 问题转换为子任务功能：用户可以将其他类型问题转换为子任务，特别的是，故事转化为子任务，故事点会修改为0。
- 问题复制功能：用户可以通过选择参数复制问题，参数包括：问题链接、子任务，复制问题会生成一条与原问题的复制类型链接。
- 版本界面新增查看发布日志。
- 版本日志Markdown文档导出功能：用户可以在发布版本的版本日志中将问题信息导出为Markdown文档。

### 改变

- 史诗类型问题默认初始颜色修改。
- 更新问题的版本关联，不能删除已经归档的版本关联。
- 优化搜索接口，修改触发逻辑。
- 优化燃尽图数据查询接口。
- 版本发布时间显示字段由开始时间修改为发布时间。
- 面板中未分配泳道没有问题时隐藏。
- 面板中的问题卡片可以查看史诗信息。
- 修改菜单顺序中活跃冲刺首位。
- 修改问题详情中的史诗名称位置。
- 问题详情样式优化。
- 待办事项问题列表样式优化。
- 去除项目设置中的项目编号重名校验。
- 面板问题排列每次刷新按照一定顺序刷新。
- 待办事项中多选问题时，若有点击其中一个问题的详情，则以点开的问题为基准多选。
- 项目创建初始化测试类型问题。
- 问题详情中操作添加转化为子任务、复制操作。
- 发布版本问题可以通过点击链接到问题管理中。
- 报告界面可以关联查看问题列表和每个问题详情。
- 报告控制台添加版本报告、累积流程图入口。
- 报告切换新增版本报告、累积流程图。
- 冲刺报告中的问题可以通过报告中的分组跳转到问题管理界面查看相关问题。
- 面板设置中泳道类型新增根据史诗泳道展示问题。

### 修复

- 问题详情锚点定位不准确。
- 问题详情中所属史诗颜色与史诗颜色不一致。
- 问题基于故事展示时，选择仅我的问题后，父任务不属于同一经办人泳道展示的缺陷。
- 选择面板样式问题。
- 活跃冲刺故事点统计颜色错误。
- 简易创建问题卡顿。
- 筛选器创建时筛选人员限制20个人员。
- 问题详情选择经办人、报告人组件问题。
- 选择链接关联问题时最多只能选择400条。
- 选择链接关联问题搜索结果创建失败。
- 待办事项问题拖动到版本中，对应版本中问题列表没有实时刷新。
- 界面提示文本错误。
- 列表展示数据操作加载延迟。
- 待办事项问题拖动至冲刺，冲刺人员信息没有更新。
- 问题标题为编辑状态时切换时，编辑框内容会被清除。
- 问题拖动到版本中没有记录日志。
- 新建项目创建问题，问题编号从2开始。
- 问题管理快速创建史诗没有史诗名称。
- 富文本编辑器在多英文的情况下断词失败。
- 更改问题类型，故事更改为其他类型故事点没有置为0，史诗类型更改为其他类型时之前属于该史诗下的问题没有更新。
- 火狐浏览器下面板展示样式错误。
- 史诗问题不存在时面板泳道错误。