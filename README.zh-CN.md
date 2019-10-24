# **敏捷服务**

**`敏捷服务`** **是猪齿鱼平台的核心服务**.

**该服务负责敏捷流程管理，包括问题管理、待办事项、发布版本、活跃冲刺、模块管理、报告等，并通过丰富的图形界面为用户提供更好的使用体验**

**敏捷管理服务主要用来管理项目的 需求、计划和 执行**  
![image](https://minio.choerodon.com.cn/knowledgebase-service/file_829d7e921c984b4385b250a9a2e4adcc_blob.png)

## 特性

* **<span style="color:#0c29f2"><span style="color:#5869df"><span style="color:#7a87e3">`版本管理 `：</span></span></span>根据项目周期，您可以对软件项目追踪不同的版本，同时可以将对应的问题分配到版本中**

* **<span style="color:#7a87e3">`活跃冲刺 `：</span>敏捷看板的活跃冲刺展示了团队目前正在进行的冲刺，能在此创建、更新、删除问题，也能将问题直接拖拽到其它列的任意状态中**

* **<span style="color:#7a87e3">`问题管理 `：</span> 问题是任何项目的基石，一个问题可以是一个史诗，一个故事，一个任务，一个缺陷等。**

* **<span style="color:#7a87e3">`代办事项管理 `：</span>待办事项是未完成的工作列表，您可以构建一个新的待办事项，或者对现有的待办事项进行处理，包括创建、排序和优先级别评估**

* **<span style="color:#7a87e3">`模块管理 `：</span> 根据项目需求，可以分拆为多个模块，每个模块可以进行负责人划分，配置后可以将项目中的问题归类到对应的模块中。例如“后端任务”，“基础架构”等等**

* **<span style="color:#7a87e3">`用户故事地图 `：</span> 利于产品管理者以及团队成员将产品待办问题清单转化为可视化的过程，确保团队以“客户至上”的心态来建立他们的产品，快速且实时地传递价值**

* **<span style="color:#7a87e3">`报告 `：</span>报告会根据您项目的进展情况以多个维度直观地记录和展示您项目、迭代、版本、进度等汇总情况**

## 基础需求

* Java8
* Redis
* [MySQL](https://www.mysql.com)

## 安装和启动

1.初始化数据库


```sql
CREATE USER 'choerodon'@'%' IDENTIFIED BY "choerodon";
CREATE DATABASE agile_service DEFAULT CHARACTER SET utf8;
GRANT ALL PRIVILEGES ON agile_service.* TO choerodon@'%';
FLUSH PRIVILEGES;
```


1. 运行命令 `sh init-local-database.sh`
2. 运行如下命令 或者 在 IntelliJ IDEA 中运行 `AgileServiceApplication`

``` bash
mvn clean spring-boot:run
```

## 服务依赖

* `go-register-server`: 注册中心
* `mysql`: 敏捷服务数据库
* `api-gateway`: 网关服务
* `oauth-server`: 权限认证中心
* `manager-service`: 配置及路由管理服务
* `file-service` : 文件服务
* `knowledgebase-service` : 知识管理服务
* `notify-service` : 通知服务
* `iam-service`：用户、角色、权限、组织、项目、密码策略、快速代码、客户端、菜单、图标、多语言等管理服务

## 问题通知

如果您发现任何缺陷或bug，请及时告知我们.  
[Reporting Issues](https://github.com/choerodon/choerodon/issues/new?template=issue_template.md).

## 如何贡献

欢迎您通过 [Follow](https://github.com/choerodon/choerodon/blob/master/CONTRIBUTING.md) 了解更多关于如何贡献的信息！