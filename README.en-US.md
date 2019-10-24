# Agile Service

`Agile Service` is the core service of Choerodon.

The service is responsible for Agile process management and providing users  
with a better user experience through rich display.

Agile management services are primarily used to manage project requirements, planning, and execution**

![image](https://minio.choerodon.com.cn/knowledgebase-service/file_829d7e921c984b4385b250a9a2e4adcc_blob.png)

## Features

* **Version Management**
* **Sprint Management**
* **Issues Management**
* **Backlog Management**
* **Module Management**
* **Story Map**
* **Project Configuration**
* **State Configuration**
* **Report**

## Requirements

* Java8
* Redis
* [MySQL](https://www.mysql.com)

## Installation and Getting Started

1. init database

``` sql
CREATE USER 'choerodon'@'%' IDENTIFIED BY "choerodon";
CREATE DATABASE agile_service DEFAULT CHARACTER SET utf8;
GRANT ALL PRIVILEGES ON agile_service.* TO choerodon@'%';
FLUSH PRIVILEGES;
```

2. run command `sh init-local-database.sh`
3. run command as follow or run `AgileServiceApplication` in IntelliJ IDEA

``` bash
mvn clean spring-boot:run
```

## Dependencies

* `go-register-server`: Register server
* `iam-service`：iam service
* `mysql`: agile_service database
* `api-gateway`: api gateway server
* `oauth-server`: oauth server
* `manager-service`: manager service
* `file-service` : file service
* `knowledgebase-service` : knowledgebase service
* `notify-service` : notify service

## Reporting Issues

If you find any shortcomings or bugs, please describe them in the [issue](https://github.com/choerodon/choerodon/issues/new?template=issue_template.md).

## How to Contribute

Pull requests are welcome! [Follow](https://github.com/choerodon/choerodon/blob/master/CONTRIBUTING.md) to know for more information on how to contribute.