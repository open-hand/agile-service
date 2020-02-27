FROM registry.cn-shanghai.aliyuncs.com/c7n/javabase:0.9.0
WORKDIR /choerodon
COPY app.jar agile-service.jar
CMD java $JAVA_OPTS $SKYWALKING_OPTS -jar /choerodon/agile-service.jar