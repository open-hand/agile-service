FROM registry.cn-shanghai.aliyuncs.com/choerodon-tools/javabase:0.8.2
COPY app.jar /agile-service.jar
CMD java $JAVA_OPTS $SKYWALKING_OPTS -jar /agile-service.jar