//package io.choerodon.agile.infra.task;
//
//import io.choerodon.agile.infra.feign.NotifyFeignClient;
//import io.choerodon.asgard.schedule.QuartzDefinition;
//import io.choerodon.asgard.schedule.annotation.JobTask;
//import io.choerodon.asgard.schedule.annotation.TimedTask;
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.stereotype.Component;
//
//import java.util.Map;
//
//@Component
//public class DataMigrationTask {
//
//    private static final Logger LOGGER = LoggerFactory.getLogger(DataMigrationTask.class);
//
//    @Autowired
//    private NotifyFeignClient notifyFeignClient;
//
//    @JobTask(maxRetryCount = 3,
//            code = "migrationAgileToBase",
//            description = "升级到0.20.0,同步敏捷数据到base")
//    @TimedTask(name = "migrationAgileToBase",
//            description = "升级到0.20.0,同步敏捷数据到base",
//            oneExecution = true,
//            repeatCount = 0,
//            repeatInterval = 1,
//            repeatIntervalUnit = QuartzDefinition.SimpleRepeatIntervalUnit.HOURS,
//            params = {})
//    public void migrationAgileToBase(Map<String, Object> map) {
//        LOGGER.info("==============================>>>>>>>> AGILE Data Migrate Start <<<<<<<<=================================");
//        notifyFeignClient.checkLog("0.20.0", "agile");
//    }
//}
