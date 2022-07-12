package io.choerodon.agile.infra.task;

import io.choerodon.agile.app.service.FixDataService;
import io.choerodon.asgard.schedule.QuartzDefinition;
import io.choerodon.asgard.schedule.annotation.JobTask;
import io.choerodon.asgard.schedule.annotation.TimedTask;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class AgileDataFixTask {

    private static final Logger LOGGER = LoggerFactory.getLogger(AgileDataFixTask.class);

    @Autowired
    private FixDataService fixDataService;

    @JobTask(maxRetryCount = 3,
            code = "fixStatusTransferRoleData",
            description = "升级到2.2.0,修复状态联动角色数据")
    @TimedTask(name = "fixStatusTransferRoleData",
            description = "升级到2.2.0,修复状态联动角色数据",
            oneExecution = true,
            repeatCount = 0,
            repeatInterval = 1,
            repeatIntervalUnit = QuartzDefinition.SimpleRepeatIntervalUnit.HOURS,
            params = {})
    public void fixStatusTransferRoleData(Map<String, Object> map) {
        LOGGER.info("==============================>>>>>>>> AGILE Data Fix Start, Version: 2.2.0 <<<<<<<<=================================");
        fixDataService.fixStatusMachineCustomTransferRoleData();
        LOGGER.info("==============================>>>>>>>> AGILE Data Fix End, Success! Version: 2.2.0 <<<<<<<<=================================");
    }
}
