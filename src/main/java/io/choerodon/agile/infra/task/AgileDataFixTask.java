package io.choerodon.agile.infra.task;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import io.choerodon.agile.app.service.FixDataService;
import io.choerodon.asgard.schedule.QuartzDefinition;
import io.choerodon.asgard.schedule.annotation.JobTask;
import io.choerodon.asgard.schedule.annotation.TimedTask;

import org.hzero.core.base.BaseConstants;

@Component
public class AgileDataFixTask {

    private static final Logger LOGGER = LoggerFactory.getLogger(AgileDataFixTask.class);

    @Autowired
    private FixDataService fixDataService;
    @Autowired
    private JdbcTemplate jdbcTemplate;

    @JobTask(maxRetryCount = 3,
            code = "fixStatusTransferRoleData",
            description = "升级到2.2.0,修复状态联动角色数据,迁移工作组数据")
    @TimedTask(name = "fixStatusTransferRoleData",
            description = "升级到2.2.0,修复状态联动角色数据,迁移工作组数据",
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

    @JobTask(maxRetryCount = 1,
            code = "fixIssueDuplicateUnclosedSprintRel",
            description = "修复批量编辑BUG造成的工作项同时关联了多个未关闭冲刺的脏数据")
    @TimedTask(name = "fixIssueDuplicateUnclosedSprintRel",
            description = "修复批量编辑BUG造成的工作项同时关联了多个未关闭冲刺的脏数据",
            oneExecution = true,
            repeatCount = 0,
            repeatInterval = 1,
            repeatIntervalUnit = QuartzDefinition.SimpleRepeatIntervalUnit.HOURS,
            params = {})
    public void fixIssueDuplicateUnclosedSprintRel(Map<String, Object> param) {
        LOGGER.info("==============================>>>>>>>> fixIssueDuplicateUnclosedSprintRel start <<<<<<<<=================================");
        final List<Long> invalidRelIds = this.jdbcTemplate.queryForList("SELECT id \n" +
                "FROM agile_issue_sprint_rel aisr2 \n" +
                "WHERE issue_id IN (\n" +
                "\tSELECT t1.issue_id \n" +
                "\tFROM (\n" +
                "\t\tSELECT\n" +
                "\t\t\taisr.issue_id, count(*) AS ct\n" +
                "\t\tFROM\n" +
                "\t\t\tagile_issue_sprint_rel aisr\n" +
                "\t\t\tJOIN agile_sprint asp ON aisr.sprint_id = asp.sprint_id \n" +
                "\t\tWHERE\n" +
                "\t\t\tasp.status_code != 'closed'\n" +
                "\t\tGROUP BY aisr.issue_id HAVING ct > 1\n" +
                "\t) t1\n" +
                ")\n" +
                "AND aisr2.id != (\n" +
                "\tSELECT MAX(ID) \n" +
                "\tFROM agile_issue_sprint_rel aisr3 \n" +
                "\tWHERE aisr2.issue_id = aisr3.issue_id\n" +
                ")", Long.class);
        if(CollectionUtils.isNotEmpty(invalidRelIds)) {
            LOGGER.info("==============================>>>>>>>> find {} error issue sprint rel, fixing... <<<<<<<<=================================", invalidRelIds.size());
            final List<List<Long>> partition = ListUtils.partition(invalidRelIds, 1000);
            for (List<Long> partitionInvalidRelIds : partition) {
                this.jdbcTemplate.update("delete from agile_issue_sprint_rel where id in (" + partitionInvalidRelIds.stream().map(String::valueOf).collect(Collectors.joining(BaseConstants.Symbol.COMMA)) + ")");
            }
        } else {
            LOGGER.info("==============================>>>>>>>> no error issue sprint rel found, skip... <<<<<<<<=================================");
        }
        LOGGER.info("==============================>>>>>>>> fixIssueDuplicateUnclosedSprintRel completed <<<<<<<<=================================");
    }
}
