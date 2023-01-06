package io.choerodon.agile.infra.task;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import com.yqcloud.core.oauth.ZKnowDetailsHelper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.collections4.SetUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import io.choerodon.agile.app.service.FixDataService;
import io.choerodon.agile.infra.enums.PersonalFilterTypeCode;
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

    @JobTask(productSource = ZKnowDetailsHelper.VALUE_CHOERODON,
            maxRetryCount = 3,
            code = "fixPersonalFilter",
            description = "升级到2.3.0,修复工作项/特性/瀑布个人筛选数据到高级筛选格式")
    @TimedTask(name = "fixPersonalFilter",
            description = "升级到2.3.0,修复工作项/特性/瀑布个人筛选数据到高级筛选格式",
            oneExecution = true,
            repeatCount = 0,
            repeatInterval = 1,
            repeatIntervalUnit = QuartzDefinition.SimpleRepeatIntervalUnit.HOURS,
            params = {})
    public void fixPersonalFilter(Map<String, Object> map) {
        LOGGER.info("==============================>>>>>>>> AGILE Data Fix Start, Version: 2.3.0 <<<<<<<<=================================");
        Set<String> typeCodes =
                SetUtils.unmodifiableSet(PersonalFilterTypeCode.AGILE_ISSUE, PersonalFilterTypeCode.FEATURE_ISSUE, PersonalFilterTypeCode.WATERFALL_ISSUE);
        fixDataService.fixPersonalFilter(typeCodes);
        LOGGER.info("==============================>>>>>>>> AGILE Data Fix End, Success! Version: 2.3.0 <<<<<<<<=================================");
    }

    @JobTask(productSource = ZKnowDetailsHelper.VALUE_CHOERODON,
            maxRetryCount = 1,
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

    @JobTask(productSource = ZKnowDetailsHelper.VALUE_CHOERODON,
            maxRetryCount = 1,
            code = "2.1&2.2-fixIssueSprintRelZeroData",
            description = "修复工作项详情,清空当前活跃冲刺时,向关系表中插入的脏数据")
    @TimedTask(name = "2.1&2.2-fixIssueSprintRelZeroData",
            description = "修复工作项详情,清空当前活跃冲刺时,向关系表中插入的脏数据",
            oneExecution = true,
            repeatCount = 0,
            repeatInterval = 1,
            repeatIntervalUnit = QuartzDefinition.SimpleRepeatIntervalUnit.HOURS,
            params = {})
    public void fixIssueSprintRelZeroData(Map<String, Object> param) {
        LOGGER.info("==============================>>>>>>>> fixIssueSprintRelZeroData start <<<<<<<<=================================");
        this.jdbcTemplate.update(
                "DELETE \n" +
                "FROM\n" +
                "\tagile_issue_sprint_rel \n" +
                "WHERE\n" +
                "\tsprint_id = 0"
        );
        LOGGER.info("==============================>>>>>>>> fixIssueSprintRelZeroData completed <<<<<<<<=================================");
    }

    @JobTask(productSource = ZKnowDetailsHelper.VALUE_CHOERODON,
            maxRetryCount = 1,
            code = "2.2-clearIssueSprintRelOldDirtyData",
            description = "清除上古时期刚上线项目群的时候,issue冲刺关系表中不存在的冲刺的脏数据")
    @TimedTask(name = "2.2-clearIssueSprintRelOldDirtyData",
            description = "清除上古时期刚上线项目群的时候,issue冲刺关系表中不存在的冲刺的脏数据",
            oneExecution = true,
            repeatCount = 0,
            repeatInterval = 1,
            repeatIntervalUnit = QuartzDefinition.SimpleRepeatIntervalUnit.HOURS,
            params = {})
    public void clearIssueSprintRelOldDirtyData(Map<String, Object> param) {
        LOGGER.info("==============================>>>>>>>> clearIssueSprintRelOldDirtyData start <<<<<<<<=================================");
        this.jdbcTemplate.update(
                "DELETE isr " +
                    "FROM agile_issue_sprint_rel isr " +
                    "WHERE NOT EXISTS (" +
                        "SELECT 1 " +
                        "FROM agile_sprint s " +
                        "WHERE s.sprint_id = isr.sprint_id " +
                    ")"
        );
        LOGGER.info("==============================>>>>>>>> clearIssueSprintRelOldDirtyData completed <<<<<<<<=================================");
    }

    @JobTask(productSource = ZKnowDetailsHelper.VALUE_CHOERODON,
            maxRetryCount = 1,
            code = "2.2-clearNoIssueWorkLog",
            description = "清除关联的工作项不存在的工时日志数据")
    @TimedTask(name = "2.2-clearNoIssueWorkLog",
            description = "清除关联的工作项不存在的工时日志数据",
            oneExecution = true,
            repeatCount = 0,
            repeatInterval = 1,
            repeatIntervalUnit = QuartzDefinition.SimpleRepeatIntervalUnit.HOURS,
            params = {})
    public void clearNoIssueWorkLog(Map<String, Object> param) {
        LOGGER.info("==============================>>>>>>>> clearNoIssueWorkLog start <<<<<<<<=================================");
        this.jdbcTemplate.update(
                "delete A " +
                    "from agile_work_log A " +
                    "WHERE not exists(" +
                        "SELECT 1 FROM agile_issue B WHERE A.issue_id = B.issue_id" +
                    ")"
        );
        LOGGER.info("==============================>>>>>>>> clearNoIssueWorkLog completed <<<<<<<<=================================");
    }

    @JobTask(productSource = ZKnowDetailsHelper.VALUE_CHOERODON,
            maxRetryCount = 1,
            code = "2.2-fixEmptyIssuePriority",
            description = "修复之前导入BUG导致的工作项优先级为空的数据")
    @TimedTask(name = "2.2-fixEmptyIssuePriority",
            description = "修复之前导入BUG导致的工作项优先级为空的数据",
            oneExecution = true,
            repeatCount = 0,
            repeatInterval = 1,
            repeatIntervalUnit = QuartzDefinition.SimpleRepeatIntervalUnit.HOURS,
            params = {})
    public void fixEmptyIssuePriority(Map<String, Object> param) {
        LOGGER.info("==============================>>>>>>>> fixEmptyIssuePriority start <<<<<<<<=================================");
        this.fixDataService.fixEmptyIssuePriority();
        LOGGER.info("==============================>>>>>>>> fixEmptyIssuePriority completed <<<<<<<<=================================");
    }
}
