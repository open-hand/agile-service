package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.business.RuleLogRelVO;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2020-12-17
 */
public interface AgileTriggerService {

    /**
     * 查询ruleLogRel
     *
     * @param ruleLogRelVO
     * @return
     */
    List<RuleLogRelVO> queryRuleLogRelList(RuleLogRelVO ruleLogRelVO);

    void delete(RuleLogRelVO ruleLogRel);

    /**
     * 修改issue相关的触发器日志的projectId
     *
     * @param projectId
     * @param targetProjectId
     * @param issueId
     */
    void updateRuleLogProjectId(Long projectId, Long targetProjectId, Long issueId);

    /**
     * 修数据，将issueTypeCode改为issueTypeId
     */
    void fixRuleIssueTypeRel();

    void insertTriggerLog(Long logId,
                          Long instanceId,
                          Long projectId,
                          String businessType);

    /**
     * 查询ruleLogRel
     * @param projectId 项目id
     * @param logIds 日志id
     * @return ruleLogRel
     */
    List<RuleLogRelVO> queryRuleLogRelByLogId(Long projectId, List<Long> logIds);

    void deleteRuleLog(Long projectId, String businessType, Set<Long> instanceIds);
}
