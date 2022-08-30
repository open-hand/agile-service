package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Set;

import io.choerodon.agile.api.vo.business.RuleLogRelVO;

/**
 * @author superlee
 * @since 2020-12-17
 */
public interface AgileTriggerService {

    /**
     * 查询ruleLogRel
     *
     * @param ruleLogRelVO ruleLogRelVO
     * @return result
     */
    List<RuleLogRelVO> queryRuleLogRelList(RuleLogRelVO ruleLogRelVO);

    void delete(RuleLogRelVO ruleLogRel);

    /**
     * 修改issue相关的触发器日志的projectId
     *
     * @param projectId projectId
     * @param targetProjectId targetProjectId
     * @param issueId issueId
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

    void deleteRuleLog(Long projectId, String businessType, Set<Long> instanceIds);
}
