package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.business.RuleLogRelVO;

import java.util.List;

/**
 * @author superlee
 * @since 2020-12-17
 */
public interface AgileTriggerService {

    /**
     * 插入ruleLogRel
     *
     * @param ruleLogRelVO
     * @return
     */
    RuleLogRelVO insertRuleLogRel(RuleLogRelVO ruleLogRelVO);

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
     * @param projectId
     * @param targetProjectId
     * @param issueId
     */
    void updateRuleLogProjectId(Long projectId, Long targetProjectId, Long issueId);
}
