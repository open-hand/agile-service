package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.IssueSubCreateVO;
import io.choerodon.agile.api.vo.IssueSubVO;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.infra.statemachineclient.dto.ExecuteResult;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;

/**
 * @author shinan.chen
 * @date 2018/10/30
 */
public interface StateMachineClientService {

    IssueVO createIssue(IssueCreateVO issueCreateVO, String applyType);

    IssueSubVO createSubIssue(IssueSubCreateVO issueSubCreateVO);

    /**
     * 执行状态转换
     *
     * @param projectId projectId
     * @param issueId issueId
     * @param transformId transformId
     * @param inputDTO inputDTO
     * @return result
     */
    ExecuteResult executeTransform(Long projectId, Long issueId, Long transformId, Long objectVersionNumber, String applyType, InputDTO inputDTO);

    ExecuteResult executeTransformForDemo(Long projectId, Long issueId, Long transformId, Long objectVersionNumber, String applyType, InputDTO inputDTO);

    void cleanInstanceCache(Long projectId, Long issueId, String applyType);

    void initRank(IssueCreateVO issueCreateVO, Long issueId, String type);

    IssueVO createIssueWithoutRuleNotice(IssueCreateVO issueCreateVO, String applyType);

    IssueSubVO createSubIssueWithoutRuleNotice(IssueSubCreateVO issueSubCreateVO);

    /**
     * 项目模板简单创建issue，不走状态联动和触发器，消息通知等逻辑
     * 用于应对修数据时高并发创建项目的场景
     *
     * @param issueCreateVO
     * @param applyType
     * @return
     */
    IssueVO projectTemplateCreateIssue(IssueCreateVO issueCreateVO, String applyType);
}
