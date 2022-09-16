package io.choerodon.agile.app.service;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import io.choerodon.agile.api.vo.IssueTypeWithStateMachineIdVO;
import io.choerodon.agile.api.vo.PriorityVO;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.api.vo.business.IssueVO;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/8/30
 */
public abstract class AbstractDemoService implements DemoService {

    @Autowired
    private StateMachineClientService stateMachineClientService;

    protected IssueVO createEpic(Long projectId,
                                 String epicName,
                                 String summary,
                                 PriorityVO defaultPriority,
                                 Map<String, IssueTypeWithStateMachineIdVO> agileIssueTypeMap,
                                 Long reporterId,
                                 String applyType) {
        IssueCreateVO issueCreateVO = new IssueCreateVO();
        issueCreateVO.setProjectId(projectId);
        issueCreateVO.setEpicName(epicName);
        issueCreateVO.setSummary(summary);
        issueCreateVO.setIssueTypeId(agileIssueTypeMap.get("issue_epic").getId());
        issueCreateVO.setTypeCode(agileIssueTypeMap.get("issue_epic").getTypeCode());
        issueCreateVO.setPriorityId(defaultPriority.getId());
        issueCreateVO.setPriorityCode("priority-" + defaultPriority.getId());
        issueCreateVO.setReporterId(reporterId);
        return stateMachineClientService.createIssue(issueCreateVO, applyType);
    }

}
