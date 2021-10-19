package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.WorkItemSearchVO;
import io.choerodon.agile.api.vo.WorkItemVO;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-11 13:56
 */
public interface WorkCalendarService {

    /**
     * 查询当前组织下，当前用户有权限项目活跃冲刺中需要经办的问题
     * @param organizationId
     * @param workItemSearchVO
     * @return
     */
    List<WorkItemVO> queryAssigneeParentIssueList(Long organizationId, WorkItemSearchVO workItemSearchVO);

    /**
     * 周视图、月视图查询工作项
     * @param organizationId
     * @param workItemSearchVO
     * @return
     */
    List<WorkItemVO> queryAssigneeIssueList(Long organizationId, WorkItemSearchVO workItemSearchVO);
}
