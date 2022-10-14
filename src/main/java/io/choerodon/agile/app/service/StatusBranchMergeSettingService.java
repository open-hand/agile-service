package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.StatusBranchMergeSettingVO;

/**
 * @author superlee
 * @since 2021-04-19
 */
public interface StatusBranchMergeSettingService {

    /**
     * 查询分支合并配置
     *
     * @param projectId projectId
     * @param issueTypeId issueTypeId
     * @param statusId statusId
     * @return result
     */
    StatusBranchMergeSettingVO query(Long projectId, Long organizationId, Long issueTypeId, Long statusId);

    /**
     * 更新autoTransform
     *
     * @param projectId projectId
     * @param issueTypeId issueTypeId
     * @param statusId statusId
     * @param autoTransform autoTransform
     */
    void updateAutoTransform(Long projectId, Long organizationId, Long issueTypeId, Long statusId, Boolean autoTransform);

    /**
     * 处理分支合并状态流转事件
     *
     * @param projectId projectId
     * @param issueId issueId
     */
    void handleBranchMergeEvent(Long projectId, Long issueId);

    List<StatusBranchMergeSettingVO> listByOptions(Long projectId, Long organizationId, Long issueTypeId, List<Long> statusIds);
}
