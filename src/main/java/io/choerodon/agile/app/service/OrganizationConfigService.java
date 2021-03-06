package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.infra.dto.OrganizationConfigDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;
import java.util.Map;

/**
 * @author zhaotianxin
 * @date 2021-03-22 14:48
 */
public interface OrganizationConfigService {
    /**
     * 初始化组织层问题类型的状态机模板
     * @param organizationId
     * @param issueTypeId
     */
    OrganizationConfigDTO initStatusMachineTemplate(Long organizationId, Long issueTypeId);

    /**
     * 查询状态机模板的转换
     * @param organizationId
     * @param issueTypeId
     * @return
     */
    List<StatusAndTransformVO> listTransform(Long organizationId, Long issueTypeId);

    /**
     * 修改状态机模板的转换
     * @param organizationId
     * @param issueTypeId
     * @param list
     * @return
     */
    List<StateMachineTransformUpdateVO> updateTransform(Long organizationId, Long issueTypeId, List<StateMachineTransformUpdateVO> list);

    /**
     * 删除状态机节点
     * @param organizationId
     * @param issueTypeId
     * @param nodeId
     */
    void deleteNode(Long organizationId, Long issueTypeId, Long nodeId);

    /**
     * 状态机关联状态
     * @param organizationId
     * @param issueTypeId
     * @param statusId
     * @param defaultStatus
     * @param transferAll
     * @return
     */
    StatusMachineNodeVO linkStatus(Long organizationId, Long issueTypeId, Long statusId, Boolean defaultStatus, Boolean transferAll);

    /**
     * 设置默认状态
     * @param organizationId
     * @param stateMachineId
     * @param statusId
     */
    void defaultStatus(Long organizationId, Long stateMachineId, Long statusId);

    /**
     * 创建状态并关联状态机模板
     * @param organizationId
     * @param issueTypeIds
     * @param statusVO
     * @return
     */
    StatusVO createStatus(Long organizationId, List<Long> issueTypeIds, StatusVO statusVO);

    void updateNodeObjectVersionNumber(Long organizationId, Long issueType, Long statusId, Long objectVersionNumber);

    Page<StatusSettingVO> statusTransformSettingList(Long organizationId, Long issueTypeId, PageRequest pageRequest, String param, String schemeCode);

    void syncStatusMachineTemplate(ProjectEvent projectEvent, String applyType);

    /**
     * 校验组织层是否配置模板
     * @param organizationId
     * @return
     */
    Map<String,Boolean> checkConfigTemplate(Long organizationId);

    /**
     * 校验组织层问题类型是否配置状态机模板
     * @param organizationId
     * @param issueTypeId
     * @return
     */
    Boolean checkStatusMachineTemplate(Long organizationId, Long issueTypeId);

    /**
     * 查询组织状态机模板的状态机id
     * @param organizationId
     * @param issueTypeId
     * @return
     */
    Long queryIssueTypeStatusMachineId(Long organizationId, Long issueTypeId);

    /**
     * 查询组织层配置状态机模板的问题类型
     * @param organizationId
     * @return
     */
    List<IssueTypeVO> listIssueType(Long organizationId);

    Boolean checkConfigured(Long organizationId, Long projectId);

    StatusBranchMergeSettingVO queryStatusBranchMergeSetting(Long organizationId, Long issueTypeId, Long statusId);

    void updateAutoTransform(Long organizationId, Long issueTypeId, Long statusId, Boolean autoTransform);

    NodeSortVO updateSort(Long organizationId, Long statusMachineId, NodeSortVO nodeSortVO);
}
