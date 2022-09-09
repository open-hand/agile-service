package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.StatusDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
public interface StatusService {

    Page<StatusWithInfoVO> queryStatusList(PageRequest pageRequest, Long organizationId, StatusSearchVO statusSearchVO);

    /**
     * 创建状态
     *
     * @param organizationId 组织id
     * @param statusVO       状态对象
     * @return 状态对象
     */
    StatusVO create(Long organizationId, StatusVO statusVO);

    /**
     * 更新状态
     *
     * @param statusVO 更新对象
     * @return 更新对象
     */
    StatusVO update(StatusVO statusVO);

    /**
     * 删除状态
     *
     * @param organizationId 组织id
     * @param statusId       状态机id
     * @return result
     */
    Boolean delete(Long organizationId, Long statusId);

    /**
     * 根据id获取状态对象
     *
     * @param organizationId 组织id
     * @param statusId       状态id
     * @return result
     */
    StatusVO queryStatusById(Long organizationId, Long statusId);

    /**
     * 获取所有
     *
     * @param organizationId 组织id
     * @return result
     */
    List<StatusVO> queryAllStatus(Long organizationId);

    Map<Long, StatusVO> queryAllStatusMap(Long organizationId);

    Map<Long, StatusVO> queryStatusMapByIds(Long organizationId, Set<Long> statusIds);

    /**
     * 校验状态名字是否未被使用
     *
     * @param organizationId 组织id
     * @param name           名称
     * @return result
     */
    StatusCheckVO checkName(Long organizationId, String name);

    Map<Long, StatusDTO> batchStatusGet(List<Long> ids);

    /**
     * 敏捷添加状态
     *
     * @param organizationId organizationId
     * @param statusVO statusVO
     * @return result
     */
    StatusVO createStatusForAgile(Long organizationId, Long stateMachineId, StatusVO statusVO);

    /**
     * 敏捷移除状态
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @param statusId statusId
     */
    void removeStatusForAgile(Long organizationId, Long stateMachineId, Long statusId);

    /**
     * 查询状态机下的所有状态
     *
     * @param organizationId organizationId
     * @param stateMachineIds stateMachineIds
     * @return result
     */
    List<StatusVO> queryByStateMachineIds(Long organizationId, List<Long> stateMachineIds);

    Page<StatusVO> pagedQueryByStateMachineIds(PageRequest pageRequest, Long organizationId, List<Long> stateMachineIds, String name);

    List<StatusAndTransformVO> queryStatusByStateMachineId(Long organizationId, Long projectId, Long stateMachineId);

    Page<ProjectStatusVO> listStatusByProjectId(Long projectId, PageRequest pageRequest, StatusSearchVO statusSearchVO);

    void deleteStatus(Long projectId,Long statusId,String applyType,List<DeleteStatusTransferVO> statusTransferVOS);

    Map<String, Object> checkDeleteStatus(Long projectId, String applyType,Long statusId);

    /**
     * 校验状态名字是否未被使用
     *
     * @param organizationId 组织id
     * @param projectId 项目Id
     * @param name           名称
     * @return result
     */
    StatusCheckVO projectCheckName(Long projectId , Long organizationId, String name);

    List<Long> filterIssueType(Long projectId, List<String> applyTypes);

    /**
     * 项目层查询状态
     *
     * @param projectId projectId
     * @param statusId statusId
     * @return result
     */
    StatusVO queryProjectStatusById(Long projectId, Long statusId);

    Page<StatusVO> queryUserProjectStatus(PageRequest pageRequest, Long organizationId, String type, StatusParamVO statusParamVO);
}
