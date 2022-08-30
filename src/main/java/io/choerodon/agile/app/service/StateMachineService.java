package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;

import io.choerodon.agile.api.vo.StateMachineWithStatusVO;
import io.choerodon.agile.api.vo.StatusMachineListVO;
import io.choerodon.agile.api.vo.StatusMachineVO;
import io.choerodon.agile.api.vo.event.ChangeStatus;
import io.choerodon.agile.api.vo.event.DeployStateMachinePayload;
import io.choerodon.agile.api.vo.event.StateMachineSchemeDeployCheckIssue;
import io.choerodon.agile.infra.dto.StatusMachineDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;


/**
 * @author peng.jiang@hand-china.com
 */
public interface StateMachineService {

    /**
     * 分页查询状态机
     *
     * @param organizationId 组织id
     * @param name           名称
     * @param description    描述
     * @param param          模糊查询参数
     * @return 状态机列表
     */
    Page<StatusMachineListVO> pageQuery(Long organizationId, PageRequest pageRequest, String name, String description, String param);

    /**
     * 创建状态机及配置
     *
     * @param organizationId 组织id
     * @param statusMachineVO 状态机及配置对象
     * @return 状态机
     */
    StatusMachineVO create(Long organizationId, StatusMachineVO statusMachineVO);

    /**
     * 更新状态机
     *
     * @param organizationId 组织id
     * @param stateMachineId 状态机id
     * @param statusMachineVO 状态机对象
     * @return 更新状态机
     */
    StatusMachineVO update(Long organizationId, Long stateMachineId, StatusMachineVO statusMachineVO);

    /**
     * 发布状态机
     *
     * @param organizationId 组织id
     * @param stateMachineId 状态机id
     * @return 发布状态机对象
     */
    Boolean deploy(Long organizationId, Long stateMachineId, Boolean isChangeStatus);

    /**
     * 删除状态机
     *
     * @param organizationId 组织id
     * @param stateMachineId 状态机id
     * @return result
     */
    void delete(Long organizationId, Long stateMachineId);

    /**
     * 删除校验
     *
     * @param organizationId 组织id
     * @param stateMachineId 状态机id
     * @return result
     */
    Map<String, Object> checkDelete(Long organizationId, Long stateMachineId);

    /**
     * 删除节点校验
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @return result
     */
    Map<String, Object> checkDeleteNode(Long organizationId, Long stateMachineId, Long statusId);

    /**
     * 使状态机变成非活跃状态
     *
     * @param organizationId organizationId
     * @param stateMachineIds stateMachineIds
     */
    void notActiveStateMachine(Long organizationId, List<Long> stateMachineIds);

    /**
     * 发布状态机时对增加与减少的状态进行处理，影响到的项目是否需要增加与减少相应的状态
     *
     * @param organizationId organizationId
     * @param ignoreStateMachineId 忽略当前修改的状态机
     * @param ignoreSchemeId       忽略当前修改的状态机方案
     * @param schemeIds schemeIds
     * @param changeStatus changeStatus
     * @return result
     */
    DeployStateMachinePayload handleStateMachineChangeStatusBySchemeIds(Long organizationId, Long ignoreStateMachineId, Long ignoreSchemeId, List<Long> schemeIds, ChangeStatus changeStatus);

    /**
     * 获取状态机及配置（草稿、活跃）
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     * @param isDraft        是否为草稿
     * @return result
     */
    StatusMachineVO queryStateMachineWithConfigById(Long organizationId, Long stateMachineId, Boolean isDraft);

    /**
     * 获取状态机及配置，用于内部状态机实例构建
     *
     * @param stateMachineId 状态机id
     * @return result
     */
    StatusMachineDTO queryDeployForInstance(Long organizationId, Long stateMachineId);

    /**
     * 删除草稿
     *
     * @param stateMachineId 状态机Id
     * @return 状态机对象
     */
    StatusMachineVO deleteDraft(Long organizationId, Long stateMachineId);

    /**
     * 获取状态机
     *
     * @param stateMachineId 状态机id
     * @return result
     */
    StatusMachineVO queryStateMachineById(Long organizationId, Long stateMachineId);

    /**
     * 获取组织默认状态机
     *
     * @param organizationId organizationId
     * @return result
     */
    StatusMachineVO queryDefaultStateMachine(Long organizationId);

    /**
     * 校验问题状态机名字是否未被使用
     *
     * @param organizationId 组织id
     * @param name           名称
     * @return result
     */
    Boolean checkName(Long organizationId, String name);

    /**
     * 获取所有状态机
     *
     * @param organizationId 组织id
     * @return 状态机列表
     */
    List<StatusMachineVO> queryAll(Long organizationId);

    /**
     * 修改状态机状态
     * 活跃 -> 草稿
     *
     * @param organizationId organizationId
     * @param stateMachineId stateMachineId
     */
    void updateStateMachineStatus(Long organizationId, Long stateMachineId);

    /**
     * 批量活跃状态机
     *
     * @param organizationId organizationId
     * @param stateMachineIds stateMachineIds
     * @return result
     */
    Boolean activeStateMachines(Long organizationId, List<Long> stateMachineIds);

    /**
     * 批量使活跃状态机变成未活跃
     *
     * @param organizationId organizationId
     * @param stateMachineIds stateMachineIds
     * @return result
     */
    Boolean notActiveStateMachines(Long organizationId, List<Long> stateMachineIds);

    /**
     * 获取组织下所有状态机，附带状态
     *
     * @param organizationId 组织id
     * @return 状态机列表
     */
    List<StateMachineWithStatusVO> queryAllWithStatus(Long organizationId);

    /**
     * 获取组织下所有状态机
     *
     * @param organizationId organizationId
     * @return result
     */
    List<StatusMachineVO> queryByOrgId(Long organizationId);

    /**
     * issue服务修改状态机方案时，校验变更的问题类型影响的issue数量
     *
     * @param organizationId organizationId
     * @param deployCheckIssue deployCheckIssue
     * @return result
     */
    Map<Long, Long> checkStateMachineSchemeChange(Long organizationId, StateMachineSchemeDeployCheckIssue deployCheckIssue);

    DeployStateMachinePayload handleStateMachineChangeStatusByStateMachineId(Long organizationId, Long stateMachineId, ChangeStatus changeStatus);

    /**
     * 复制状态机
     * @param organizationId organizationId
     * @param currentStateMachineId currentStateMachineId
     * @param issueTypeId issueTypeId
     * @return result
     */
    Long copyStateMachine(Long organizationId, Long currentStateMachineId,Long issueTypeId);
}
