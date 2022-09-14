package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.infra.dto.StatusDTO;
import io.choerodon.agile.infra.enums.InitStatus;

/**
 * @author shinan.chen
 * @date 2018/10/15
 */
public interface InitService {
    /**
     * 初始化状态
     *
     * @param organizationId organizationId
     */
    List<StatusDTO> initStatus(Long organizationId,List<InitStatus> initStatusList);

    /**
     * 创建项目时初始化状态机
     *
     * @param organizationId organizationId
     * @param applyType applyType
     * @param projectEvent projectEvent
     * @return result
     */
    Long createStateMachineWithCreateProject(Long organizationId, String applyType, ProjectEvent projectEvent);

    /**
     * 初始化默认状态机
     *
     * @param organizationId organizationId
     * @return result
     */
    Long initDefaultStateMachine(Long organizationId);

    /**
     * 初始化敏捷状态机
     *
     * @param organizationId organizationId
     * @param projectEvent projectEvent
     * @return result
     */
    Long initAGStateMachine(Long organizationId, ProjectEvent projectEvent);

    Long initStateMachine(Long organizationId,
                          ProjectEvent projectEvent,
                          String name,
                          String applyType);

    /**
     * 初始化测试状态机
     *
     * @param organizationId organizationId
     * @param projectEvent projectEvent
     * @return result
     */
    Long initTEStateMachine(Long organizationId, ProjectEvent projectEvent);

    void createStateMachineDetail(Long organizationId, Long stateMachineId, String applyType);

    void initStatusIfNotExisted(Long organizationId);
}

