package io.choerodon.agile.app.service;

import java.util.Set;

public interface FixDataService {

    void fixCreateProject();

    void fixCreateProjectSingle(Long projectId);

    void fixDateStateMachineAndPage();

    void fixPage();

    /**
     * 修复数据库中issue_type相关数据
     * 将type_code关联改为issue_type_id关联
     */
    void fixIssueTypeData();

    void fixAgileAndProgram();

    /**
     * 修复状态机自定义流转角色数据
     */
    void fixStatusMachineCustomTransferRoleData();


    /**
     * 高级筛选修数据
     *
     * @param typeCodes
     */
    void fixPersonalFilter(Set<String> typeCodes);
}
