package io.choerodon.agile.app.service;

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
     * 2.2-修复之前导入BUG导致的工作项优先级为空的数据
     */
    void fixEmptyIssuePriority();
}
