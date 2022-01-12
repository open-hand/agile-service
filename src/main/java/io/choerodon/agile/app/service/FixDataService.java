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
}
