package io.choerodon.agile.app.service;

public interface FixDataService {

    void fixCreateProject();

    void fixCreateProjectSingle(Long projectId);

    void fixDateStateMachine();

    void fixPage();
}
