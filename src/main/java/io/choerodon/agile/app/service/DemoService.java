package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.event.OrganizationRegisterEventPayload;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/01/07.
 * Email: fuqianghuang01@gmail.com
 */
public interface DemoService {

    String handleProjectCategory();

    void demoInit(OrganizationRegisterEventPayload demoProjectPayload, ProjectVO projectVO);

    /**
     * 初始化项目模板数据
     *
     * @param projectId
     */
    void initProjectTemplateData(Long projectId);
}
