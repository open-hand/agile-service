package io.choerodon.agile.app.service;

import java.util.Set;

import io.choerodon.agile.api.vo.event.ProjectEvent;

/**
 * 项目模版复制
 *
 * @author superlee
 * @since 2023/4/26
 */
public interface ProjectTemplateService {

    /**
     * 根据模版复制项目数据
     *
     * @param projectEvent
     * @param categoryCodes
     */
    void cloneByTemplate(ProjectEvent projectEvent, Set<String> categoryCodes);
}
