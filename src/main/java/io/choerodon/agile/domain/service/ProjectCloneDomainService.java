package io.choerodon.agile.domain.service;

import io.choerodon.agile.domain.context.ProjectCloneContext;

/**
 * 项目复制 领域Service
 * @author gaokuo.dai@zknow.com 2023-05-06
 * @since 2.5
 */
public interface ProjectCloneDomainService {

    /**
     * 项目复制
     *
     * @param sourceProjectId   源项目ID
     * @param targetProjectId   目标项目ID
     * @param context           复制上下文
     */
    void cloneProject(Long sourceProjectId, Long targetProjectId, ProjectCloneContext context);

}
