package io.choerodon.agile.app.service.impl;

import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.app.service.AgileWaterfallService;
import io.choerodon.agile.app.service.ProjectTemplateService;
import io.choerodon.agile.infra.enums.ProjectCategory;

/**
 * @author superlee
 * @since 2023/4/26
 */
@Service
public class ProjectTemplateServiceImpl implements ProjectTemplateService {

    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void cloneByTemplate(ProjectEvent projectEvent, Set<String> categoryCodes) {
        Long fromTemplateId = projectEvent.getFromTemplateId();
        if (categoryCodes.contains(ProjectCategory.MODULE_WATERFALL)) {
            //瀑布项目根据模版复制
            if (agileWaterfallService != null) {
                agileWaterfallService.cloneByTemplate(projectEvent.getProjectId(), fromTemplateId);
            }
        }
    }
}
