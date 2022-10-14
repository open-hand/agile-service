package io.choerodon.agile.infra.utils;

import io.choerodon.agile.api.vo.ProjectVO;
import org.springframework.stereotype.Component;

/**
 * @author shinan.chen
 * @date 2018/9/7
 * 通过projectId获取organizationId
 */
@Component
public class ProjectUtil {

    public Long getOrganizationId(Long projectId) {
        return queryProject(projectId).getOrganizationId();
    }

    public String getCode(Long projectId) {
        return queryProject(projectId).getCode();
    }

    public String getName(Long projectId) {
        return queryProject(projectId).getName();
    }

    private ProjectVO queryProject(Long projectId) {
        return ConvertUtil.queryProject(projectId);
    }
}
