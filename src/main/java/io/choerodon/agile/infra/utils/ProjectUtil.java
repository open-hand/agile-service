package io.choerodon.agile.infra.utils;

import org.springframework.stereotype.Component;

import io.choerodon.agile.api.vo.ProjectVO;

/**
 * @author shinan.chen
 * @date 2018/9/7
 * 通过projectId获取organizationId
 */
@Component
public class ProjectUtil {

    /**
     * 【注意】：在项目创建初始化/更新初始化不要使用该方法，由于choerodon-base更新项目sagaTask在最后，导致可能查到的类别和saga里的真正的项目类别不一致，导致问题
     * @param projectId
     * @return
     */
    public Long getOrganizationId(Long projectId) {
        return queryProject(projectId).getOrganizationId();
    }

    /**
     * 【注意】：在项目创建初始化/更新初始化不要使用该方法，由于choerodon-base更新项目sagaTask在最后，导致可能查到的类别和saga里的真正的项目类别不一致，导致问题
     * @param projectId
     * @return
     */
    public String getCode(Long projectId) {
        return queryProject(projectId).getCode();
    }

    /**
     * 【注意】：在项目创建初始化/更新初始化不要使用该方法，由于choerodon-base更新项目sagaTask在最后，导致可能查到的类别和saga里的真正的项目类别不一致，导致问题
     * @param projectId
     * @return
     */
    public String getName(Long projectId) {
        return queryProject(projectId).getName();
    }

    private ProjectVO queryProject(Long projectId) {
        return ConvertUtil.queryProject(projectId, false);
    }
}
