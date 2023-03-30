package io.choerodon.agile.app.service;

import java.util.Map;
/**
 * 用户偏好设置应用服务
 *
 * @author gaokuo.dai@zknow.com 2023-03-30 19:21:29
 * @since 2.4
 */
public interface UserPreferenceService {

    /**
     * 保存用户在某项目/组织下的偏好设置
     * @param organizationId    组织ID
     * @param projectId         项目ID
     * @param userId            用户ID
     * @param preferenceToSave  待保存的配置
     */
    void save(Long organizationId, Long projectId, Long userId, Map<String, Object> preferenceToSave);
}
