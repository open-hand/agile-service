package io.choerodon.agile.domain.repository;

import java.util.Collection;
import java.util.Map;

import io.choerodon.agile.domain.entity.UserPreference;

import org.hzero.mybatis.base.BaseRepository;

/**
 * 用户偏好设置资源库
 *
 * @author gaokuo.dai@zknow.com 2023-03-30 19:21:29
 * @since 2.4
 */
public interface UserPreferenceRepository extends BaseRepository<UserPreference> {

    /**
     * 查询用户在某项目/组织下的偏好设置
     * @param organizationId    组织ID
     * @param projectId         项目ID
     * @param userId            用户ID
     * @return                  查询结果
     */
    Map<String, Object> findUserPreferencesDetail(Long organizationId, Long projectId, Long userId);

    /**
     * 查询用户在某项目/组织下的偏好设置--按key部分查询
     * @param organizationId    组织ID
     * @param projectId         项目ID
     * @param userId            用户ID
     * @return                  查询结果
     */
    Map<String, Object> findUserPreferencesPartialDetail(Long organizationId, Long projectId, Long userId, Collection<String> preferenceKeys);
}
