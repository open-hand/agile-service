package io.choerodon.agile.infra.repository.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Repository;

import io.choerodon.agile.domain.entity.UserPreference;
import io.choerodon.agile.domain.repository.UserPreferenceRepository;
import io.choerodon.core.iam.ResourceLevel;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.Pair;
import org.hzero.mybatis.base.impl.BaseRepositoryImpl;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;

/**
 * 用户偏好设置 资源库实现
 *
 * @author gaokuo.dai@zknow.com 2023-03-30 19:21:29
 * @since 2.4
 */
@Repository
public class UserPreferenceRepositoryImpl extends BaseRepositoryImpl<UserPreference> implements UserPreferenceRepository {

    @Override
    public Map<String, Object> findUserPreferencesDetail(Long organizationId, Long projectId, Long userId) {
        if(userId == null) {
            return Collections.emptyMap();
        }
        if(organizationId == null) {
            organizationId = BaseConstants.DEFAULT_TENANT_ID;
        }
        if(projectId == null) {
            projectId = BaseConstants.DEFAULT_TENANT_ID;
        }
        final ResourceLevel resourceLevel = UserPreference.calculateResourceLevel(organizationId, projectId);

        List<UserPreference> userPreferences = this.selectByCondition(Condition.builder(UserPreference.class).andWhere(Sqls.custom()
                .andEqualTo(UserPreference.FIELD_ORGANIZATION_ID, organizationId)
                .andEqualTo(UserPreference.FIELD_PROJECT_ID, projectId)
                .andEqualTo(UserPreference.FIELD_USER_ID, userId)
        ).build());

        userPreferences = UserPreference.batchProcessDefaultValue(userPreferences, resourceLevel);

        return userPreferences.stream()
                .map(UserPreference::toRealValue)
                .collect(Collectors.toMap(Pair::getFirst, Pair::getSecond));
    }

    @Override
    public Map<String, Object> findUserPreferencesPartialDetail(Long organizationId, Long projectId, Long userId, Collection<String> preferenceKeys) {
        if(userId == null) {
            return Collections.emptyMap();
        }
        if(CollectionUtils.isEmpty(preferenceKeys)) {
            return Collections.emptyMap();
        }
        if(organizationId == null) {
            organizationId = BaseConstants.DEFAULT_TENANT_ID;
        }
        if(projectId == null) {
            projectId = BaseConstants.DEFAULT_TENANT_ID;
        }
        final ResourceLevel resourceLevel = UserPreference.calculateResourceLevel(organizationId, projectId);

        List<UserPreference> userPreferences = this.selectByCondition(Condition.builder(UserPreference.class).andWhere(Sqls.custom()
                .andEqualTo(UserPreference.FIELD_ORGANIZATION_ID, organizationId)
                .andEqualTo(UserPreference.FIELD_PROJECT_ID, projectId)
                .andEqualTo(UserPreference.FIELD_USER_ID, userId)
                .andIn(UserPreference.FIELD_PREFERENCE_KEY, preferenceKeys)
        ).build());

        userPreferences = UserPreference.batchProcessDefaultValueSpecifiedKeys(userPreferences, resourceLevel, preferenceKeys);

        return userPreferences.stream()
                .map(UserPreference::toRealValue)
                .collect(Collectors.toMap(Pair::getFirst, Pair::getSecond));
    }

}
