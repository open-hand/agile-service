package io.choerodon.agile.app.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.validation.Validator;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.collections4.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.app.service.UserPreferenceService;
import io.choerodon.agile.domain.entity.UserPreference;
import io.choerodon.agile.domain.repository.UserPreferenceRepository;
import io.choerodon.agile.infra.enums.UserPreferenceInfo;

import org.hzero.core.base.BaseAppService;
import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.ValidUtils;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;

/**
 * 用户偏好设置应用服务默认实现
 *
 * @author gaokuo.dai@zknow.com 2023-03-30 19:21:29
 * @since 2.4
 */
@Service
public class UserPreferenceServiceImpl extends BaseAppService implements UserPreferenceService {

    @Autowired
    private UserPreferenceRepository userPreferenceRepository;
    @Autowired
    private Validator validator;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(Long organizationId, Long projectId, Long userId, Map<String, Object> preferenceToSave) {
        if(userId == null) {
            return;
        }
        if(MapUtils.isEmpty(preferenceToSave)) {
            return;
        }
        if(organizationId == null) {
            organizationId = BaseConstants.DEFAULT_TENANT_ID;
        }
        if(projectId == null) {
            projectId = BaseConstants.DEFAULT_TENANT_ID;
        }
        final Long finalOrganizationId = organizationId;
        final Long finalProjectId = projectId;
        List<UserPreference> userPreferencesToSaveList = preferenceToSave.entrySet().stream()
                .map(entry -> {
                    final String key = entry.getKey();
                    final Object realValue = entry.getValue();
                    final Function<Object, String> typeHandler = UserPreferenceInfo.getStoreValueTypeHandler(key);
                    return new UserPreference()
                            .setOrganizationId(finalOrganizationId)
                            .setProjectId(finalProjectId)
                            .setUserId(userId)
                            .setPreferenceKey(key)
                            .setPreferenceValue(typeHandler.apply(realValue));
                })
                .collect(Collectors.toList());

        Map<String, UserPreference> userPreferencesInDBMap = this.userPreferenceRepository.selectByCondition(Condition.builder(UserPreference.class).andWhere(Sqls.custom()
                .andEqualTo(UserPreference.FIELD_ORGANIZATION_ID, organizationId)
                .andEqualTo(UserPreference.FIELD_PROJECT_ID, projectId)
                .andEqualTo(UserPreference.FIELD_USER_ID, userId)
                .andIn(UserPreference.FIELD_PREFERENCE_KEY, preferenceToSave.keySet())
        ).build()).stream().collect(Collectors.toMap(UserPreference::getPreferenceKey, Function.identity()));

        List<UserPreference> createList = new ArrayList<>(userPreferencesToSaveList.size());
        List<UserPreference> updateList = new ArrayList<>(userPreferencesToSaveList.size());

        for (UserPreference userPreferencesToSave : userPreferencesToSaveList) {
            final String key = userPreferencesToSave.getPreferenceKey();
            if(userPreferencesInDBMap.containsKey(key)) {
                final UserPreference userPreferenceInDB = userPreferencesInDBMap.get(key);
                if(!Objects.equals(userPreferencesToSave.getPreferenceValue(), userPreferenceInDB.getPreferenceValue())) {
                    userPreferenceInDB.setPreferenceValue(userPreferencesToSave.getPreferenceValue());
                    updateList.add(userPreferenceInDB);
                }
            } else {
                createList.add(userPreferencesToSave);
            }
        }

        ValidUtils.valid(this.validator, ListUtils.union(createList, updateList));

        if(CollectionUtils.isNotEmpty(createList)) {
            this.userPreferenceRepository.batchInsert(createList);
        }
        if(CollectionUtils.isNotEmpty(updateList)) {
            this.userPreferenceRepository.batchUpdateOptional(updateList, UserPreference.FIELD_PREFERENCE_VALUE);
        }
    }
}
