package io.choerodon.agile.domain.entity;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.validator.constraints.Length;
import org.springframework.util.Assert;

import io.choerodon.agile.infra.enums.UserPreferenceInfo;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.choerodon.mybatis.domain.AuditDomain;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.Pair;

/**
 * 用户偏好设置
 *
 * @author gaokuo.dai@zknow.com 2023-03-30 19:21:29
 * @since 2.4
 */
@ApiModel("用户偏好设置")
@VersionAudit
@ModifyAudit
@JsonInclude(value = JsonInclude.Include.NON_NULL)
@Table(name = "agile_user_preference")
public class UserPreference extends AuditDomain {

    public static final String FIELD_ID = "id";
    public static final String FIELD_ORGANIZATION_ID = "organizationId";
    public static final String FIELD_PROJECT_ID = "projectId";
    public static final String FIELD_USER_ID = "userId";
    public static final String FIELD_PREFERENCE_KEY = "preferenceKey";
    public static final String FIELD_PREFERENCE_VALUE = "preferenceValue";

//
// 业务方法(按public protected private顺序排列)
// ------------------------------------------------------------------------------

    /**
     * 处理默认值, 所有没值的部分都会被赋予默认值
     * @param userPreferences   入参
     * @return                  出参
     */
    public static List<UserPreference> batchProcessDefaultValue(List<UserPreference> userPreferences, ResourceLevel resourceLevel) {
        Assert.notNull(resourceLevel, BaseConstants.ErrorCode.NOT_NULL);
        if(userPreferences == null) {
            userPreferences = Collections.emptyList();
        }
        final Map<String, String> preferenceStoreValueMap = userPreferences.stream().collect(Collectors.toMap(UserPreference::getPreferenceKey, UserPreference::getPreferenceValue));
        Map<String, String> allPreferenceDefaultValueMap = UserPreferenceInfo.allPreferenceDefaultValue(resourceLevel);
        return innerProcessDefaultValue(preferenceStoreValueMap, allPreferenceDefaultValueMap);
    }

    public static List<UserPreference> batchProcessDefaultValueSpecifiedKeys(List<UserPreference> userPreferences, ResourceLevel resourceLevel, Collection<String> preferenceKeys) {
        Assert.notNull(resourceLevel, BaseConstants.ErrorCode.NOT_NULL);
        if(userPreferences == null) {
            userPreferences = Collections.emptyList();
        }
        if(CollectionUtils.isEmpty(preferenceKeys)) {
            return userPreferences;
        }
        final Map<String, String> preferenceStoreValueMap = userPreferences.stream().collect(Collectors.toMap(UserPreference::getPreferenceKey, UserPreference::getPreferenceValue));
        Map<String, String> preferenceDefaultValueBySpecifiedKeysMap = UserPreferenceInfo.preferenceDefaultValueBySpecifiedKeys(preferenceKeys);
        return innerProcessDefaultValue(preferenceStoreValueMap, preferenceDefaultValueBySpecifiedKeysMap);
    }

    /**
     * 将存储值转化为真实值
     * @return Pair{key, 真实值}
     */
    public Pair<String, Object> toRealValue() {
        final String key = this.getPreferenceKey();
        if(StringUtils.isBlank(key)) {
            return new Pair<>();
        }
        if(this.preferenceValue == null) {
            return new Pair<>(key, null);
        }
        final Function<String, Object> typeHandler = UserPreferenceInfo.getRealValueTypeHandler(key);
        return new Pair<>(key, typeHandler.apply(this.preferenceValue));
    }

    /**
     * 根据组织ID和项目ID计算资源层级
     * @param organizationId    组织ID
     * @param projectId         项目ID
     * @return                  资源层级
     */
    public static ResourceLevel calculateResourceLevel(Long organizationId, Long projectId) {
        if(organizationId == null) {
            organizationId = BaseConstants.DEFAULT_TENANT_ID;
        }
        if(projectId == null) {
            projectId = BaseConstants.DEFAULT_TENANT_ID;
        }
        final ResourceLevel resourceLevel;
        if(!BaseConstants.DEFAULT_TENANT_ID.equals(projectId)) {
            resourceLevel = ResourceLevel.PROJECT;
        } else if(!BaseConstants.DEFAULT_TENANT_ID.equals(organizationId)) {
            resourceLevel = ResourceLevel.ORGANIZATION;
        } else {
            resourceLevel = ResourceLevel.SITE;
        }
        return resourceLevel;
    }

    /**
     * 内部计算, 处理默认值
     * @param preferenceStoreValueMap       存储值Map
     * @param preferenceDefaultValueByMap   默认值Map
     * @return                              处理后的结果
     */
    private static List<UserPreference> innerProcessDefaultValue(Map<String, String> preferenceStoreValueMap, Map<String, String> preferenceDefaultValueByMap) {
        if(preferenceStoreValueMap == null) {
            preferenceStoreValueMap = Collections.emptyMap();
        }
        if(preferenceDefaultValueByMap == null) {
            preferenceDefaultValueByMap = Collections.emptyMap();
        }
        for (Map.Entry<String, String> defaultValueEntry : preferenceDefaultValueByMap.entrySet()) {
            preferenceStoreValueMap.computeIfAbsent(defaultValueEntry.getKey(), k -> defaultValueEntry.getValue());
        }
        return preferenceStoreValueMap.entrySet().stream()
                .map(entry -> new UserPreference().setPreferenceKey(entry.getKey()).setPreferenceValue(entry.getValue()))
                .collect(Collectors.toList());
    }

//
// 数据库字段
// ------------------------------------------------------------------------------


    @ApiModelProperty("主键")
    @Id
    @GeneratedValue
    private Long id;
    @ApiModelProperty(value = "组织ID", required = true)
    @NotNull
    private Long organizationId;
    @ApiModelProperty(value = "项目ID", required = true)
    @NotNull
    private Long projectId;
    @ApiModelProperty(value = "用户ID", required = true)
    @NotNull
    private Long userId;
    @ApiModelProperty(value = "偏好设置键，用于表示设置项", required = true)
    @NotBlank
    @Length(max = 120)
    private String preferenceKey;
    @ApiModelProperty(value = "偏好设置值，用于表示设置对应的值")
    @Length(max = 480)
    private String preferenceValue;

//
// 非数据库字段
// ------------------------------------------------------------------------------

//
// getter/setter
// ------------------------------------------------------------------------------

    /**
     * @return 主键
     */
    public Long getId() {
        return id;
    }

    public UserPreference setId(Long id) {
        this.id = id;
        return this;
    }

    /**
     * @return 组织ID
     */
    public Long getOrganizationId() {
        return organizationId;
    }

    public UserPreference setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
        return this;
    }

    /**
     * @return 项目ID
     */
    public Long getProjectId() {
        return projectId;
    }

    public UserPreference setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    /**
     * @return 用户ID
     */
    public Long getUserId() {
        return userId;
    }

    public UserPreference setUserId(Long userId) {
        this.userId = userId;
        return this;
    }

    /**
     * @return 偏好设置键，用于表示设置项
     */
    public String getPreferenceKey() {
        return preferenceKey;
    }

    public UserPreference setPreferenceKey(String preferenceKey) {
        this.preferenceKey = preferenceKey;
        return this;
    }

    /**
     * @return 偏好设置值，用于表示设置对应的值
     */
    public String getPreferenceValue() {
        return preferenceValue;
    }

    public UserPreference setPreferenceValue(String preferenceValue) {
        this.preferenceValue = preferenceValue;
        return this;
    }

}
