package io.choerodon.agile.infra.enums;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.validation.constraints.NotNull;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.util.Assert;

import io.choerodon.core.iam.ResourceLevel;

import org.hzero.core.base.BaseConstants;

/**
 * 用户偏好设置信息
 *
 * @author gaokuo.dai@zknow.com 2023-03-30 19:21:29
 * @since 2.4
 */
public enum UserPreferenceInfo {
    PROJECT_ISSUE_LIST_TODO_SHOW_SPRINT_NOT_STARTED(
            "project.issue-list.todo.show-sprint-not-started",
            ResourceLevel.PROJECT,
            "false",
            UserPreferenceInfoTypeHandlers.BOOLEAN_REAL_VALUE_TYPE_HANDLER,
            UserPreferenceInfoTypeHandlers.BOOLEAN_STORE_VALUE_TYPE_HANDLER
    ),
    PROJECT_ISSUE_LIST_ALL_DISPLAY_VIEW_TYPE(
            "project.issue-list.all.display-view-type",
            ResourceLevel.PROJECT,
            "TREE",
            UserPreferenceInfoTypeHandlers.STRING_REAL_VALUE_TYPE_HANDLER,
            UserPreferenceInfoTypeHandlers.STRING_STORE_VALUE_TYPE_HANDLER
    ),
    PROJECT_ISSUE_LIST_ALL_ALWAYS_SHOW_CHILDREN(
            "project.issue-list.all.always-show-children",
            ResourceLevel.PROJECT,
            "false",
            UserPreferenceInfoTypeHandlers.BOOLEAN_REAL_VALUE_TYPE_HANDLER,
            UserPreferenceInfoTypeHandlers.BOOLEAN_STORE_VALUE_TYPE_HANDLER
    );

    /**
     * key to 枚举的Map
     */
    private static final Map<String, UserPreferenceInfo> keyToInfoMap;
    static {
        keyToInfoMap = Arrays.stream(UserPreferenceInfo.values()).collect(Collectors.toMap(UserPreferenceInfo::getPreferenceKey, Function.identity()));
    }

    /**
     * 获取所有配置项的默认值
     * @param resourceLevel 资源层级
     * @return              Map{key, defaultValue}
     */
    public static Map<String, String> allPreferenceDefaultValue(ResourceLevel resourceLevel) {
        Assert.notNull(resourceLevel, BaseConstants.ErrorCode.NOT_NULL);
        return Arrays.stream(UserPreferenceInfo.values())
                .filter(info -> info.getResourceLevel().equals(resourceLevel))
                .collect(Collectors.toMap(UserPreferenceInfo::getPreferenceKey, UserPreferenceInfo::getDefaultValue));
    }

    /**
     * 根据指定key获取默认值
     * @param preferenceKeys    指定keu
     * @return                  Map{key, defaultValue}
     */
    public static Map<String, String> preferenceDefaultValueBySpecifiedKeys(Collection<String> preferenceKeys) {
        if(CollectionUtils.isEmpty(preferenceKeys)) {
            return Collections.emptyMap();
        }
        return preferenceKeys.stream()
                .map(keyToInfoMap::get)
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(UserPreferenceInfo::getPreferenceKey, UserPreferenceInfo::getDefaultValue));
    }

    /**
     * 根据key获取存储值转真实值的TypeHandler
     * @param key   key
     * @return      存储值转真实值的TypeHandler
     */
    public static Function<String, Object> getRealValueTypeHandler(String key) {
        return Optional.ofNullable(keyToInfoMap.get(key))
                .map(UserPreferenceInfo::getRealValueTypeHandler)
                .orElse(UserPreferenceInfoTypeHandlers.STRING_REAL_VALUE_TYPE_HANDLER);
    }

    /**
     * 根据key获取真实值转存储值的TypeHandler
     * @param key   key
     * @return      真实值转存储值的TypeHandler
     */
    public static Function<Object, String> getStoreValueTypeHandler(String key) {
        return Optional.ofNullable(keyToInfoMap.get(key))
                .map(UserPreferenceInfo::getStoreValueTypeHandler)
                .orElse(UserPreferenceInfoTypeHandlers.STRING_STORE_VALUE_TYPE_HANDLER);
    }


    /**
     * 构造函数
     *
     * @param preferenceKey         preferenceKey
     * @param resourceLevel         resourceLevel
     * @param defaultValue          defaultValue
     * @param realValueTypeHandler  realValueTypeHandler
     * @param storeValueTypeHandler storeValueTypeHandler
     */
    UserPreferenceInfo(
            @NotNull String preferenceKey,
            @NotNull ResourceLevel resourceLevel,
            @NotNull String defaultValue,
            @NotNull Function<String, Object> realValueTypeHandler,
            @NotNull Function<Object, String> storeValueTypeHandler
    ) {
        this.preferenceKey = Objects.requireNonNull(preferenceKey);
        this.resourceLevel = Objects.requireNonNull(resourceLevel);
        this.defaultValue = Objects.requireNonNull(defaultValue);
        this.realValueTypeHandler = Objects.requireNonNull(realValueTypeHandler);
        this.storeValueTypeHandler = Objects.requireNonNull(storeValueTypeHandler);
    }

    /**
     * 用户偏好设置信息类型转换器<br/>
     * <span color="red">必须封装到一个静态内部类中, 不然枚举中无法引用</span>
     */
    private static final class UserPreferenceInfoTypeHandlers{
        private static final Function<String, Object> STRING_REAL_VALUE_TYPE_HANDLER = s -> s;
        private static final Function<Object, String> STRING_STORE_VALUE_TYPE_HANDLER = String::valueOf;
        private static final Function<String, Object> BOOLEAN_REAL_VALUE_TYPE_HANDLER = s -> Boolean.TRUE.toString().equals(s);
        private static final Function<Object, String> BOOLEAN_STORE_VALUE_TYPE_HANDLER = String::valueOf;
    }

    /**
     * 偏好设置键，用于表示设置项
     */
    private final String preferenceKey;
    /**
     * 资源层级
     */
    private final ResourceLevel resourceLevel;
    /**
     * 默认值
     */
    private final String defaultValue;
    /**
     * 存储值转真实值的TypeHandler
     */
    private final Function<String, Object> realValueTypeHandler;
    /**
     * 真实值转存储值的TypeHandler
     */
    private final Function<Object, String> storeValueTypeHandler;

    /**
     * @return 偏好设置键，用于表示设置项
     */
    public String getPreferenceKey() {
        return preferenceKey;
    }

    /**
     * @return 资源层级
     */
    public ResourceLevel getResourceLevel() {
        return resourceLevel;
    }

    /**
     * @return 默认值
     */
    public String getDefaultValue() {
        return defaultValue;
    }

    /**
     * @return 存储值转真实值的TypeHandler
     */
    public Function<String, Object> getRealValueTypeHandler() {
        return realValueTypeHandler;
    }

    /**
     * @return 真实值转存储值的TypeHandler
     */
    public Function<Object, String> getStoreValueTypeHandler() {
        return storeValueTypeHandler;
    }
}
