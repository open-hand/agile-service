package io.choerodon.agile.infra.enums;

import java.util.HashMap;
import java.util.Map;

/**
 * @author superlee
 * @since 2021-10-27
 */
public class IssueSortFieldMapping {

    private static final Map<String, String> SORT_FIELD_MAP = new HashMap<>();

    private static final String PRIORITY_ID = "priorityId";
    private static final String ASSIGNEE_ID = "assigneeId";
    private static final String CREATE_USER_KEY = "createUser";
    private static final String CREATE_USER_VALUE = "createdBy";
    private static final String UPDATE_USER_KEY = "updateUser";
    private static final String UPDATE_USER_VALUE = "lastUpdatedBy";
    private static final String STATUS_ID = "statusId";
    private static final String REPORTER_ID = "reporterId";
    private static final String FEATURE_ID = "featureId";
    private static final String EPIC_ID = "epicId";
    private static final String MAIN_RESPONSIBLE_USER = "mainResponsibleUser";
    private static final String MAIN_RESPONSIBLE_ID = "mainResponsibleId";
    private static final String ENVIRONMENT_NAME = "environmentName";

    static {
        SORT_FIELD_MAP.put(FieldCode.PRIORITY, PRIORITY_ID);
        SORT_FIELD_MAP.put(FieldCode.ASSIGNEE, ASSIGNEE_ID);
        SORT_FIELD_MAP.put(CREATE_USER_KEY, CREATE_USER_VALUE);
        SORT_FIELD_MAP.put(UPDATE_USER_KEY, UPDATE_USER_VALUE);
        SORT_FIELD_MAP.put(FieldCode.STATUS, STATUS_ID);
        SORT_FIELD_MAP.put(FieldCode.REPORTER, REPORTER_ID);
        SORT_FIELD_MAP.put(FieldCode.FEATURE, FEATURE_ID);
        SORT_FIELD_MAP.put(FieldCode.EPIC, EPIC_ID);
        SORT_FIELD_MAP.put(MAIN_RESPONSIBLE_USER, MAIN_RESPONSIBLE_ID);
        SORT_FIELD_MAP.put(ENVIRONMENT_NAME, FieldCode.ENVIRONMENT);
    }

    public static String getSortByField(String key) {
        return SORT_FIELD_MAP.get(key);
    }
}
