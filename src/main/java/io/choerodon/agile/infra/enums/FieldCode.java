package io.choerodon.agile.infra.enums;

import io.choerodon.core.exception.CommonException;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/4/2
 */
public class FieldCode {
    private FieldCode() {
    }

    public static final String ISSUE_TYPE = "issueType";
    public static final String SUMMARY = "summary";
    public static final String DESCRIPTION = "description";
    public static final String REMAINING_TIME = "remainingTime";
    public static final String STORY_POINTS = "storyPoints";
    public static final String STATUS = "status";
    public static final String PRIORITY = "priority";
    public static final String COMPONENT = "component";
    public static final String LABEL = "label";
    public static final String INFLUENCE_VERSION = "influenceVersion";
    public static final String FIX_VERSION = "fixVersion";
    public static final String EPIC = "epic";
    public static final String SPRINT = "sprint";
    public static final String EPIC_NAME = "epicName";
    public static final String REPORTER = "reporter";
    public static final String ASSIGNEE = "assignee";
    public static final String CREATION_DATE = "creationDate";
    public static final String LAST_UPDATE_DATE = "lastUpdateDate";
    public static final String TIME_TRACE = "timeTrace";
    public static final String BENFIT_HYPOTHESIS = "benfitHypothesis";
    public static final String ACCEPTANCE_CRITERA = "acceptanceCritera";
    public static final String FEATURE_TYPE = "featureType";
    public static final String PI = "pi";
    public static final String FEATURE = "feature";
    public static final String CREATOR = "created_user";
    public static final String UPDATOR = "last_updated_user";
    public static final String TAG = "tag";
    public static final String ACTUAL_START_TIME = "actualStartTime";
    public static final String ACTUAL_END_TIME = "actualEndTime";
    public static final String PARTICIPANT = "participant";
    public static final String ESTIMATE_TIME = "estimateTime";

    public static final String SUB_PROJECT = "subProject";
    public static final String ESTIMATED_START_TIME = "estimatedStartTime";
    public static final String ESTIMATED_END_TIME = "estimatedEndTime";
    public static final String PROGRAM_VERSION = "programVersion";
    public static final String MAIN_RESPONSIBLE = "mainResponsible";
    public static final String ENVIRONMENT = "environment";
    public static final String BACKLOG_CLASSIFICATION = "backlogClassification";
    public static final String BACKLOG_TYPE = "backlogType";
    public static final String PROCESSOR = "processor";
    public static final String URGENT = "urgent";
    public static final String PROGRESS_FEEDBACK = "progressFeedback";
    public static final String EMAIL = "email";
    public static final String BELONG_TO_BACKLOG = "belongToBacklog";

    public static final String ISSUE_STATUS = "issueStatus";

    private static final List<String> FIELD_CODE_LIST = new ArrayList<>();

    public static List<String> values() {
        List<String> result = new ArrayList<>();
        if (!FIELD_CODE_LIST.isEmpty()) {
            result.addAll(FIELD_CODE_LIST);
            return result;
        }
        List<String> ignoreFields = Arrays.asList("FIELD_CODE_LIST");
        Field[] fields = FieldCode.class.getDeclaredFields();
        for (Field field : fields) {
            String fieldName = field.getName();
            if (ignoreFields.contains(fieldName)) {
                continue;
            }
            field.setAccessible(true);
            try {
                String value = (String) field.get(null);
                result.add(value);
            } catch (IllegalAccessException e) {
                throw new CommonException("error.reflection.FieldCode.get.value");
            }
        }
        return result;
    }

}
