package io.choerodon.agile.infra.enums;

/**
 * @author zhaotianxin
 * @date 2020-10-12 9:59
 */
public enum AgileSystemFieldContext {
    ISSUE_TYPE(FieldCode.ISSUE_TYPE,"story,issue_epic,bug,task,sub_task"),
    SUMMARY(FieldCode.SUMMARY,"story,issue_epic,bug,task,sub_task"),
    DESCRIPTION(FieldCode.DESCRIPTION,"story,issue_epic,bug,task,sub_task"),
    REMAINING_TIME(FieldCode.REMAINING_TIME,"story,bug,task,sub_task"),
    STORY_POINTS(FieldCode.STORY_POINTS,"story"),
    STATUS(FieldCode.STATUS,"story,issue_epic,bug,task,sub_task"),
    PRIORITY(FieldCode.PRIORITY,"story,issue_epic,bug,task,sub_task"),
    COMPONENT(FieldCode.COMPONENT,"story,issue_epic,bug,task,sub_task"),
    LABEL(FieldCode.LABEL,"story,issue_epic,bug,task,sub_task"),
    INFLUENCE_VERSION(FieldCode.INFLUENCE_VERSION,"bug"),
    FIX_VERSION(FieldCode.FIX_VERSION,"story,issue_epic,bug,task,sub_task"),
    EPIC(FieldCode.EPIC,"story,bug,task"),
    SPRINT(FieldCode.SPRINT,"story,issue_epic,bug,task,sub_task"),
    EPIC_NAME(FieldCode.EPIC_NAME,"issue_epic"),
    REPORTER(FieldCode.REPORTER,"story,issue_epic,bug,task,sub_task"),
    ASSIGNEE(FieldCode.ASSIGNEE,"story,issue_epic,bug,task,sub_task"),
    CREATION_DATE(FieldCode.CREATION_DATE,"story,issue_epic,bug,task,sub_task"),
    LAST_UPDATE_DATE(FieldCode.LAST_UPDATE_DATE,"story,issue_epic,bug,task,sub_task"),
    TIME_TRACE(FieldCode.TIME_TRACE,"story,bug,task,sub_task"),
    ESTIMATED_START_TIME(FieldCode.ESTIMATED_START_TIME,"story,issue_epic,bug,task,sub_task"),
    ESTIMATED_END_TIME(FieldCode.ESTIMATED_END_TIME,"story,issue_epic,bug,task,sub_task"),
    MAIN_RESPONSIBLE(FieldCode.MAIN_RESPONSIBLE,"story,task,bug,sub_task"),
    ENVIRONMENT(FieldCode.ENVIRONMENT,"bug"),
    CREATOR(FieldCode.CREATOR,"story,issue_epic,bug,task,sub_task"),
    UPDATER(FieldCode.UPDATOR,"story,issue_epic,bug,task,sub_task"),
    TAG(FieldCode.TAG,"story,bug,task,sub_task"),
    ACTUAL_START_TIME(FieldCode.ACTUAL_START_TIME,"story,issue_epic,bug,task,sub_task"),
    ACTUAL_END_TIME(FieldCode.ACTUAL_END_TIME,"story,issue_epic,bug,task,sub_task"),
    ;
    private String fieldCode;
    private String context;

    AgileSystemFieldContext(String fieldCode, String context) {
        this.fieldCode = fieldCode;
        this.context = context;
    }

    public String getFieldCode() {
        return fieldCode;
    }

    public String getContext() {
        return context;
    }

    public static String getContextByFieldCode(String fieldCode) {
        for (AgileSystemFieldContext field : AgileSystemFieldContext.values()) {
            if (field.fieldCode.equals(fieldCode)) {
                return field.context;
            }
        }
        return null;
    }

}
