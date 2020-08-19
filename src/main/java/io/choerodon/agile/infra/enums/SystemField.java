package io.choerodon.agile.infra.enums;

/**
 * 页面配置中，无法被编辑的系统字段
 *
 * @author superlee
 * @since 2020-08-19
 */
public enum SystemField {

    ISSUE_TYPE("issueType"),
    STATUS("status"),
    SUMMARY("summary"),
    PRIORITY("priority"),
    EPIC_NAME("epicName"),
    FEATURE_TYPE("featureType"),
    REPORTER("reporter"),
    ASSIGNEE("assignee"),
    DESCRIPTION("description"),
    CREATION_DATE("creationDate"),
    LAST_UPDATE_DATE("lastUpdateDate"),
    SPRINT("sprint"),
    COMPONENT("component"),
    INFLUENCE_VERSION("influenceVersion"),
    FIX_VERSION("fixVersion"),
    EPIC("epic"),
    PI("pi"),
    STORY_POINTS("storyPoints"),
    REMAINING_TIME("remainingTime"),
    TIME_TRACE("timeTrace");

    private String code;

    SystemField(String code) {
        this.code = code;
    }

    public String code() {
        return code;
    }
}
