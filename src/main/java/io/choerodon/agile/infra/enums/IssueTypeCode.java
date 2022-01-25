package io.choerodon.agile.infra.enums;

/**
 * @author superlee
 * @since 2020-04-07
 */
public enum IssueTypeCode {
    BUG("bug"),
    FEATURE("feature"),
    ISSUE_AUTO_TEST("issue_auto_test"),
    ISSUE_EPIC("issue_epic"),
    ISSUE_TEST("issue_test"),
    STORY("story"),
    SUB_TASK("sub_task"),
    TASK("task"),
    BACKLOG("backlog"),
    ;

    public static final String[] ISSUE_TYPE_CODE_WITH_FEATURE = {STORY.value(), TASK.value(), BUG.value()};

    private String value;

    IssueTypeCode(String value) {
        this.value = value;
    }

    public String value() {
        return this.value;
    }

    public static boolean isBug(String value) {
        return BUG.value.equals(value);
    }

    public static boolean isFeature(String value) {
        return FEATURE.value.equals(value);
    }

    public static boolean isEpic(String value) {
        return ISSUE_EPIC.value.equals(value);
    }

    public static boolean isStory(String value) {
        return STORY.value.equals(value);
    }

    public static boolean isTask(String value) {
        return TASK.value.equals(value);
    }

    public static boolean isSubTask(String value) {
        return SUB_TASK.value.equals(value);
    }

    public static boolean contains(String typeCode) {
        for (IssueTypeCode issueTypeCode : IssueTypeCode.values()) {
            if(issueTypeCode.value().equals(typeCode)) {
                return true;
            }
        }
        return false;
    }

}
