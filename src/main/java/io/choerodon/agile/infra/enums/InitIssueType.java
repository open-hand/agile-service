package io.choerodon.agile.infra.enums;

import java.util.ArrayList;
import java.util.List;


public enum InitIssueType {
    /**
     * 史诗
     */
    EPIC("agile_epic", "史诗", "可以是公司的关键战略举措，可以是重大的业务方向，也可以是重大的技术演讲。通常和公司的经营、竞争力、市场环境紧密相关", "#743be7", "issue_epic"),
    /**
     * 故事
     */
    STORY("agile_story", "故事", "是从用户角度出发，对所需要的一个小功能的简短描述", "#00bfa5", "story"),
    /**
     * 缺陷
     */
    BUG("agile_fault", "缺陷", "指影响功能正常运行的问题、错误或者隐藏缺陷", "#f44336", "bug"),
    /**
     * 任务
     */
    TASK("agile_task", "任务", "是敏捷开发下，完成用户需求的过程性的工作，表示用户故事开发任务的完成", "#4d90fe", "task"),
    /**
     * 子任务
     */
    SUB_TASK("agile_subtask", "子任务", "通常是故事或任务的具体拆分，由单人承接，且通常能在短时间内完成", "#4d90fe", "sub_task"),
    /**
     * 测试
     */
    TEST("test-case", "测试", "测试", "#4D90FE", "issue_test"),
    /**
     * 自动化测试
     */
    AUTO_TEST("test-automation", "自动化测试", "自动化测试", "#FA8C16", "issue_auto_test"),
    /**
     * 特性
     */
    FEATURE("agile-feature", "特性", "用于描述满足用户需求的大型系统行为", "#3D5AFE", "feature"),

    /**
     * 阶段
     */
    STAGE("agile_view_timeline", "阶段", "一组具有逻辑关系的项目活动的集合", "#FBBC57", "stage"),

    /**
     * 里程碑
     */
    MILESTONE("agile_milestone", "里程碑", "项目中的重要时点或事件，里程碑的持续时间为零，因为它们代表的是一个重要时间点或事件", "#6ED9C3", "milestone"),

    /**
     * 活动
     */
    ACTIVITY("agile_activity", "活动", "在进度计划中所列，并在项目过程中实施的工作组成部分", "#4D90FE", "activity")
    ,

    /**
     * 风险
     */
    RISK("agile_epic", "风险", "项目过程中出现某种影响项目正常开展或对项目造成直接损失的可能性", "#F76776", "risk");

    private String icon;
    private String name;
    private String description;
    private String colour;
    private String typeCode;

    InitIssueType(String icon, String name, String description, String colour, String typeCode) {
        this.icon = icon;
        this.name = name;
        this.description = description;
        this.colour = colour;
        this.typeCode = typeCode;
    }

    public String getIcon() {
        return icon;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }

    public String getColour() {
        return colour;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public static List<InitIssueType> listByApplyType(String applyType) {
        List<InitIssueType> result = new ArrayList<>();
        switch (applyType) {
            case SchemeApplyType.AGILE:
                result.add(InitIssueType.EPIC);
                result.add(InitIssueType.STORY);
                result.add(InitIssueType.BUG);
                result.add(InitIssueType.TASK);
                result.add(InitIssueType.SUB_TASK);
                break;
            case SchemeApplyType.TEST:
                result.add(InitIssueType.TEST);
                result.add(InitIssueType.AUTO_TEST);
                break;
            case SchemeApplyType.PROGRAM:
                result.add(InitIssueType.EPIC);
                result.add(InitIssueType.FEATURE);
                break;
            case SchemeApplyType.WATERFALL:
                result.add(InitIssueType.STAGE);
                result.add(InitIssueType.MILESTONE);
                result.add(InitIssueType.ACTIVITY);
            case SchemeApplyType.RISK:
                result.add(InitIssueType.RISK);
            default:
                break;
        }
        return result;
    }
}
