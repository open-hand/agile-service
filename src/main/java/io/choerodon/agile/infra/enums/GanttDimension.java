package io.choerodon.agile.infra.enums;

/**
 * 甘特图查询维度
 *
 * @author superlee
 * @since 2021-09-26
 */
public enum GanttDimension {
    TASK,
    ASSIGNEE,
    SPRINT,
    EPIC,
    FEATURE;

    public static boolean contains(String value) {
        for (GanttDimension dimension : GanttDimension.values()) {
            if (dimension.name().equalsIgnoreCase(value)) {
                return true;
            }
        }
        return false;
    }

    public static boolean isFeature(String dimension) {
        return FEATURE.name().equalsIgnoreCase(dimension);
    }

    public static boolean isSprint(String dimension) {
        return SPRINT.name().equalsIgnoreCase(dimension);
    }

    public static boolean isEpic(String dimension) {
        return EPIC.name().equalsIgnoreCase(dimension);
    }

    public static boolean isTask(String dimension) {
        return TASK.name().equalsIgnoreCase(dimension);
    }

    public static boolean isAssignee(String dimension) {
        return ASSIGNEE.name().equalsIgnoreCase(dimension);
    }
}
