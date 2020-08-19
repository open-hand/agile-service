package io.choerodon.agile.infra.enums;

import io.choerodon.agile.api.vo.PageConfigFieldEditedVO;

import java.util.HashMap;
import java.util.Map;

/**
 * @author superlee
 * @since 2020-08-19
 */
public class SystemFieldCanNotEdit {

    private SystemFieldCanNotEdit() {}

    public static Map<String, PageConfigFieldEditedVO> fieldEdited(String issueType) {
        return  buildByIssueType(issueType);
    }

    private static Map<String, PageConfigFieldEditedVO> buildByIssueType(String issueType) {
        if (ObjectSchemeFieldContext.EPIC.equals(issueType)) {
            return epicMap;
        } else if (ObjectSchemeFieldContext.STORY.equals(issueType)) {
            return storyMap;
        } else if (ObjectSchemeFieldContext.TASK.equals(issueType)
                || ObjectSchemeFieldContext.SUB_TASK.equals(issueType)) {
            return taskMap;
        } else if (ObjectSchemeFieldContext.BUG.equals(issueType)) {
            return bugMap;
        } else {
            return null;
        }
    }

    private static Map<String, PageConfigFieldEditedVO> epicMap;
    private static Map<String, PageConfigFieldEditedVO> storyMap;
    private static Map<String, PageConfigFieldEditedVO> taskMap;
    private static Map<String, PageConfigFieldEditedVO> bugMap;

    static {
        epicMap = new HashMap<>();
        epicMap.put(SystemField.ISSUE_TYPE.code(), new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(SystemField.STATUS.code(), new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(SystemField.SUMMARY.code(), new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(SystemField.PRIORITY.code(), new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(SystemField.EPIC_NAME.code(), new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(SystemField.ASSIGNEE.code(), new PageConfigFieldEditedVO(false, true, true));
        epicMap.put(SystemField.DESCRIPTION.code(), new PageConfigFieldEditedVO(false, true, true));
        epicMap.put(SystemField.CREATION_DATE.code(), new PageConfigFieldEditedVO(false, true, true));
        epicMap.put(SystemField.LAST_UPDATE_DATE.code(), new PageConfigFieldEditedVO(false, true, true));
        epicMap.put(SystemField.SPRINT.code(), new PageConfigFieldEditedVO(false, true, true));
        epicMap.put(SystemField.COMPONENT.code(), new PageConfigFieldEditedVO(false, true, true));
        epicMap.put(SystemField.FIX_VERSION.code(), new PageConfigFieldEditedVO(false, true, true));
        epicMap.put(SystemField.INFLUENCE_VERSION.code(), new PageConfigFieldEditedVO(false, true, true));

        storyMap = new HashMap<>();
        storyMap.put(SystemField.ISSUE_TYPE.code(), new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(SystemField.PRIORITY.code(), new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(SystemField.SUMMARY.code(), new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(SystemField.REPORTER.code(), new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(SystemField.DESCRIPTION.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.ASSIGNEE.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.EPIC.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.SPRINT.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.CREATION_DATE.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.LAST_UPDATE_DATE.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.INFLUENCE_VERSION.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.FIX_VERSION.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.STORY_POINTS.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.REMAINING_TIME.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.TIME_TRACE.code(), new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(SystemField.COMPONENT.code(), new PageConfigFieldEditedVO(false, true, true));

        taskMap = new HashMap<>();
        taskMap.put(SystemField.ISSUE_TYPE.code(), new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(SystemField.PRIORITY.code(), new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(SystemField.SUMMARY.code(), new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(SystemField.REPORTER.code(), new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(SystemField.DESCRIPTION.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.ASSIGNEE.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.EPIC.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.SPRINT.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.FIX_VERSION.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.INFLUENCE_VERSION.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.REMAINING_TIME.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.CREATION_DATE.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.LAST_UPDATE_DATE.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.TIME_TRACE.code(), new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(SystemField.COMPONENT.code(), new PageConfigFieldEditedVO(false, true, true));

        bugMap = new HashMap<>();
        bugMap.put(SystemField.ISSUE_TYPE.code(), new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(SystemField.PRIORITY.code(), new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(SystemField.SUMMARY.code(), new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(SystemField.REPORTER.code(), new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(SystemField.DESCRIPTION.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.ASSIGNEE.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.ASSIGNEE.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.EPIC.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.SPRINT.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.INFLUENCE_VERSION.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.FIX_VERSION.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.REMAINING_TIME.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.CREATION_DATE.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.LAST_UPDATE_DATE.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.TIME_TRACE.code(), new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(SystemField.COMPONENT.code(), new PageConfigFieldEditedVO(false, true, true));
    }

}
