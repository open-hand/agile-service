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
        } else if (ObjectSchemeFieldContext.FEATURE.equals(issueType)) {
            return featureMap;
        }
        else {
            return null;
        }
    }

    private static Map<String, PageConfigFieldEditedVO> epicMap;
    private static Map<String, PageConfigFieldEditedVO> storyMap;
    private static Map<String, PageConfigFieldEditedVO> taskMap;
    private static Map<String, PageConfigFieldEditedVO> bugMap;
    private static Map<String, PageConfigFieldEditedVO> featureMap;

    static {
        epicMap = new HashMap<>();
        epicMap.put(FieldCode.ISSUE_TYPE, new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(FieldCode.STATUS, new PageConfigFieldEditedVO(true, false, true));
        epicMap.put(FieldCode.SUMMARY, new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(FieldCode.PRIORITY, new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(FieldCode.EPIC_NAME, new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(FieldCode.DESCRIPTION, new PageConfigFieldEditedVO(false, true, true));
        epicMap.put(FieldCode.CREATION_DATE, new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(FieldCode.LAST_UPDATE_DATE, new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(FieldCode.REPORTER, new PageConfigFieldEditedVO(true, false, true));
        epicMap.put(FieldCode.CREATOR, new PageConfigFieldEditedVO(true, true, true));
        epicMap.put(FieldCode.UPDATOR, new PageConfigFieldEditedVO(true, true, true));

        featureMap = new HashMap<>();
        featureMap.put(FieldCode.ISSUE_TYPE, new PageConfigFieldEditedVO(true, true, true));
        featureMap.put(FieldCode.SUMMARY, new PageConfigFieldEditedVO(true, true, true));
        featureMap.put(FieldCode.DESCRIPTION, new PageConfigFieldEditedVO(false, true, true));
        featureMap.put(FieldCode.CREATION_DATE, new PageConfigFieldEditedVO(true, true, true));
        featureMap.put(FieldCode.LAST_UPDATE_DATE, new PageConfigFieldEditedVO(true, true, true));
        featureMap.put(FieldCode.STATUS, new PageConfigFieldEditedVO(true, false, true));
        featureMap.put(FieldCode.REPORTER, new PageConfigFieldEditedVO(true, false, true));
        featureMap.put(FieldCode.SUB_PROJECT, new PageConfigFieldEditedVO(false, true, true));
        featureMap.put(FieldCode.CREATOR, new PageConfigFieldEditedVO(true, true, true));
        featureMap.put(FieldCode.UPDATOR, new PageConfigFieldEditedVO(true, true, true));
        featureMap.put(FieldCode.TAG, new PageConfigFieldEditedVO(false, false, false));
        featureMap.put(FieldCode.FEATURE_TYPE, new PageConfigFieldEditedVO(true, true, true));

        storyMap = new HashMap<>();
        storyMap.put(FieldCode.ISSUE_TYPE, new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(FieldCode.PRIORITY, new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(FieldCode.SUMMARY, new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(FieldCode.DESCRIPTION, new PageConfigFieldEditedVO(false, true, true));
        storyMap.put(FieldCode.CREATION_DATE, new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(FieldCode.LAST_UPDATE_DATE, new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(FieldCode.STATUS, new PageConfigFieldEditedVO(true, false, true));
        storyMap.put(FieldCode.REPORTER, new PageConfigFieldEditedVO(true, false, true));
        storyMap.put(FieldCode.CREATOR, new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(FieldCode.UPDATOR, new PageConfigFieldEditedVO(true, true, true));
        storyMap.put(FieldCode.TAG, new PageConfigFieldEditedVO(false, false, false));
        storyMap.put(FieldCode.STORY_POINTS, new PageConfigFieldEditedVO(false, false, true));
        storyMap.put(FieldCode.REMAINING_TIME, new PageConfigFieldEditedVO(false, false, true));
        storyMap.put(FieldCode.TIME_TRACE, new PageConfigFieldEditedVO(true, true, false));
        storyMap.put(FieldCode.ESTIMATE_TIME, new PageConfigFieldEditedVO(false, false, true));


        taskMap = new HashMap<>();
        taskMap.put(FieldCode.ISSUE_TYPE, new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(FieldCode.PRIORITY, new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(FieldCode.SUMMARY, new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(FieldCode.DESCRIPTION, new PageConfigFieldEditedVO(false, true, true));
        taskMap.put(FieldCode.CREATION_DATE, new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(FieldCode.LAST_UPDATE_DATE, new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(FieldCode.STATUS, new PageConfigFieldEditedVO(true, false, true));
        taskMap.put(FieldCode.REPORTER, new PageConfigFieldEditedVO(true, false, true));
        taskMap.put(FieldCode.CREATOR, new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(FieldCode.UPDATOR, new PageConfigFieldEditedVO(true, true, true));
        taskMap.put(FieldCode.TAG, new PageConfigFieldEditedVO(false, false, false));
        taskMap.put(FieldCode.REMAINING_TIME, new PageConfigFieldEditedVO(false, false, true));
        taskMap.put(FieldCode.TIME_TRACE, new PageConfigFieldEditedVO(true, true, false));
        taskMap.put(FieldCode.ESTIMATE_TIME, new PageConfigFieldEditedVO(false, false, true));

        bugMap = new HashMap<>();
        bugMap.put(FieldCode.ISSUE_TYPE, new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(FieldCode.PRIORITY, new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(FieldCode.SUMMARY, new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(FieldCode.DESCRIPTION, new PageConfigFieldEditedVO(false, true, true));
        bugMap.put(FieldCode.CREATION_DATE, new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(FieldCode.LAST_UPDATE_DATE, new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(FieldCode.STATUS, new PageConfigFieldEditedVO(true, false, true));
        bugMap.put(FieldCode.REPORTER, new PageConfigFieldEditedVO(true, false, true));
        bugMap.put(FieldCode.CREATOR, new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(FieldCode.UPDATOR, new PageConfigFieldEditedVO(true, true, true));
        bugMap.put(FieldCode.TAG, new PageConfigFieldEditedVO(false, false, false));
        bugMap.put(FieldCode.REMAINING_TIME, new PageConfigFieldEditedVO(false, false, true));
        bugMap.put(FieldCode.TIME_TRACE, new PageConfigFieldEditedVO(true, true, false));
        bugMap.put(FieldCode.ESTIMATE_TIME, new PageConfigFieldEditedVO(false, false, true));
    }

}
