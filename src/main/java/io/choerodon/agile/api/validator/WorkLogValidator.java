package io.choerodon.agile.api.validator;


import io.choerodon.agile.api.vo.WorkLogVO;
import io.choerodon.agile.infra.dto.IssueDTO;
import io.choerodon.core.exception.CommonException;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/8/13.
 * Email: fuqianghuang01@gmail.com
 */

public class WorkLogValidator {

    private static final String ERROR_OBJECTVERSIONNUMBER_ISNULL = "error.objectVersionNumber.isNull";
    private static final String ERROR_LOGID_ISNULL = "error.logId.isNull";
    private static final String ERROR_PROJECTID_NOTNULL = "error.projectId.notEqual";
    private static final String ERROR_ISSUE_GET = "error.issue.get";

    private WorkLogValidator() {
    }

    public static void checkCreateWorkLog(Long projectId, WorkLogVO workLogVO, IssueDTO issueDTO) {
        if (!projectId.equals(workLogVO.getProjectId())) {
            throw new CommonException(ERROR_PROJECTID_NOTNULL);
        }
        if (issueDTO == null) {
            throw new CommonException(ERROR_ISSUE_GET);
        }
    }

    public static void checkUpdateWorkLog(WorkLogVO workLogVO) {
        if (workLogVO.getLogId() == null) {
            throw new CommonException(ERROR_LOGID_ISNULL);
        }
        if (workLogVO.getObjectVersionNumber() == null) {
            throw new CommonException(ERROR_OBJECTVERSIONNUMBER_ISNULL);
        }
    }

}
