package io.choerodon.agile.api.validator;


import io.choerodon.agile.api.vo.IssueComponentVO;
import io.choerodon.core.exception.CommonException;
import org.springframework.util.ObjectUtils;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/8/13.
 * Email: fuqianghuang01@gmail.com
 */
public class IssueComponentValidator {

    private static final String ERROR_PROJECTID_NOTEQUAL = "error.projectId.notEqual";
    private static final String ERROR_COMPONENTNAME_ISNULL = "error.componentName.isNull";
    private static final String ERROR_COMPONENT_SEQUENCE = "error.component.sequence.overflow";
    private static final Integer MAX_SEQUENCE = 100000;

    private IssueComponentValidator() {}

    public static void checkCreateComponent(Long projectId, IssueComponentVO issueComponentVO) {
        if (!projectId.equals(issueComponentVO.getProjectId())) {
            throw new CommonException(ERROR_PROJECTID_NOTEQUAL);
        }
        if (issueComponentVO.getName() == null) {
            throw new CommonException(ERROR_COMPONENTNAME_ISNULL);
        }

        if (!ObjectUtils.isEmpty(issueComponentVO.getSequence()) && issueComponentVO.getSequence() > MAX_SEQUENCE) {
            throw new CommonException(ERROR_COMPONENT_SEQUENCE);
        }
    }

    public static void checkUpdateComponent(Long projectId, IssueComponentVO issueComponentVO) {
        if (!ObjectUtils.isEmpty(issueComponentVO.getSequence()) && issueComponentVO.getSequence() > MAX_SEQUENCE) {
            throw new CommonException(ERROR_COMPONENT_SEQUENCE);
        }
    }

}
