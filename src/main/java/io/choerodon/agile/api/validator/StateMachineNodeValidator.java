package io.choerodon.agile.api.validator;

import io.choerodon.agile.api.vo.StatusMachineNodeVO;
import io.choerodon.core.exception.CommonException;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
@Component
public class StateMachineNodeValidator {

    public void createValidate(StatusMachineNodeVO statusMachineNodeVO) {
        if (StringUtils.isEmpty(statusMachineNodeVO.getStateMachineId())) {
            throw new CommonException("error.stateMachineNode.stateMachineId.empty");
        }
        if (StringUtils.isEmpty(statusMachineNodeVO.getStatusId()) && statusMachineNodeVO.getStatusVO() == null) {
            throw new CommonException("error.stateMachineNode.state.null");
        }
        if (StringUtils.isEmpty(statusMachineNodeVO.getStatusId()) && statusMachineNodeVO.getStatusVO() != null && StringUtils.isEmpty(statusMachineNodeVO.getStatusVO().getName())) {
            throw new CommonException("error.stateMachineNode.state.name.empty");
        }
    }

    public void updateValidate(StatusMachineNodeVO statusMachineNodeVO) {
        if (StringUtils.isEmpty(statusMachineNodeVO.getStatusId()) && statusMachineNodeVO.getStatusVO() == null) {
            throw new CommonException("error.stateMachineNode.state.null");
        }
        if (statusMachineNodeVO.getStatusVO() != null && StringUtils.isEmpty(statusMachineNodeVO.getStatusVO().getName())) {
            throw new CommonException("error.stateMachineNode.state.name.empty");
        }
    }
}
