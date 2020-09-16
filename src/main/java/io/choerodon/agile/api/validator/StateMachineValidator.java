package io.choerodon.agile.api.validator;

import io.choerodon.agile.api.vo.StatusMachineVO;
import io.choerodon.core.exception.CommonException;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
@Component
public class StateMachineValidator {

    public void createValidate(StatusMachineVO statusMachineVO) {
        if (StringUtils.isEmpty(statusMachineVO.getName())) {
            throw new CommonException("error.stateMachine.name.empty");
        }
    }

    public void updateValidate(StatusMachineVO statusMachineVO) {
        if (statusMachineVO.getName() != null && statusMachineVO.getName().length() == 0) {
            throw new CommonException("error.stateMachine.name.empty");
        }
    }
}
