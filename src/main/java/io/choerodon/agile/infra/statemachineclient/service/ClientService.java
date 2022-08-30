package io.choerodon.agile.infra.statemachineclient.service;

import java.util.List;

import io.choerodon.agile.infra.statemachineclient.dto.ExecuteResult;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;
import io.choerodon.agile.infra.statemachineclient.dto.TransformInfo;

/**
 * 状态机客户端回调service
 *
 * @author shinan.chen
 * @author dinghuang123@gmail.com
 * @since 2018/10/11
 */
public interface ClientService {

    /**
     * 根据条件过滤转换
     *
     * @param instanceId     instanceId
     * @param transformInfos transformInfos
     * @return TransformInfo
     */
    List<TransformInfo> conditionFilter(Long instanceId, List<TransformInfo> transformInfos);


    /**
     * 执行条件
     *
     * @param targetStatusId targetStatusId
     * @param conditionStrategy conditionStrategy
     * @param inputDTO inputDTO
     * @return result
     */
    ExecuteResult configExecuteCondition(Long targetStatusId, String conditionStrategy, InputDTO inputDTO);

    /**
     * 执行验证
     *
     * @param targetStatusId targetStatusId
     * @param inputDTO inputDTO
     * @return result
     */
    ExecuteResult configExecuteValidator(Long targetStatusId, InputDTO inputDTO);

    /**
     * 执行后置动作，单独出来，才能生效回归
     *
     * @param targetStatusId targetStatusId
     * @param transformType transformType
     * @param inputDTO inputDTO
     * @return result
     */
    ExecuteResult configExecutePostAction(Long targetStatusId, String transformType, InputDTO inputDTO);
}
