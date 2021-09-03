package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.StatusDTO;
import io.choerodon.agile.infra.dto.StatusWithInfoDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author peng.jiang, dinghuang123@gmail.com
 */
public interface StatusMapper extends BaseMapper<StatusDTO> {

    List<Long> selectStatusIds(@Param("organizationId") Long organizationId, @Param("statusSearchVO") StatusSearchVO statusSearchVO);

    List<StatusWithInfoDTO> queryStatusList(@Param("organizationId") Long organizationId, @Param("statusIds") List<Long> statusIds);

    StatusDTO queryById(@Param("organizationId") Long organizationId, @Param("id") Long id);

    List<StatusDTO> batchStatusGet(@Param("ids") List<Long> ids);

    /**
     * 查询状态机下的所有状态
     *
     * @param organizationId
     * @param stateMachineIds
     * @return
     */
    List<StatusDTO> queryByStateMachineIds(@Param("organizationId") Long organizationId, @Param("stateMachineIds") List<Long> stateMachineIds);

    List<StatusAndTransformVO> queryByStateMachineId(@Param("organizationId") Long organizationId,
                                                     @Param("projectId") Long projectId,
                                                     @Param("stateMachineId") Long stateMachineId);

    List<StatusSettingVO> listStatusTransferByStateMachineId(@Param("organizationId") Long organizationId,@Param("stateMachineId") Long stateMachineId,@Param("param") String param);

    List<ProjectStatusVO> listStatusByProjectId(@Param("projectId") Long projectId,@Param("organizationId") Long organizationId,@Param("statusSearchVO") StatusSearchVO statusSearchVO);

    List<StatusDTO> queryByStateMachineIdsAndParam(@Param("organizationId") Long organizationId, @Param("stateMachineIds") List<Long> stateMachineIds, @Param("param") String param);

    List<StatusVO> queryByOrgId(@Param("organizationId") Long organizationId);

    List<StatusVO> queryStatusByIds(@Param("organizationId") Long organizationId, @Param("statusIds") Set<Long> statusIds);
}
