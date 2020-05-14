package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.StateMachineSchemeConfigDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author peng.jiang@hand-china.com
 */
@Component
public interface StateMachineSchemeConfigMapper extends BaseMapper<StateMachineSchemeConfigDTO> {
    StateMachineSchemeConfigDTO selectDefault(@Param("organizationId") Long organizationId, @Param("schemeId") Long schemeId);

    List<StateMachineSchemeConfigDTO> queryByStateMachineIds(@Param("organizationId") Long organizationId, @Param("stateMachineIds") List<Long> stateMachineIds);

    List<StateMachineSchemeConfigDTO> queryByOrgId(@Param("organizationId") Long organizationId);
}
