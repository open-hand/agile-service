package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.StatusMachineSchemeConfigVO;
import io.choerodon.agile.infra.dto.StatusMachineSchemeConfigDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;

/**
 * @author peng.jiang@hand-china.com
 */
@Component
public interface StatusMachineSchemeConfigMapper extends BaseMapper<StatusMachineSchemeConfigDTO> {
    StatusMachineSchemeConfigDTO selectDefault(@Param("organizationId") Long organizationId, @Param("schemeId") Long schemeId);

    List<StatusMachineSchemeConfigDTO> queryByStateMachineIds(@Param("organizationId") Long organizationId, @Param("stateMachineIds") List<Long> stateMachineIds);

    List<StatusMachineSchemeConfigDTO> queryByOrgId(@Param("organizationId") Long organizationId);

    void migrateStatusMachineSchemeConfig();

    List<StatusMachineSchemeConfigDTO> selectBySchemeIds(@Param("organizationId") Long organizationId,
                                                         @Param("schemeIds") Set<Long> schemeIds);

    Long selectStatusMachineIdByIssueTypeId(@Param("organizationId") Long organizationId,
                                            @Param("projectId") Long projectId,
                                            @Param("applyType") String applyType,
                                            @Param("issueTypeId") Long issueTypeId);

    List<StatusMachineSchemeConfigVO> queryStatusMachineMapByAppleTypes(@Param("organizationId") Long organizationId,
                                                                        @Param("projectId") Long projectId,
                                                                        @Param("applyTypes")List<String> applyTypes);
}
