package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.StatusTransferSettingDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-12 9:58
 */
public interface StatusTransferSettingMapper extends BaseMapper<StatusTransferSettingDTO> {
    List<StatusTransferSettingDTO> listByStatusId(@Param("projectId") Long projectId,@Param("issueTypeId") Long issueTypeId,@Param("statusIds") List<Long> statusIds);

    List<StatusTransferSettingDTO> listOptions(@Param("organizationId") Long organizationId,@Param("issueTypeId") Long issueTypeId,@Param("statusIds") List<Long> statusIds);

    List<Long> queryStatusTransferByIssueTypeAndUserType(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("issueTypeId") Long issueTypeId, @Param("userType") String userType);

    void deleteByIssueTypeId(@Param("organizationId") Long organizationId,
                             @Param("projectId") Long projectId,
                             @Param("issueTypeId") Long issueTypeId);
}
