package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.StatusLinkageVO;
import io.choerodon.agile.infra.dto.StatusLinkageDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-17 19:10
 */
public interface StatusLinkageMapper extends BaseMapper<StatusLinkageDTO> {

    List<StatusLinkageDTO> selectByStatusIds(@Param("projectId") Long projectId, @Param("issueTypeId") Long issueTypeId, @Param("statusIds") List<Long> statusId);

    List<StatusLinkageDTO> listByIssueTypeIdsParentTypeId(@Param("projectId") Long projectId, @Param("parentIssueTypeId") Long parentIssueTypeId, @Param("issueTypeIds") List<Long> issueTypeIds, @Param("parentChangeStatusId") Long parentChangeStatusId);

    List<StatusLinkageVO> selectWithStatusByProjectId(@Param("projectId") Long projectId);

    List<StatusLinkageDTO> selectExistStatusLink(@Param("projectId") Long projectId, @Param("statusId") Long statusId, @Param("issueTypeId") Long issueTypeId);

    List<StatusLinkageDTO> listOptions(@Param("organizationId") Long organizationId, @Param("issueTypeId") Long issueTypeId, @Param("statusIds") List<Long> statusId);

    void deleteByIssueTypeId(@Param("organizationId") Long organizationId,
                             @Param("projectId") Long projectId,
                             @Param("issueTypeId") Long issueTypeId);
}
