package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.StatusFieldSettingVO;
import io.choerodon.agile.infra.dto.StatusFieldSettingDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-08-13 14:45
 */
public interface StatusFieldSettingMapper extends BaseMapper<StatusFieldSettingDTO> {
    List<StatusFieldSettingVO> listByStatusIds(@Param("projectId") Long projectId,@Param("issueTypeId") Long issueTypeId,@Param("statusIds") List<Long> statusIds);

    List<StatusFieldSettingVO> listOptions(@Param("organizationId") Long organizationId,@Param("issueTypeId") Long issueTypeId,@Param("statusIds") List<Long> statusIds);


    void deleteByIssueTypeId(@Param("organizationId") Long organizationId,
                             @Param("projectId") Long projectId,
                             @Param("issueTypeId") Long issueTypeId);
}
