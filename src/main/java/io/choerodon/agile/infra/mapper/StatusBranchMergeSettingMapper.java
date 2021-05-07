package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.StatusBranchMergeSettingDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author superlee
 * @since 2021-04-19
 */
public interface StatusBranchMergeSettingMapper extends BaseMapper<StatusBranchMergeSettingDTO> {

    List<StatusBranchMergeSettingDTO> listByOptions(@Param("projectId") Long projectId,
                                                    @Param("organizationId") Long organizationId,
                                                    @Param("issueTypeId") Long issueTypeId,
                                                    @Param("statusIds") List<Long> statusIds);
}
