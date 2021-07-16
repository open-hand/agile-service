package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.StarBeaconDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * Mapper
 *
 * @author jiaxu.cui@hand-china.com 2020-11-09 17:08:45
 */
public interface StarBeaconMapper extends BaseMapper<StarBeaconDTO> {
    /**
     * 根据issueIds查询星标
     * @param issueIds
     * @param projectIds
     * @param userId
     * @return
     */
    List<Long> selectStarIssuesByIds(@Param("issueIds") List<Long> issueIds, @Param("projectIds") List<Long> projectIds, @Param("userId") Long userId);

    List<Long> selectUsersByInstanceId(@Param("projectId") Long projectId, @Param("instanceId") Long instanceId);
}

