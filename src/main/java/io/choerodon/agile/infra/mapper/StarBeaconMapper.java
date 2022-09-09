package io.choerodon.agile.infra.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.infra.dto.StarBeaconDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * Mapper
 *
 * @author jiaxu.cui@hand-china.com 2020-11-09 17:08:45
 */
public interface StarBeaconMapper extends BaseMapper<StarBeaconDTO> {
    /**
     * 根据issueIds查询星标
     * @param issueIds issueIds
     * @param projectIds projectIds
     * @param userId userId
     * @return result
     */
    List<Long> selectStarIssuesByIds(@Param("issueIds") List<Long> issueIds, @Param("projectIds") List<Long> projectIds, @Param("userId") Long userId);

    List<Long> selectUsersByInstanceId(@Param("projectId") Long projectId, @Param("instanceId") Long instanceId);
}

