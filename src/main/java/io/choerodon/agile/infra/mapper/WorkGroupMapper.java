package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.WorkGroupDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-11-08 15:30
 */
public interface WorkGroupMapper extends BaseMapper<WorkGroupDTO> {

    void deleteByWorkGroupIds(@Param("organizationId") Long organiztionId, @Param("workGroupIds")List<Long> workGroupIds);

    String queryMinRank(@Param("organizationId")  Long organizationId, @Param("parentId")  Long parentId);

    List<WorkGroupDTO> selectByOrganiztionId(@Param("organizationId") Long organizationId);

    String queryRank(@Param("organizationId") Long organizationId, @Param("parentId") Long parentId, @Param("outSetId") Long outSetId);

    String queryLeftRank(@Param("organizationId") Long organizationId,@Param("parentId") Long parentId, @Param("rightRank") String rightRank);

    String queryRightRank(@Param("organizationId") Long organizationId,@Param("parentId") Long parentId, @Param("leftRank") String leftRank);

    List<Long> selectIdsByOrganizationId(@Param("organizationId")  Long organizationId, @Param("ignoredWorkGroupIds")  List<Long> ignoredWorkGroupIds);
}
