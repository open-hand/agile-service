package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.PublishVersionVO;
import io.choerodon.agile.infra.dto.PublishVersionDTO;
import io.choerodon.mybatis.common.BaseMapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-09
 */
public interface PublishVersionMapper extends BaseMapper<PublishVersionDTO> {

    List<PublishVersionVO> selectByProjectIds(@Param("projectIds") Set<Long> projectIds);

    /**
     * 查询项目下应用版本
     *
     * @param projectId        项目id
     * @param publishVersionVO 查询参数
     * @return 项目下应用版本
     */
    List<PublishVersionVO> listByOptions(@Param("projectId") Long projectId,
                                         @Param("publishVersionVO") PublishVersionVO publishVersionVO);

    /**
     * 查询publish version关联的issue
     *
     * @param projectIds
     * @param organizationId
     * @param publishVersionIds
     * @return
     */
    Set<Long> selectIssueIds(@Param("projectIds") Set<Long> projectIds,
                             @Param("organizationId") Long organizationId,
                             @Param("publishVersionIds") Set<Long> publishVersionIds);
}