package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.PublishVersionWithIssueVO;
import io.choerodon.agile.infra.dto.PublishVersionIssueRelDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-09
 */
public interface PublishVersionIssueRelMapper extends BaseMapper<PublishVersionIssueRelDTO> {

    List<PublishVersionWithIssueVO> selectCompletedBugByAppVersionIds(@Param("projectIds") Set<Long> projectIds,
                                                                      @Param("organizationId") Long organizationId,
                                                                      @Param("appVersionIds") Set<Long> appVersionIds);

    /**
     * 删除问题与应用版本的关系
     *
     * @param projectId     项目id
     * @param issueId       问题id
     * @param appVersionIds 应用版本id
     */
    void deleteIssueRelByAppVersionIds(@Param("projectId") Long projectId, @Param("issueId") Long issueId, @Param("appVersionIds") List<Long> appVersionIds);
}
