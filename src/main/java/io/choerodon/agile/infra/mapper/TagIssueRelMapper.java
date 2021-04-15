package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.TagVO;
import io.choerodon.agile.api.vo.TagWithIssueVO;
import io.choerodon.agile.infra.dto.TagIssueRelDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-25
 */
public interface TagIssueRelMapper extends BaseMapper<TagIssueRelDTO> {

    Set<Long> selectByTagAndIssueType(@Param("projectId") Long projectId,
                                      @Param("organizationId") Long organizationId,
                                      @Param("tags") List<TagVO> tags,
                                      @Param("issueTypeCode") String issueTypeCode);

    List<TagWithIssueVO> selectCompletedStoryByTags(@Param("projectIds") Set<Long> projectIds,
                                                    @Param("organizationId") Long organizationId,
                                                    @Param("tags") Set<TagVO> tags,
                                                    @Param("withFeature") Boolean withFeature);

    List<TagWithIssueVO> selectCompletedBugOrTaskByTags(@Param("projectIds") Set<Long> projectIds,
                                                        @Param("organizationId") Long organizationId,
                                                        @Param("tags") Set<TagVO> tags,
                                                        @Param("issueTypeCode") String issueTypeCode);
}
