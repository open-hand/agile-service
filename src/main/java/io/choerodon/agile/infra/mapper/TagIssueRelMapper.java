package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.IssueTypeCountVO;
import io.choerodon.agile.api.vo.TagVO;
import io.choerodon.agile.api.vo.TagWithIssueVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.infra.dto.TagIssueRelDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-25
 */
public interface TagIssueRelMapper extends BaseMapper<TagIssueRelDTO> {

    List<Long> selectByTagAndIssueType(@Param("projectIds") Set<Long> projectIds,
                                       @Param("organizationId") Long organizationId,
                                       @Param("tagProjectIds") Set<Long> tagProjectIds,
                                       @Param("tagNames") Set<String> tagNames,
                                       @Param("appServiceCodes") Set<String> appServiceCodes,
                                       @Param("issueTypeCode") String issueTypeCode);

    List<Long> selectFeatureByTag(@Param("projectIds") Set<Long> projectIds,
                                  @Param("organizationId") Long organizationId,
                                  @Param("tagProjectIds") Set<Long> tagProjectIds,
                                  @Param("tagNames") Set<String> tagNames,
                                  @Param("appServiceCodes") Set<String> appServiceCodes);

    List<TagWithIssueVO> selectCompletedStoryByTags(@Param("projectIds") Set<Long> projectIds,
                                                    @Param("organizationId") Long organizationId,
                                                    @Param("tags") Set<TagVO> tags,
                                                    @Param("withFeature") Boolean withFeature);

    List<TagWithIssueVO> selectCompletedBugOrTaskByTags(@Param("projectIds") Set<Long> projectIds,
                                                        @Param("organizationId") Long organizationId,
                                                        @Param("tags") Set<TagVO> tags,
                                                        @Param("issueTypeCode") String issueTypeCode);

    List<TagIssueRelDTO> selectByOptions(@Param("organizationId") Long organizationId,
                                         @Param("projectIds") Set<Long> projectIds,
                                         @Param("issueIds") List<Long> issueIds);

    List<IssueTypeCountVO> statisticsByIssueType(@Param("organizationId") Long organizationId,
                                                 @Param("projectId") Long projectId,
                                                 @Param("tags") Set<TagVO> tags);

    void deleteByIssueIds(@Param("projectId") Long projectId,
                          @Param("organizationId") Long organizationId,
                          @Param("issueIds") Set<Long> issueIds);

    List<TagIssueRelDTO> selectByTags(@Param("tags") Set<TagVO> tags,
                                      @Param("projectIds") Set<Long> projectIds);

    List<IssueDTO> selectStoryByFeatureIds(@Param("featureIds") List<Long> featureIds,
                                           @Param("subProjectIds") Set<Long> subProjectIds);

    List<IssueListFieldKVVO> selectIssueByStoryAndTag(@Param("storyIds") Set<Long> storyIds,
                                                      @Param("subProjectIds") Set<Long> subProjectIds,
                                                      @Param("tagProjectIds") Set<Long> tagProjectIds,
                                                      @Param("tagNames") Set<String> tagNames,
                                                      @Param("appServiceCodes") Set<String> appServiceCodes);
}
