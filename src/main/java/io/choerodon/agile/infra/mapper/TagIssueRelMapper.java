package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.TagIssueRelDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-25
 */
public interface TagIssueRelMapper extends BaseMapper<TagIssueRelDTO> {

    Set<Long> selectByTagIdsAndIssueType(@Param("projectId") Long projectId,
                                         @Param("organizationId") Long organizationId,
                                         @Param("tagIds") Set<Long> tagIds,
                                         @Param("issueTypeCode") String issueTypeCode);

}
