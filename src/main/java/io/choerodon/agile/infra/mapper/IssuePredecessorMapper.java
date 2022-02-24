package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.IssuePredecessorDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-11-10
 */
public interface IssuePredecessorMapper extends BaseMapper<IssuePredecessorDTO> {

    Set<IssuePredecessorDTO> selectInList(@Param("organizationId") Long organizationId,
                                          @Param("projectId") Long projectId,
                                          @Param("issuePredecessorList") List<IssuePredecessorDTO> issuePredecessorList);

    void batchInsert(@Param("issuePredecessorList") List<IssuePredecessorDTO> issuePredecessorList,
                     @Param("operator") Long operator);

    void deleteByIssueId(@Param("organizationId") Long organizationId,
                         @Param("projectId") Long projectId,
                         @Param("issueId") Long issueId);

    List<IssuePredecessorDTO> selectByIssueIds(@Param("projectIds") Set<Long> projectIds,
                                               @Param("issueIds") Set<Long> issueIds);

    List<Long> selectByPredecessorId(@Param("projectId") Long projectId,
                                     @Param("predecessorId") Long predecessorId);
}
