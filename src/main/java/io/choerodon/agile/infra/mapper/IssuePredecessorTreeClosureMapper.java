package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.IssuePredecessorTreeClosureDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-11-10
 */
public interface IssuePredecessorTreeClosureMapper extends BaseMapper<IssuePredecessorTreeClosureDTO> {

    Set<IssuePredecessorTreeClosureDTO> selectInList(@Param("organizationId") Long organizationId,
                                                     @Param("projectId") Long projectId,
                                                     @Param("treeNodes") Set<IssuePredecessorTreeClosureDTO> treeNodes);

    void batchInsert(@Param("treeNodes") Set<IssuePredecessorTreeClosureDTO> treeNodes,
                     @Param("operator") Long operator);

    List<IssuePredecessorTreeClosureDTO> selectByDescendantIds(@Param("organizationId") Long organizationId,
                                                               @Param("projectId") Long projectId,
                                                               @Param("descendantIds") Set<Long> descendantIds);

    List<IssuePredecessorTreeClosureDTO> selectByAncestorIds(@Param("organizationId") Long organizationId,
                                                             @Param("projectId") Long projectId,
                                                             @Param("ancestorIds") Set<Long> ancestorIds);

    void batchDelete(@Param("organizationId") Long organizationId,
                     @Param("projectId") Long projectId,
                     @Param("deleteSet") Set<IssuePredecessorTreeClosureDTO> deleteSet);
}
