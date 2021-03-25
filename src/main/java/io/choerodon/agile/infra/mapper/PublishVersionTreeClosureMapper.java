package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.PublishVersionTreeClosureDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-19
 */
public interface PublishVersionTreeClosureMapper extends BaseMapper<PublishVersionTreeClosureDTO> {


    /**
     * 查询节点所有祖先
     *
     * @param projectId
     * @param organizationId
     * @param descendantId
     * @return
     */
    Set<PublishVersionTreeClosureDTO> selectAncestors(@Param("projectId") Long projectId,
                                                      @Param("organizationId") Long organizationId,
                                                      @Param("descendantId") Long descendantId);

    /**
     * 查询节点的所有后代
     *
     * @param projectIds
     * @param organizationId
     * @param ancestorIds
     * @return
     */
    Set<PublishVersionTreeClosureDTO> selectDescendants(@Param("projectIds") Set<Long> projectIds,
                                                        @Param("organizationId") Long organizationId,
                                                        @Param("ancestorIds") Set<Long> ancestorIds,
                                                        @Param("descendantParent") Long descendantParent);

    /**
     * 查询存在的数据并返回
     *
     * @param descendantSet
     * @param projectId
     * @param organizationId
     * @return
     */
    Set<PublishVersionTreeClosureDTO> selectInList(@Param("descendantSet") Set<PublishVersionTreeClosureDTO> descendantSet,
                                                   @Param("projectId") Long projectId,
                                                   @Param("organizationId") Long organizationId);

    /**
     * 批量插入
     *
     * @param insertSet
     * @param operator
     */
    void batchInsert(@Param("insertSet") Set<PublishVersionTreeClosureDTO> insertSet,
                     @Param("operator") Long operator);

    /**
     * @param descendantSet
     * @param ancestorIds
     * @param projectId
     * @param organizationId
     * @return
     */
    Set<PublishVersionTreeClosureDTO> selectAncestorsByIds(@Param("descendantSet") Set<PublishVersionTreeClosureDTO> descendantSet,
                                                           @Param("ancestorIds") Set<Long> ancestorIds,
                                                           @Param("projectId") Long projectId,
                                                           @Param("organizationId") Long organizationId);

    /**
     * 批量删除
     *
     * @param deleteSet
     * @param projectId
     * @param organizationId
     */
    void batchDelete(@Param("deleteSet") Set<PublishVersionTreeClosureDTO> deleteSet,
                     @Param("projectId") Long projectId,
                     @Param("organizationId") Long organizationId);
}
