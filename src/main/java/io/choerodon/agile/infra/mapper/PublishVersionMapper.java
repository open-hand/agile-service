package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.PublishVersionVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.dto.PublishVersionDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
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
     * 查询应用版本关联的故事
     *
     * @param projectId         项目id
     * @param publishVersionIds 发布版本ids
     * @param searchVO          查询参数
     * @return 应用版本关联的故事
     */
    List<IssueDTO> listRelStoryByOption(@Param("projectId") Long projectId,
                                        @Param("publishVersionIds") List<Long> publishVersionIds,
                                        @Param("searchVO") SearchVO searchVO);

    /**
     * 查询应用版本关联的缺陷
     *
     * @param projectId     项目id
     * @param publishVersionIds 发布版本ids
     * @param searchVO      查询参数
     * @return 应用版本关联的故事
     */
    List<IssueDTO> listRelBugByOption(@Param("projectId") Long projectId, @Param("publishVersionIds") List<Long> publishVersionIds, @Param("searchVO") SearchVO searchVO);
}
