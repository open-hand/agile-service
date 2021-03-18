package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.AppVersionSearchVO;
import io.choerodon.agile.api.vo.AppVersionVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.dto.AppVersionDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.mybatis.common.BaseMapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-09
 */
public interface AppVersionMapper extends BaseMapper<AppVersionDTO> {

    List<AppVersionVO> selectByProjectIds(@Param("projectIds") Set<Long> projectIds);

    /**
     * 查询项目下应用版本
     *
     * @param projectId          项目id
     * @param appVersionSearchVO 查询参数
     * @return 项目下应用版本
     */
    List<AppVersionVO> listAppVersionByProjectId(@Param("projectId") Long projectId, @Param("appVersionSearchVO") AppVersionSearchVO appVersionSearchVO);

    /**
     * 查询应用版本关联的故事
     *
     * @param projectId     项目id
     * @param appVersionIds 应用版本ids
     * @param searchVO      查询参数
     * @return 应用版本关联的故事
     */
    List<IssueDTO> listRelStoryByOption(@Param("projectId") Long projectId, @Param("appVersionIds") List<Long> appVersionIds, @Param("searchVO") SearchVO searchVO);

    /**
     * 查询应用版本关联的缺陷
     *
     * @param projectId     项目id
     * @param appVersionIds 应用版本ids
     * @param searchVO      查询参数
     * @return 应用版本关联的故事
     */
    List<IssueDTO> listRelBugByOption(@Param("projectId") Long projectId, @Param("appVersionIds") List<Long> appVersionIds, @Param("searchVO") SearchVO searchVO);
}
