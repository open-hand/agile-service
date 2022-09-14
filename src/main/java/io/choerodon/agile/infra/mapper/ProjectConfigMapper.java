package io.choerodon.agile.infra.mapper;

import java.util.Collection;
import java.util.List;

import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import io.choerodon.agile.infra.dto.ProjectConfigDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author shinan.chen
 * @date 2018/9/4
 */
@Component
public interface ProjectConfigMapper extends BaseMapper<ProjectConfigDTO> {
    List<ProjectConfigDTO> queryByProjectId(@Param("projectId") Long projectId);

    ProjectConfigDTO queryBySchemeTypeAndApplyType(@Param("projectId") Long projectId, @Param("schemeType") String schemeType, @Param("applyType") String applyType);


    List<ProjectConfigDTO> queryByProjectIdsAndOptions(@Param("projectIds") Collection<Long> projectIds,
                                                       @Param("schemeType") String schemeType,
                                                       @Param("applyType") String applyType);

    List<ProjectConfigDTO> queryBySchemeIds(@Param("schemeIds") Collection<Long> schemeIds, @Param("schemeType") String schemeType);

    /**
     * 通过方案ids查询出关联的项目（项目关联的状态机方案）
     *
     * @param schemeIds schemeIds
     * @param schemeType schemeType
     * @return result
     */
    List<ProjectConfigDTO> handleRemoveStatus(@Param("schemeIds") Collection<Long> schemeIds, @Param("schemeType") String schemeType);

    List<ProjectConfigDTO> queryByProjectIds(@Param("projectIds") Collection<Long> projectIds);

    List<ProjectConfigDTO> queryConfigsBySchemeId(@Param("schemeType") String schemeType, @Param("schemeId") Long schemeId);

    List<Long> getExistStatusTypeIds(@Param("organizationId") Long organizationId,
                                     @Param("projectId") Long projectId,
                                     @Param("statusId") Long statusId,
                                     @Param("applyTypes") Collection<String> applyTypes);
}
