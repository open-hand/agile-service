package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.TagCompareHistoryDTO;
import io.choerodon.mybatis.common.BaseMapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-04-18
 */
public interface TagCompareHistoryMapper extends BaseMapper<TagCompareHistoryDTO> {

    List<TagCompareHistoryDTO> selectByAppServiceCodes(@Param("projectId") Long projectId,
                                                       @Param("organizationId") Long organizationId,
                                                       @Param("appServiceCodes") Set<String> appServiceCodes);

    /**
     * 查询最新的对比记录以自动填充界面
     *
     * @param projectId        项目id
     * @param organizationId   组织id
     * @param publishVersionId 发布版本id
     * @return 最新的对比记录
     */
    List<TagCompareHistoryDTO> selectLastCompareHistory(@Param("projectId") Long projectId, @Param("organizationId") Long organizationId, @Param("publishVersionId") Long publishVersionId);
}
