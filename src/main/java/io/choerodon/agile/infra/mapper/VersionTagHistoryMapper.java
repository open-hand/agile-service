package io.choerodon.agile.infra.mapper;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.infra.dto.VersionTagHistoryDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/05/19 11:00
 */
public interface VersionTagHistoryMapper extends BaseMapper<VersionTagHistoryDTO> {

    /**
     * 查询最近的tag历史
     *
     * @param projectId        项目id
     * @param publishVersionId 发布版本id
     * @param userId           用户id
     * @return 最近的tag历史
     */
    VersionTagHistoryDTO queryLatestHistory(@Param("projectId") Long projectId,
                                            @Param("publishVersionId") Long publishVersionId,
                                            @Param("userId") Long userId,
                                            @Param("versionType") String versionType);
}
