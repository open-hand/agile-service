package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.AppVersionSearchVO;
import io.choerodon.agile.api.vo.AppVersionVO;
import io.choerodon.agile.infra.dto.AppVersionDTO;
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
}
