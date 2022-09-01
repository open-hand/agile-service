package io.choerodon.agile.infra.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.api.vo.PageSearchVO;
import io.choerodon.agile.infra.dto.PageDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
public interface PageMapper extends BaseMapper<PageDTO> {
    /**
     * 分页查询页面
     *
     * @param organizationId organizationId
     * @param searchVO searchVO
     * @return result
     */
    List<PageDTO> fulltextSearch(@Param("organizationId") Long organizationId, @Param("searchVO") PageSearchVO searchVO);
}
