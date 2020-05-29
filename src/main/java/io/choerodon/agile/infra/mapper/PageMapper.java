package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.PageSearchVO;
import io.choerodon.agile.infra.dto.PageDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
public interface PageMapper extends BaseMapper<PageDTO> {
    /**
     * 分页查询页面
     *
     * @param organizationId
     * @param searchVO
     * @return
     */
    List<PageDTO> fulltextSearch(@Param("organizationId") Long organizationId, @Param("searchVO") PageSearchVO searchVO);
}
