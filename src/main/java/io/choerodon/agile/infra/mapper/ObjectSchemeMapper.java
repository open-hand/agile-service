package io.choerodon.agile.infra.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.api.vo.ObjectSchemeSearchVO;
import io.choerodon.agile.infra.dto.ObjectSchemeDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
public interface ObjectSchemeMapper extends BaseMapper<ObjectSchemeDTO> {
    /**
     * 分页查询对象方案
     *
     * @param organizationId organizationId
     * @param searchVO searchVO
     * @return result
     */
    List<ObjectSchemeDTO> fulltextSearch(@Param("organizationId") Long organizationId, @Param("searchVO") ObjectSchemeSearchVO searchVO);
}
