package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.ObjectSchemeSearchVO;
import io.choerodon.agile.infra.dto.ObjectSchemeDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
public interface ObjectSchemeMapper extends BaseMapper<ObjectSchemeDTO> {
    /**
     * 分页查询对象方案
     *
     * @param organizationId
     * @param searchVO
     * @return
     */
    List<ObjectSchemeDTO> fulltextSearch(@Param("organizationId") Long organizationId, @Param("searchVO") ObjectSchemeSearchVO searchVO);
}
