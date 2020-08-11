package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.ObjectSchemeFieldDetailVO;
import io.choerodon.agile.api.vo.ObjectSchemeFieldSearchVO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
public interface ObjectSchemeFieldMapper extends BaseMapper<ObjectSchemeFieldDTO> {
    /**
     * 根据对象方案编码查询方案字段
     *
     * @param organizationId
     * @return
     */
    List<ObjectSchemeFieldDTO> listQuery(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("searchVO") ObjectSchemeFieldSearchVO searchVO);

    ObjectSchemeFieldDTO queryById(@Param("fieldId") Long fieldId);

    ObjectSchemeFieldDTO queryByFieldCode(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("fieldCode") String fieldCode);

    List<ObjectSchemeFieldDetailVO> selectCustomFieldList(@Param("organizationId") Long organizationId,@Param("projectId") Long projectId);

    /**
     * 根据fd_object_scheme_field_extend id查询字段
     * @param extendIds
     */
    List<ObjectSchemeFieldDTO> selectByExtendIds(@Param("extendIds") Set<Long> extendIds);
}
