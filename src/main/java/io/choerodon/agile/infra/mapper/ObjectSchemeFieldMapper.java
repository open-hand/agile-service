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

    List<ObjectSchemeFieldDetailVO> selectCustomFieldList(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId);

    /**
     * 根据fd_object_scheme_field_extend id查询字段
     *
     * @param extendIds
     */
    List<ObjectSchemeFieldDTO> selectByExtendIds(@Param("extendIds") Set<Long> extendIds);

    /**
     * 查询项目层或者组织层的字段，项目层可以看到组织层的字段，如果项目和组织层都有字段，以项目层为准
     *
     * @param organizationId
     * @param projectId
     * @param schemeCode
     * @return
     */
    List<ObjectSchemeFieldDTO> selectByOptions(@Param("organizationId") Long organizationId,
                                               @Param("projectId") Long projectId,
                                               @Param("schemeCode") String schemeCode,
                                               @Param("fieldId") Long fieldId,
                                               @Param("issueTypeId") Long issueTypeId);

    /**
     * 查询类型对应的字段以及自定义字段的选项
     *
     * @param organizationId
     * @param projectId
     * @param issueTypeId
     * @return
     */
    List<ObjectSchemeFieldDetailVO> selectFieldsWithOptionals(@Param("organizationId") Long organizationId,
                                                         @Param("projectId") Long projectId,
                                                         @Param("issueTypeId") Long issueTypeId);

    /**
     * 删除字段，级联删除字段扩展数据
     *
     * @param organizationId
     * @param projectId
     * @param fieldId
     */
    void cascadeDelete(@Param("organizationId") Long organizationId,
                       @Param("projectId") Long projectId,
                       @Param("fieldId") Long fieldId);
}
