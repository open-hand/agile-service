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
    List<ObjectSchemeFieldDTO> listQuery(@Param("organizationId") Long organizationId,
                                         @Param("projectIds") Set<Long> projectIds,
                                         @Param("searchVO") ObjectSchemeFieldSearchVO searchVO,
                                         @Param("issueTypes") List<String> issueTypes);

    ObjectSchemeFieldDTO queryById(@Param("fieldId") Long fieldId);

    ObjectSchemeFieldDTO queryByFieldCode(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("fieldCode") String fieldCode);

    List<ObjectSchemeFieldDTO> queryByFieldCodeList(@Param("organizationId") Long organizationId,
                                                    @Param("projectIds") Set<Long> projectIds,
                                                    @Param("fieldCodeList") List<String> fieldCodeList);

    List<ObjectSchemeFieldDetailVO> selectCustomFieldList(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("issueTypeList") String issueTypeList);

    List<ObjectSchemeFieldDetailVO> selectFieldByProjectIdsWithoutOptions(@Param("organizationId") Long organizationId,
                                                                          @Param("projectIds") List<Long> projectIds,
                                                                          @Param("issueTypeList") String issueTypeList,
                                                                          @Param("issueTypes") List<String> issueTypes);

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
                                               @Param("issueTypeId") Long issueTypeId,
                                               @Param("issueTypes") List<String> issueTypes);

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
                                                              @Param("issueTypeId") Long issueTypeId,
                                                              @Param("issueTypes") List<String> issueTypes);

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

    /**
     * 查询项目层或者组织层的人员字段，项目层可以看到组织层的字段，如果项目和组织层都有字段，以项目层为准
     *
     * @param organizationId 组织id
     * @param projectId 项目id
     * @param schemeCode 方案编码
     * @return 人员字段list
     */
    List<ObjectSchemeFieldDTO> selectMemberByOptions(@Param("organizationId") Long organizationId,
                                               @Param("projectId") Long projectId,
                                               @Param("schemeCode") String schemeCode,
                                               @Param("issueTypeId") Long issueTypeId,
                                               @Param("fieldCodeList") List<String> fieldCodeList,
                                               @Param("issueTypes") List<String> issueTypes);
    /**
     * 查询字段附带页面配置数据
     * @param objectSchemeField
     * @return
     */
    List<ObjectSchemeFieldDTO> selectFieldsWithPages(@Param("objectSchemeField") ObjectSchemeFieldDTO objectSchemeField);

    /**
     * 根据fieldCodes查询字段
     * @param fieldCodes
     * @return
     */
    List<ObjectSchemeFieldDTO> selectFieldsByFieldCodes(@Param("fieldCodes") List<String> fieldCodes);

    List<ObjectSchemeFieldDTO> selectNotSyncField(@Param("systemFieldIds") List<Long> systemFieldIds, @Param("includeBacklogSystemField") boolean includeBacklogSystemField);

    List<ObjectSchemeFieldDTO> selectNotSyncFieldByFieldConfig(@Param("organizationId") Long organizationId,
                                                               @Param("projectId") Long projectId,
                                                               @Param("issueTypeId") Long issueTypeId);

    List<Long> filterNotExistFields(@Param("fieldIds") List<Long> fieldIds);

    /**
     * 查询项目下自定义字段，不包含option值
     * @param organizationId  组织id
     * @param projectId 项目id
     * @param issueTypeList 问题类型列表
     * @return 项目下自定义字段
     */
    List<ObjectSchemeFieldDetailVO> selectCustomFieldListWithOutOption(
            @Param("organizationId") Long organizationId,
            @Param("projectId") Long projectId,
            @Param("issueTypeList") String issueTypeList);


    List<ObjectSchemeFieldDTO> listFieldWithExtendList(@Param("organizationId") Long organizationId,
                                                       @Param("projectId") Long projectId,
                                                       @Param("fieldIds") Set<Long> fieldIds,
                                                       @Param("issueTypeIds") Set<Long> issueTypeIds);

    List<ObjectSchemeFieldDTO> listByProjectIds(@Param("organizationId") Long organizationId,
                                                @Param("projectIds") List<Long> projectIds,
                                                @Param("searchVO") ObjectSchemeFieldSearchVO searchVO,
                                                @Param("issueTypes") List<String> issueTypes);
}
