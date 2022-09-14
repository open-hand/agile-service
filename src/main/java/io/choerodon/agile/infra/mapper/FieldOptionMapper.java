package io.choerodon.agile.infra.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.infra.dto.FieldOptionDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
public interface FieldOptionMapper extends BaseMapper<FieldOptionDTO> {

    /**
     * 根据字段id获取字段选项
     *
     * @param organizationId organizationId
     * @param fieldId fieldId
     * @return result
     */
    List<FieldOptionDTO> selectByFieldId(@Param("organizationId") Long organizationId, @Param("fieldId") Long fieldId);

    /**
     * 根据字段id列表获取字段选项
     *
     * @param organizationId organizationId
     * @param fieldIds fieldIds
     * @return result
     */
    List<FieldOptionDTO> selectByFieldIds(@Param("organizationId") Long organizationId, @Param("fieldIds") List<Long> fieldIds);

    /**
     * 根据optionIds查询对象
     *
     * @param organizationId organizationId
     * @param optionIds optionIds
     * @return result
     */
    List<FieldOptionDTO> selectByOptionIds(@Param("organizationId") Long organizationId, @Param("optionIds") List<Long> optionIds);

    /**
     * 根据fieldId和value查询option
     *
     * @param organizationId 组织id
     * @param fieldId        field id
     * @param searchValue    搜索参数
     * @param selected       已选择的选项
     * @param enabled        是否启用
     * @return option
     */
    List<FieldOptionDTO> selectByFieldIdAndValue(@Param("organizationId") Long organizationId, @Param("fieldId") Long fieldId, @Param("searchValue") String searchValue, @Param("selected") List<Long> selected, @Param("enabled") Boolean enabled);

    /**
     * 获取通过code或value获取选项
     *
     * @param organizationId 组织id
     * @param fieldId        字段id
     * @param code           code
     * @param value          value
     * @return 选项
     */
    List<FieldOptionDTO> selectByCodeOrValue(@Param("organizationId") Long organizationId, @Param("fieldId") Long fieldId, @Param("code") String code, @Param("value") String value);

    /**
     * 选项sequence-1
     * @param start 开始减1的顺序
     * @param end   结束减1的顺序
     * @param fieldId 字段id
     * @param organizationId 组织id
     */
    void sequenceDecrement(@Param("start") int start, @Param("end") int end, @Param("fieldId") Long fieldId, @Param("organizationId") Long organizationId);

    /**
     * 选项sequence+1
     * @param start 开始加1的顺序
     * @param end 开始加1的顺序
     * @param fieldId 字段id
     * @param organizationId 组织id
     */
    void sequenceIncrement(@Param("start") int start, @Param("end") int end, @Param("fieldId") Long fieldId, @Param("organizationId") Long organizationId);
}
