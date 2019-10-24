package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.base.domain.PageRequest;

import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/4/8
 */
public interface FieldValueService {
    /**
     * 填充字段值
     *
     * @param organizationId
     * @param projectId
     * @param instanceId
     * @param schemeCode
     * @param pageFieldViews
     */
    void fillValues(Long organizationId, Long projectId, Long instanceId, String schemeCode, List<PageFieldViewVO> pageFieldViews);

    /**
     * 创建实例时，批量创建值
     *
     * @param organizationId
     * @param projectId
     * @param instanceId
     * @param schemeCode
     * @param createDTOs
     */
    void createFieldValues(Long organizationId, Long projectId, Long instanceId, String schemeCode, List<PageFieldViewCreateVO> createDTOs);

    /**
     * 保存值/修改值
     *
     * @param organizationId
     * @param projectId
     * @param instanceId
     * @param fieldId
     * @param schemeCode
     * @param updateDTO
     * @return
     */
    List<FieldValueVO> updateFieldValue(Long organizationId, Long projectId, Long instanceId, Long fieldId, String schemeCode, PageFieldViewUpdateVO updateDTO);

    /**
     * 根据optionIds删除值
     *
     * @param fieldId
     * @param optionIds
     */
    void deleteByOptionIds(Long fieldId, List<Long> optionIds);

    /**
     * 删除字段相关值
     *
     * @param fieldId
     */
    void deleteByFieldId(Long fieldId);

    /**
     * 快速创建实例时，批量创建字段值（默认值）
     *
     * @param organizationId
     * @param projectId
     * @param instanceId
     * @param paramDTO
     */
    void createFieldValuesWithQuickCreate(Long organizationId, Long projectId, Long instanceId, PageFieldViewParamVO paramDTO);

    /**
     * 获取instanceIds，根据指定自定义字段进行排序
     *
     * @param organizationId
     * @param projectId
     * @param pageRequest
     * @return
     */
    List<Long> sortIssueIdsByFieldValue(Long organizationId, Long projectId, PageRequest pageRequest);
}
