package io.choerodon.agile.app.service;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.TriggerCarrierVO;
import io.choerodon.agile.infra.dto.FieldValueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;
import java.util.Map;

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

    List<FieldValueDTO> validateFieldValueDTOS(Long organizationId, Long projectId, String schemeCode,
                                               List<PageFieldViewCreateVO> createDTOs);

    void checkCreateCustomField(Long projectId, Long issueId, String schemeCode, List<FieldValueDTO> fieldValues, List<String> fieldList);

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

    List<FieldValueVO> updateFieldValue(Long organizationId, Long projectId, Long instanceId, Long fieldId, String schemeCode, PageFieldViewUpdateVO updateDTO, String fieldCode);

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
     * @param schemeCode
     * @return
     */
    List<Long> sortIssueIdsByFieldValue(Long organizationId, Long projectId, PageRequest pageRequest, String schemeCode);


    /**
     * 批量处理自定义字段的值
     * @param projectId
     * @param customFields
     * @param schemeCode
     * @param issueIds
     */
    void handlerCustomFields(Long projectId, List<PageFieldViewUpdateVO> customFields, String schemeCode, List<Long> issueIds,BatchUpdateFieldStatusVO batchUpdateFieldStatusVO, boolean sendMsg, Map<Long, TriggerCarrierVO> triggerCarrierMap);

    /**
     * 批量处理预定义字段值
     * @param projectId
     * @param issueIds
     * @param predefinedFields
     */
    void handlerPredefinedFields(Long projectId,
                                 List<Long> issueIds,
                                 JSONObject predefinedFields,
                                 BatchUpdateFieldStatusVO batchUpdateFieldStatusVO,
                                 String applyType,
                                 boolean sendMsg,
                                 Map<Long, TriggerCarrierVO> triggerCarrierMap);

    void copyCustomFieldValue(Long projectId, IssueDetailDTO issueDetailDTO, Long newIssueId, List<Long> customFieldIds, List<PageFieldViewCreateVO> copyRequireFields);

    void checkCreateCustomFieldWithoutRuleNotice(Long projectId, Long id, String schemeCode, List<FieldValueDTO> fieldValues, List<String> fieldList);

    void createFieldValuesWithoutRuleNotice(Long organizationId, Long projectId, Long instanceId, String schemeCode, List<PageFieldViewCreateVO> createDTOs);
}
