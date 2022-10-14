package io.choerodon.agile.app.service;

import java.util.List;
import java.util.Map;

import com.alibaba.fastjson.JSONObject;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.TriggerCarrierVO;
import io.choerodon.agile.infra.dto.FieldValueDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author shinan.chen
 * @since 2019/4/8
 */
public interface FieldValueService {
    /**
     * 填充字段值
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param instanceId instanceId
     * @param schemeCode schemeCode
     * @param pageFieldViews pageFieldViews
     */
    void fillValues(Long organizationId, Long projectId, Long instanceId, String schemeCode, List<PageFieldViewVO> pageFieldViews);

    /**
     * 创建实例时，批量创建值
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param instanceId instanceId
     * @param schemeCode schemeCode
     * @param createDTOs createDTOs
     */
    void createFieldValues(Long organizationId, Long projectId, Long instanceId, String schemeCode, List<PageFieldViewCreateVO> createDTOs);

    List<FieldValueDTO> validateFieldValueDTOS(Long organizationId, Long projectId, String schemeCode,
                                               List<PageFieldViewCreateVO> createDTOs);

    void checkCreateCustomField(Long projectId, Long issueId, String schemeCode, List<FieldValueDTO> fieldValues, List<String> fieldList);

    /**
     * 保存值/修改值
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param instanceId instanceId
     * @param fieldId fieldId
     * @param schemeCode schemeCode
     * @param updateDTO updateDTO
     * @return result
     */
    List<FieldValueVO> updateFieldValue(Long organizationId, Long projectId, Long instanceId, Long fieldId, String schemeCode, PageFieldViewUpdateVO updateDTO);

    List<FieldValueVO> updateFieldValue(Long organizationId, Long projectId, Long instanceId, Long fieldId, String schemeCode, PageFieldViewUpdateVO updateDTO, String fieldCode);

    /**
     * 根据optionIds删除值
     *
     * @param fieldId fieldId
     * @param optionIds optionIds
     */
    void deleteByOptionIds(Long fieldId, List<Long> optionIds);

    /**
     * 删除字段相关值
     *
     * @param fieldId fieldId
     */
    void deleteByFieldId(Long fieldId);

    /**
     * 快速创建实例时，批量创建字段值（默认值）
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param instanceId instanceId
     * @param paramDTO paramDTO
     */
    void createFieldValuesWithQuickCreate(Long organizationId, Long projectId, Long instanceId, PageFieldViewParamVO paramDTO);

    /**
     * 获取instanceIds，根据指定自定义字段进行排序
     *
     * @param organizationId organizationId
     * @param projectId projectId
     * @param pageRequest pageRequest
     * @param schemeCode schemeCode
     * @return result
     */
    List<Long> sortIssueIdsByFieldValue(Long organizationId, Long projectId, PageRequest pageRequest, String schemeCode);


    /**
     * 批量处理自定义字段的值
     * @param projectId projectId
     * @param customFields customFields
     * @param schemeCode schemeCode
     * @param issueIds issueIds
     */
    void handlerCustomFields(Long projectId, List<PageFieldViewUpdateVO> customFields, String schemeCode, List<Long> issueIds,BatchUpdateFieldStatusVO batchUpdateFieldStatusVO, boolean sendMsg, Map<Long, TriggerCarrierVO> triggerCarrierMap);

    /**
     * 批量处理预定义字段值
     * @param projectId projectId
     * @param issueIds issueIds
     * @param predefinedFields predefinedFields
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

    void handlerIssueFields(Long projectId,
                            List<Long> issueIds,
                            JSONObject predefinedFields,
                            BatchUpdateFieldStatusVO batchUpdateFieldStatusVO,
                            String applyType,
                            boolean sendMsg,
                            String schemeCode,
                            List<PageFieldViewUpdateVO> customFields,
                            Map<Long, TriggerCarrierVO> triggerCarrierMap);

    void handleIssueField(Long projectId,
                          IssueDTO issueDTO,
                          JSONObject predefinedFields,
                          BatchUpdateFieldStatusVO batchUpdateFieldStatusVO,
                          String applyType,
                          boolean sendMsg,
                          String schemeCode,
                          Map<Long, List<PageFieldViewUpdateVO>> issueCustomFieldMap,
                          Map<Long, TriggerCarrierVO> triggerCarrierMap,
                          Map<String, Object> programMap,
                          List<VersionIssueRelVO> fixVersion,
                          List<VersionIssueRelVO> influenceVersion);
}
