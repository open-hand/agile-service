package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.apache.commons.lang3.StringUtils;
import org.hzero.boot.message.MessageClient;
import org.hzero.core.base.AopProxy;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author shinan.chen
 * @since 2019/4/8
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FieldValueServiceImpl implements FieldValueService, AopProxy<FieldValueService> {
    private static final String ERROR_PAGECODE_ILLEGAL = "error.pageCode.illegal";
    private static final String ERROR_CONTEXT_ILLEGAL = "error.context.illegal";
    protected static final String ERROR_SCHEMECODE_ILLEGAL = "error.schemeCode.illegal";
    private static final String ERROR_OPTION_ILLEGAL = "error.option.illegal";
    protected static final String ERROR_FIELDTYPE_ILLEGAL = "error.fieldType.illegal";
    private static final String ERROR_SYSTEM_ILLEGAL = "error.system.illegal";

    @Autowired
    protected FieldValueMapper fieldValueMapper;
    @Autowired
    private PageFieldService pageFieldService;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private FieldDataLogMapper fieldDataLogMapper;
    @Autowired
    private VerifyUpdateUtil verifyUpdateUtil;
    @Autowired
    private IssueService issueService;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private MessageClient messageClient;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;

    @Override
    public void fillValues(Long organizationId, Long projectId, Long instanceId, String schemeCode, List<PageFieldViewVO> pageFieldViews) {
        List<FieldValueDTO> values = fieldValueMapper.queryList(projectId, instanceId, schemeCode, null);
        Map<Long, UserDTO> userMap = FieldValueUtil.handleUserMap(values.stream().filter(x -> (x.getFieldType().equals(FieldType.MEMBER) || x.getFieldType().equals(FieldType.MULTI_MEMBER))).map(FieldValueDTO::getOptionId).collect(Collectors.toList()));
        Map<Long, List<FieldValueDTO>> valueGroup = values.stream().collect(Collectors.groupingBy(FieldValueDTO::getFieldId));
        pageFieldViews.forEach(view -> {
            List<FieldValueDTO> fieldValues = valueGroup.get(view.getFieldId());
            FieldValueUtil.handleDTO2Value(view, view.getFieldType(), fieldValues, userMap, false);
        });
    }

    @Override
    public void createFieldValues(Long organizationId, Long projectId, Long instanceId, String schemeCode, List<PageFieldViewCreateVO> createDTOs) {
        List<FieldValueDTO> fieldValues = this.self().validateFieldValueDTOS(organizationId, projectId, schemeCode, createDTOs);
        this.self().checkCreateCustomField(projectId, instanceId, schemeCode, fieldValues, createDTOs.stream().map(PageFieldViewCreateVO::getFieldCode).collect(Collectors.toList()));
    }

    @Override
    public List<FieldValueDTO> validateFieldValueDTOS(Long organizationId, Long projectId, String schemeCode,
                                                       List<PageFieldViewCreateVO> createDTOs) {
        if (!EnumUtil.contain(ObjectSchemeCode.class, schemeCode)) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        List<FieldValueDTO> fieldValues = new ArrayList<>();
        createDTOs.forEach(createDTO -> {
            List<FieldValueDTO> values = new ArrayList<>();
            FieldValueUtil.handleValue2DTO(values, createDTO.getFieldType(), createDTO.getValue());
            //校验
            ObjectSchemeFieldDTO field = objectSchemeFieldService.baseQueryById(organizationId, projectId, createDTO.getFieldId());
            if (field.getSystem()) {
                throw new CommonException(ERROR_SYSTEM_ILLEGAL);
            }
            values.forEach(value -> value.setFieldId(createDTO.getFieldId()));
            fieldValues.addAll(values);
        });
        return fieldValues;
    }

    @RuleNotice(event = RuleNoticeEvent.ISSUE_CREATED, fieldListName = "fieldList", idPosition = "arg")
    @Override
    public void checkCreateCustomField(Long projectId, Long id, String schemeCode, List<FieldValueDTO> fieldValues, List<String> fieldList) {
        if (!fieldValues.isEmpty()) {
            fieldValueMapper.batchInsert(projectId, id, schemeCode, fieldValues);
        }
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, instanceId = "instanceId", idPosition = "arg", fieldListName = "fieldCode")
    public List<FieldValueVO> updateFieldValue(Long organizationId, Long projectId, Long instanceId, Long fieldId, String schemeCode, PageFieldViewUpdateVO updateDTO, String fieldCode){
        return this.updateFieldValue(organizationId, projectId, instanceId, fieldId, schemeCode, updateDTO);
    }

    @Override
    public List<FieldValueVO> updateFieldValue(Long organizationId, Long projectId, Long instanceId, Long fieldId, String schemeCode, PageFieldViewUpdateVO updateDTO) {
        if (!EnumUtil.contain(ObjectSchemeCode.class, schemeCode)) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        if (!EnumUtil.contain(FieldType.class, updateDTO.getFieldType())) {
            throw new CommonException(ERROR_FIELDTYPE_ILLEGAL);
        }
        ObjectSchemeFieldDTO objectSchemeFieldDTO = objectSchemeFieldMapper.selectByPrimaryKey(fieldId);
        if (ObjectUtils.isEmpty(objectSchemeFieldDTO)) {
            throw new CommonException("error.field.not.exist");
        }
        //获取原fieldValue
        List<FieldValueDTO> oldFieldValues = fieldValueMapper.queryList(projectId, instanceId, schemeCode, fieldId);
        //删除原fieldValue
        if (!oldFieldValues.isEmpty()) {
            fieldValueMapper.deleteList(projectId, instanceId, schemeCode, fieldId);
        }
        //创建新fieldValue
        List<FieldValueDTO> newFieldValues = new ArrayList<>();
        FieldValueUtil.handleValue2DTO(newFieldValues, updateDTO.getFieldType(), updateDTO.getValue());
        newFieldValues.forEach(fieldValue -> fieldValue.setFieldId(fieldId));
        if (!newFieldValues.isEmpty()) {
            fieldValueMapper.batchInsert(projectId, instanceId, schemeCode, newFieldValues);
        }
        //处理字段日志
        FieldValueUtil.handleDataLog(organizationId, projectId, instanceId, fieldId, updateDTO.getFieldType(), schemeCode, oldFieldValues, newFieldValues);
        // 更新issue更新时间
        BaseFieldUtil.updateIssueLastUpdateInfo(instanceId, projectId);
        return modelMapper.map(fieldValueMapper.queryList(projectId, instanceId, schemeCode, fieldId), new TypeToken<List<FieldValueVO>>() {
        }.getType());
    }

    @Override
    public void deleteByOptionIds(Long fieldId, List<Long> optionIds) {
        if (!optionIds.isEmpty()) {
            for (Long optionId : optionIds) {
                if (optionId == null) {
                    throw new CommonException(ERROR_OPTION_ILLEGAL);
                }
            }
            fieldValueMapper.deleteByOptionIds(fieldId, optionIds);
        }
    }

    @Override
    public void deleteByFieldId(Long fieldId) {
        FieldValueDTO delete = new FieldValueDTO();
        delete.setFieldId(fieldId);
        fieldValueMapper.delete(delete);
    }

    @Override
    public void createFieldValuesWithQuickCreate(Long organizationId, Long projectId, Long instanceId, PageFieldViewParamVO paramDTO) {
        if (!EnumUtil.contain(PageCode.class, paramDTO.getPageCode())) {
            throw new CommonException(ERROR_PAGECODE_ILLEGAL);
        }
        if (!EnumUtil.contain(ObjectSchemeCode.class, paramDTO.getSchemeCode())) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        if (!EnumUtil.contain(ObjectSchemeFieldContext.class, paramDTO.getContext())) {
            throw new CommonException(ERROR_CONTEXT_ILLEGAL);
        }
        List<PageFieldDTO> pageFields = pageFieldService.queryPageField(organizationId, projectId, paramDTO.getPageCode(), paramDTO.getContext());
        //过滤掉不显示字段和系统字段
        pageFields = pageFields.stream().filter(PageFieldDTO::getDisplay).filter(x -> !x.getSystem()).collect(Collectors.toList());
        List<FieldValueDTO> fieldValues = new ArrayList<>();
        pageFields.forEach(create -> {
            List<FieldValueDTO> values = new ArrayList<>();
            //处理默认值
            FieldValueUtil.handleDefaultValue2DTO(values, create);
            values.forEach(value -> value.setFieldId(create.getFieldId()));
            fieldValues.addAll(values);
        });
        this.self().checkCreateCustomField(projectId, instanceId, paramDTO.getSchemeCode(), fieldValues, pageFields.stream().map(PageFieldDTO::getFieldCode).collect(Collectors.toList()));
    }

    @Override
    public List<Long> sortIssueIdsByFieldValue(Long organizationId, Long projectId, PageRequest pageRequest) {
        if (!ObjectUtils.isEmpty(pageRequest.getSort())) {
            Iterator<Sort.Order> iterator = pageRequest.getSort().iterator();
            String fieldCode = "";
            while (iterator.hasNext()) {
                Sort.Order order = iterator.next();
                fieldCode = order.getProperty();
            }
            ObjectSchemeFieldDTO objectSchemeField = objectSchemeFieldService.queryByFieldCode(organizationId, projectId, fieldCode);
            String fieldType = objectSchemeField.getFieldType();
            FieldValueUtil.handleAgileSortPageRequest(fieldCode, fieldType, pageRequest);
            return fieldValueMapper.sortIssueIdsByFieldValue(organizationId, projectId, objectSchemeField.getId(), PageableHelper.getSortSql(pageRequest.getSort()));
        } else {
            return new ArrayList<>();
        }
    }

    @Override
    public void handlerPredefinedFields(Long projectId,
                                        List<Long> issueIds,
                                        JSONObject predefinedFields,
                                        BatchUpdateFieldStatusVO batchUpdateFieldStatusVO,
                                        String appleType) {
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds, null);
        if (CollectionUtils.isEmpty(issueDTOS)) {
            throw new CommonException("error.issues.null");
        }
        List<VersionIssueRelVO> fixVersion = ObjectUtils.isEmpty(predefinedFields.get("fixVersion")) ? null : EncryptionUtils.jsonToList(predefinedFields.get("fixVersion"),VersionIssueRelVO.class);
        List<VersionIssueRelVO> influenceVersion = ObjectUtils.isEmpty(predefinedFields.get("influenceVersion")) ? null : EncryptionUtils.jsonToList(predefinedFields.get("influenceVersion"),VersionIssueRelVO.class);
        predefinedFields.remove("fixVersion");
        predefinedFields.remove("influenceVersion");
        Map<String,Object> programMap = new HashMap<>();
        if (agilePluginService != null) {
            agilePluginService.handlerProgramPredefinedFields(projectId,predefinedFields,programMap,appleType);
        }
        issueDTOS.forEach(v -> {
            IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
            List<String> fieldList = verifyUpdateUtil.verifyUpdateData(predefinedFields, issueUpdateVO);
            if (!"story".equals(v.getTypeCode())) {
                fieldList.remove(String.valueOf("storyPoints"));
                issueUpdateVO.setStoryPoints(null);
            }

            if ("story".equals(v.getTypeCode()) && agilePluginService != null) {
                agilePluginService.setFeatureId(issueUpdateVO,programMap,fieldList);
            }

            if ("issue_epic".equals(v.getTypeCode())) {
                fieldList.remove(String.valueOf("epicId"));
                issueUpdateVO.setEpicId(null);
            }

            if (!CollectionUtils.isEmpty(fixVersion)) {
                issueUpdateVO.setVersionType("fix");
                issueUpdateVO.setVersionIssueRelVOList(fixVersion);
            }
            // 获取传入的状态
            Long statusId = issueUpdateVO.getStatusId();
            if (!ObjectUtils.isEmpty(statusId)) {
                fieldList.remove("statusId");
                issueUpdateVO.setStatusId(null);
            }
            issueUpdateVO.setIssueId(v.getIssueId());
            issueUpdateVO.setObjectVersionNumber(v.getObjectVersionNumber());
            handlerEstimatedTime(v, issueUpdateVO, fieldList);
            IssueVO issueVO = issueService.updateIssue(projectId, issueUpdateVO, fieldList);
            if ("bug".equals(v.getTypeCode())) {
                IssueUpdateVO issueUpdateVO1 = new IssueUpdateVO();
                if (!CollectionUtils.isEmpty(influenceVersion)) {
                    issueUpdateVO1.setVersionType("influence");
                    issueUpdateVO1.setVersionIssueRelVOList(influenceVersion);
                }
                issueUpdateVO1.setIssueId(v.getIssueId());
                issueUpdateVO1.setObjectVersionNumber(issueVO.getObjectVersionNumber());
                issueService.updateIssue(projectId, issueUpdateVO1, new ArrayList<>());
            }
            // 修改issue的状态
            if (!ObjectUtils.isEmpty(statusId)) {
                List<TransformVO> transformVOS = projectConfigService.queryTransformsByProjectId(projectId, v.getStatusId(), v.getIssueId(), v.getIssueTypeId(), appleType);
                if (!CollectionUtils.isEmpty(transformVOS)) {
                    Map<Long, TransformVO> map = transformVOS.stream().collect(Collectors.toMap(TransformVO::getEndStatusId, Function.identity()));
                    TransformVO transformVO = map.get(statusId);
                    if (!ObjectUtils.isEmpty(transformVO)) {
                        issueService.updateIssueStatus(projectId, v.getIssueId(), transformVO.getId(), transformVO.getStatusVO().getObjectVersionNumber(), appleType);
                    }
                }
            }
            if (agilePluginService != null) {
                agilePluginService.handlerFeatureField(projectId,v,programMap);
            }
            batchUpdateFieldStatusVO.setProcess( batchUpdateFieldStatusVO.getProcess() + batchUpdateFieldStatusVO.getIncrementalValue());
            messageClient.sendByUserId(batchUpdateFieldStatusVO.getUserId(), batchUpdateFieldStatusVO.getKey(), JSON.toJSONString(batchUpdateFieldStatusVO));
        });
    }

    private void handlerEstimatedTime(IssueDTO issueDTO, IssueUpdateVO issueUpdateVO, List<String> fieldList) {
        Date estimatedStartTime = issueDTO.getEstimatedStartTime();
        Date estimatedEndTime = issueDTO.getEstimatedEndTime();
        if (fieldList.contains("estimatedStartTime")) {
            estimatedStartTime = issueUpdateVO.getEstimatedStartTime();
        }
        if (fieldList.contains("estimatedEndTime")) {
            estimatedEndTime = issueUpdateVO.getEstimatedEndTime();
        }
        if (ObjectUtils.isEmpty(estimatedStartTime) || ObjectUtils.isEmpty(estimatedEndTime)) {
            return;
        }
        if (estimatedStartTime.after(estimatedEndTime)) {
            fieldList.remove("estimatedEndTime");
            fieldList.remove("estimatedStartTime");
            issueUpdateVO.setEstimatedStartTime(null);
            issueUpdateVO.setEstimatedEndTime(null);
        }
    }

    @Override
    public void handlerCustomFields(Long projectId,
                                    List<PageFieldViewUpdateVO> customFields,
                                    String schemeCode,
                                    List<Long> issueIds,
                                    BatchUpdateFieldStatusVO batchUpdateFieldStatusVO,
                                    boolean sendMsg) {
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds, null);
        if (CollectionUtils.isEmpty(customFields)) {
            throw new CommonException("error.customFields.null");
        }
        // 判断这个字段哪些问题类型可以添加
        customFields.forEach(v -> {
            List<ObjectSchemeFieldExtendDTO> objectSchemeFieldExtendDTOS = objectSchemeFieldExtendMapper.selectExtendFields(ConvertUtil.getOrganizationId(projectId), v.getFieldId(), projectId);
            List<String> contexts = objectSchemeFieldExtendDTOS.stream().map(ObjectSchemeFieldExtendDTO::getIssueType).collect(Collectors.toList());
            List<Long> needAddIssueIds = issueDTOS.stream().filter(issueDTO -> contexts.contains(issueDTO.getTypeCode())).map(IssueDTO::getIssueId).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(needAddIssueIds)) {
                batchHandlerCustomFields(projectId, v, schemeCode, needAddIssueIds);
            }
            if (sendMsg) {
                batchUpdateFieldStatusVO.setProcess( batchUpdateFieldStatusVO.getProcess() + batchUpdateFieldStatusVO.getIncrementalValue());
                messageClient.sendByUserId(batchUpdateFieldStatusVO.getUserId(), batchUpdateFieldStatusVO.getKey(), JSON.toJSONString(batchUpdateFieldStatusVO));
            }
        });
    }

    @Override
    public void copyCustomFieldValue(Long projectId, IssueDetailDTO issueDetailDTO, Long newIssueId) {
        // 查询原来的值
        Long issueId = issueDetailDTO.getIssueId();
        List<FieldValueDTO> fieldValueDTOS = fieldValueMapper.queryListByInstanceIds(Arrays.asList(projectId), Arrays.asList(issueId), "agile_issue", null);
        if (!CollectionUtils.isEmpty(fieldValueDTOS)) {
            Map<Long, List<FieldValueDTO>> listMap = fieldValueDTOS.stream().collect(Collectors.groupingBy(FieldValueDTO::getFieldId));
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            List<PageFieldViewCreateVO> createDTOs = new ArrayList<>();
            handlerFieldValue(listMap, createDTOs);
            createFieldValues(organizationId, projectId, newIssueId, "agile_issue", createDTOs);
        }
    }

    private void handlerFieldValue(Map<Long, List<FieldValueDTO>> listMap, List<PageFieldViewCreateVO> createDTOs) {
        Set<Long> keySet = listMap.keySet();
        List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS = objectSchemeFieldMapper.selectByIds(StringUtils.join(keySet, ","));
        Map<Long, ObjectSchemeFieldDTO> schemeFieldDTOMap = objectSchemeFieldDTOS.stream().collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, Function.identity()));
        for (Map.Entry<Long, List<FieldValueDTO>> entry : listMap.entrySet()) {
            Long key = entry.getKey();
            ObjectSchemeFieldDTO objectSchemeFieldDTO = schemeFieldDTOMap.get(key);
            if (ObjectUtils.isEmpty(objectSchemeFieldDTO)) {
                continue;
            }
            if (Boolean.TRUE.equals(objectSchemeFieldDTO.getSystem())) {
                continue;
            }
            PageFieldViewCreateVO pageFieldViewCreateVO = new PageFieldViewCreateVO();
            pageFieldViewCreateVO.setFieldId(key);
            pageFieldViewCreateVO.setFieldCode(objectSchemeFieldDTO.getCode());
            pageFieldViewCreateVO.setFieldType(objectSchemeFieldDTO.getFieldType());
            pageFieldViewCreateVO.setValue(setFiledValue(objectSchemeFieldDTO, listMap));
            createDTOs.add(pageFieldViewCreateVO);
        }
    }

    private Object setFiledValue(ObjectSchemeFieldDTO objectSchemeFieldDTO, Map<Long, List<FieldValueDTO>> listMap) {
        Object value = null;
        switch (objectSchemeFieldDTO.getFieldType()) {
            case FieldType.CHECKBOX:
            case FieldType.MULTI_MEMBER:
            case FieldType.MULTIPLE:
                List<FieldValueDTO> fieldValueDTOS = listMap.get(objectSchemeFieldDTO.getId());
                List<String> values = new ArrayList<>();
                if (!CollectionUtils.isEmpty(fieldValueDTOS)) {
                    values.addAll(fieldValueDTOS.stream().map(v -> String.valueOf(v.getOptionId())).collect(Collectors.toList()));
                }
                value = values;
                break;
            case FieldType.MEMBER:
            case FieldType.SINGLE:
            case FieldType.RADIO:
                List<FieldValueDTO> singleFields = listMap.get(objectSchemeFieldDTO.getId());
                if (!CollectionUtils.isEmpty(singleFields)) {
                    value = singleFields.get(0).getOptionId();
                }
                break;
            case FieldType.DATE:
            case FieldType.DATETIME:
            case FieldType.TIME:
                List<FieldValueDTO> dateFields = listMap.get(objectSchemeFieldDTO.getId());
                if (!CollectionUtils.isEmpty(dateFields)) {
                    Date dateValue = dateFields.get(0).getDateValue();
                    DateFormat dfff = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                    if (dateValue != null) {
                        value = dfff.format(dateValue);
                    }
                }
                break;
            case FieldType.INPUT:
                List<FieldValueDTO> inputFields = listMap.get(objectSchemeFieldDTO.getId());
                if (!CollectionUtils.isEmpty(inputFields)) {
                    value = inputFields.get(0).getStringValue();
                }
                break;
            case FieldType.TEXT:
                List<FieldValueDTO> textFields = listMap.get(objectSchemeFieldDTO.getId());
                if (!CollectionUtils.isEmpty(textFields)) {
                    value = textFields.get(0).getTextValue();
                }
                break;
            case FieldType.NUMBER:
                List<FieldValueDTO> numberFields = listMap.get(objectSchemeFieldDTO.getId());
                if (!CollectionUtils.isEmpty(numberFields)) {
                    value = numberFields.get(0).getNumberValue();
                }
                break;
            default:
                break;
        }
        return value;
    }

    protected void batchHandlerCustomFields(Long projectId, PageFieldViewUpdateVO pageFieldViewUpdateVO, String schemeCode, List<Long> needAddIssueIds) {
        if (Boolean.FALSE.equals(EnumUtil.contain(FieldType.class, pageFieldViewUpdateVO.getFieldType()))) {
            throw new CommonException(ERROR_FIELDTYPE_ILLEGAL);
        }
        Long fieldId = pageFieldViewUpdateVO.getFieldId();
        //获取原fieldValue
        List<FieldValueDTO> oldFieldValues = fieldValueMapper.listByInstanceIdsAndFieldId(projectId, needAddIssueIds, schemeCode, fieldId);
        //删除原fieldValue
        Map<Long, List<FieldValueDTO>> oldFieldMap = new HashMap<>();
        if (!oldFieldValues.isEmpty()) {
            fieldValueMapper.deleteByInstanceIds(projectId, needAddIssueIds, schemeCode, fieldId);
            oldFieldMap.putAll(oldFieldValues.stream().collect(Collectors.groupingBy(FieldValueDTO::getInstanceId)));
        }
        List<FieldValueDTO> allFieldValue = new ArrayList<>();
        needAddIssueIds.forEach(issueId -> {
            //创建新fieldValue
            List<FieldValueDTO> newFieldValues = new ArrayList<>();
            FieldValueUtil.handleValue2DTO(newFieldValues, pageFieldViewUpdateVO.getFieldType(), pageFieldViewUpdateVO.getValue());
            newFieldValues.forEach(fieldValue -> {
                fieldValue.setFieldId(fieldId);
                fieldValue.setInstanceId(issueId);
                fieldValue.setFieldType(pageFieldViewUpdateVO.getFieldType());
            });
            allFieldValue.addAll(newFieldValues);
        });
        // 批量写入表中
        fieldValueMapper.batchInsertField(projectId, schemeCode, allFieldValue);
        // 批量生产日志
        Map<Long, List<FieldValueDTO>> newFieldMap = new HashMap<>();
        newFieldMap.putAll(allFieldValue.stream().collect(Collectors.groupingBy(FieldValueDTO::getInstanceId)));
        List<FieldDataLogCreateVO> list = new ArrayList<>();
        needAddIssueIds.forEach(v -> {
            List<FieldValueDTO> oldFiledList = oldFieldMap.get(v);
            List<FieldValueDTO> newFiledList = newFieldMap.get(v);
            list.addAll(FieldValueUtil.batchHandlerFiledLog(projectId, v, oldFiledList, newFiledList));
        });
        if (!CollectionUtils.isEmpty(list) && "agile_issue".equals(schemeCode)) {
            CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
            fieldDataLogMapper.batchInsert(projectId, schemeCode, list, customUserDetails.getUserId());
        }
    }
}
