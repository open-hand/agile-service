package io.choerodon.agile.app.service.impl;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.validator.IssueValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.business.TriggerCarrierVO;
import io.choerodon.agile.app.assembler.IssueAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;

import org.hzero.core.base.AopProxy;
import org.hzero.core.base.BaseConstants;
import org.hzero.websocket.helper.SocketSendHelper;

/**
 * @author shinan.chen
 * @since 2019/4/8
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FieldValueServiceImpl implements FieldValueService, AopProxy<FieldValueService> {
    private static final Logger LOGGER = LoggerFactory.getLogger(FieldValueServiceImpl.class);
    private static final String ERROR_PAGECODE_ILLEGAL = "error.pageCode.illegal";
    private static final String ERROR_CONTEXT_ILLEGAL = "error.context.illegal";
    protected static final String ERROR_SCHEMECODE_ILLEGAL = "error.schemeCode.illegal";
    private static final String ERROR_OPTION_ILLEGAL = "error.option.illegal";
    protected static final String ERROR_FIELDTYPE_ILLEGAL = "error.fieldType.illegal";
    private static final String ERROR_SYSTEM_ILLEGAL = "error.system.illegal";

    private static final String EPIC_ID = "epicId";
    private static final String FIX_VERSION = "fixVersion";
    private static final String INFLUENCE_VERSION = "influenceVersion";
    private static final String SCHEME_CODE = "agile_issue";

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
    private SocketSendHelper socketSendHelper;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    @Autowired
    private IssueAssembler issueAssembler;
    @Autowired
    private IssueValidator issueValidator;


    @Override
    public void fillValues(Long organizationId, Long projectId, Long instanceId, String schemeCode, List<PageFieldViewVO> pageFieldViews) {
        List<FieldValueDTO> values = fieldValueMapper.queryList(projectId, instanceId, schemeCode, null);
        Map<Long, UserDTO> userMap =
                FieldValueUtil.handleUserMap(
                        values
                                .stream()
                                .filter(x -> (FieldType.MEMBER.equals(x.getFieldType()) || FieldType.MULTI_MEMBER.equals(x.getFieldType())))
                                .map(FieldValueDTO::getOptionId)
                                .collect(Collectors.toList()));
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
    public void createFieldValuesWithoutRuleNotice(Long organizationId, Long projectId, Long instanceId, String schemeCode, List<PageFieldViewCreateVO> createDTOs) {
        List<FieldValueDTO> fieldValues = this.self().validateFieldValueDTOS(organizationId, projectId, schemeCode, createDTOs);
        this.self().checkCreateCustomFieldWithoutRuleNotice(projectId, instanceId, schemeCode, fieldValues, createDTOs.stream().map(PageFieldViewCreateVO::getFieldCode).collect(Collectors.toList()));
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
            if (Boolean.TRUE.equals(field.getSystem())) {
                throw new CommonException(ERROR_SYSTEM_ILLEGAL);
            }
            values.forEach(value -> value.setFieldId(createDTO.getFieldId()));
            fieldValues.addAll(values);
        });
        return fieldValues;
    }

    @RuleNotice(event = RuleNoticeEvent.ISSUE_CREATED, instanceId = "issueId", idPosition = "arg", allFieldCheck = true)
    @Override
    public void checkCreateCustomField(Long projectId, Long issueId, String schemeCode, List<FieldValueDTO> fieldValues, List<String> fieldList) {
        checkCreateCustomFieldWithoutRuleNotice(projectId, issueId, schemeCode, fieldValues,fieldList);
    }

    @Override
    public void checkCreateCustomFieldWithoutRuleNotice(Long projectId, Long id, String schemeCode, List<FieldValueDTO> fieldValues, List<String> fieldList) {
        //处理数字类型自定义字段默认值和状态联动更新属性自定义字段add操作结果不正确的问题
        List<FieldValueDTO> fieldValueList = calcNumberByDefaultValue(projectId, id, schemeCode, fieldValues, fieldList);
        if (!fieldValueList.isEmpty()) {
            fieldValueMapper.batchInsert(projectId, id, schemeCode, fieldValueList);
        }
        //创建问题通知自定义字段人员
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, id);
        IssueVO result = issueAssembler.issueDetailDTOToVO(issue, new HashMap<>(), new HashMap<>(), new HashMap<>());
        sendMsgUtil.sendMsgToCustomFieldUsersByIssueCreate(projectId, result, DetailsHelper.getUserDetails().getUserId());
    }

    private List<FieldValueDTO> calcNumberByDefaultValue(Long projectId,
                                                         Long instanceId,
                                                         String schemeCode,
                                                         List<FieldValueDTO> fieldValues,
                                                         List<String> inputFieldCodes) {
        Long issueTypeId = null;
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        if (ObjectSchemeCode.BACKLOG.equals(schemeCode) && !ObjectUtils.isEmpty(backlogExpandService)) {
            IssueTypeDTO dto = new IssueTypeDTO();
            dto.setOrganizationId(organizationId);
            dto.setTypeCode(IssueTypeCode.BACKLOG.value());
            IssueTypeDTO backlogType = issueTypeMapper.selectOne(dto);
            if (!ObjectUtils.isEmpty(backlogType)) {
                issueTypeId = backlogType.getId();
            }
        } else {
            IssueDTO issue = issueMapper.selectByPrimaryKey(instanceId);
            if (ObjectUtils.isEmpty(issue)) {
                throw new CommonException("error.issue.type.not.existed");
            }
            issueTypeId = issue.getIssueTypeId();
        }
        //数字类型字段，没有权限，需要在后台设置默认值，保证有权限人员看到的值是正确的
        addNoPermissionNumberFields(projectId, issueTypeId, fieldValues, inputFieldCodes);
        List<FieldValueDTO> result = new ArrayList<>();
        List<FieldValueDTO> numberFields = new ArrayList<>();
        Set<Long> fieldIds = new HashSet<>();
        for (FieldValueDTO fieldValue : fieldValues) {
            String numberValue = fieldValue.getNumberValue();
            Long fieldId = fieldValue.getFieldId();
            if (!ObjectUtils.isEmpty(numberValue)) {
                numberFields.add(fieldValue);
                fieldIds.add(fieldId);
            } else {
                result.add(fieldValue);
            }
        }
        if (!fieldIds.isEmpty()) {
            Map<Long, ObjectSchemeFieldDTO> fieldMap =
                    objectSchemeFieldMapper.selectByIds(StringUtils.join(fieldIds, ","))
                            .stream()
                            .collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, Function.identity()));
            if (!ObjectUtils.isEmpty(issueTypeId)) {
                Map<Long, String> defaultValueMap =
                        queryFieldDefaultMap(Arrays.asList(issueTypeId), organizationId, fieldIds, projectId);
                for (FieldValueDTO dto : numberFields) {
                    Long fieldId = dto.getFieldId();
                    String defaultValue = defaultValueMap.get(fieldId);
                    if (ObjectUtils.isEmpty(defaultValue)) {
                        //创建时暂无默认值，插入
                        result.add(dto);
                    } else {
                        //计算状态联动增加值和默认值的逻辑
                        BigDecimal defaultNumberValue = new BigDecimal(defaultValue);
                        FieldValueDTO fieldValue = new FieldValueDTO();
                        fieldValue.setInstanceId(instanceId);
                        fieldValue.setFieldId(fieldId);
                        fieldValue.setProjectId(projectId);
                        fieldValue.setSchemeCode(schemeCode);
                        FieldValueDTO statusLinkedValue = fieldValueMapper.selectOne(fieldValue);
                        if (!ObjectUtils.isEmpty(statusLinkedValue)) {
                            String oldValue = statusLinkedValue.getNumberValue();
                            BigDecimal old = new BigDecimal(oldValue);
                            BigDecimal resultValue = old.add(defaultNumberValue);
                            ObjectSchemeFieldDTO field = fieldMap.get(fieldId);
                            PageFieldViewUpdateVO pageFieldViewUpdateVO = new PageFieldViewUpdateVO();
                            pageFieldViewUpdateVO.setFieldType(field.getFieldType());
                            pageFieldViewUpdateVO.setFieldId(fieldId);
                            pageFieldViewUpdateVO.setFieldCode(field.getCode());
                            pageFieldViewUpdateVO.setValue(resultValue.toString());
                            updateFieldValue(organizationId, projectId, instanceId, fieldId, schemeCode, pageFieldViewUpdateVO);
                        } else {
                            //有默认值，但没有执行状态联动逻辑，插入
                            result.add(dto);
                        }
                    }
                }
            }
        }
        return result;
    }

    private void addNoPermissionNumberFields(Long projectId,
                                             Long issueTypeId,
                                             List<FieldValueDTO> fieldValues,
                                             List<String> inputFieldCodes) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<PageFieldDTO> pageFields =
                pageFieldService.queryPageField(organizationId, projectId, PageCode.AGILE_ISSUE_CREATE, issueTypeId);
        Set<Long> fieldIds = new HashSet<>();
        Set<String> fieldCodes = new HashSet<>();
        for (FieldValueDTO dto : fieldValues) {
            fieldIds.add(dto.getFieldId());
            fieldCodes.add(dto.getFieldCode());
        }
        Set<String> nullValueFieldCodes = new HashSet<>();
        for (String inputFieldCode : inputFieldCodes) {
            if (!fieldCodes.contains(inputFieldCode)) {
                nullValueFieldCodes.add(inputFieldCode);
            }
        }
        for (PageFieldDTO field : pageFields) {
            Long fieldId = field.getFieldId();
            String fieldCode = field.getFieldCode();
            String fieldType = field.getFieldType();
            boolean isNumber = FieldType.NUMBER.equals(fieldType);
            boolean isSkipped = fieldIds.contains(fieldId)
                    || !Boolean.TRUE.equals(field.getDisplay())
                    || Boolean.TRUE.equals(field.getSystem())
                    || !isNumber
                    || nullValueFieldCodes.contains(fieldCode);
            if (isSkipped) {
                continue;
            }
            List<FieldValueDTO> values = new ArrayList<>();
            FieldValueUtil.handleDefaultValue2DTO(values, field);
            for (FieldValueDTO fieldValue : values) {
                fieldValue.setFieldId(fieldId);
            }
            fieldValues.addAll(values);
        }
    }

    private Map<Long, String> queryFieldDefaultMap(List<Long> issueTypeIds,
                                                   Long organizationId,
                                                   Set<Long> fieldIds,
                                                   Long projectId) {
        Map<Long, String> map = new HashMap<>();
        List<ObjectSchemeFieldExtendDTO> fieldExtendList =
                objectSchemeFieldExtendMapper.selectExtendFieldsByOptions(issueTypeIds, organizationId, fieldIds, null);
        Map<Long, List<ObjectSchemeFieldExtendDTO>> fieldMap =
                fieldExtendList.stream().collect(Collectors.groupingBy(ObjectSchemeFieldExtendDTO::getFieldId));
        for (Long fieldId : fieldIds) {
            List<ObjectSchemeFieldExtendDTO> objectSchemeFieldExtends = fieldMap.get(fieldId);
            Map<Long, String> projectFieldMap = new HashMap<>();
            if (!ObjectUtils.isEmpty(objectSchemeFieldExtends)) {
                for (ObjectSchemeFieldExtendDTO dto : objectSchemeFieldExtends) {
                    Long thisProjectId = dto.getProjectId();
                    if (ObjectUtils.isEmpty(thisProjectId)) {
                        thisProjectId = 0L;
                    }
                    projectFieldMap.put(thisProjectId, dto.getDefaultValue());
                }
            }
            if (ObjectUtils.isEmpty(projectId)) {
                projectId = 0L;
            }
            String defaultValue = projectFieldMap.get(projectId);
            if (ObjectUtils.isEmpty(defaultValue)) {
                defaultValue = projectFieldMap.get(0L);
            }
            map.put(fieldId, defaultValue);
        }
        return map;
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
        // 新值旧值比较
        if (FieldValueUtil.compareNewFieldValueWithOld(oldFieldValues, updateDTO.getFieldType(), updateDTO.getValue())) {
            return modelMapper.map(oldFieldValues, new TypeToken<List<FieldValueVO>>() {}.getType());
        }
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
        List<IssueTypeVO> issueTypes = this.issueValidator.checkIssueTypeExists(organizationId, projectId, Collections.singletonList(paramDTO.getIssueTypeId()), true);
        IssueTypeVO issueType = issueTypes.get(0);
        String typeCode = issueType.getTypeCode();
        Long issueTypeId = issueType.getId();

        if (!EnumUtil.contain(PageCode.class, paramDTO.getPageCode())) {
            throw new CommonException(ERROR_PAGECODE_ILLEGAL);
        }
        if (!EnumUtil.contain(ObjectSchemeCode.class, paramDTO.getSchemeCode())) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        if (!EnumUtil.contain(ObjectSchemeFieldContext.class, typeCode)) {
            throw new CommonException(ERROR_CONTEXT_ILLEGAL);
        }
        List<PageFieldDTO> pageFields = pageFieldService.queryPageField(organizationId, projectId, paramDTO.getPageCode(), issueTypeId);
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
    public List<Long> sortIssueIdsByFieldValue(Long organizationId, Long projectId, PageRequest pageRequest, String schemeCode) {
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
            return fieldValueMapper.sortIssueIdsByFieldValue(organizationId, projectId, objectSchemeField.getId(), PageableHelper.getSortSql(pageRequest.getSort()), schemeCode);
        } else {
            return new ArrayList<>();
        }
    }

    @Override
    public void handlerPredefinedFields(Long projectId,
                                        List<Long> issueIds,
                                        JSONObject predefinedFields,
                                        BatchUpdateFieldStatusVO batchUpdateFieldStatusVO,
                                        String appleType,
                                        boolean sendMsg,
                                        Map<Long, TriggerCarrierVO> triggerCarrierMap) {
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds, null);
        if (CollectionUtils.isEmpty(issueDTOS)) {
            throw new CommonException("error.issues.null");
        }
        //史诗校验
        checkEpic(projectId, predefinedFields);
        List<VersionIssueRelVO> fixVersion = buildVersionData(predefinedFields.get(FIX_VERSION));
        List<VersionIssueRelVO> influenceVersion = buildVersionData(predefinedFields.get(INFLUENCE_VERSION));
        predefinedFields.remove(FIX_VERSION);
        predefinedFields.remove(INFLUENCE_VERSION);
        Map<String,Object> programMap = new HashMap<>();
        if (agilePluginService != null) {
            agilePluginService.handlerProgramPredefinedFields(projectId,predefinedFields,programMap,appleType);
        }
        issueDTOS.forEach(v -> {
            TriggerCarrierVO triggerCarrierVO = triggerCarrierMap.getOrDefault(v.getIssueId(), null);
            if (ObjectUtils.isEmpty(triggerCarrierVO)) {
                triggerCarrierVO = buildTriggerCarrierVO(v, projectId);
            }
            IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
            List<String> fieldList = verifyUpdateUtil.verifyUpdateData(predefinedFields, issueUpdateVO);
            fieldListRemove(v, fieldList, issueUpdateVO, programMap);
            if (fixVersion != null) {
                issueUpdateVO.setVersionType(ProductVersionService.VERSION_RELATION_TYPE_FIX);
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
            boolean doCheck = IssueTypeCode.FEATURE.value().equals(v.getTypeCode()) && fieldList.contains(EPIC_ID);
            if (doCheck
                    && !ObjectUtils.isEmpty(agilePluginService)
                    && agilePluginService.checkFeatureSummaryAndReturn(issueUpdateVO, projectId)) {
                fieldList.remove(EPIC_ID);
                issueUpdateVO.setEpicId(null);
                addErrMessage(v, batchUpdateFieldStatusVO, EPIC_ID);
            }
            IssueVO issueVO = issueService.updateIssueWithoutRuleNotice(projectId, issueUpdateVO, fieldList);
            // 处理影响的版本
            if(influenceVersion != null){
                fieldList.add("versionId");
                handlerBugInfluenceVersion(projectId,v, influenceVersion, issueVO);
            }
            // 修改issue的状态
            if(!ObjectUtils.isEmpty(statusId)){
                fieldList.add("statusId");
                updateIssueStatus(projectId, statusId, v, appleType);
            }
            if (agilePluginService != null) {
                agilePluginService.handlerFeatureField(projectId,v,programMap, triggerCarrierVO);
            }
            issueService.addCollectionFieldIfNotNull(issueUpdateVO, fieldList);
            triggerCarrierVO.getFieldList().addAll(fieldList);
            triggerCarrierMap.put(v.getIssueId(), triggerCarrierVO);
            if (sendMsg) {
                batchUpdateFieldStatusVO.setProcess(batchUpdateFieldStatusVO.getProcess() + batchUpdateFieldStatusVO.getIncrementalValue());
                if (batchUpdateFieldStatusVO.getProcess() - batchUpdateFieldStatusVO.getLastProcess() >= 0.1) {
                    socketSendHelper.sendByUserId(batchUpdateFieldStatusVO.getUserId(), batchUpdateFieldStatusVO.getKey(), JSON.toJSONString(batchUpdateFieldStatusVO));
                    batchUpdateFieldStatusVO.setLastProcess(batchUpdateFieldStatusVO.getProcess());
                }
            }
        });
    }

    @Override
    public void handlerIssueFields(Long projectId,
                                   List<Long> issueIds,
                                   JSONObject predefinedFields,
                                   BatchUpdateFieldStatusVO batchUpdateFieldStatusVO,
                                   String applyType,
                                   boolean sendMsg,
                                   String schemeCode,
                                   List<PageFieldViewUpdateVO> customFields,
                                   Map<Long, TriggerCarrierVO> triggerCarrierMap) {
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds, null);
        if (CollectionUtils.isEmpty(issueDTOS)) {
            throw new CommonException("error.issues.null");
        }
        Map<Long, List<PageFieldViewUpdateVO>> issueCustomFieldMap = buildIssueCustomFieldMap(projectId, customFields, issueDTOS);
        //史诗校验
        checkEpic(projectId, predefinedFields);
        List<VersionIssueRelVO> fixVersion = buildVersionData(predefinedFields.get(FIX_VERSION));
        List<VersionIssueRelVO> influenceVersion = buildVersionData(predefinedFields.get(INFLUENCE_VERSION));
        predefinedFields.remove(FIX_VERSION);
        predefinedFields.remove(INFLUENCE_VERSION);
        Map<String, Object> programMap = new HashMap<>();
        if (agilePluginService != null) {
            agilePluginService.handlerProgramPredefinedFields(projectId, predefinedFields, programMap, applyType);
        }
        for (IssueDTO issue : issueDTOS) {
            try {
                this.self().handleIssueField(projectId, issue, predefinedFields, batchUpdateFieldStatusVO, applyType, sendMsg, schemeCode, issueCustomFieldMap, triggerCarrierMap, programMap, fixVersion, influenceVersion);
            } catch (Exception e) {
                LOGGER.info("update issue exception:", e);
                int failedCount = ObjectUtils.isEmpty(batchUpdateFieldStatusVO.getFailedCount()) ? 0 : batchUpdateFieldStatusVO.getFailedCount();
                batchUpdateFieldStatusVO.setFailedCount(failedCount++);
            }
        }
    }

    @Override
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void handleIssueField(Long projectId,
                                 IssueDTO issueDTO,
                                 JSONObject predefinedFields,
                                 BatchUpdateFieldStatusVO batchUpdateFieldStatusVO,
                                 String applyType,
                                 boolean sendMsg,
                                 String schemeCode,
                                 Map<Long, List<PageFieldViewUpdateVO>> issueCustomFieldMap,
                                 Map<Long, TriggerCarrierVO> triggerCarrierMap,
                                 Map<String,Object> programMap,
                                 List<VersionIssueRelVO> fixVersion,
                                 List<VersionIssueRelVO> influenceVersion){
        TriggerCarrierVO triggerCarrierVO = triggerCarrierMap.getOrDefault(issueDTO.getIssueId(), null);
        if (ObjectUtils.isEmpty(triggerCarrierVO)) {
            triggerCarrierVO = buildTriggerCarrierVO(issueDTO, projectId);
        }
        IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
        List<String> fieldList = verifyUpdateUtil.verifyUpdateData(predefinedFields, issueUpdateVO);
        fieldListRemove(issueDTO, fieldList, issueUpdateVO, programMap);
        if (fixVersion != null) {
            issueUpdateVO.setVersionType(ProductVersionService.VERSION_RELATION_TYPE_FIX);
            issueUpdateVO.setVersionIssueRelVOList(fixVersion);
        }
        // 获取传入的状态
        Long statusId = issueUpdateVO.getStatusId();
        if (!ObjectUtils.isEmpty(statusId)) {
            fieldList.remove("statusId");
            issueUpdateVO.setStatusId(null);
        }
        issueUpdateVO.setIssueId(issueDTO.getIssueId());
        issueUpdateVO.setObjectVersionNumber(issueDTO.getObjectVersionNumber());
        handlerEstimatedTime(issueDTO, issueUpdateVO, fieldList);
        boolean doCheck = IssueTypeCode.FEATURE.value().equals(issueDTO.getTypeCode()) && fieldList.contains(EPIC_ID);
        if (doCheck && agilePluginService.checkFeatureSummaryAndReturn(issueUpdateVO, projectId)) {
            fieldList.remove(EPIC_ID);
            issueUpdateVO.setEpicId(null);
            addErrMessage(issueDTO, batchUpdateFieldStatusVO, EPIC_ID);
        }
        // 子任务跳过设置史诗, 但是允许清空史诗
        if(
                IssueTypeCode.SUB_TASK.value().equals(issueDTO.getTypeCode()) &&
                        (issueUpdateVO.getEpicId() != null &&
                                !Objects.equals(BaseConstants.DEFAULT_TENANT_ID, issueUpdateVO.getEpicId())
                        )
        ) {
            fieldList.remove(EPIC_ID);
            issueUpdateVO.setEpicId(null);
        }
        IssueVO issueVO = issueService.updateIssueWithoutRuleNotice(projectId, issueUpdateVO, fieldList);
        // 处理影响的版本
        if (influenceVersion != null) {
            fieldList.add("versionId");
            handlerBugInfluenceVersion(projectId, issueDTO, influenceVersion, issueVO);
        }
        // 修改issue的状态
        if (!ObjectUtils.isEmpty(statusId)) {
            fieldList.add("statusId");
            updateIssueStatus(projectId, statusId, issueDTO, applyType);
        }
        if (agilePluginService != null) {
            agilePluginService.handlerFeatureField(projectId, issueDTO, programMap, triggerCarrierVO);
        }
        issueService.addCollectionFieldIfNotNull(issueUpdateVO, fieldList);
        List<PageFieldViewUpdateVO> pageFieldViewUpdateVOS = issueCustomFieldMap.get(issueDTO.getIssueId());
        if (!CollectionUtils.isEmpty(pageFieldViewUpdateVOS)) {
            for (PageFieldViewUpdateVO pageFieldViewUpdateVO : pageFieldViewUpdateVOS) {
                batchHandlerCustomFields(projectId, pageFieldViewUpdateVO, schemeCode, new ArrayList<>(Arrays.asList(issueDTO.getIssueId())));
                if (ObjectUtils.isEmpty(triggerCarrierVO)) {
                    triggerCarrierVO = buildTriggerCarrierVO(issueDTO, projectId);
                }
                triggerCarrierVO.getFieldList().add(pageFieldViewUpdateVO.getFieldCode());
            }
        }
        triggerCarrierVO.getFieldList().addAll(fieldList);
        triggerCarrierMap.put(issueDTO.getIssueId(), triggerCarrierVO);
        if (sendMsg) {
            batchUpdateFieldStatusVO.setProcess(batchUpdateFieldStatusVO.getProcess() + batchUpdateFieldStatusVO.getIncrementalValue());
            if (batchUpdateFieldStatusVO.getProcess() - batchUpdateFieldStatusVO.getLastProcess() >= 0.1) {
                socketSendHelper.sendByUserId(batchUpdateFieldStatusVO.getUserId(), batchUpdateFieldStatusVO.getKey(), JSON.toJSONString(batchUpdateFieldStatusVO));
                batchUpdateFieldStatusVO.setLastProcess(batchUpdateFieldStatusVO.getProcess());
            }
        }
    }

    private Map<Long, List<PageFieldViewUpdateVO>> buildIssueCustomFieldMap(Long projectId, List<PageFieldViewUpdateVO> customFields, List<IssueDTO> issueDTOS) {
        Map<Long, List<PageFieldViewUpdateVO>> map = new HashMap<>();
        // 根据issueTypeId判断这个字段哪些问题类型可以添加
        customFields.forEach(v -> {
            List<ObjectSchemeFieldExtendDTO> objectSchemeFieldExtendDTOS = objectSchemeFieldExtendMapper.selectExtendFields(ConvertUtil.getOrganizationId(projectId), v.getFieldId(), projectId, null);
            List<Long> issueTypeIds = objectSchemeFieldExtendDTOS.stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toList());
            List<Long> needAddIssueIds = issueDTOS.stream().filter(issueDTO -> issueTypeIds.contains(issueDTO.getIssueTypeId())).map(IssueDTO::getIssueId).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(needAddIssueIds)) {
                for (Long needAddIssueId : needAddIssueIds) {
                    List<PageFieldViewUpdateVO> fieldViewUpdateVOS = map.getOrDefault(needAddIssueId, new ArrayList<>());
                    fieldViewUpdateVOS.add(v);
                    map.put(needAddIssueId, fieldViewUpdateVOS);
                }
            }
        });
        return map;
    }

    private void checkEpic(Long projectId, JSONObject predefinedFields) {
        if (predefinedFields.containsKey(EPIC_ID) && !ObjectUtils.isEmpty(predefinedFields.get(EPIC_ID))) {
            issueValidator.judgeEpicCanUpdateAndExist(projectId, EncryptionUtils.decrypt(predefinedFields.get(EPIC_ID).toString(), EncryptionUtils.BLANK_KEY));
        }
    }

    private TriggerCarrierVO buildTriggerCarrierVO(IssueDTO v, Long projectId) {
        TriggerCarrierVO triggerCarrierVO = new TriggerCarrierVO();
        triggerCarrierVO.setProjectId(projectId);
        triggerCarrierVO.setMemberFieldIds(new HashSet<>());
        triggerCarrierVO.setFieldList(new ArrayList<>());
        triggerCarrierVO.setExecutedRules(new ArrayList<>());
        triggerCarrierVO.setIssueTypeId(v.getIssueTypeId());
        triggerCarrierVO.setNoticeInstanceId(v.getIssueId());
        triggerCarrierVO.setInstanceId(v.getIssueId());
        triggerCarrierVO.setAuditDomain(v);
        return triggerCarrierVO;
    }

    private List<VersionIssueRelVO> buildVersionData(Object object) {
       return object == null ? null : EncryptionUtils.jsonToList(object,VersionIssueRelVO.class);
    }

    private void updateIssueStatus(Long projectId, Long statusId, IssueDTO issueDTO, String appleType) {
        if (!ObjectUtils.isEmpty(statusId)) {
            List<TransformVO> transformVOS = projectConfigService.queryTransformsByProjectId(projectId, issueDTO.getStatusId(), issueDTO.getIssueId(), issueDTO.getIssueTypeId(), appleType);
            if (!CollectionUtils.isEmpty(transformVOS)) {
                Map<Long, TransformVO> map = transformVOS.stream().collect(Collectors.toMap(TransformVO::getEndStatusId, Function.identity()));
                TransformVO transformVO = map.get(statusId);
                if (!ObjectUtils.isEmpty(transformVO)) {
                    issueService.updateIssueStatusWithoutRuleNotice(projectId, issueDTO.getIssueId(), transformVO.getId(), transformVO.getStatusVO().getObjectVersionNumber(), appleType, null, false);
                }
            }
        }
    }

    private void handlerBugInfluenceVersion(Long projectId,IssueDTO issueDTO, List<VersionIssueRelVO> influenceVersion, IssueVO issueVO) {
        if ("bug".equals(issueDTO.getTypeCode()) && influenceVersion != null) {
            IssueUpdateVO issueUpdateVO1 = new IssueUpdateVO();
            issueUpdateVO1.setVersionType(ProductVersionService.VERSION_RELATION_TYPE_INFLUENCE);
            issueUpdateVO1.setVersionIssueRelVOList(influenceVersion);
            issueUpdateVO1.setIssueId(issueDTO.getIssueId());
            issueUpdateVO1.setObjectVersionNumber(issueVO.getObjectVersionNumber());
            issueService.updateIssueWithoutRuleNotice(projectId, issueUpdateVO1, Collections.singletonList("objectVersionNumber"));
        }
    }

    private void fieldListRemove(IssueDTO v, List<String> fieldList, IssueUpdateVO issueUpdateVO, Map<String,Object> programMap) {
        if (!"story".equals(v.getTypeCode())) {
            fieldList.remove(String.valueOf("storyPoints"));
            issueUpdateVO.setStoryPoints(null);
        }

        if (!"bug".equals(v.getTypeCode())) {
            fieldList.remove(String.valueOf("environment"));
            issueUpdateVO.setEnvironment(null);
        }

        if (Arrays.asList(IssueTypeCode.ISSUE_TYPE_CODE_WITH_FEATURE).contains(v.getTypeCode()) && agilePluginService != null) {
            agilePluginService.setFeatureId(issueUpdateVO,programMap,fieldList);
        }

        if ("issue_epic".equals(v.getTypeCode())) {
            fieldList.remove(EPIC_ID);
            issueUpdateVO.setEpicId(null);
        }
    }

    private void addErrMessage(IssueDTO issueDTO, BatchUpdateFieldStatusVO batchUpdateFieldStatusVO, String field) {
        if (Objects.isNull(batchUpdateFieldStatusVO.getErrorMsgMap())) {
            batchUpdateFieldStatusVO.setErrorMsgMap(new HashMap<>());
        }
        StringBuilder issueNums = new StringBuilder();
        if (batchUpdateFieldStatusVO.getErrorMsgMap().containsKey(field)) {
            issueNums.append(batchUpdateFieldStatusVO.getErrorMsgMap().get(field));
            issueNums.append("," + issueDTO.getIssueNum());
        } else {
            issueNums.append(issueDTO.getIssueNum());
        }
        batchUpdateFieldStatusVO.getErrorMsgMap().put(field, issueNums);
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
                                    boolean sendMsg,
                                    Map<Long, TriggerCarrierVO> triggerCarrierMap) {
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds, null);
        if (CollectionUtils.isEmpty(customFields)) {
            throw new CommonException("error.customFields.null");
        }
        Map<Long, IssueDTO> issueMap = issueDTOS.stream().collect(Collectors.toMap(IssueDTO::getIssueId, Function.identity()));
        // 根据issueTypeId判断这个字段哪些问题类型可以添加
        customFields.forEach(v -> {
            List<ObjectSchemeFieldExtendDTO> objectSchemeFieldExtendDTOS = objectSchemeFieldExtendMapper.selectExtendFields(ConvertUtil.getOrganizationId(projectId), v.getFieldId(), projectId, null);
            List<Long> issueTypeIds = objectSchemeFieldExtendDTOS.stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toList());
            List<Long> needAddIssueIds = issueDTOS.stream().filter(issueDTO -> issueTypeIds.contains(issueDTO.getIssueTypeId())).map(IssueDTO::getIssueId).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(needAddIssueIds)) {
                batchHandlerCustomFields(projectId, v, schemeCode, needAddIssueIds);
                for (Long needAddIssueId : needAddIssueIds) {
                    IssueDTO issueDTO = issueMap.getOrDefault(needAddIssueId, null);
                    if (!ObjectUtils.isEmpty(issueDTO)) {
                        TriggerCarrierVO triggerCarrierVO = triggerCarrierMap.getOrDefault(needAddIssueId, null);
                        if (ObjectUtils.isEmpty(triggerCarrierVO)) {
                            triggerCarrierVO = buildTriggerCarrierVO(issueDTO, projectId);
                        }
                        triggerCarrierVO.getFieldList().add(v.getFieldCode());
                    }
                }
            }
            if (sendMsg) {
                batchUpdateFieldStatusVO.setProcess( batchUpdateFieldStatusVO.getProcess() + batchUpdateFieldStatusVO.getIncrementalValue());
                if (batchUpdateFieldStatusVO.getProcess() - batchUpdateFieldStatusVO.getLastProcess() >= 0.1) {
                    socketSendHelper.sendByUserId(batchUpdateFieldStatusVO.getUserId(), batchUpdateFieldStatusVO.getKey(), JSON.toJSONString(batchUpdateFieldStatusVO));
                    batchUpdateFieldStatusVO.setLastProcess(batchUpdateFieldStatusVO.getProcess());
                }
            }
        });
    }

    @Override
    public void copyCustomFieldValue(Long projectId, IssueDetailDTO issueDetailDTO, Long newIssueId, List<Long> customFieldIds, List<PageFieldViewCreateVO> copyRequireFields) {
        // 查询原来的值
        Long issueId = issueDetailDTO.getIssueId();
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<PageFieldViewCreateVO> createDTOs = new ArrayList<>();
        List<FieldValueDTO> fieldValueDTOS = fieldValueMapper.queryListByInstanceIds(Arrays.asList(projectId), Arrays.asList(issueId), SCHEME_CODE, null);
        if (!CollectionUtils.isEmpty(fieldValueDTOS)) {
            Map<Long, List<FieldValueDTO>> listMap;
            boolean copySubIssue = CollectionUtils.isEmpty(customFieldIds);
            //复制子问题的全部自定义字段
            if (copySubIssue) {
                listMap = fieldValueDTOS.stream().collect(Collectors.groupingBy(FieldValueDTO::getFieldId));
            } else {
                listMap = fieldValueDTOS.stream().filter(v -> customFieldIds.contains(v.getFieldId())).collect(Collectors.groupingBy(FieldValueDTO::getFieldId));
            }
            handlerFieldValue(listMap, createDTOs);
        }
        if (!CollectionUtils.isEmpty(copyRequireFields)) {
            createDTOs.addAll(copyRequireFields);
        }
        if (!CollectionUtils.isEmpty(createDTOs)) {
            createFieldValuesWithoutRuleNotice(organizationId, projectId, newIssueId, SCHEME_CODE, createDTOs);
            List<FieldDataLogCreateVO> list = new ArrayList<>();
            for (PageFieldViewCreateVO createDTO : createDTOs) {
                List<FieldValueDTO> addFieldValue = fieldValueMapper.listByInstanceIdsAndFieldId(projectId, Arrays.asList(newIssueId), SCHEME_CODE, createDTO.getFieldId());
                list.addAll(FieldValueUtil.batchHandlerFiledLog(projectId, newIssueId, new ArrayList<>(), addFieldValue));
            }
            if (!CollectionUtils.isEmpty(list) ) {
                CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
                fieldDataLogMapper.batchInsert(projectId, SCHEME_CODE, list, customUserDetails.getUserId());
            }
        }
    }

    private void handlerFieldValue(Map<Long, List<FieldValueDTO>> listMap, List<PageFieldViewCreateVO> createDTOs) {
        Set<Long> keySet = listMap.keySet();
        List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS = objectSchemeFieldMapper.selectByIds(StringUtils.join(keySet, ","));
        Map<Long, ObjectSchemeFieldDTO> schemeFieldDTOMap = objectSchemeFieldDTOS.stream().collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, Function.identity()));
        for (Map.Entry<Long, List<FieldValueDTO>> entry : listMap.entrySet()) {
            Long key = entry.getKey();
            ObjectSchemeFieldDTO objectSchemeFieldDTO = schemeFieldDTOMap.get(key);
            if (ObjectUtils.isEmpty(objectSchemeFieldDTO) || Boolean.TRUE.equals(objectSchemeFieldDTO.getSystem())) {
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
                value = handlerMultiple(listMap, objectSchemeFieldDTO.getId());
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

    private Object handlerMultiple(Map<Long, List<FieldValueDTO>> listMap, Long fieldId) {
        List<FieldValueDTO> fieldValueDTOS = listMap.get(fieldId);
        List<String> values = new ArrayList<>();
        if (!CollectionUtils.isEmpty(fieldValueDTOS)) {
            values.addAll(fieldValueDTOS.stream().map(v -> String.valueOf(v.getOptionId())).collect(Collectors.toList()));
        }
        return values;
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
        if (!CollectionUtils.isEmpty(allFieldValue)) {
            fieldValueMapper.batchInsertField(projectId, schemeCode, allFieldValue);
        }
        // 批量生产日志
        Map<Long, List<FieldValueDTO>> newFieldMap = new HashMap<>();
        newFieldMap.putAll(allFieldValue.stream().collect(Collectors.groupingBy(FieldValueDTO::getInstanceId)));
        List<FieldDataLogCreateVO> list = new ArrayList<>();
        needAddIssueIds.forEach(v -> {
            List<FieldValueDTO> oldFiledList = oldFieldMap.get(v);
            List<FieldValueDTO> newFiledList = newFieldMap.get(v);
            list.addAll(FieldValueUtil.batchHandlerFiledLog(projectId, v, oldFiledList, newFiledList));
        });
        if (!CollectionUtils.isEmpty(list) && SCHEME_CODE.equals(schemeCode)) {
            CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
            fieldDataLogMapper.batchInsert(projectId, schemeCode, list, customUserDetails.getUserId());
        }
    }
}
