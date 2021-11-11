package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.TriggerCarrierVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.commons.lang.StringUtils;
import org.modelmapper.ModelMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2020-08-13 14:51
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusFieldSettingServiceImpl implements StatusFieldSettingService {

    private static final Logger LOGGER = LoggerFactory.getLogger(StatusFieldSettingServiceImpl.class);

    private static final String[] FILTER_FIELD_TYPE = {"checkbox", "multiple", "member", "radio", "single"};
    private static final String[] FEATURE_FIELD = {FieldCode.ACCEPTANCE_CRITERA, FieldCode.BENFIT_HYPOTHESIS, FieldCode.PROGRAM_VERSION, FieldCode.SUB_PROJECT, FieldCode.FEATURE_TYPE};
    public static final Map<String, String> FIELD_CODE = new LinkedHashMap<>();
    public static final Map<String, String> PROGRAM_FIELD_CODE = new LinkedHashMap<>();
    private static final String CLEAR = "clear";
    private static final String OPERATOR = "operator";
    private static final String CREATOR = "creator";
    private static final String REPORTOR = "reportor";
    private static final String ASSIGNEE = "assignee";
    private static final String MAIN_RESPONSIBLE = "mainResponsible";
    private static final String PARTICIPANT = "participant";
    private static final String SPECIFIER = "specifier";
    private static final String COPY_CUSTOM_FIELD = "copy_custom_field";
    private static final String CURRENT_TIME = "current_time";
    private static final String ADD = "add";
    private static final String[] CLEAR_FIELD = {FieldCode.LABEL, FieldCode.COMPONENT, FieldCode.TAG, FieldCode.PARTICIPANT};
    private static final List<String> MEMBER_FIELD_TYPES = Arrays.asList(FieldType.MEMBER, FieldType.MULTI_MEMBER);
    private static final List<String> OPERATE_TYPE = Arrays.asList(CLEAR, OPERATOR, CREATOR, REPORTOR, ASSIGNEE, MAIN_RESPONSIBLE, PARTICIPANT, SPECIFIER, COPY_CUSTOM_FIELD, CURRENT_TIME, ADD);
    private static final String ERROR_FIELD_VALUE_UPDATE_TO_SELF = "error.fieldValue.cannot.update.to.self";

    @Autowired
    private StatusFieldSettingMapper statusFieldSettingMapper;
    @Autowired
    private StatusFieldValueSettingMapper statusFieldValueSettingMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private IssueLabelMapper issueLabelMapper;
    @Autowired
    private ProductVersionMapper productVersionMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private PriorityMapper priorityMapper;
    @Autowired
    private FieldOptionMapper fieldOptionMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private FieldValueMapper fieldValueMapper;
    @Autowired
    private IssueService issueService;
    @Autowired
    private FieldValueService fieldValueService;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private LookupValueService lookupValueService;
    @Autowired
    private OrganizationConfigService organizationConfigService;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private IssueParticipantRelMapper issueParticipantRelMapper;
    static {
        FIELD_CODE.put(FieldCode.ASSIGNEE, "assigneeId");
        FIELD_CODE.put(FieldCode.REPORTER, "reporterId");
        FIELD_CODE.put(FieldCode.COMPONENT, "componentIssueRelVOList");
        FIELD_CODE.put(FieldCode.LABEL, "labelIssueRelVOList");
        FIELD_CODE.put(FieldCode.FIX_VERSION, "versionIssueRelVOList");
        FIELD_CODE.put(FieldCode.INFLUENCE_VERSION, "versionIssueRelVOList");
        FIELD_CODE.put(FieldCode.DESCRIPTION, "description");
        FIELD_CODE.put(FieldCode.STORY_POINTS, "storyPoints");
        FIELD_CODE.put(FieldCode.REMAINING_TIME, "remainingTime");
        FIELD_CODE.put(FieldCode.ESTIMATE_TIME, "estimateTime");
        FIELD_CODE.put(FieldCode.PRIORITY, "priorityId");
        FIELD_CODE.put(FieldCode.EPIC, "epicId");
        FIELD_CODE.put(FieldCode.CREATION_DATE, "creationDate");
        FIELD_CODE.put(FieldCode.LAST_UPDATE_DATE, "lastUpdateDate");
        FIELD_CODE.put(FieldCode.ESTIMATED_END_TIME, "estimatedEndTime");
        FIELD_CODE.put(FieldCode.ACTUAL_END_TIME, "actualEndTime");
        FIELD_CODE.put(FieldCode.ACTUAL_START_TIME, "actualStartTime");
        FIELD_CODE.put(FieldCode.ESTIMATED_START_TIME, "estimatedStartTime");
        FIELD_CODE.put(FieldCode.MAIN_RESPONSIBLE, "mainResponsibleId");
        FIELD_CODE.put(FieldCode.ENVIRONMENT, "environment");
        FIELD_CODE.put(FieldCode.TAG, "tags");
        FIELD_CODE.put(FieldCode.PARTICIPANT, "participantIds");
        PROGRAM_FIELD_CODE.put(FieldCode.PROGRAM_VERSION, "programVersion");
    }
    @Override
    public List<StatusFieldSettingVO> createOrUpdate(Long project, Long issueType, Long statusId, Long objectVersionNumber, String applyType, List<StatusFieldSettingVO> list) {
        checkStatusFieldSettings(list);
        List<StatusFieldSettingDTO> statusFieldSettingDTOS = listFieldSetting(0L, project, issueType, statusId);
        if (!CollectionUtils.isEmpty(statusFieldSettingDTOS)) {
            deleteStatusFieldSetting(statusFieldSettingDTOS);
        }
        // 遍历
        for (StatusFieldSettingVO statusFieldSettingVO : list) {
            StatusFieldSettingDTO map = modelMapper.map(statusFieldSettingVO, StatusFieldSettingDTO.class);
            map.setProjectId(project);
            map.setStatusId(statusId);
            map.setOrganizationId(0L);
            map.setIssueTypeId(issueType);
            baseInsert(map);
            // 插入field值
            List<StatusFieldValueSettingDTO> fieldValueList = statusFieldSettingVO.getFieldValueList();
            if (!CollectionUtils.isEmpty(fieldValueList)) {
                insertStatusFieldValue(0L, project, map.getId(), fieldValueList);
            }
        }
        // 更新node
        projectConfigService.updateNodeObjectVersionNumber(project, issueType, statusId, objectVersionNumber, applyType);
        return list(project, issueType, statusId);
    }

    private void checkStatusFieldSettings(List<StatusFieldSettingVO> list) {
        if (!CollectionUtils.isEmpty(list)) {
            List<Long> fieldIds = list.stream().map(StatusFieldSettingVO::getFieldId).filter(Objects::nonNull).collect(Collectors.toList());
            Map<Long, ObjectSchemeFieldDTO> fieldMap =
                    objectSchemeFieldMapper.selectByIds(StringUtils.join(fieldIds, ","))
                    .stream()
                    .collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, Function.identity()));
            list.forEach(setting -> {
                Long fieldId = setting.getFieldId();
                AssertUtilsForCommonException.notNull(fieldId, "error.fieldId.null");
                List<StatusFieldValueSettingDTO> fieldValueList = setting.getFieldValueList();
                AssertUtilsForCommonException.notNull(fieldValueList, "error.fieldValue.null");
                ObjectSchemeFieldDTO objectSchemeFieldDTO = fieldMap.get(fieldId);
                AssertUtilsForCommonException.notNull(objectSchemeFieldDTO, "error.field.not.exist");
                String fieldType = fieldValueList.get(0).getFieldType();
                Set<Long> copyFieldIds = new HashSet<>();
                checkFieldValueList(copyFieldIds, objectSchemeFieldDTO, fieldValueList, fieldId);
                checkCopyFields(copyFieldIds, fieldType);
            });
        }
    }

    private void checkFieldValueList(Set<Long> copyFieldIds,
                                     ObjectSchemeFieldDTO objectSchemeFieldDTO,
                                     List<StatusFieldValueSettingDTO> fieldValueList,
                                     Long fieldId) {
        fieldValueList.forEach(fieldValue -> {
            String operateType = fieldValue.getOperateType();
            String fieldType = fieldValue.getFieldType();
            AssertUtilsForCommonException.notNull(fieldType, "error.fieldValue.filedType.null");
            if (!fieldType.equals(objectSchemeFieldDTO.getFieldType())) {
                throw new CommonException("error.fieldValue.fieldType.not.correct");
            }
            if (!OPERATE_TYPE.contains(operateType)) {
                throw new CommonException("error.illegal.fieldValue.operateType");
            }
            checkMemberField(operateType, fieldType, fieldId, objectSchemeFieldDTO, fieldValue, fieldValueList);
            checkMemberFieldCopyCustomField(operateType, fieldType, fieldValue, copyFieldIds);
        });
    }

    private void checkMemberFieldCopyCustomField(String operateType,
                                                 String fieldType,
                                                 StatusFieldValueSettingDTO fieldValue,
                                                 Set<Long> copyFieldIds) {
        if (COPY_CUSTOM_FIELD.equals(operateType)) {
            // 人员类型才能指定自定义字段的值
            if (!MEMBER_FIELD_TYPES.contains(fieldType)) {
                throw new CommonException("error.illegal.fieldType." + fieldType + ".for.copy_custom_field");
            }
            Long customFieldId = fieldValue.getCustomFieldId();
            AssertUtilsForCommonException.notNull(customFieldId, "error.fieldValue.customFieldId.null");
            copyFieldIds.add(customFieldId);
        }
    }

    private void checkMemberField(String operateType,
                                  String fieldType,
                                  Long fieldId,
                                  ObjectSchemeFieldDTO objectSchemeFieldDTO,
                                  StatusFieldValueSettingDTO fieldValue,
                                  List<StatusFieldValueSettingDTO> fieldValueList) {
        if (MEMBER_FIELD_TYPES.contains(fieldType)) {
            String fieldCode = objectSchemeFieldDTO.getCode();
            if (SPECIFIER.equals(operateType)) {
                Long userId = fieldValue.getUserId();
                AssertUtilsForCommonException.notEmpty(userId, "error.fieldValues.userId.empty");
            } else if (REPORTOR.equals(operateType) && FieldCode.REPORTER.equals(fieldCode)) {
                throw new CommonException(ERROR_FIELD_VALUE_UPDATE_TO_SELF);
            } else if (CREATOR.equals(operateType) && FieldCode.CREATOR.equals(fieldCode)) {
                throw new CommonException(ERROR_FIELD_VALUE_UPDATE_TO_SELF);
            } else if (COPY_CUSTOM_FIELD.equals(operateType) && fieldId.equals(fieldValue.getCustomFieldId())) {
                throw new CommonException(ERROR_FIELD_VALUE_UPDATE_TO_SELF);
            } else if (operateType.equals(fieldCode)) {
                throw new CommonException(ERROR_FIELD_VALUE_UPDATE_TO_SELF);
            }
        }
        if (FieldType.MEMBER.equals(fieldType) && (fieldValueList.size() > 1 || PARTICIPANT.equals(operateType))) {
            throw new CommonException("error.member.field.cannot.set.to.multiMember");
        }
    }

    private void checkCopyFields(Set<Long> copyFieldIds,
                                 String fieldType) {
        if (ObjectUtils.isEmpty(copyFieldIds)) {
            return;
        }
        // 校验自定义人员字段
        Map<Long, String> fieldTypeMap =
                objectSchemeFieldMapper.selectByIds(StringUtils.join(copyFieldIds, ","))
                        .stream()
                        .collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, ObjectSchemeFieldDTO::getFieldType));
        copyFieldIds.forEach(customFieldId -> {
            //member -> member;  multiMember -> multiMember & member
            String customFieldType = fieldTypeMap.get(customFieldId);
            if (FieldType.MEMBER.equals(fieldType)
                    && !FieldType.MEMBER.equals(customFieldType)) {
                throw new CommonException("error.illegal.fieldValue.customFieldId");
            }
            if (FieldType.MULTI_MEMBER.equals(fieldType)
                    && !FieldType.MEMBER.equals(customFieldType)
                    && !FieldType.MULTI_MEMBER.equals(customFieldType)) {
                throw new CommonException("error.illegal.fieldValue.customFieldId");
            }
        });
    }

    @Override
    public List<StatusFieldSettingDTO> listFieldSetting(Long organizationId, Long project, Long issueType, Long statusId) {
        StatusFieldSettingDTO statusFieldSettingDTO = new StatusFieldSettingDTO();
        statusFieldSettingDTO.setIssueTypeId(issueType);
        statusFieldSettingDTO.setStatusId(statusId);
        statusFieldSettingDTO.setProjectId(project);
        statusFieldSettingDTO.setOrganizationId(organizationId);
        return statusFieldSettingMapper.select(statusFieldSettingDTO);
    }

    @Override
    public List<StatusFieldSettingVO> list(Long projectId, Long issueType, Long statusId) {
        List<StatusFieldSettingDTO> statusFieldSettingDTOS = listFieldSetting(0L, projectId, issueType, statusId);
        if (CollectionUtils.isEmpty(statusFieldSettingDTOS)) {
            return new ArrayList<>();
        }
        List<StatusFieldSettingVO> list = new ArrayList<>();
        for (StatusFieldSettingDTO statusFieldSettingDTO : statusFieldSettingDTOS) {
            StatusFieldSettingVO map = modelMapper.map(statusFieldSettingDTO, StatusFieldSettingVO.class);
            map.setFieldValueList(listFieldValueSetting(0L, projectId, map.getId(), map.getFieldId()));
            list.add(map);
        }
        return list;
    }

    @Override
    public List<StatusFieldSettingVO> listByStatusIds(Long projectId, Long issueType, List<Long> statusIds) {
        List<StatusFieldSettingVO> list = statusFieldSettingMapper.listByStatusIds(projectId, issueType, statusIds);
        list.forEach(statusFieldSettingVO -> {
            String fieldType = statusFieldSettingVO.getFieldType();
            List<String> fieldTypes = Arrays.asList(FILTER_FIELD_TYPE);
            List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS = listFieldValueSetting(0L, projectId, statusFieldSettingVO.getId(), statusFieldSettingVO.getFieldId());
            if (!CollectionUtils.isEmpty(statusFieldValueSettingDTOS)) {
                if (!fieldTypes.contains(fieldType)) {
                    statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                    return;
                }
                if (!Objects.equals(SPECIFIER, statusFieldValueSettingDTOS.get(0).getOperateType())) {
                    statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                    return;
                }
                if ("member".equals(statusFieldSettingVO.getFieldType())) {
                    // 查询用户信息
                    List<Long> userIds = statusFieldValueSettingDTOS.stream().map(StatusFieldValueSettingDTO::getUserId).collect(Collectors.toList());
                    List<UserDTO> body = baseFeignClient.listUsersByIds(userIds.toArray(new Long[userIds.size()]), false).getBody();
                    if (CollectionUtils.isEmpty(body)) {
                        statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                        return;
                    }
                    Map<Long, UserDTO> userDTOMap = body.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(userDTOMap.get(v.getUserId())) ? null : userDTOMap.get(v.getUserId()).getRealName()));
                    statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                    return;
                }
                handlerFieldValue(statusFieldSettingVO, statusFieldValueSettingDTOS);
                statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
            }
        });
        return list;
    }

    @Override
    public void handlerSettingToUpdateIssue(Long projectId, Long issueId, TriggerCarrierVO triggerCarrierVO) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        List<StatusFieldSettingVO> list = statusFieldSettingMapper.listByStatusIds(projectId, issueDTO.getIssueTypeId(), Arrays.asList(issueDTO.getStatusId()));
        if (CollectionUtils.isEmpty(list)) {
            return;
        }
        IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
        List<PageFieldViewUpdateVO> customField = new ArrayList<>();
        List<String> field = new ArrayList<>();
        field.add("objectVersionNumber");
        Map<String,List<VersionIssueRelVO>> versionMap = new HashMap<>();
        Map<String,Object> specifyMap = new HashMap<>();
        list.forEach(fieldSetting -> {
            Long fieldSettingId = fieldSetting.getId();
            Long fieldId = fieldSetting.getFieldId();
            String fieldCode = fieldSetting.getFieldCode();
            String fieldType = fieldSetting.getFieldType();
            boolean isSystemField = Boolean.TRUE.equals(fieldSetting.getSystem());
            List<StatusFieldValueSettingDTO> statusFieldValueSettings = listFieldValueSetting(0L, projectId, fieldSettingId, fieldId);
            if (ObjectUtils.isEmpty(statusFieldValueSettings)) {
                return;
            }
            statusFieldValueSettings = convertCopyMemberFieldToDetail(statusFieldValueSettings, issueDTO, fieldType);
            if (isSystemField) {
                processSystemFieldValues(issueDTO, issueUpdateVO, field, versionMap, specifyMap, fieldCode, statusFieldValueSettings);
            } else {
                processCustomFieldValues(issueDTO, customField, fieldId, fieldType, statusFieldValueSettings, fieldCode);
            }
        });
        // 执行更新
        updateIssue(issueDTO,field,issueUpdateVO,customField,versionMap,specifyMap, false, triggerCarrierVO);
    }

    private List<StatusFieldValueSettingDTO> convertCopyMemberFieldToDetail(List<StatusFieldValueSettingDTO> statusFieldValueSettings,
                                                                            IssueDTO issue,
                                                                            String fieldType) {
        Set<Long> fieldIds =
                statusFieldValueSettings
                        .stream()
                        .filter(value -> COPY_CUSTOM_FIELD.equals(value.getOperateType()) && !ObjectUtils.isEmpty(value.getCustomFieldId()))
                        .map(StatusFieldValueSettingDTO::getCustomFieldId)
                        .collect(Collectors.toSet());
        if (!CollectionUtils.isEmpty(fieldIds)) {
            Long issueId = issue.getIssueId();
            Long projectId = issue.getProjectId();
            Set<Long> userIds =
                    fieldValueMapper.selectByFieldIds(projectId, issueId, ObjectSchemeCode.AGILE_ISSUE, fieldIds)
                            .stream()
                            .map(FieldValueDTO::getOptionId)
                            .filter(optionId -> !ObjectUtils.isEmpty(optionId))
                            .collect(Collectors.toSet());
            List<StatusFieldValueSettingDTO> result = new ArrayList<>();
            statusFieldValueSettings.forEach(v -> {
                if (!COPY_CUSTOM_FIELD.equals(v.getOperateType())) {
                    result.add(v);
                }
            });
            userIds.forEach(userId -> {
                StatusFieldValueSettingDTO valueSetting = new StatusFieldValueSettingDTO();
                valueSetting.setUserId(userId);
                valueSetting.setProjectId(projectId);
                valueSetting.setFieldType(fieldType);
                valueSetting.setOperateType(SPECIFIER);
                result.add(valueSetting);
            });
            return result;
        } else {
            return statusFieldValueSettings;
        }
    }

    @Override
    public void processCustomFieldValues(IssueDTO issueDTO,
                                         List<PageFieldViewUpdateVO> customField,
                                         Long fieldId, String fieldType,
                                         List<StatusFieldValueSettingDTO> statusFieldValueSettings,
                                         String fieldCode) {
        if (ObjectUtils.isEmpty(statusFieldValueSettings)) {
            return;
        }
        PageFieldViewUpdateVO pageFieldViewUpdateVO = new PageFieldViewUpdateVO();
        pageFieldViewUpdateVO.setFieldType(fieldType);
        pageFieldViewUpdateVO.setFieldId(fieldId);
        pageFieldViewUpdateVO.setFieldCode(fieldCode);
        setCustomFieldValue(issueDTO, fieldType, pageFieldViewUpdateVO, statusFieldValueSettings, fieldId);
        customField.add(pageFieldViewUpdateVO);
    }

    @Override
    public void processSystemFieldValues(IssueDTO issueDTO,
                                         IssueUpdateVO issueUpdateVO,
                                         List<String> field,
                                         Map<String, List<VersionIssueRelVO>> versionMap,
                                         Map<String, Object> specifyMap,
                                         String fieldCode,
                                         List<StatusFieldValueSettingDTO> statusFieldValueSettings) {
        if (ObjectUtils.isEmpty(statusFieldValueSettings)) {
            return;
        }
        Boolean isVersion = FieldCode.FIX_VERSION.equals(fieldCode) || FieldCode.INFLUENCE_VERSION.equals(fieldCode);
        if (Boolean.TRUE.equals(isVersion)) {
            handlerVersion(versionMap, fieldCode, statusFieldValueSettings);
        } else if (Arrays.asList(FEATURE_FIELD).contains(fieldCode)) {
            if (agilePluginService != null) {
                agilePluginService.handlerFeatureFieldValue(fieldCode, issueUpdateVO, specifyMap, statusFieldValueSettings, issueDTO, field);
            }
        } else {
            handlerPredefinedValue(issueUpdateVO, field, issueDTO, fieldCode, statusFieldValueSettings);
        }
    }

    @Override
    public List<StatusFieldSettingVO> saveStatusFieldSettings(Long organizationId, Long issueType, Long statusId, Long objectVersionNumber, List<StatusFieldSettingVO> list) {
        checkStatusFieldSettings(list);
        List<StatusFieldSettingDTO> statusFieldSettingDTOS = listFieldSetting(organizationId, 0L, issueType, statusId);
        if (!CollectionUtils.isEmpty(statusFieldSettingDTOS)) {
            deleteStatusFieldSetting(statusFieldSettingDTOS);
        }
        // 遍历
        for (StatusFieldSettingVO statusFieldSettingVO : list) {
            StatusFieldSettingDTO map = modelMapper.map(statusFieldSettingVO, StatusFieldSettingDTO.class);
            map.setProjectId(0L);
            map.setStatusId(statusId);
            map.setIssueTypeId(issueType);
            map.setOrganizationId(organizationId);
            baseInsert(map);
            // 插入field值
            List<StatusFieldValueSettingDTO> fieldValueList = statusFieldSettingVO.getFieldValueList();
            if (!CollectionUtils.isEmpty(fieldValueList)) {
                insertStatusFieldValue(organizationId, 0L, map.getId(), fieldValueList);
            }
        }
        // 更新node
        organizationConfigService.updateNodeObjectVersionNumber(organizationId, issueType, statusId, objectVersionNumber);
        return listByOptions(organizationId, issueType, statusId);
    }

    @Override
    public List<StatusFieldSettingVO> listByOptions(Long organizationId, Long issueType, Long statusId) {
        List<StatusFieldSettingDTO> statusFieldSettingDTOS = listFieldSetting(organizationId, 0L, issueType, statusId);
        if (CollectionUtils.isEmpty(statusFieldSettingDTOS)) {
            return new ArrayList<>();
        }
        List<StatusFieldSettingVO> list = new ArrayList<>();
        for (StatusFieldSettingDTO statusFieldSettingDTO : statusFieldSettingDTOS) {
            StatusFieldSettingVO map = modelMapper.map(statusFieldSettingDTO, StatusFieldSettingVO.class);
            map.setFieldValueList(listFieldValueSetting(organizationId, 0L, map.getId(), map.getFieldId()));
            list.add(map);
        }
        return list;
    }

    @Override
    public List<StatusFieldSettingVO> listStatusFieldSetting(Long organizationId, Long issueType, List<Long> statusIds) {
        List<StatusFieldSettingVO> list = statusFieldSettingMapper.listOptions(organizationId, issueType, statusIds);
        list.forEach(statusFieldSettingVO -> {
            String fieldType = statusFieldSettingVO.getFieldType();
            List<String> fieldTypes = Arrays.asList(FILTER_FIELD_TYPE);
            List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS = listFieldValueSetting(organizationId, 0L, statusFieldSettingVO.getId(), statusFieldSettingVO.getFieldId());
            if (!CollectionUtils.isEmpty(statusFieldValueSettingDTOS)) {
                if (!fieldTypes.contains(fieldType)) {
                    statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                    return;
                }
                if (!Objects.equals(SPECIFIER, statusFieldValueSettingDTOS.get(0).getOperateType())) {
                    statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                    return;
                }
                if ("member".equals(statusFieldSettingVO.getFieldType())) {
                    // 查询用户信息
                    List<Long> userIds = statusFieldValueSettingDTOS.stream().map(StatusFieldValueSettingDTO::getUserId).collect(Collectors.toList());
                    List<UserDTO> body = baseFeignClient.listUsersByIds(userIds.toArray(new Long[userIds.size()]), false).getBody();
                    if (CollectionUtils.isEmpty(body)) {
                        statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                        return;
                    }
                    Map<Long, UserDTO> userDTOMap = body.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(userDTOMap.get(v.getUserId())) ? null : userDTOMap.get(v.getUserId()).getRealName()));
                    statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                    return;
                }
                handlerFieldValue(statusFieldSettingVO, statusFieldValueSettingDTOS);
                statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
            }
        });
        return list;
    }

    @Override
    public void updateIssue(IssueDTO issueDTO,
                            List<String> field,
                            IssueUpdateVO issueUpdateVO,
                            List<PageFieldViewUpdateVO> customField,
                            Map<String, List<VersionIssueRelVO>> versionMap,
                            Map<String, Object> specifyMap,
                            boolean doRuleNotice,
                            TriggerCarrierVO triggerCarrierVO) {
        Set<String> fieldList = new HashSet<>();
        if (!CollectionUtils.isEmpty(triggerCarrierVO.getFieldList())) {
            fieldList.addAll(triggerCarrierVO.getFieldList());
        }
        Set<String> customFiledCodes = new HashSet<>();
        Long organizationId = ConvertUtil.getOrganizationId(issueDTO.getProjectId());
        Long objectVersionNumber = issueDTO.getObjectVersionNumber();
        if (!ObjectUtils.isEmpty(field) && field.size() > 1) {
            handlerFiledList(issueUpdateVO, fieldList);
            issueUpdateVO.setIssueId(issueDTO.getIssueId());
            issueUpdateVO.setObjectVersionNumber(objectVersionNumber);
            if (doRuleNotice) {
                issueService.updateIssue(issueDTO.getProjectId(), issueUpdateVO, field);
            } else {
                issueService.updateIssueWithoutRuleNotice(issueDTO.getProjectId(), issueUpdateVO, field);
            }
            objectVersionNumber += 1;
        }
        // 单独更新版本
        if (!CollectionUtils.isEmpty(versionMap)) {
            fieldList.add("versionId");
            for (Map.Entry<String, List<VersionIssueRelVO>> entry : versionMap.entrySet()) {
                IssueUpdateVO issueUpdateVO1 = new IssueUpdateVO();
                issueUpdateVO1.setIssueId(issueDTO.getIssueId());
                issueUpdateVO1.setObjectVersionNumber(objectVersionNumber);
                issueUpdateVO1.setVersionType(entry.getKey());
                issueUpdateVO1.setVersionIssueRelVOList(entry.getValue());
                if (doRuleNotice) {
                    issueService.updateIssue(issueDTO.getProjectId(), issueUpdateVO1, new ArrayList<>());
                } else {
                    issueService.updateIssueWithoutRuleNotice(issueDTO.getProjectId(), issueUpdateVO1, new ArrayList<>());
                }
                objectVersionNumber += 1;
            }
        }
        if (!CollectionUtils.isEmpty(customField)) {
            for (PageFieldViewUpdateVO pageFieldViewUpdateVO : customField) {
                customFiledCodes.add(pageFieldViewUpdateVO.getFieldCode());
                fieldValueService.updateFieldValue(organizationId, issueDTO.getProjectId(), issueDTO.getIssueId(), pageFieldViewUpdateVO.getFieldId(), "agile_issue", pageFieldViewUpdateVO);
            }
        }
        fieldList.addAll(field);
        fieldList.addAll(customFiledCodes);
        triggerCarrierVO.setMemberFieldIds(new HashSet<>());
        triggerCarrierVO.setFieldList(new ArrayList<>(fieldList));
        if (agilePluginService != null) {
            agilePluginService.handlerSpecifyProgramField(issueDTO, specifyMap, doRuleNotice, triggerCarrierVO);
        }
    }

    private void handlerFiledList(IssueUpdateVO issueUpdateVO, Set<String> fieldList) {
        if (issueUpdateVO.getComponentIssueRelVOList() != null) {
            fieldList.add("componentId");
        }
        if (issueUpdateVO.getLabelIssueRelVOList() != null) {
            fieldList.add("labelId");
        }
        if (issueUpdateVO.getParticipantIds() != null) {
            fieldList.add("participantId");
        }
    }

    private void handlerVersion(Map<String, List<VersionIssueRelVO>> versionMap,
                                String fieldCode,
                                List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS) {
        Boolean isVersion = FieldCode.FIX_VERSION.equals(fieldCode) || FieldCode.INFLUENCE_VERSION.equals(fieldCode);
        if (Boolean.TRUE.equals(isVersion)) {
            List<VersionIssueRelVO> versionIssueRelVOS = new ArrayList<>();
            if (!CLEAR.equals(statusFieldValueSettingDTOS.get(0).getOperateType())) {
                versionIssueRelVOS = statusFieldValueSettingDTOS.stream().map(settingDTO -> {
                    VersionIssueRelVO versionIssueRelVO = new VersionIssueRelVO();
                    versionIssueRelVO.setVersionId(settingDTO.getOptionId());
                    return versionIssueRelVO;
                }).collect(Collectors.toList());
            }
            String versionType = FieldCode.FIX_VERSION.equals(fieldCode) ? "fix" : "influence";
            versionMap.put(versionType,versionIssueRelVOS);
        }
    }

    private void handlerPredefinedValue(IssueUpdateVO issueUpdateVO,
                                        List<String> fieldList,
                                        IssueDTO issueDTO,
                                        String fieldCode,
                                        List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS) {
        String fieldName = FIELD_CODE.get(fieldCode);
        Class clazz = issueUpdateVO.getClass();
        try {
            Field field = clazz.getDeclaredField(fieldName);
            field.setAccessible(true);
            StatusFieldValueSettingDTO statusFieldValueSettingDTO = statusFieldValueSettingDTOS.get(0);
            fieldList.add(fieldName);
            if (CLEAR.equals(statusFieldValueSettingDTO.getOperateType())) {
                if (Arrays.asList(CLEAR_FIELD).contains(fieldCode)) {
                    field.set(issueUpdateVO, new ArrayList<>());
                } else {
                    field.set(issueUpdateVO, null);
                }
                return;
            }
            handlerFieldName(issueUpdateVO, statusFieldValueSettingDTOS, issueDTO, statusFieldValueSettingDTO, fieldCode, field);
        } catch (Exception e) {
            throw new CommonException("error.transform.object", e);
        }
    }

    private void handlerFieldName(IssueUpdateVO issueUpdateVO,
                                  List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS,
                                  IssueDTO issueDTO,
                                  StatusFieldValueSettingDTO fieldValueSettingDTO,
                                  String fieldCode,
                                  Field field) throws IllegalAccessException {
        switch (fieldCode) {
            case FieldCode.REPORTER:
                Boolean canSetValue = ((MAIN_RESPONSIBLE.equals(fieldValueSettingDTO.getOperateType()) && !ObjectUtils.isEmpty(issueDTO.getMainResponsibleId()))
                        || !(ASSIGNEE.equals(fieldValueSettingDTO.getOperateType()) || MAIN_RESPONSIBLE.equals(fieldValueSettingDTO.getOperateType()))
                        || (ASSIGNEE.equals(fieldValueSettingDTO.getOperateType()) && !ObjectUtils.isEmpty(issueDTO.getAssigneeId())));
                if (Boolean.TRUE.equals(canSetValue)) {
                    field.set(issueUpdateVO, handlerMember(fieldValueSettingDTO, issueDTO));
                } else {
                    field.set(issueUpdateVO, issueDTO.getReporterId());
                }
                break;
            case FieldCode.ASSIGNEE:
            case FieldCode.MAIN_RESPONSIBLE:
                field.set(issueUpdateVO, handlerMember(fieldValueSettingDTO, issueDTO));
                break;
            case FieldCode.CREATION_DATE:
            case FieldCode.LAST_UPDATE_DATE:
            case FieldCode.ESTIMATED_START_TIME:
            case FieldCode.ESTIMATED_END_TIME:
            case FieldCode.ACTUAL_START_TIME:
            case FieldCode.ACTUAL_END_TIME:
                field.set(issueUpdateVO, handlerPredefinedTimeField(fieldValueSettingDTO));
                break;
            case FieldCode.EPIC:
            case FieldCode.PRIORITY:
                field.set(issueUpdateVO, fieldValueSettingDTO.getOptionId());
                break;
            case FieldCode.REMAINING_TIME:
                BigDecimal bigDecimal = issueDTO.getRemainingTime();
                field.set(issueUpdateVO, handlerPredefinedNumber(fieldValueSettingDTO, bigDecimal));
                break;
            case FieldCode.ESTIMATE_TIME:
                field.set(issueUpdateVO, handlerPredefinedNumber(fieldValueSettingDTO, issueDTO.getEstimateTime()));
                break;
            case FieldCode.STORY_POINTS:
                BigDecimal storyPoints = issueDTO.getStoryPoints();
                field.set(issueUpdateVO, handlerPredefinedNumber(fieldValueSettingDTO, storyPoints));
                break;
            case FieldCode.DESCRIPTION:
                field.set(issueUpdateVO, fieldValueSettingDTO.getTextValue());
                break;
            case FieldCode.COMPONENT:
                List<ComponentIssueRelVO> componentIssueRelVOS = statusFieldValueSettingDTOS.stream().map(settingDTO -> {
                    ComponentIssueRelVO componentIssueRelVO = new ComponentIssueRelVO();
                    componentIssueRelVO.setComponentId(settingDTO.getOptionId());
                    return componentIssueRelVO;
                }).collect(Collectors.toList());
                field.set(issueUpdateVO, componentIssueRelVOS);
                break;
            case FieldCode.PARTICIPANT:
                List<Long> participantIds = statusFieldValueSettingDTOS.stream().map(settingDTO -> handlerMember(settingDTO, issueDTO)).collect(Collectors.toList());
                field.set(issueUpdateVO, participantIds);
                break;
            case FieldCode.LABEL:
                List<LabelIssueRelVO> labelIssueRelVOS = statusFieldValueSettingDTOS.stream().map(settingDTO -> {
                    LabelIssueRelVO labelIssueRelVO = new LabelIssueRelVO();
                    labelIssueRelVO.setLabelId(settingDTO.getOptionId());
                    labelIssueRelVO.setProjectId(issueDTO.getProjectId());
                    IssueLabelDTO issueLabelDTO = issueLabelMapper.selectByPrimaryKey(settingDTO.getOptionId());
                    if (!ObjectUtils.isEmpty(issueLabelDTO)) {
                        labelIssueRelVO.setLabelName(issueLabelDTO.getLabelName());
                    }
                    return labelIssueRelVO;
                }).collect(Collectors.toList());
                field.set(issueUpdateVO, labelIssueRelVOS);
                break;
            case FieldCode.ENVIRONMENT:
                StatusFieldValueSettingDTO statusFieldValueSettingDTO = statusFieldValueSettingDTOS.get(0);
                field.set(issueUpdateVO, statusFieldValueSettingDTO.getStringValue());
                break;
            default:
                break;
        }
    }

    private Date handlerPredefinedTimeField(StatusFieldValueSettingDTO fieldValueSettingDTO) {
        Date date = null;
        if ("add".equals(fieldValueSettingDTO.getOperateType())) {
            BigDecimal dateAddValue = fieldValueSettingDTO.getDateAddValue();
            Calendar cal = Calendar.getInstance();
            cal.add(Calendar.DAY_OF_MONTH, dateAddValue.intValue());
            date = cal.getTime();
        } else if (CURRENT_TIME.equals(fieldValueSettingDTO.getOperateType())) {
            date = new Date();
        } else {
            date = fieldValueSettingDTO.getDateValue();
        }
        return date;
    }

    private BigDecimal handlerPredefinedNumber(StatusFieldValueSettingDTO statusFieldValueSettingDTO, BigDecimal oldValue) {
        BigDecimal bigDecimal = null;
        if ("add".equals(statusFieldValueSettingDTO.getOperateType())) {
            BigDecimal numberAddValue = statusFieldValueSettingDTO.getNumberAddValue();
            if (ObjectUtils.isEmpty(oldValue)) {
                return numberAddValue;
            }
            return oldValue.add(numberAddValue);
        } else {
            bigDecimal = statusFieldValueSettingDTO.getNumberValue();
        }
        return bigDecimal;
    }

    private void setCustomFieldValue(IssueDTO issueDTO,
                                     String fieldType,
                                     PageFieldViewUpdateVO pageFieldViewUpdateVO,
                                     List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS,
                                     Long fieldId) {
        if (CLEAR.equals(statusFieldValueSettingDTOS.get(0).getOperateType())) {
            return;
        }
        Long projectId = issueDTO.getProjectId();
        StatusFieldValueSettingDTO statusFieldValueSettingDTO = statusFieldValueSettingDTOS.get(0);
        switch (fieldType) {
            case FieldType.CHECKBOX:
            case FieldType.MULTIPLE:
                pageFieldViewUpdateVO.setValue(statusFieldValueSettingDTOS.stream().map(settingDTO -> settingDTO.getOptionId().toString()).collect(Collectors.toList()));
                break;
            case FieldType.RADIO:
            case FieldType.SINGLE:
                pageFieldViewUpdateVO.setValue(statusFieldValueSettingDTO.getOptionId());
                break;
            case FieldType.DATE:
            case FieldType.DATETIME:
            case FieldType.TIME:
                pageFieldViewUpdateVO.setValue(handlerTimeField(statusFieldValueSettingDTO));
                break;
            case FieldType.MEMBER:
                pageFieldViewUpdateVO.setValue(handlerMember(statusFieldValueSettingDTO, issueDTO));
                break;
            case FieldType.TEXT:
                pageFieldViewUpdateVO.setValue(statusFieldValueSettingDTO.getTextValue());
                break;
            case FieldType.INPUT:
                pageFieldViewUpdateVO.setValue(statusFieldValueSettingDTO.getStringValue());
                break;
            case FieldType.NUMBER:
                pageFieldViewUpdateVO.setValue(handlerNumber(projectId, fieldId, statusFieldValueSettingDTO, issueDTO));
                break;
            case FieldType.MULTI_MEMBER:
                pageFieldViewUpdateVO.setValue(handlerMultiMember(statusFieldValueSettingDTOS, issueDTO));
                break;
            default:
                break;
        }
    }

    private Object handlerMultiMember(List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS, IssueDTO issueDTO) {
        if (CollectionUtils.isEmpty(statusFieldValueSettingDTOS)) {
            return null;
        }
        StatusFieldValueSettingDTO settingDTO = statusFieldValueSettingDTOS.get(0);
        if (CLEAR.equals(settingDTO.getOperateType())) {
            return null;
        }
        Set<Long> userIds = new HashSet<>();
        for (StatusFieldValueSettingDTO statusFieldValueSettingDTO : statusFieldValueSettingDTOS) {
            Long userId = handlerMember(statusFieldValueSettingDTO, issueDTO);
            if (!ObjectUtils.isEmpty(userId)) {
                userIds.add(userId);
            }
            if (PARTICIPANT.equals(statusFieldValueSettingDTO.getOperateType())) {
                List<Long> participants = issueParticipantRelMapper.listByIssueId(issueDTO.getProjectId(), issueDTO.getIssueId());
                if (!CollectionUtils.isEmpty(participants)) {
                    userIds.addAll(participants);
                }
            }
        }
        return userIds.stream().map(String::valueOf).collect(Collectors.toList());
    }

    private BigDecimal handlerNumber(Long projectId,
                                     Long fieldId,
                                     StatusFieldValueSettingDTO statusFieldValueSettingDTO,
                                     IssueDTO issueDTO) {
        BigDecimal bigDecimal = null;
        if ("add".equals(statusFieldValueSettingDTO.getOperateType())) {
            List<FieldValueDTO> fieldValueDTOS = fieldValueMapper.queryList(projectId, issueDTO.getIssueId(), "agile_issue", fieldId);
            BigDecimal numberAddValue = statusFieldValueSettingDTO.getNumberAddValue();
            if (CollectionUtils.isEmpty(fieldValueDTOS)) {
                return numberAddValue;
            }
            String numberValue = fieldValueDTOS.get(0).getNumberValue();
            BigDecimal oldValue = new BigDecimal(numberValue);
            bigDecimal = oldValue.add(numberAddValue);
        } else {
            bigDecimal = statusFieldValueSettingDTO.getNumberValue();
        }
        return bigDecimal;
    }

    private Long handlerMember(StatusFieldValueSettingDTO statusFieldValueSettingDTO, IssueDTO issueDTO) {
        Long userId = null;
        String operateType = statusFieldValueSettingDTO.getOperateType();
        if (OPERATOR.equals(operateType)) {
            userId = DetailsHelper.getUserDetails().getUserId();
        } else if (CREATOR.equals(operateType)) {
            userId = issueDTO.getCreatedBy();
        } else if (REPORTOR.equals(operateType)) {
            userId = issueDTO.getReporterId();
        } else if (ASSIGNEE.equals(operateType)) {
            userId = issueDTO.getAssigneeId();
        } else if (MAIN_RESPONSIBLE.equals(operateType)) {
            userId = issueDTO.getMainResponsibleId();
        } else {
            userId = statusFieldValueSettingDTO.getUserId();
        }
        return userId;
    }

    private String handlerTimeField(StatusFieldValueSettingDTO statusFieldValueSettingDTO) {
        Date date = null;
        if ("add".equals(statusFieldValueSettingDTO.getOperateType())) {
            BigDecimal dateAddValue = statusFieldValueSettingDTO.getDateAddValue();
            Calendar cal = Calendar.getInstance();
            cal.add(Calendar.DAY_OF_MONTH, dateAddValue.intValue());
            date = cal.getTime();
        } else if (CURRENT_TIME.equals(statusFieldValueSettingDTO.getOperateType())) {
            date = new Date();
        } else {
            date = statusFieldValueSettingDTO.getDateValue();
        }
        String dateFormat = null;
        SimpleDateFormat dff = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        try {
            dateFormat = dff.format(date.getTime());
        } catch (Exception e) {
            LOGGER.error("format date error: {}", e);
        }
        return dateFormat;
    }

    private void handlerFieldValue(StatusFieldSettingVO statusFieldSettingVO, List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS) {
        List<Long> ids = statusFieldValueSettingDTOS.stream().map(StatusFieldValueSettingDTO::getOptionId).collect(Collectors.toList());
        if (Boolean.TRUE.equals(statusFieldSettingVO.getSystem())) {
            switch (statusFieldSettingVO.getFieldCode()) {
                case "component":
                    List<IssueComponentDTO> issueComponentDTOS = issueComponentMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(issueComponentDTOS)) {
                        break;
                    }
                    Map<Long, IssueComponentDTO> collect = issueComponentDTOS.stream().collect(Collectors.toMap(IssueComponentDTO::getComponentId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(collect.get(v.getOptionId())) ? null : collect.get(v.getOptionId()).getName()));
                    break;
                case "label":
                    List<IssueLabelDTO> issueLabelDTOS = issueLabelMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(issueLabelDTOS)) {
                        break;
                    }
                    Map<Long, IssueLabelDTO> labelDTOMap = issueLabelDTOS.stream().collect(Collectors.toMap(IssueLabelDTO::getLabelId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(labelDTOMap.get(v.getOptionId())) ? null : labelDTOMap.get(v.getOptionId()).getLabelName()));
                    break;
                case "influenceVersion":
                case "fixVersion":
                    List<ProductVersionDTO> productVersionDTOS = productVersionMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(productVersionDTOS)) {
                        break;
                    }
                    Map<Long, ProductVersionDTO> versionDTOMap = productVersionDTOS.stream().collect(Collectors.toMap(ProductVersionDTO::getVersionId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(versionDTOMap.get(v.getOptionId())) ? null : versionDTOMap.get(v.getOptionId()).getName()));
                    break;
                case "priority":
                    List<PriorityDTO> priorityDTOS = priorityMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(priorityDTOS)) {
                        break;
                    }
                    Map<Long, PriorityDTO> priorityDTOMap = priorityDTOS.stream().collect(Collectors.toMap(PriorityDTO::getId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(priorityDTOMap.get(v.getOptionId())) ? null : priorityDTOMap.get(v.getOptionId()).getName()));
                    break;
                case "epic":
                    List<IssueDTO> issueDTOS = issueMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(issueDTOS)) {
                        break;
                    }
                    Map<Long, IssueDTO> issueDTOMap = issueDTOS.stream().collect(Collectors.toMap(IssueDTO::getIssueId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(issueDTOMap.get(v.getOptionId())) ? null : issueDTOMap.get(v.getOptionId()).getSummary()));
                    break;
                case FieldCode.ENVIRONMENT:
                    Map<String, String> environment = lookupValueService.queryMapByTypeCode("environment");
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(environment.get(v.getStringValue())));
                    break;
                default:
                    break;
            }
            if (agilePluginService != null && PROGRAM_FIELD_CODE.containsValue(statusFieldSettingVO.getFieldCode())) {
                agilePluginService.handlerProgramFieldValue(statusFieldSettingVO, statusFieldValueSettingDTOS);
            }
        } else {
            List<FieldOptionDTO> fieldOptionDTOS = fieldOptionMapper.selectByIds(StringUtils.join(ids, ","));
            if (CollectionUtils.isEmpty(fieldOptionDTOS)) {
                return;
            }
            Map<Long, FieldOptionDTO> fieldOptionDTOMap = fieldOptionDTOS.stream().collect(Collectors.toMap(FieldOptionDTO::getId, Function.identity()));
            statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(fieldOptionDTOMap.get(v.getOptionId())) ? null : fieldOptionDTOMap.get(v.getOptionId()).getValue()));
        }
    }

    private List<StatusFieldValueSettingDTO> listFieldValueSetting(Long organizationId, Long projectId, Long fieldSettingId, Long fieldId) {
        StatusFieldValueSettingDTO statusFieldValueSettingDTO = new StatusFieldValueSettingDTO();
        statusFieldValueSettingDTO.setProjectId(projectId);
        statusFieldValueSettingDTO.setOrganizationId(organizationId);
        statusFieldValueSettingDTO.setStatusFieldSettingId(fieldSettingId);
        List<StatusFieldValueSettingDTO> select = statusFieldValueSettingMapper.select(statusFieldValueSettingDTO);
        if(CollectionUtils.isEmpty(select)){
            return new ArrayList<>();
        }
        //过滤不存在的字段选项
        List<Long> fieldOptionIds = select.stream()
                .filter(v -> !Objects.isNull(v.getOptionId()))
                .map(StatusFieldValueSettingDTO::getOptionId)
                .collect(Collectors.toList());
        ObjectSchemeFieldDTO fieldDTO = objectSchemeFieldMapper.queryById(fieldId);
        if (Boolean.FALSE.equals(fieldDTO.getSystem()) && !CollectionUtils.isEmpty(fieldOptionIds)) {
            List<StatusFieldValueSettingDTO> result = new ArrayList<>();
            if (ObjectUtils.isEmpty(organizationId) || Objects.equals(0L, organizationId)) {
                organizationId = ConvertUtil.getOrganizationId(projectId);
            }
            List<Long> existFieldOptionIds = fieldOptionMapper.selectByOptionIds(organizationId, fieldOptionIds)
                    .stream().filter(v -> fieldId.equals(v.getFieldId())).map(FieldOptionDTO::getId).collect(Collectors.toList());
            select.forEach(v -> {
                if (Objects.isNull(v.getOptionId()) || existFieldOptionIds.contains(v.getOptionId())) {
                    result.add(v);
                }
            });
            handlerDTO(result);
            return result;
        } else {
            handlerDTO(select);
            return select;
        }
    }

    private void handlerDTO(List<StatusFieldValueSettingDTO> statusFieldValueSetting) {
        List<Long> userIds = statusFieldValueSetting.stream().filter(v -> (FieldType.MEMBER.equals(v.getFieldType()) || FieldType.MULTI_MEMBER.equals(v.getFieldType())) && !ObjectUtils.isEmpty(v.getUserId())).map(StatusFieldValueSettingDTO::getUserId).collect(Collectors.toList());
        Map<Long, String> userMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(userIds)) {
            List<UserDTO> userDTOS = baseFeignClient.listUsersByIds(userIds.toArray(new Long[userIds.size()]), true).getBody();
            userMap.putAll(userDTOS.stream().collect(Collectors.toMap(UserDTO::getId, UserDTO::getRealName)));
        }
        statusFieldValueSetting.forEach(v -> {
            if ((FieldType.MEMBER.equals(v.getFieldType()) || FieldType.MULTI_MEMBER.equals(v.getFieldType())) && !ObjectUtils.isEmpty(v.getUserId())) {
                v.setName(userMap.get(v.getUserId()));
            }
        });
    }

    private void deleteStatusFieldSetting(List<StatusFieldSettingDTO> statusFieldSettingDTOS) {
        for (StatusFieldSettingDTO statusFieldSettingDTO : statusFieldSettingDTOS) {
            StatusFieldValueSettingDTO statusFieldValueSettingDTO = new StatusFieldValueSettingDTO();
            statusFieldValueSettingDTO.setProjectId(statusFieldSettingDTO.getProjectId());
            statusFieldValueSettingDTO.setStatusFieldSettingId(statusFieldSettingDTO.getId());
            statusFieldValueSettingMapper.delete(statusFieldValueSettingDTO);
            statusFieldSettingMapper.deleteByPrimaryKey(statusFieldSettingDTO.getId());
        }
    }

    private void insertStatusFieldValue(Long organizationId,Long projectId, Long fieldSettingId, List<StatusFieldValueSettingDTO> fieldValueList) {
        for (StatusFieldValueSettingDTO statusFieldValueSettingDTO : fieldValueList) {
            statusFieldValueSettingDTO.setStatusFieldSettingId(fieldSettingId);
            statusFieldValueSettingDTO.setProjectId(projectId);
            statusFieldValueSettingDTO.setOrganizationId(organizationId);
            baseInertFieldValue(statusFieldValueSettingDTO);
        }
    }

    private void baseInertFieldValue(StatusFieldValueSettingDTO statusFieldValueSettingDTO) {
        if (statusFieldValueSettingMapper.insert(statusFieldValueSettingDTO) != 1) {
            throw new CommonException("error.insert.status.field.value.setting");
        }
    }

    private void baseInsert(StatusFieldSettingDTO map) {
        if (statusFieldSettingMapper.insert(map) != 1) {
            throw new CommonException("error.insert.status.field.setting");
        }
    }
}
