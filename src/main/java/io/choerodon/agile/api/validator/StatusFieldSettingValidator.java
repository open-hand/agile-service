package io.choerodon.agile.api.validator;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.StatusFieldSettingVO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.StatusFieldValueSettingDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldMapper;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.core.exception.CommonException;

/**
 * @author huaxin.deng@hand-china.com 2021-11-11 11:45:23
 */
@Component
public class StatusFieldSettingValidator {

    public static final String OPERATE_TYPE_CLEAR = "clear";
    public static final String OPERATE_TYPE_OPERATOR = "operator";
    public static final String OPERATE_TYPE_CREATOR = "creator";
    public static final String OPERATE_TYPE_REPORTER = "reportor";
    public static final String OPERATE_TYPE_ASSIGNEE = "assignee";
    public static final String OPERATE_TYPE_MAIN_RESPONSIBLE = "mainResponsible";
    public static final String OPERATE_TYPE_PARTICIPANT = "participant";
    public static final String OPERATE_TYPE_SPECIFIER = "specifier";
    public static final String OPERATE_TYPE_COPY_CUSTOM_FIELD = "copy_custom_field";
    public static final String OPERATE_TYPE_CURRENT_TIME = "current_time";
    public static final String OPERATE_TYPE_ADD = "add";

    private static final List<String> MEMBER_FIELD_TYPES = Arrays.asList(FieldType.MEMBER, FieldType.MULTI_MEMBER);
    private static final List<String> SYSTEM_MULTI_MEMBER_FIELDS = Collections.singletonList(OPERATE_TYPE_PARTICIPANT);
    private static final List<String> OPERATE_TYPE = Arrays.asList(
            OPERATE_TYPE_CLEAR, OPERATE_TYPE_OPERATOR, OPERATE_TYPE_CREATOR, OPERATE_TYPE_REPORTER, OPERATE_TYPE_ASSIGNEE,
            OPERATE_TYPE_MAIN_RESPONSIBLE, OPERATE_TYPE_PARTICIPANT, OPERATE_TYPE_SPECIFIER, OPERATE_TYPE_COPY_CUSTOM_FIELD,
            OPERATE_TYPE_CURRENT_TIME, OPERATE_TYPE_ADD
    );
    private static final String ERROR_FIELD_VALUE_UPDATE_TO_SELF = "error.fieldValue.cannot.update.to.self";

    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;

    public void checkStatusFieldSettings(List<StatusFieldSettingVO> list) {
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
        if (OPERATE_TYPE_COPY_CUSTOM_FIELD.equals(operateType)) {
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
            if (OPERATE_TYPE_SPECIFIER.equals(operateType)) {
                Long userId = fieldValue.getUserId();
                AssertUtilsForCommonException.notEmpty(userId, "error.fieldValues.userId.empty");
            } else if (OPERATE_TYPE_REPORTER.equals(operateType) && FieldCode.REPORTER.equals(fieldCode)) {
                throw new CommonException(ERROR_FIELD_VALUE_UPDATE_TO_SELF);
            } else if (OPERATE_TYPE_CREATOR.equals(operateType) && FieldCode.CREATOR.equals(fieldCode)) {
                throw new CommonException(ERROR_FIELD_VALUE_UPDATE_TO_SELF);
            } else if (OPERATE_TYPE_COPY_CUSTOM_FIELD.equals(operateType) && fieldId.equals(fieldValue.getCustomFieldId())) {
                throw new CommonException(ERROR_FIELD_VALUE_UPDATE_TO_SELF);
            } else if (operateType.equals(fieldCode)) {
                throw new CommonException(ERROR_FIELD_VALUE_UPDATE_TO_SELF);
            }
        }
        if (FieldType.MEMBER.equals(fieldType) && (fieldValueList.size() > 1 || SYSTEM_MULTI_MEMBER_FIELDS.contains(operateType))) {
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
}
