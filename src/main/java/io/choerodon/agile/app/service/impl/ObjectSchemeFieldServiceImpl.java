package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.stream.Collectors;

import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import org.apache.commons.lang.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.enums.LookupType;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.enums.ObjectSchemeFieldContext;
import io.choerodon.core.exception.CommonException;
import org.springframework.util.ObjectUtils;


/**
 * @author shinan.chen
 * @since 2019/3/29
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ObjectSchemeFieldServiceImpl implements ObjectSchemeFieldService {
    private static final String ERROR_FIELD_ILLEGAL = "error.field.illegal";
    private static final String ERROR_FIELD_CREATE = "error.field.create";
    private static final String ERROR_FIELD_DELETE = "error.field.delete";
    private static final String ERROR_FIELD_NOTFOUND = "error.field.notFound";
    private static final String ERROR_FIELD_UPDATE = "error.field.update";
    private static final String ERROR_SCHEMECODE_ILLEGAL = "error.schemeCode.illegal";
    private static final String ERROR_CONTEXT_ILLEGAL = "error.context.illegal";
    private static final String ERROR_FIELDTYPE_ILLEGAL = "error.fieldType.illegal";
    private static final String ERROR_FIELD_NAMEEXIST = "error.field.nameExist";
    private static final String ERROR_FIELD_CODEEXIST = "error.field.codeExist";
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private ObjectSchemeMapper objectSchemeMapper;
    @Autowired
    private FieldOptionService fieldOptionService;
    @Autowired
    private PageFieldService pageFieldService;
    @Autowired
    private FieldValueService fieldValueService;
    @Autowired
    private LookupValueMapper lookupValueMapper;
    @Autowired
    private FieldDataLogService fieldDataLogService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private IssueTypeFieldMapper issueTypeFieldMapper;

    @Override
    public ObjectSchemeFieldDTO baseCreate(ObjectSchemeFieldDTO field, String[] contexts) {
        Long organizationId = field.getOrganizationId();
        Long projectId = field.getProjectId();
        field.setSystem(false);
        field.setRequired(false);
        if (objectSchemeFieldMapper.insert(field) != 1) {
            throw new CommonException(ERROR_FIELD_CREATE);
        }
        Long fieldId = field.getId();
        //  创建object_scheme_field_extend
        Map<String, Long> issueTypeMap = getIssueTypeMap(organizationId);
        if (ObjectSchemeFieldContext.isGlobal(contexts)) {
            getInsertExtendList(organizationId, projectId, fieldId, ObjectSchemeFieldContext.ISSUE_TYPES, issueTypeMap);
        } else {
            getInsertExtendList(organizationId, projectId, fieldId, contexts, issueTypeMap);
        }
        return objectSchemeFieldMapper.selectByPrimaryKey(field.getId());
    }

    private Map<String, Long> getIssueTypeMap(Long organizationId) {
        IssueTypeDTO dto = new IssueTypeDTO();
        dto.setOrganizationId(organizationId);
        return issueTypeMapper.select(dto).stream().collect(Collectors.toMap(IssueTypeDTO::getTypeCode, IssueTypeDTO::getId));
    }

    private void getInsertExtendList(Long organizationId,
                                     Long projectId,
                                     Long fieldId,
                                     String[] contexts,
                                     Map<String, Long> issueTypeMap) {
        String minRank = null;
        for (String ctx : contexts) {
            ObjectSchemeFieldExtendDTO dto = new ObjectSchemeFieldExtendDTO();
            dto.setIssueType(ctx);
            dto.setProjectId(projectId);
            dto.setOrganizationId(organizationId);
            dto.setFieldId(fieldId);
            if (objectSchemeFieldExtendMapper.select(dto).isEmpty()) {
                dto.setRequired(true);
                dto.setCreated(true);
                dto.setEdited(true);
                dto.setIssueTypeId(
                        Optional
                                .ofNullable(issueTypeMap.get(ctx))
                                .orElse(0L));
                minRank = getMinRank(organizationId, projectId, ctx, minRank);
                dto.setRank(minRank);
                objectSchemeFieldExtendMapper.insert(dto);
            }
        }
    }

    private String getMinRank(Long organizationId, Long projectId, String ctx, String minRank) {
        if (ObjectUtils.isEmpty(minRank)) {
            String rank = objectSchemeFieldExtendMapper.selectMinRank(organizationId, projectId, ctx);
            if (ObjectUtils.isEmpty(rank)) {
                minRank =  RankUtil.mid();
            }
        }
        return RankUtil.genPre(minRank);
    }

    @Override
    public void baseDelete(Long fieldId) {
        if (objectSchemeFieldMapper.deleteByPrimaryKey(fieldId) != 1) {
            throw new CommonException(ERROR_FIELD_DELETE);
        }
    }

    @Override
    public void baseUpdate(ObjectSchemeFieldDTO field) {
        if (objectSchemeFieldMapper.updateByPrimaryKeySelective(field) != 1) {
            throw new CommonException(ERROR_FIELD_UPDATE);
        }
    }

    @Override
    public ObjectSchemeFieldDTO baseQueryById(Long organizationId, Long projectId, Long fieldId) {
        ObjectSchemeFieldDTO field = objectSchemeFieldMapper.queryById(fieldId);
        if (field == null) {
            throw new CommonException(ERROR_FIELD_NOTFOUND);
        }
        if (!field.getOrganizationId().equals(organizationId) && !field.getOrganizationId().equals(0L)) {
            throw new CommonException(ERROR_FIELD_ILLEGAL);
        }
        if (field.getProjectId() != null && !field.getProjectId().equals(projectId) && !field.getProjectId().equals(0L)) {
            throw new CommonException(ERROR_FIELD_ILLEGAL);
        }
        return field;
    }

    @Override
    public List<ObjectSchemeFieldDTO> listQuery(Long organizationId, Long projectId, ObjectSchemeFieldSearchVO searchDTO) {
        return objectSchemeFieldMapper.listQuery(organizationId, projectId, searchDTO);
    }

    @Override
    public ObjectSchemeFieldDTO queryByFieldCode(Long organizationId, Long projectId, String fieldCode) {
        return objectSchemeFieldMapper.queryByFieldCode(organizationId, projectId, fieldCode);
    }

    @Override
    public Map<String, Object> listQuery(Long organizationId, Long projectId, String schemeCode) {
        Map<String, Object> result = new HashMap<>(2);
        if (!EnumUtil.contain(ObjectSchemeCode.class, schemeCode)) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        ObjectSchemeFieldSearchVO searchDTO = new ObjectSchemeFieldSearchVO();
        searchDTO.setSchemeCode(schemeCode);
        List<ObjectSchemeFieldVO> fieldDTOS = modelMapper.map(listQuery(organizationId, projectId, searchDTO), new TypeToken<List<ObjectSchemeFieldVO>>() {
        }.getType());
        fillContextName(fieldDTOS);
        ObjectSchemeDTO select = new ObjectSchemeDTO();
        select.setSchemeCode(schemeCode);
        result.put("name", objectSchemeMapper.selectOne(select).getName());
        result.put("content", fieldDTOS);
        return result;
    }

    /**
     * 填充contextName
     *
     * @param fieldDTOS
     */
    private void fillContextName(List<ObjectSchemeFieldVO> fieldDTOS) {
        LookupTypeWithValuesDTO typeWithValues = lookupValueMapper.queryLookupValueByCode(LookupType.CONTEXT);
        Map<String, String> codeMap = typeWithValues.getLookupValues().stream().collect(Collectors.toMap(LookupValueDTO::getValueCode, LookupValueDTO::getName));
        for (ObjectSchemeFieldVO fieldDTO : fieldDTOS) {
            String[] contextCodes = fieldDTO.getContext().split(",");
            List<String> contextNames = new ArrayList<>(contextCodes.length);
            for (String contextCode : contextCodes) {
                contextNames.add(codeMap.get(contextCode));
            }
            fieldDTO.setContextName(contextNames.stream().collect(Collectors.joining(",")));
        }
    }

    @Override
    public ObjectSchemeFieldDetailVO create(Long organizationId, Long projectId, ObjectSchemeFieldCreateVO fieldCreateDTO) {
        if (!EnumUtil.contain(FieldType.class, fieldCreateDTO.getFieldType())) {
            throw new CommonException(ERROR_FIELDTYPE_ILLEGAL);
        }
        if (checkName(organizationId, projectId, fieldCreateDTO.getName(), fieldCreateDTO.getSchemeCode())) {
            throw new CommonException(ERROR_FIELD_NAMEEXIST);
        }
        if (checkCode(organizationId, projectId, fieldCreateDTO.getCode(), fieldCreateDTO.getSchemeCode())) {
            throw new CommonException(ERROR_FIELD_CODEEXIST);
        }

        String[] contexts = fieldCreateDTO.getContext();
        if (ObjectUtils.isEmpty(contexts)) {
            throw new CommonException("error.filed.context.empty");
        }
        ObjectSchemeFieldContext.isIllegalContexts(contexts);

        ObjectSchemeFieldDTO field = modelMapper.map(fieldCreateDTO, ObjectSchemeFieldDTO.class);
        field.setContext(Arrays.asList(fieldCreateDTO.getContext()).stream().collect(Collectors.joining(",")));
        field.setOrganizationId(organizationId);
        field.setProjectId(projectId);

        String defaultValue = tryDecryptDefaultValue(field.getDefaultValue());
        if (defaultValue != null) {
            field.setDefaultValue(defaultValue);
        }
        field = baseCreate(field, contexts);

        //处理字段选项
        if (fieldCreateDTO.getFieldOptions() != null) {
            String defaultIds = fieldOptionService.handleFieldOption(organizationId, field.getId(), fieldCreateDTO.getFieldOptions());
            if (defaultIds != null && !"".equals(defaultIds)) {
                field.setDefaultValue(defaultIds);
                objectSchemeFieldMapper.updateOptional(field, "defaultValue");
            }
        }

        return queryById(organizationId, projectId, field.getId());
    }

    @Override
    public ObjectSchemeFieldDetailVO queryById(Long organizationId, Long projectId, Long fieldId) {
        ObjectSchemeFieldDTO field = baseQueryById(organizationId, projectId, fieldId);
        ObjectSchemeFieldDetailVO fieldDetailDTO = modelMapper.map(field, ObjectSchemeFieldDetailVO.class);
        fieldDetailDTO.setContext(field.getContext().split(","));
        //获取字段选项，并设置默认值
        List<FieldOptionVO> fieldOptions = fieldOptionService.queryByFieldId(organizationId, fieldId);
        if (!fieldOptions.isEmpty()) {
            if (!ObjectUtils.isEmpty(field.getDefaultValue())) {
                List<String> defaultIds = Arrays.asList(field.getDefaultValue().split(","));
                fieldOptions.forEach(fieldOption -> {
                    if (defaultIds.contains(fieldOption.getId().toString())) {
                        fieldOption.setIsDefault(true);
                    } else {
                        fieldOption.setIsDefault(false);
                    }
                });
                List<String> encryptList = EncryptionUtils.encryptListToStr(defaultIds);
                fieldDetailDTO.setDefaultValue(StringUtils.join(encryptList.toArray(),","));
            } else {
                fieldOptions.forEach(fieldOption -> {
                    fieldOption.setIsDefault(false);
                });
            }
            fieldDetailDTO.setFieldOptions(fieldOptions);
        }
        FieldValueUtil.handleDefaultValue(fieldDetailDTO);
        return fieldDetailDTO;
    }

    @Override
    public void delete(Long organizationId, Long projectId, Long fieldId) {
        ObjectSchemeFieldDTO field = baseQueryById(organizationId, projectId, fieldId);
        //组织层无法删除项目层
        if (projectId == null && field.getProjectId() != null) {
            throw new CommonException(ERROR_FIELD_ILLEGAL);
        }
        //项目层无法删除组织层
        if (projectId != null && field.getProjectId() == null) {
            throw new CommonException(ERROR_FIELD_ILLEGAL);
        }
        //无法删除系统字段
        if (field.getSystem()) {
            throw new CommonException(ERROR_FIELD_ILLEGAL);
        }
        baseDelete(fieldId);
        //删除pageFields
        pageFieldService.deleteByFieldId(fieldId);
        //删除字段值
        fieldValueService.deleteByFieldId(fieldId);
        //删除日志
        fieldDataLogService.deleteByFieldId(projectId, fieldId);
    }

    @Override
    public ObjectSchemeFieldDetailVO update(Long organizationId, Long projectId, Long fieldId, ObjectSchemeFieldUpdateVO updateDTO) {
        //处理字段选项
        if (updateDTO.getFieldOptions() != null) {
            String defaultIds = fieldOptionService.handleFieldOption(organizationId, fieldId, updateDTO.getFieldOptions());
            if (defaultIds != null && !"".equals(defaultIds)) {
                updateDTO.setDefaultValue(defaultIds);
            }
        }
        ObjectSchemeFieldDTO update = modelMapper.map(updateDTO, ObjectSchemeFieldDTO.class);
        //处理context
        String[] contexts = updateDTO.getContext();
        if (contexts != null && contexts.length != 0) {
            for (String context : contexts) {
                if (!EnumUtil.contain(ObjectSchemeFieldContext.class, context)) {
                    throw new CommonException(ERROR_CONTEXT_ILLEGAL);
                }
            }
            update.setContext(Arrays.asList(contexts).stream().collect(Collectors.joining(",")));
        }
        String defaultValue = tryDecryptDefaultValue(update.getDefaultValue());
        if (defaultValue != null) {
            update.setDefaultValue(defaultValue);
        }
        update.setId(fieldId);
        baseUpdate(update);
        return queryById(organizationId, projectId, fieldId);
    }

    private String tryDecryptDefaultValue(String defaultValue) {
        try {
            return EncryptionUtils.decrypt(defaultValue);
        } catch (Exception e) {
            //do nothing
        }
        return null;
    }

    @Override
    public Boolean checkName(Long organizationId, Long projectId, String name, String schemeCode) {
        if (!EnumUtil.contain(ObjectSchemeCode.class, schemeCode)) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        ObjectSchemeFieldSearchVO search = new ObjectSchemeFieldSearchVO();
        search.setName(name);
        search.setSchemeCode(schemeCode);
        return !listQuery(organizationId, projectId, search).isEmpty();
    }

    @Override
    public Boolean checkCode(Long organizationId, Long projectId, String code, String schemeCode) {
        if (!EnumUtil.contain(ObjectSchemeCode.class, schemeCode)) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        ObjectSchemeFieldSearchVO search = new ObjectSchemeFieldSearchVO();
        search.setCode(code);
        search.setSchemeCode(schemeCode);
        return !listQuery(organizationId, projectId, search).isEmpty();
    }

    @Override
    public List<AgileIssueHeadVO> getIssueHeadForAgile(Long organizationId, Long projectId, String schemeCode) {
        if (!EnumUtil.contain(ObjectSchemeCode.class, schemeCode)) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        ObjectSchemeFieldSearchVO searchDTO = new ObjectSchemeFieldSearchVO();
        searchDTO.setSchemeCode(schemeCode);
        List<ObjectSchemeFieldDTO> objectSchemeFields = listQuery(organizationId, projectId, searchDTO)
                .stream().filter(objectSchemeField -> !objectSchemeField.getSystem()).collect(Collectors.toList());
        List<AgileIssueHeadVO> agileIssueHeadDTOS = new ArrayList<>();
        objectSchemeFields.forEach(objectSchemeField -> {
            AgileIssueHeadVO agileIssueHeadDTO = new AgileIssueHeadVO();
            agileIssueHeadDTO.setTitle(objectSchemeField.getName());
            agileIssueHeadDTO.setCode(objectSchemeField.getCode());
            agileIssueHeadDTO.setSortId(objectSchemeField.getCode());
            agileIssueHeadDTO.setFieldType(objectSchemeField.getFieldType());
            agileIssueHeadDTOS.add(agileIssueHeadDTO);
        });
        return agileIssueHeadDTOS;
    }

    @Override
    public List<ObjectSchemeFieldDetailVO> queryCustomFieldList(Long projectId) {
        List<ObjectSchemeFieldDetailVO> objectSchemeFieldDetailVOList = objectSchemeFieldMapper.selectCustomFieldList(ConvertUtil.getOrganizationId(projectId),projectId);
        if (objectSchemeFieldDetailVOList != null && !objectSchemeFieldDetailVOList.isEmpty()) {
            return objectSchemeFieldDetailVOList;
        } else {
            return new ArrayList<>();
        }
    }

    @Override
    public ObjectSchemeFieldDTO selectById(Long fieldId) {
        return objectSchemeFieldMapper.selectByPrimaryKey(fieldId);
    }

    @Override
    public void config(Long organizationId, Long projectId, PageConfigUpdateVO pageConfigUpdateVO) {
        String issueType = pageConfigUpdateVO.getIssueType();
        List<PageConfigUpdateVO.Field> fields = pageConfigUpdateVO.getFields();
        IssueTypeFieldVO issueTypeFieldVO = pageConfigUpdateVO.getIssueTypeFieldVO();
        Set<Long> deleteIds = pageConfigUpdateVO.getDeleteIds();

        ObjectSchemeFieldContext.isIllegalIssueTypes(issueType);
        if (!ObjectUtils.isEmpty(fields)) {
            updateFieldConfig(organizationId, projectId, issueType, fields);
        }
        if (!ObjectUtils.isEmpty(projectId)
                && !ObjectUtils.isEmpty(issueTypeFieldVO)
                && !StringUtils.isEmpty(issueTypeFieldVO.getTemplate())) {
            updateTemplate(organizationId, projectId, issueType, issueTypeFieldVO);
        }
        if (!ObjectUtils.isEmpty(deleteIds)) {
            deleteFieldConfig(organizationId, projectId, deleteIds);
        }
    }

    private void deleteFieldConfig(Long organizationId, Long projectId, Set<Long> deleteIds) {
        boolean editOnProjectLevel = (projectId != null);
        if (editOnProjectLevel) {
            //项目层无法删除组织层的字段
            List<ObjectSchemeFieldDTO> objectSchemeFieldList =
                    objectSchemeFieldMapper.selectByExtendIds(deleteIds);
            objectSchemeFieldList.forEach(o -> {
                if (o.getProjectId() == null) {
                    throw new CommonException("error.project.can.not.delete.organization.field");
                }
            });
            ObjectSchemeFieldExtendDTO example = new ObjectSchemeFieldExtendDTO();
            example.setOrganizationId(organizationId);
            example.setProjectId(projectId);
            deleteIds.forEach(d -> {
                ObjectSchemeFieldExtendDTO extend =
                        objectSchemeFieldExtendMapper.selectByPrimaryKey(d);
                Long fieldId = extend.getFieldId();
                example.setFieldId(fieldId);
                if (objectSchemeFieldExtendMapper.select(example).size() <= 1) {
                    //删除最后一个关联关系时，同时删除字段
                    objectSchemeFieldMapper.deleteByPrimaryKey(fieldId);
                }
                objectSchemeFieldExtendMapper.deleteByPrimaryKey(d);
            });
        } else {
            deleteIds.forEach(d -> {
                ObjectSchemeFieldExtendDTO extend =
                        objectSchemeFieldExtendMapper.selectByPrimaryKey(d);
                Long fieldId = extend.getFieldId();
                if (objectSchemeFieldExtendMapper
                        .selectOrganizationExtendField(null, organizationId, fieldId).size() <= 1) {
                    //删除最后一个关联关系时，同时删除字段
                    objectSchemeFieldMapper.deleteByPrimaryKey(fieldId);
                }
                ObjectSchemeFieldExtendDTO target = new ObjectSchemeFieldExtendDTO();
                target.setOrganizationId(organizationId);
                target.setFieldId(fieldId);
                target.setIssueTypeId(extend.getIssueTypeId());
                target.setIssueType(extend.getIssueType());
                objectSchemeFieldExtendMapper.delete(target);
            });
        }
    }

    @Override
    public List<PageConfigVO> listConfigs(Long organizationId, Long projectId, String issueType) {
        return objectSchemeFieldExtendMapper.listConfigs(organizationId, projectId, issueType);
    }

    private void updateTemplate(Long organizationId, Long projectId, String issueType, IssueTypeFieldVO issueTypeFieldVO) {
        Map<String, Long> issueTypeMap = getIssueTypeMap(organizationId);
        Long issueTypeId = issueTypeMap.get(issueType);
        if (ObjectUtils.isEmpty(issueTypeId)) {
            throw new CommonException("error.issue.type.not.existed", issueType);
        }
        IssueTypeFieldDTO dto = new IssueTypeFieldDTO();
        dto.setProjectId(projectId);
        dto.setIssueTypeId(issueTypeId);
        List<IssueTypeFieldDTO> result = issueTypeFieldMapper.select(dto);
        if (result.isEmpty()) {
            //create
            dto.setTemplate(issueTypeFieldVO.getTemplate());
            issueTypeFieldMapper.insertSelective(dto);
        } else {
            //update
            Long objectVersionNumber = issueTypeFieldVO.getObjectVersionNumber();
            if (ObjectUtils.isEmpty(objectVersionNumber)) {
                throw new CommonException("error.issueTypeField.objectVersionNumber.null");
            }
            IssueTypeFieldDTO target = result.get(0);
            target.setObjectVersionNumber(objectVersionNumber);
            target.setTemplate(issueTypeFieldVO.getTemplate());
            if (issueTypeFieldMapper.updateByPrimaryKeySelective(target) != 1) {
                throw new CommonException("error.issueTypeField.update");
            }
        }
    }

    private void updateFieldConfig(Long organizationId, Long projectId, String issueType, List<PageConfigUpdateVO.Field> fields) {
        boolean onProjectLevel = (projectId != null);
        Map<String, Long> issueTypeMap = getIssueTypeMap(organizationId);
        fields.forEach(f -> {
            Long fieldId = f.getFieldId();
            if (ObjectUtils.isEmpty(f.getRequired())
                    || ObjectUtils.isEmpty(f.getCreated())
                    || ObjectUtils.isEmpty(f.getEdited())) {
                throw new CommonException("error.page.config.field.selectBox.empty");
            }
            if (ObjectUtils.isEmpty(f.getObjectVersionNumber())) {
                throw new CommonException("error.page.config.field.objectVersionNumber.null");
            }
            if (onProjectLevel) {
                //查询字段配置是否存在，存在则更新不存在则创建
                ObjectSchemeFieldExtendDTO dto = new ObjectSchemeFieldExtendDTO();
                dto.setIssueType(issueType);
                dto.setOrganizationId(organizationId);
                dto.setFieldId(fieldId);
                dto.setProjectId(projectId);
                List<ObjectSchemeFieldExtendDTO> result = objectSchemeFieldExtendMapper.select(dto);
                if (result.isEmpty()) {
                    dto.setIssueTypeId(
                            Optional
                                    .ofNullable(issueTypeMap.get(issueType))
                                    .orElse(0L));
                    dto.setRequired(f.getRequired());
                    dto.setCreated(f.getCreated());
                    dto.setEdited(f.getEdited());
                    objectSchemeFieldExtendMapper.insertSelective(dto);
                } else {
                    updateObjectSchemeFieldExtend(f, result);
                }
            } else {
                List<ObjectSchemeFieldExtendDTO> result =
                        objectSchemeFieldExtendMapper.selectOrganizationExtendField(issueType, organizationId, fieldId);
                if (result.isEmpty()) {
                    throw new CommonException("error.page.config.field.not.existed");
                } else {
                    updateObjectSchemeFieldExtend(f, result);
                }
            }
        });
    }

    private void updateObjectSchemeFieldExtend(PageConfigUpdateVO.Field field, List<ObjectSchemeFieldExtendDTO> result) {
        ObjectSchemeFieldExtendDTO target = result.get(0);
        target.setRequired(field.getRequired());
        target.setEdited(field.getEdited());
        target.setCreated(field.getCreated());
        target.setObjectVersionNumber(field.getObjectVersionNumber());
        if (objectSchemeFieldExtendMapper.updateByPrimaryKeySelective(target) != 1) {
            throw new CommonException("error.page.config.field.update");
        }
    }
}
