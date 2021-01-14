package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import org.apache.commons.beanutils.ConvertUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.core.exception.CommonException;
import org.springframework.util.ObjectUtils;


/**
 * @author shinan.chen
 * @since 2019/3/29
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ObjectSchemeFieldServiceImpl implements ObjectSchemeFieldService {
    protected static final String ERROR_FIELD_ILLEGAL = "error.field.illegal";
    protected static final String ERROR_FIELD_CREATE = "error.field.create";
    protected static final String ERROR_FIELD_NOTFOUND = "error.field.notFound";
    protected static final String ERROR_FIELD_UPDATE = "error.field.update";
    protected static final String ERROR_SCHEMECODE_ILLEGAL = "error.schemeCode.illegal";
    protected static final String ERROR_FIELDTYPE_ILLEGAL = "error.fieldType.illegal";
    protected static final String ERROR_FIELD_NAMEEXIST = "error.field.nameExist";
    protected static final String ERROR_FIELD_CODEEXIST = "error.field.codeExist";
    @Autowired
    protected ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    protected ObjectSchemeMapper objectSchemeMapper;
    @Autowired
    protected FieldOptionService fieldOptionService;
    @Autowired
    protected FieldValueService fieldValueService;
    @Autowired
    protected FieldDataLogService fieldDataLogService;
    @Autowired
    protected ModelMapper modelMapper;
    @Autowired
    protected ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired
    protected IssueTypeService issueTypeService;
    @Autowired
    private IssueTypeFieldMapper issueTypeFieldMapper;
    @Autowired
    private FieldOptionService optionService;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private IssueLabelMapper issueLabelMapper;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private ProductVersionMapper productVersionMapper;

    @Override
    public ObjectSchemeFieldDTO baseCreate(ObjectSchemeFieldDTO field,
                                           List<IssueTypeVO> issueTypes,
                                           String issueTypeForRank) {
        Long organizationId = field.getOrganizationId();
        Long projectId = field.getProjectId();
        field.setSystem(false);
        field.setRequired(false);
        if (objectSchemeFieldMapper.insert(field) != 1) {
            throw new CommonException(ERROR_FIELD_CREATE);
        }
        String defaultValue = field.getDefaultValue();
        Boolean extraConfig = field.getExtraConfig();
        //  创建object_scheme_field_extend
        insertExtendList(organizationId, projectId, field, issueTypes, issueTypeForRank, defaultValue, extraConfig);
        return objectSchemeFieldMapper.selectByPrimaryKey(field.getId());
    }

    private void insertExtendList(Long organizationId,
                                  Long projectId,
                                  ObjectSchemeFieldDTO field,
                                  List<IssueTypeVO> issueTypes,
                                  String issueTypeForRank,
                                  String defaultValue,
                                  Boolean extraConfig) {
        Long fieldId = field.getId();
        String rank = field.getRank();
        for (IssueTypeVO issueType : issueTypes) {
            String type = issueType.getTypeCode();
            Long typeId = issueType.getId();
            ObjectSchemeFieldExtendDTO dto = new ObjectSchemeFieldExtendDTO();
            dto.setIssueType(type);
            dto.setProjectId(projectId);
            dto.setOrganizationId(organizationId);
            dto.setFieldId(fieldId);
            if (objectSchemeFieldExtendMapper.select(dto).isEmpty()) {
                dto.setRequired(false);
                dto.setCreated(true);
                dto.setEdited(true);
                dto.setIssueTypeId(typeId);
                dto.setDefaultValue(defaultValue);
                dto.setExtraConfig(extraConfig);
                if (Objects.equals(type, issueTypeForRank)
                        && !StringUtils.isEmpty(rank)) {
                    dto.setRank(rank);
                } else {
                    String minRank = getMinRank(organizationId, projectId, type, null);
                    dto.setRank(minRank);
                }
                objectSchemeFieldExtendMapper.insert(dto);
            }
        }
    }

    private String getMinRank(Long organizationId, Long projectId, String issueType, String minRank) {
        if (ObjectUtils.isEmpty(minRank)) {
            String rank = objectSchemeFieldExtendMapper.selectMinRank(organizationId, projectId, issueType);
            if (ObjectUtils.isEmpty(rank)) {
                minRank = RankUtil.mid();
            } else {
                minRank = rank;
            }
        }
        return RankUtil.genPre(minRank);
    }

    @Override
    public void baseUpdate(ObjectSchemeFieldDTO field) {
        if (objectSchemeFieldMapper.updateByPrimaryKeySelective(field) != 1) {
            throw new CommonException(ERROR_FIELD_UPDATE);
        }
    }

    @Override
    public ObjectSchemeFieldDTO baseQueryById(Long organizationId, Long projectId, Long fieldId) {
        ObjectSchemeFieldDTO field = selectOneByFieldId(organizationId, projectId, fieldId);
        if (!field.getOrganizationId().equals(organizationId) && !field.getOrganizationId().equals(0L)) {
            throw new CommonException(ERROR_FIELD_ILLEGAL);
        }
        if (field.getProjectId() != null && !field.getProjectId().equals(projectId) && !field.getProjectId().equals(0L)) {
            throw new CommonException(ERROR_FIELD_ILLEGAL);
        }
        return field;
    }

    private ObjectSchemeFieldDTO selectOneByFieldId(Long organizationId,
                                                    Long projectId,
                                                    Long fieldId) {
        List<ObjectSchemeFieldDTO> dtoList =
                objectSchemeFieldMapper.selectByOptions(organizationId, projectId, ObjectSchemeCode.AGILE_ISSUE, fieldId, null, null);
        if (dtoList.isEmpty()) {
            throw new CommonException(ERROR_FIELD_NOTFOUND);
        } else {
            return dtoList.get(0);
        }
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
        createSystemFieldIfNotExisted(organizationId);
        List<ObjectSchemeFieldVO> fieldViews = generateFieldViews(organizationId, projectId, schemeCode);
        ObjectSchemeDTO select = new ObjectSchemeDTO();
        select.setSchemeCode(schemeCode);
        result.put("name", objectSchemeMapper.selectOne(select).getName());
        result.put("content", fieldViews);
        return result;
    }

    protected List<ObjectSchemeFieldVO> generateFieldViews(Long organizationId, Long projectId, String schemeCode) {
        List<ObjectSchemeFieldDTO> fields = selectFieldsByOptions(organizationId, projectId, schemeCode, null, null);
        List<ObjectSchemeFieldVO> fieldViews = new ArrayList<>();
        List<IssueTypeVO> issueTypeVOS = issueTypeService.queryByOrgId(organizationId);
        Map<String, IssueTypeVO> issueTypeVOMap = issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getTypeCode, Function.identity()));
        fields.forEach(f -> {
            ObjectSchemeFieldVO vo = modelMapper.map(f, ObjectSchemeFieldVO.class);
            List<ObjectSchemeFieldExtendDTO> extendList = f.getExtendFields();
            List<String> issueTypes = new ArrayList<>();
            List<String> issueTypeNames = new ArrayList<>();
            boolean containsAllIssueTypes = containsAllIssueTypes(organizationId, projectId, issueTypes);
            String requiredScope =
                    processIssueTyeAndRequiredScope(f,issueTypes, issueTypeNames, true, extendList, containsAllIssueTypes);
            vo.setContext(String.join(",", issueTypes));
            vo.setContexts(issueTypes);
            if (!CollectionUtils.isEmpty(issueTypes) && CollectionUtils.isEmpty(issueTypeNames)) {
                issueTypes.forEach(issueType -> {
                    IssueTypeVO issueTypeVO = issueTypeVOMap.get(issueType);
                    if (!ObjectUtils.isEmpty(issueTypeVO)) {
                        issueTypeNames.add(issueTypeVO.getName());
                    }
                });
            }
            vo.setContextName(String.join(",", issueTypeNames));
            vo.setRequiredScope(requiredScope);
            fieldViews.add(vo);
        });
        return fieldViews;
    }

    @Override
    public List<ObjectSchemeFieldVO> listPageFieldWithOption(Long organizationId, Long projectId, String schemeCode,
                                                         List<String> issueTypeList) {
        List<ObjectSchemeFieldVO> fieldVOS = generateFieldViews(organizationId, projectId, schemeCode);
        List<ObjectSchemeFieldDetailVO> objectSchemeFieldDetailVOList = objectSchemeFieldMapper.selectCustomFieldList(ConvertUtil.getOrganizationId(projectId), projectId, null);
        if (CollectionUtils.isEmpty(objectSchemeFieldDetailVOList)){
            return fieldVOS.stream()
                    .filter(vo -> CollectionUtils.isEmpty(issueTypeList) || issueTypeList.stream().anyMatch(item -> vo.getContexts().contains(item)))
                    .collect(Collectors.toList());
        }
        Map<Long, ObjectSchemeFieldDetailVO> map =
                objectSchemeFieldDetailVOList.stream().collect(Collectors.toMap(ObjectSchemeFieldDetailVO::getId,
                        Function.identity()));
        return fieldVOS.stream()
                .filter(vo -> CollectionUtils.isEmpty(issueTypeList) || issueTypeList.stream().anyMatch(item -> vo.getContexts().contains(item)))
                .peek(vo ->vo.setFieldOptions(map.getOrDefault(vo.getId(), new ObjectSchemeFieldDetailVO()).getFieldOptions()))
                .collect(Collectors.toList());
    }

    protected List<ObjectSchemeFieldDTO> selectFieldsByOptions(Long organizationId,
                                                               Long projectId,
                                                               String schemeCode,
                                                               Long fieldId,
                                                               Long issueTypeId) {
        List<String> issueTypes = null;
        Boolean isProgram = false;
        if (!ObjectUtils.isEmpty(projectId)) {
            ProjectVO body = ConvertUtil.queryProjectWithoutAgile(projectId);
            if (!ObjectUtils.isEmpty(body) && Objects.equals(body.getCategory(),"PROGRAM")) {
                isProgram = true;
                if(agilePluginService != null){
                    issueTypes = agilePluginService.addProgramIssueType();
                }
            } else {
                issueTypes = new ArrayList<>(ObjectSchemeFieldContext.NORMAL_PROJECT);
            }
            if (backlogExpandService != null) {
                if (Boolean.TRUE.equals(backlogExpandService.enabled(projectId))) {
                    issueTypes.add(ObjectSchemeFieldContext.BACKLOG);
                }
            }
        }
        List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS = objectSchemeFieldMapper.selectByOptions(organizationId, projectId, schemeCode, fieldId, issueTypeId, issueTypes);
        addNotSyncedField(objectSchemeFieldDTOS,issueTypes);
        if(isProgram && agilePluginService != null){
           return agilePluginService.filterProgramEpic(objectSchemeFieldDTOS);
        }
        return objectSchemeFieldDTOS;
    }

    private void addNotSyncedField(List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS, List<String> issueTypes) {
        List<Long> systemFieldIds = objectSchemeFieldDTOS.stream().filter(v -> Boolean.TRUE.equals(v.getSystem())).map(ObjectSchemeFieldDTO::getId).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(systemFieldIds)) {
            return;
        }
        List<ObjectSchemeFieldDTO> notSyncFields = objectSchemeFieldMapper.selectNotSyncField(systemFieldIds);
        if (CollectionUtils.isEmpty(issueTypes)) {
            objectSchemeFieldDTOS.addAll(notSyncFields);
            return;
        }
        List<ObjectSchemeFieldDTO> notSyncFilterFields = notSyncFields.stream().filter(v -> {
            List<String> contexts = new ArrayList<>(Arrays.asList(getFieldContext(v.getCode()).split(",")));
            if (CollectionUtils.isEmpty(contexts)) {
                return false;
            }
            contexts.retainAll(issueTypes);
            return !CollectionUtils.isEmpty(contexts);
        }).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(notSyncFilterFields)) {
            objectSchemeFieldDTOS.addAll(notSyncFilterFields);
        }
    }

    @Override
    public void createSystemFieldIfNotExisted(Long organizationId) {
        List<ObjectSchemeFieldExtendDTO> result =
                objectSchemeFieldExtendMapper
                        .selectExtendField(
                                ObjectSchemeFieldContext.fixDataIssueType(),
                                organizationId,
                                null,
                                null);
        if (result.isEmpty()) {
            ObjectSchemeFieldDTO dto = new ObjectSchemeFieldDTO();
            dto.setSystem(true);
            List<ObjectSchemeFieldDTO> systemFields = objectSchemeFieldMapper.select(dto);
            systemFields.forEach(field -> {
                String context = getFieldContext(field.getCode());
                List<IssueTypeDTO> issueTypes = convertContextToIssueTypes(context, organizationId);
                String code = field.getCode();
                Boolean required = field.getRequired();
                SystemFieldPageConfig.CommonField commonField = SystemFieldPageConfig.CommonField.queryByField(code);
                Boolean created;
                Boolean edited;
                if (ObjectUtils.isEmpty(commonField)) {
                    created = false;
                    edited = false;
                } else {
                    created = commonField.created();
                    edited = commonField.edited();
                }
                issueTypes.forEach(issueType -> {
                    ObjectSchemeFieldExtendDTO extendField = new ObjectSchemeFieldExtendDTO();
                    extendField.setFieldId(field.getId());
                    extendField.setOrganizationId(organizationId);
                    extendField.setIssueType(issueType.getTypeCode());
                    extendField.setIssueTypeId(issueType.getId());
                    extendField.setRequired(required);
                    extendField.setCreated(created);
                    extendField.setEdited(edited);
                    extendField.setRank(getMinRank(organizationId, null, issueType.getTypeCode(), null));
                    objectSchemeFieldExtendMapper.insertSelective(extendField);
                });

            });
        }
    }

    @Override
    public List<IssueTypeVO> issueTypes(Long organizationId, Long projectId) {
        List<IssueTypeVO> issueTypes = issueTypeService.queryByOrgId(organizationId);
        //组织没有backlog类型的数据，则不反悔backlog类型
        boolean containsBacklog =
                !objectSchemeFieldExtendMapper
                        .selectExtendField(Arrays.asList(ObjectSchemeFieldContext.BACKLOG), organizationId, null, null)
                        .isEmpty();
        List<IssueTypeVO> result = new ArrayList<>();
        for (IssueTypeVO vo : issueTypes) {
            String typeCode = vo.getTypeCode();
            if (ObjectSchemeFieldContext.getIssueTye().contains(typeCode)) {
                if (ObjectSchemeFieldContext.BACKLOG.equals(typeCode)) {
                    if (containsBacklog) {
                        result.add(vo);
                    }
                } else {
                    result.add(vo);
                }
            }
        }
        return filterBacklog(projectId, result);
    }

    private List<IssueTypeDTO> convertContextToIssueTypes(String context, Long organizationId) {
        List<IssueTypeDTO> result = new ArrayList<>();
        Map<String, Long> issueTypeMap = issueTypeService.queryIssueTypeMap(organizationId);
        List<String> fixDataIssueTypes = ObjectSchemeFieldContext.fixDataIssueType();
        String[] contextArray = context.split(",");
        if (ObjectSchemeFieldContext.isGlobal(contextArray)) {
            for (Map.Entry<String, Long> entry : issueTypeMap.entrySet()) {
                String issueType = entry.getKey();
                if (fixDataIssueTypes.contains(issueType)) {
                    IssueTypeDTO dto = new IssueTypeDTO();
                    dto.setId(entry.getValue());
                    dto.setTypeCode(entry.getKey());
                    result.add(dto);
                }
            }
        } else {
            for (String ctx : contextArray) {
                Long id = issueTypeMap.get(ctx);
                if (ObjectUtils.isEmpty(id)
                        || !fixDataIssueTypes.contains(ctx)) {
                    continue;
                }
                IssueTypeDTO dto = new IssueTypeDTO();
                dto.setId(id);
                dto.setTypeCode(ctx);
                result.add(dto);
            }
        }
        return result;
    }

    private String processIssueTyeAndRequiredScope(ObjectSchemeFieldDTO fieldDTO,
                                                   List<String> issueTypes,
                                                   List<String> issueTypeNames,
                                                   boolean resetIssueType,
                                                   List<ObjectSchemeFieldExtendDTO> extendList,
                                                   boolean containsAllIssueTypes) {
        boolean allIsRequired = true;
        boolean allIsNotRequired = false;
        if (CollectionUtils.isEmpty(extendList)) {
            String fieldContext = getFieldContext(fieldDTO.getCode());
            issueTypes.addAll(Arrays.asList(fieldContext.split(",")));
            allIsRequired = allIsRequired && fieldDTO.getRequired();
            allIsNotRequired = allIsNotRequired || fieldDTO.getRequired();
        }
        for (ObjectSchemeFieldExtendDTO e : extendList) {
            issueTypes.add(e.getIssueType());
            issueTypeNames.add(e.getIssueTypeName());
            allIsRequired = allIsRequired && e.getRequired();
            allIsNotRequired = allIsNotRequired || e.getRequired();
        }
        if (resetIssueType && containsAllIssueTypes) {
            issueTypes.clear();
            issueTypeNames.clear();
            issueTypes.add(ObjectSchemeFieldContext.GLOBAL);
            issueTypeNames.add("全部类型");
        }
        if (allIsRequired) {
            return ObjectSchemeFieldRequiredScope.ALL.name();
        } else if (!allIsNotRequired) {
            return ObjectSchemeFieldRequiredScope.NONE.name();
        } else {
            return ObjectSchemeFieldRequiredScope.PART.name();
        }
    }

    @Override
    public Boolean containsAllIssueTypes(Long organizationId, Long projectId, List<String> issueTypes) {
        List<IssueTypeVO> issueTypeList = issueTypeService.queryByOrgId(organizationId);
        issueTypeList =
                issueTypeList
                        .stream()
                        .filter(i -> ObjectSchemeFieldContext.getIssueTye().contains(i.getTypeCode()))
                        .collect(Collectors.toList());
        issueTypeList = filterBacklog(projectId, issueTypeList);
        Boolean result = true;
        for (IssueTypeVO vo : issueTypeList) {
            result =  result && issueTypes.contains(vo.getTypeCode());
        }
        return result;
    }

    private List<IssueTypeVO> filterBacklog(Long projectId, List<IssueTypeVO> issueTypeList) {
        if (backlogExpandService == null) {
            return issueTypeList;
        }
        if (!ObjectUtils.isEmpty(projectId) && Boolean.FALSE.equals(backlogExpandService.enabled(projectId))) {
            //没有开启需求池，则去除backlog类型
            issueTypeList =
                    issueTypeList
                            .stream()
                            .filter(i -> !ObjectSchemeFieldContext.BACKLOG.equals(i.getTypeCode()))
                            .collect(Collectors.toList());
        }
        return issueTypeList;
    }

    @Override
    public IssueTypeFieldVO queryDescriptionTemplate(Long projectId,
                                                     String issueType,
                                                     Long organizationId) {
        List<IssueTypeVO> issueTypes =
                issueTypeService.queryByOrgId(organizationId)
                        .stream()
                        .filter(i -> i.getTypeCode().equals(issueType))
                        .collect(Collectors.toList());
        if (ObjectUtils.isEmpty(issueTypes)) {
            throw new CommonException("error.illegal.issueType");
        }
        Long issueTypeId = issueTypes.get(0).getId();
        IssueTypeFieldDTO dto = new IssueTypeFieldDTO();
        dto.setIssueTypeId(issueTypeId);
        dto.setProjectId(projectId);
        IssueTypeFieldDTO result = issueTypeFieldMapper.selectOne(dto);
        if (ObjectUtils.isEmpty(result)) {
            return null;
        }
        return modelMapper.map(result, IssueTypeFieldVO.class);
    }

    @Override
    public ObjectSchemeFieldDetailVO create(Long organizationId,
                                            Long projectId,
                                            ObjectSchemeFieldCreateVO fieldCreateDTO,
                                            String issueTypeForRank) {
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
        List<IssueTypeVO> issueTypes = getIssueTypeByContexts(contexts, organizationId, projectId);
        ObjectSchemeFieldDTO field = modelMapper.map(fieldCreateDTO, ObjectSchemeFieldDTO.class);
        field.setContext(Arrays.asList(fieldCreateDTO.getContext()).stream().collect(Collectors.joining(",")));
        field.setOrganizationId(organizationId);
        field.setProjectId(projectId);

        String defaultValue = tryDecryptDefaultValue(field.getFieldType(), field.getDefaultValue());
        if (defaultValue != null) {
            field.setDefaultValue(defaultValue);
        }
        field = baseCreate(field, issueTypes, issueTypeForRank);
        //处理字段选项
        if (fieldCreateDTO.getFieldOptions() != null) {
            String defaultIds = fieldOptionService.handleFieldOption(organizationId, field.getId(), fieldCreateDTO.getFieldOptions());
            if (defaultIds != null && !"".equals(defaultIds)) {
                field.setDefaultValue(defaultIds);
                objectSchemeFieldMapper.updateOptional(field, "defaultValue");
                objectSchemeFieldExtendMapper.selectExtendField(null, organizationId, field.getId(), projectId).stream().forEach(f -> {
                    f.setDefaultValue(defaultIds);
                    objectSchemeFieldExtendMapper.updateByPrimaryKey(f);
                });
            }
        }
        if (!ObjectUtils.isEmpty(issueTypeForRank)
                && Arrays.asList(contexts).contains(issueTypeForRank)) {
            Map<String, Long> issueTypeMap = issueTypeService.queryIssueTypeMap(organizationId);
            insertObjectSchemeFieldExtend(organizationId, projectId, field.getId(), fieldCreateDTO.getRequired(), issueTypeMap, issueTypeForRank, fieldCreateDTO.getCreated(), fieldCreateDTO.getEdited(), field.getDefaultValue(), field.getExtraConfig());
        }
        return queryById(organizationId, projectId, field.getId());
    }

    @Override
    public String getFieldContext(String code) {
        List<String> contexts = new ArrayList<>();
        List<AgileSystemFieldContext> values = Arrays.asList(AgileSystemFieldContext.values());
        AgileSystemFieldContext agileSystemFieldContext = values.stream().filter(v -> code.equals(v.getFieldCode())).findAny().orElse(null);
        if (!ObjectUtils.isEmpty(agileSystemFieldContext)) {
            String context = agileSystemFieldContext.getContext();
            contexts.addAll(Arrays.asList(context.split(",")));
        }
        if (agilePluginService != null) {
            String context = agilePluginService.getSystemFieldContext(code);
            if (!ObjectUtils.isEmpty(context)) {
                contexts.add(context);
            }
        }
        if (backlogExpandService != null) {
            String context = backlogExpandService.getSystemFieldContext(code);
            if (!ObjectUtils.isEmpty(context)) {
                contexts.add(context);
            }
        }
        return contexts.stream().collect(Collectors.joining(","));
    }

    protected List<IssueTypeVO> getIssueTypeByContexts(String[] contexts,
                                                       Long organizationId,
                                                       Long projectId) {
        ObjectSchemeFieldContext.isIllegalContexts(contexts);
        List<String> contextArray = Arrays.asList(contexts);
        List<IssueTypeVO> issueTypes = issueTypeService.queryByOrgId(organizationId);
        if (ObjectUtils.isEmpty(projectId)) {
            return issueTypes
                    .stream()
                    .filter(i -> ObjectSchemeFieldContext.getIssueTye().contains(i.getTypeCode())
                            && contextArray.contains(i.getTypeCode()))
                    .collect(Collectors.toList());
        } else {
            List<IssueTypeVO> backlogs = queryBacklogIssueType(projectId, issueTypes, contextArray);
            List<IssueTypeVO> issueTypeList = queryProjectIssueType(projectId, issueTypes, contextArray);
            issueTypeList.addAll(backlogs);
            return issueTypeList;
        }
    }

    protected List<IssueTypeVO> queryProjectIssueType(Long projectId,
                                                    List<IssueTypeVO> issueTypes,
                                                    List<String> contextArray) {
            if(agilePluginService != null){
                return agilePluginService.queryProgramIssueType(projectId,issueTypes,contextArray);
            }
            else {
                return issueTypes
                        .stream()
                        .filter(i -> ObjectSchemeFieldContext.NORMAL_PROJECT.contains(i.getTypeCode())
                                && contextArray.contains(i.getTypeCode()))
                        .collect(Collectors.toList());
            }
    }

    @Override
    public ObjectSchemeFieldDetailVO queryById(Long organizationId, Long projectId, Long fieldId) {
        ObjectSchemeFieldDTO field = baseQueryById(organizationId, projectId, fieldId);
        List<ObjectSchemeFieldExtendDTO> extendList = field.getExtendFields();
        List<String> issueTypes = new ArrayList<>();
        List<String> issueTypeNames = new ArrayList<>();
        boolean containsAllIssueTypes = containsAllIssueTypes(organizationId, projectId, issueTypes);
        String requiredScope =
                processIssueTyeAndRequiredScope(field,issueTypes, issueTypeNames, false, extendList, containsAllIssueTypes);
        ObjectSchemeFieldDetailVO fieldDetailDTO = modelMapper.map(field, ObjectSchemeFieldDetailVO.class);
        fieldDetailDTO.setContext(issueTypes.toArray(new String[issueTypes.size()]));
        fieldDetailDTO.setRequiredScope(requiredScope);
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
                fieldDetailDTO.setDefaultValue(StringUtils.join(encryptList.toArray(), ","));
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

        objectSchemeFieldMapper.cascadeDelete(organizationId, projectId, fieldId);
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
        String context = Arrays.asList(contexts).stream().filter(string -> !string.isEmpty()).collect(Collectors.joining(","));
        update.setContext(context);
        String defaultValue = tryDecryptDefaultValue(update.getFieldType(), update.getDefaultValue());
        updateFieldIssueTypeAndDefaultValue(organizationId, projectId, fieldId, contexts, defaultValue, update.getExtraConfig());
        if (defaultValue != null) {
            update.setDefaultValue(defaultValue);
        }
        update.setId(fieldId);
        baseUpdate(update);
        return queryById(organizationId, projectId, fieldId);
    }

    private void updateFieldIssueTypeAndDefaultValue(Long organizationId,
                                                     Long projectId,
                                                     Long fieldId,
                                                     String[] contexts,
                                                     String defaultValue,
                                                     Boolean extraConfig) {
        if (ObjectUtils.isEmpty(contexts)) {
            throw new CommonException("error.field.context.empty");
        }

        List<IssueTypeVO> issueTypes = getIssueTypeByContexts(contexts, organizationId, projectId);
        Map<String, Long> issueTypeMap =
                issueTypes.stream().collect(Collectors.toMap(IssueTypeVO::getTypeCode, IssueTypeVO::getId));
        List<String> contextList = new ArrayList<>(issueTypeMap.keySet());
        ObjectSchemeFieldDTO field =
                selectOneByFieldId(organizationId, projectId, fieldId);
        List<ObjectSchemeFieldExtendDTO> intersection = new ArrayList<>();
        List<ObjectSchemeFieldExtendDTO> deleteList = new ArrayList<>();
        Set<String> insertSet = new HashSet<>();
        filterByIssueType(intersection, deleteList, insertSet, contextList, field);

        dealWithExtendFields(organizationId, projectId, fieldId, deleteList, insertSet, issueTypeMap, defaultValue, extraConfig);
    }

    private void dealWithExtendFields(Long organizationId,
                                      Long projectId,
                                      Long fieldId,
                                      List<ObjectSchemeFieldExtendDTO> deleteList,
                                      Set<String> insertSet,
                                      Map<String, Long> issueTypeMap,
                                      String defaultValue,
                                      Boolean extraConfig) {
        boolean onProjectLevel = (projectId != null);
        if (onProjectLevel) {
            deleteList.forEach(d -> objectSchemeFieldExtendMapper.deleteByPrimaryKey(d));
            insertSet.forEach(i -> insertObjectSchemeFieldExtend(organizationId, projectId, fieldId, false, issueTypeMap, i, true, true, defaultValue, extraConfig));
        } else {
            //组织层新增或删除，项目层数据同时新增或删除
            deleteList.forEach(d -> {
                String issueType = d.getIssueType();
                ObjectSchemeFieldExtendDTO target = new ObjectSchemeFieldExtendDTO();
                target.setIssueType(issueType);
                target.setOrganizationId(organizationId);
                target.setFieldId(fieldId);
                objectSchemeFieldExtendMapper.delete(target);
            });
            //查询该组织下已经配置过的项目，这些项目要级联创建字段类型
            Set<Long> projectIds =
                    objectSchemeFieldExtendMapper.selectProjectIdsByOrganizationId(organizationId);
            insertSet.forEach(i -> {
                insertObjectSchemeFieldExtend(organizationId, null, fieldId, false, issueTypeMap, i, true, true, defaultValue, extraConfig);
                projectIds.forEach(p -> insertObjectSchemeFieldExtend(organizationId, p, fieldId, false, issueTypeMap, i, true, true, defaultValue, extraConfig));
            });
        }

    }

    private void updateExtendDefaultValue(Long organizationId,
                                          Long projectId,
                                          Long fieldId,
                                          String defaultValue,
                                          List<String> issueTypes,
                                          Boolean extraConfig) {
        if (!CollectionUtils.isEmpty(issueTypes)) {
            objectSchemeFieldExtendMapper.selectExtendField(null, organizationId, fieldId, projectId)
                    .forEach(i -> {
                        if (issueTypes.contains(i.getIssueType())) {
                            i.setDefaultValue(defaultValue);
                            //同步修改默认值时，同步修改额外配置
                            i.setExtraConfig(extraConfig);
                            objectSchemeFieldExtendMapper.updateByPrimaryKey(i);
                        }
                    });
        }
    }

    @Override
    public void syncDefaultValue(Long organizationId, Long projectId, Long fieldId, String syncDefaultValueIssueTypes, Boolean extraConfig) {

        String defaultValue = getObjectSchemeFieldByFieldId(organizationId, projectId, fieldId).getDefaultValue();

        List<String> issueTypes = Arrays.asList(syncDefaultValueIssueTypes.split(","));
        updateExtendDefaultValue(organizationId, projectId, fieldId, defaultValue, issueTypes, extraConfig);
    }

    private ObjectSchemeFieldDTO getObjectSchemeFieldByFieldId(Long organizationId, Long projectId, Long fieldId) {
        ObjectSchemeFieldDTO search = new ObjectSchemeFieldDTO();
        search.setId(fieldId);
        search.setProjectId(projectId);
        search.setOrganizationId(organizationId);

        return objectSchemeFieldMapper.selectOne(search);
    }

    private void filterByIssueType(List<ObjectSchemeFieldExtendDTO> intersection,
                                   List<ObjectSchemeFieldExtendDTO> deleteList,
                                   Set<String> insertSet,
                                   List<String> contextList,
                                   ObjectSchemeFieldDTO field) {
        List<ObjectSchemeFieldExtendDTO> extendList = field.getExtendFields();
        //交集
        extendList.forEach(e -> {
            if (contextList.contains(e.getIssueType())) {
                intersection.add(e);
            }
        });
        Set<String> intersectionIssueTypes =
                intersection.stream().map(ObjectSchemeFieldExtendDTO::getIssueType).collect(Collectors.toSet());
        //删除的类型
        extendList.forEach(e -> {
            if (!intersectionIssueTypes.contains(e.getIssueType())) {
                deleteList.add(e);
            }
        });
        //插入的类型
        contextList.forEach(c -> {
            if (!intersectionIssueTypes.contains(c)) {
                insertSet.add(c);
            }
        });
    }

    private ObjectSchemeFieldExtendDTO insertObjectSchemeFieldExtend(Long organizationId,
                                                                     Long projectId,
                                                                     Long fieldId,
                                                                     Boolean required,
                                                                     Map<String, Long> issueTypeMap,
                                                                     String issueType,
                                                                     Boolean created,
                                                                     Boolean edited,
                                                                     String defaultValue,
                                                                     Boolean extraConfig) {
        ObjectSchemeFieldExtendDTO dto = new ObjectSchemeFieldExtendDTO();
        dto.setIssueType(issueType);
        dto.setFieldId(fieldId);
        dto.setOrganizationId(organizationId);

        List<ObjectSchemeFieldExtendDTO> existedList;
        if (ObjectUtils.isEmpty(projectId)) {
            existedList = objectSchemeFieldExtendMapper.selectExtendField(Arrays.asList(issueType), organizationId, fieldId, null);
        } else {
            dto.setProjectId(projectId);
            existedList = objectSchemeFieldExtendMapper.select(dto);
        }
        Long extendId = null;
        if (existedList.isEmpty()) {
            dto.setIssueTypeId(issueTypeMap.get(issueType));
            dto.setRequired(required);
            dto.setCreated(created);
            dto.setEdited(edited);
            dto.setRank(getMinRank(organizationId, projectId, issueType, null));
            dto.setDefaultValue(defaultValue);
            dto.setExtraConfig(extraConfig);
            objectSchemeFieldExtendMapper.insertSelective(dto);
            extendId = dto.getId();
        } else {
            ObjectSchemeFieldExtendDTO existedExtendField = existedList.get(0);
            existedExtendField.setCreated(Optional.ofNullable(created).orElse(true));
            existedExtendField.setRequired(required);
            existedExtendField.setEdited(Optional.ofNullable(edited).orElse(true));
            existedExtendField.setDefaultValue(defaultValue);
            dto.setExtraConfig(extraConfig);
            if (objectSchemeFieldExtendMapper.updateByPrimaryKeySelective(existedExtendField) != 1) {
                throw new CommonException("error.extend.field.update");
            }
            extendId = existedExtendField.getId();
        }
        return objectSchemeFieldExtendMapper.selectByPrimaryKey(extendId);
    }

    private String tryDecryptDefaultValue(String fieldType, String defaultValue) {
        if (EncryptContext.isEncrypt()) {
            if (Objects.equals(FieldType.MULTI_MEMBER, fieldType) && !ObjectUtils.isEmpty(defaultValue)) {
                String[] splits = defaultValue.split(",");
                List<String> list = new ArrayList<>();
                for (String split : splits) {
                    list.add(EncryptionUtils.decrypt(split));
                }
                return list.stream().collect(Collectors.joining(","));
            } else {
                try {
                    return EncryptionUtils.decrypt(defaultValue);
                } catch (Exception e) {
                    //do nothing
                }
                return null;
            }
        }
        return defaultValue;
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
    public List<AgileIssueHeadVO> getIssueHeadForAgile(Long organizationId, Long projectId, String schemeCode, String issueTypeList) {
        if (!EnumUtil.contain(ObjectSchemeCode.class, schemeCode)) {
            throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
        }
        ObjectSchemeFieldSearchVO searchDTO = new ObjectSchemeFieldSearchVO();
        searchDTO.setSchemeCode(schemeCode);
        searchDTO.setIssueTypeList(issueTypeList);
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
    public List<ObjectSchemeFieldDetailVO> queryCustomFieldList(Long projectId, String issueTypeList) {
        List<ObjectSchemeFieldDetailVO> objectSchemeFieldDetailVOList = objectSchemeFieldMapper.selectCustomFieldList(ConvertUtil.getOrganizationId(projectId), projectId, issueTypeList);
        if (objectSchemeFieldDetailVOList != null && !objectSchemeFieldDetailVOList.isEmpty()) {
            return objectSchemeFieldDetailVOList;
        } else {
            return new ArrayList<>();
        }
    }

    @Override
    public List<ObjectSchemeFieldDetailVO> listFieldsWithOptionals(Long projectId, Long issueTypeId, Long organizationId) {
        List<ObjectSchemeFieldDetailVO> list = objectSchemeFieldMapper.selectFieldsWithOptionals(organizationId, projectId, issueTypeId, null);
        // 增加在扩展表中未同步的系统字段
        IssueTypeVO issueTypeVO = issueTypeService.queryById(ConvertUtil.getOrganizationId(organizationId), issueTypeId);
        List<String> fieldCodes = getIssueTypeFieldCodes(issueTypeVO.getTypeCode());
        List<String> existFieldCodes = list.stream().filter(v -> Boolean.TRUE.equals(v.getSystem())).map(ObjectSchemeFieldDetailVO::getCode).collect(Collectors.toList());
        fieldCodes.removeAll(existFieldCodes);
        if (!CollectionUtils.isEmpty(fieldCodes)) {
            list.addAll(objectSchemeFieldMapper.selectFieldsByFieldCodes(fieldCodes));
        }
        return list;
    }

    private List<String> getIssueTypeFieldCodes(String typeCode) {
        List<String> fieldCodeS = new ArrayList<>();
        List<AgileSystemFieldContext> agileSystemFieldContexts = Arrays.asList(AgileSystemFieldContext.values());
        agileSystemFieldContexts.stream().filter(v -> Arrays.asList(v.getContext().split(",")).contains(typeCode)).forEach(pageConfigFieldVO -> fieldCodeS.add(pageConfigFieldVO.getFieldCode()));
        if (agilePluginService != null) {
            agilePluginService.getIssueTypeFieldCodes(fieldCodeS,typeCode);
        }
        if (backlogExpandService != null) {
            backlogExpandService.getBacklogFieldCodes(fieldCodeS,typeCode);
        }
        return fieldCodeS;
    }

    @Override
    public void updateRequired(Long organizationId, Long projectId, Long fieldId, Boolean required) {
        if (ObjectUtils.isEmpty(required)) {
            throw new CommonException("error.field.required.null");
        }
        boolean onProjectLevel = (projectId != null);
        if (onProjectLevel) {
            List<ObjectSchemeFieldExtendDTO> extendList =
                    objectSchemeFieldExtendMapper.selectExtendField(null, organizationId, fieldId, projectId);
            if (extendList.isEmpty()) {
                //项目层暂未配置，查组织层并新建
                extendList = objectSchemeFieldExtendMapper.selectExtendField(null, organizationId, fieldId, null);
            }
            Map<String, Long> issueTypeMap = issueTypeService.queryIssueTypeMap(organizationId);
            ObjectSchemeFieldDTO objectSchemeFieldDTO = getObjectSchemeFieldByFieldId(organizationId, projectId, fieldId);
            extendList.forEach(e ->
                    insertObjectSchemeFieldExtend(organizationId, projectId, fieldId, required, issueTypeMap, e.getIssueType(), e.getCreated(), e.getEdited(), objectSchemeFieldDTO.getDefaultValue(), objectSchemeFieldDTO.getExtraConfig()));
        } else {
            objectSchemeFieldExtendMapper.batchUpdateRequired(null, organizationId, fieldId, required);
        }
    }

    @Override
    public String queryRank(String previousRank, String nextRank) {
        if (!ObjectUtils.isEmpty(previousRank) || !ObjectUtils.isEmpty(nextRank)) {
            if (StringUtils.isEmpty(previousRank)) {
                return RankUtil.genPre(nextRank);
            } else if (StringUtils.isEmpty(nextRank)) {
                return RankUtil.genNext(previousRank);
            } else {
                return RankUtil.between(nextRank, previousRank);
            }
        } else {
            throw new CommonException("error.at.least.one.rank");
        }
    }

    @Override
    public List<ObjectSchemeFieldVO> selectMemberList(Long organizationId, Long projectId, String schemeCode, Long issueTypeId, List<String> fieldCodeList) {
        List<ObjectSchemeFieldDTO> list =
                objectSchemeFieldMapper.selectMemberByOptions(organizationId, projectId, schemeCode, issueTypeId, fieldCodeList, null);
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }
        return list.stream().map(f -> modelMapper.map(f, ObjectSchemeFieldVO.class)).collect(Collectors.toList());
    }

    @Override
    public List<ObjectSchemeFieldVO> unselected(Long organizationId, Long projectId, String issueType) {
        List<ObjectSchemeFieldVO> unselected = objectSchemeFieldExtendMapper.unselected(organizationId, projectId, issueType);
        if (CollectionUtils.isEmpty(unselected)) {
            return new ArrayList<>();
        }
        unselected.forEach(v -> {
            //获取字段选项，并设置默认值
            List<FieldOptionVO> fieldOptions = fieldOptionService.queryByFieldId(organizationId, v.getId());
            if (!fieldOptions.isEmpty()) {
                if (!ObjectUtils.isEmpty(v.getDefaultValue())) {
                    List<String> defaultIds = Arrays.asList(v.getDefaultValue().split(","));
                    List<String> encryptList = EncryptionUtils.encryptListToStr(defaultIds);
                    v.setDefaultValue(StringUtils.join(encryptList.toArray(), ","));
                }
                v.setFieldOptions(fieldOptions);
            }
            if (FieldType.MEMBER.equals(v.getFieldType())) {
                BaseFeignClient baseFeignClient = SpringBeanUtil.getBean(BaseFeignClient.class);
                if (v.getDefaultValue() != null && !"".equals(v.getDefaultValue())) {
                    Long defaultValue = Long.valueOf(String.valueOf(v.getDefaultValue()));
                    v.setDefaultValue(EncryptionUtils.encrypt(defaultValue));
                    List<UserDTO> list = baseFeignClient.listUsersByIds(Arrays.asList(defaultValue).toArray(new Long[1]), false).getBody();
                    if (!list.isEmpty()) {
                        v.setDefaultValueObj(list.get(0));
                    }
                }
            }
        });
        return unselected;
    }

    @Override
    public ObjectSchemeFieldDTO selectById(Long fieldId) {
        return objectSchemeFieldMapper.selectByPrimaryKey(fieldId);
    }

    @Override
    public void config(Long organizationId, Long projectId, PageConfigUpdateVO pageConfigUpdateVO) {
        String issueType = pageConfigUpdateVO.getIssueType();
        List<PageConfigFieldVO> fields = pageConfigUpdateVO.getFields();
        IssueTypeFieldVO issueTypeFieldVO = pageConfigUpdateVO.getIssueTypeFieldVO();
        Set<Long> deleteIds = pageConfigUpdateVO.getDeleteIds();

        ObjectSchemeFieldContext.isIllegalIssueType(issueType);
        Map<String, Long> issueTypeMap = issueTypeService.queryIssueTypeMap(organizationId);
        if (!ObjectUtils.isEmpty(fields)) {
            updateFieldConfig(organizationId, projectId, issueType, fields, issueTypeMap);
        }
        if (!ObjectUtils.isEmpty(projectId)
                && !ObjectUtils.isEmpty(issueTypeFieldVO)
                && !StringUtils.isEmpty(issueTypeFieldVO.getTemplate())) {
            updateTemplate(projectId, issueType, issueTypeFieldVO, issueTypeMap);
        }
        if (!ObjectUtils.isEmpty(deleteIds)) {
            deleteFieldConfig(organizationId, projectId, deleteIds);
        }
        List<ObjectSchemeFieldCreateVO> createdField = pageConfigUpdateVO.getCreatedFields();
        if (!ObjectUtils.isEmpty(createdField)) {
            createdField.forEach(c -> create(organizationId, projectId, c, issueType));
        }
        List<PageConfigFieldVO> addFields = pageConfigUpdateVO.getAddFields();
        if (!ObjectUtils.isEmpty(addFields)) {
            addFieldConfig(organizationId, projectId, addFields, issueType, issueTypeMap);
        }
    }

    private void addFieldConfig(Long organizationId,
                                Long projectId,
                                List<PageConfigFieldVO> addFields,
                                String issueType,
                                Map<String, Long> issueTypeMap) {
        addFields.forEach(a -> {
            Long fieldId = a.getFieldId();
            String rank = a.getRank();
            if (a.getFieldOptions() != null) {
                List<FieldOptionUpdateVO> fieldOptionUpdateVOList = modelMapper.map(a.getFieldOptions(), new TypeToken<List<FieldOptionUpdateVO>>(){}.getType());
                String defaultIds = fieldOptionService.handleFieldOption(organizationId, fieldId, fieldOptionUpdateVOList);
                if (defaultIds != null && !"".equals(defaultIds)) {
                    a.setDefaultValue(defaultIds);
                }
            }
            String defaultValue = tryDecryptDefaultValue(a.getFieldType(), a.getDefaultValue().toString());
            if (defaultValue != null) {
                a.setDefaultValue(defaultValue);
            }
            ObjectSchemeFieldExtendDTO result =
                    insertObjectSchemeFieldExtend(organizationId, projectId, fieldId, a.getRequired(), issueTypeMap, issueType, a.getCreated(), a.getEdited() ,a.getDefaultValue().toString(), a.getExtraConfig());
            if (!ObjectUtils.isEmpty(rank)) {
                result.setRank(rank);
                objectSchemeFieldExtendMapper.updateByPrimaryKeySelective(result);
            }
        });
    }

    private void deleteFieldConfig(Long organizationId, Long projectId, Set<Long> deleteIds) {
        boolean editOnProjectLevel = (projectId != null);
        if (editOnProjectLevel) {
            //项目层无法删除组织层的字段
            List<ObjectSchemeFieldDTO> objectSchemeFieldList =
                    objectSchemeFieldMapper.selectByExtendIds(deleteIds);
            objectSchemeFieldList.forEach(o -> {
                if (o.getProjectId() == null || Objects.equals(0L, o.getOrganizationId())) {
                    //系统字段或者组织层字段
                    throw new CommonException("error.project.can.not.delete.organization.or.system.field");
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
                        .selectExtendField(null, organizationId, fieldId, null).size() <= 1) {
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
    public PageConfigVO listConfigs(Long organizationId, Long projectId, String issueType) {
        PageConfigVO result = new PageConfigVO();
        List<PageConfigFieldVO> pageConfigFields = queryPageConfigFields(organizationId, projectId, issueType);
        result.setFields(pageConfigFields);
        //处理默认值
        processDefaultValue(pageConfigFields, organizationId, projectId);
        //处理字段是否可被编辑
        processFieldEdited(issueType, pageConfigFields);
        if (!ObjectUtils.isEmpty(projectId)) {
            Map<String, Long> issueTypeMap = issueTypeService.queryIssueTypeMap(organizationId);
            Long issueTypeId = issueTypeMap.get(issueType);
            if (ObjectUtils.isEmpty(issueTypeId)) {
                throw new CommonException("error.issue.type.not.existed");
            }
            IssueTypeFieldDTO dto = new IssueTypeFieldDTO();
            dto.setIssueTypeId(issueTypeId);
            dto.setProjectId(projectId);
            List<IssueTypeFieldDTO> list = issueTypeFieldMapper.select(dto);
            if (!list.isEmpty()) {
                result.setIssueTypeFieldVO(modelMapper.map(list.get(0), IssueTypeFieldVO.class));
            }
        }
        return result;
    }

    protected List<PageConfigFieldVO> queryPageConfigFields(Long organizationId, Long projectId, String issueType) {
        List<PageConfigFieldVO> pageConfigFieldVOS = objectSchemeFieldExtendMapper.listConfigs(organizationId, projectId, issueType);
        // 有新增系统字段未配置到字段扩展表直接添加字段到pageConfigFieldVOS
        addPageFieldVOS(pageConfigFieldVOS,organizationId,projectId,issueType);
        if (agilePluginService != null) {
           return agilePluginService.queryProgramPageConfigFields(projectId,issueType,pageConfigFieldVOS);
        }
        return pageConfigFieldVOS;
    }

    private void addPageFieldVOS(List<PageConfigFieldVO> pageConfigFieldVOS, Long organizationId, Long projectId, String issueType) {
        List<PageConfigFieldVO> systemFieldS = pageConfigFieldVOS.stream().filter(pageConfigFieldVO -> Objects.equals(pageConfigFieldVO.getCreatedLevel(), "system")).collect(Collectors.toList());
        // 当前问题类型的预定义字段
        List<String> fieldCodeS = getIssueTypeFieldCodes(issueType);
        if (!CollectionUtils.isEmpty(fieldCodeS)) {
            PageConfigFieldVO pageConfigFieldVO = pageConfigFieldVOS.get(pageConfigFieldVOS.size() - 1);
            List<String> existField = systemFieldS.stream().map(PageConfigFieldVO::getFieldCode).collect(Collectors.toList());
            fieldCodeS.removeAll(existField);
            if (CollectionUtils.isEmpty(fieldCodeS)) {
                return;
            }
            String endRank = pageConfigFieldVO.getRank();
            List<PageConfigFieldVO> configFieldVOS = objectSchemeFieldExtendMapper.listConfigsByFieldCodes(fieldCodeS);
            for (PageConfigFieldVO configFieldVO : configFieldVOS) {
                SystemFieldPageConfig.CommonField commonField = SystemFieldPageConfig.CommonField.queryByField(configFieldVO.getFieldCode());
                if (!ObjectUtils.isEmpty(commonField)) {
                    configFieldVO.setCreated(commonField.created());
                    configFieldVO.setEdited(commonField.edited());
                }
                String preRank = RankUtil.genPre(endRank);
                configFieldVO.setRank(preRank);
                endRank = preRank;
                configFieldVO.setIssueType(issueType);
            }
            pageConfigFieldVOS.addAll(configFieldVOS);
        }
    }

    private void processFieldEdited(String issueType, List<PageConfigFieldVO> pageConfigFields) {
        Map<String, PageConfigFieldEditedVO> map = new HashMap<>();
        Map<String, PageConfigFieldEditedVO> fieldEditedVOMap = SystemFieldCanNotEdit.fieldEdited(issueType);
        if (!ObjectUtils.isEmpty(fieldEditedVOMap)) {
            map.putAll(fieldEditedVOMap);
        }
        if (backlogExpandService != null) {
            map.putAll(backlogExpandService.fieldEdited(issueType));
        }
        if (!ObjectUtils.isEmpty(map)) {
            pageConfigFields.forEach(p -> {
                String fieldCode = p.getFieldCode();
                PageConfigFieldEditedVO fieldEdited = map.get(fieldCode);
                if (!ObjectUtils.isEmpty(fieldCode)) {
                    p.setPageConfigFieldEdited(fieldEdited);
                }
            });
        }
    }

    private void processDefaultValue(List<PageConfigFieldVO> pageConfigFields,
                                     Long organizationId,
                                     Long projectId) {
        List<PageFieldViewVO> pageFieldViews = new ArrayList<>();
        pageConfigFields.forEach(p -> {
            PageFieldViewVO vo = new PageFieldViewVO();
            vo.setFieldId(p.getFieldId());
            vo.setDefaultValue(p.getDefaultValue());
            vo.setFieldType(p.getFieldType());
            vo.setExtraConfig(p.getExtraConfig());
            vo.setFieldCode(p.getFieldCode());
            pageFieldViews.add(vo);
        });
        optionService.fillOptions(organizationId, null, pageFieldViews);
        FieldValueUtil.handleDefaultValue(pageFieldViews);
        setDefaultValueObjs(pageFieldViews, projectId, organizationId);
        Map<Long, PageFieldViewVO> pageFieldViewMap =
                pageFieldViews.stream().collect(Collectors.toMap(PageFieldViewVO::getFieldId, x -> x));
        pageConfigFields.forEach(p -> {
            Long fieldId = p.getFieldId();
            PageFieldViewVO vo = pageFieldViewMap.get(fieldId);
            if (!ObjectUtils.isEmpty(vo)) {
                p.setDefaultValue(vo.getDefaultValue());
                p.setDefaultValueObj(vo.getDefaultValueObj());
                p.setFieldOptions(vo.getFieldOptions());
                p.setDefaultValueObjs(vo.getDefaultValueObjs());
            }
        });
    }

    @Override
    public void setDefaultValueObjs(List<PageFieldViewVO> pageFieldViews, Long projectId, Long organizationId) {
        //模块、标签、冲刺、史诗、修复的版本、影响的版本
        for (PageFieldViewVO view : pageFieldViews) {
            Object defaultValue = view.getDefaultValue();
            if (!Objects.isNull(defaultValue) && !Objects.equals(defaultValue.toString(), "")) {
                List<Long> defaultIds;
                String[] ids;
                long defaultId;
                List<Object> defaultObjs = new ArrayList<>();
                switch (view.getFieldCode()) {
                    //多选
                    case "component":
                        List<IssueComponentVO> issueComponentList = modelMapper.map(issueComponentMapper.selectByProjectId(projectId), new TypeToken<List<IssueComponentVO>>(){}.getType());
                        Map<Long, IssueComponentVO> issueComponentMap = issueComponentList.stream().collect(Collectors.toMap(IssueComponentVO::getComponentId, Function.identity()));

                        ids = (String[]) view.getDefaultValue();
                        defaultIds = Arrays.asList((Long[]) ConvertUtils.convert(ids, Long.class));
                        if (!CollectionUtils.isEmpty(defaultIds)) {
                            defaultIds.forEach(id -> {
                                if (issueComponentMap.containsKey(id)) {
                                    defaultObjs.add(issueComponentMap.get(id));
                                }
                            });
                            view.setDefaultValueObjs(defaultObjs);
                        }
                        break;
                    //多选
                    case "label":
                        List<IssueLabelVO> issueLabelList = modelMapper.map(issueLabelMapper.selectByProjectIds(Collections.singletonList(projectId)), new TypeToken<List<IssueLabelVO>>(){}.getType());
                        Map<Long, IssueLabelVO> issueLabelMap = issueLabelList.stream().collect(Collectors.toMap(IssueLabelVO::getLabelId, Function.identity()));

                        ids = (String[]) view.getDefaultValue();
                        defaultIds = Arrays.asList((Long[]) ConvertUtils.convert(ids, Long.class));
                        if (!CollectionUtils.isEmpty(defaultIds)) {
                            defaultIds.forEach(id -> {
                                if (issueLabelMap.containsKey(id)) {
                                    defaultObjs.add(issueLabelMap.get(id));
                                }
                            });
                            view.setDefaultValueObjs(defaultObjs);
                        }
                        break;
                    //多选
                    case "influenceVersion":
                        List<ProductVersionNameVO> influenceVersionList = modelMapper.map(productVersionMapper.queryNameByOptions(projectId, null), new TypeToken<List<ProductVersionNameVO>>(){}.getType());
                        Map<Long, ProductVersionNameVO> influenceVersionMap = influenceVersionList.stream().collect(Collectors.toMap(ProductVersionNameVO::getVersionId, Function.identity()));

                        ids = (String[]) view.getDefaultValue();
                        defaultIds = Arrays.asList((Long[]) ConvertUtils.convert(ids, Long.class));
                        if (!CollectionUtils.isEmpty(defaultIds)) {
                            defaultIds.forEach(id -> {
                                if (influenceVersionMap.containsKey(id)) {
                                    defaultObjs.add(influenceVersionMap.get(id));
                                }
                            });
                            view.setDefaultValueObjs(defaultObjs);
                        }
                        break;
                    //多选
                    case "fixVersion":
                        List<ProductVersionNameVO> fixVersionList = modelMapper.map(productVersionMapper.queryNameByOptions(projectId, Collections.singletonList("version_planning")), new TypeToken<List<ProductVersionNameVO>>(){}.getType());
                        Map<Long, ProductVersionNameVO> fixVersionMap = fixVersionList.stream().collect(Collectors.toMap(ProductVersionNameVO::getVersionId, Function.identity()));

                        ids = (String[]) view.getDefaultValue();
                        defaultIds = Arrays.asList((Long[]) ConvertUtils.convert(ids, Long.class));
                        if (!CollectionUtils.isEmpty(defaultIds)) {
                            defaultIds.forEach(id -> {
                                if (fixVersionMap.containsKey(id)) {
                                    defaultObjs.add(fixVersionMap.get(id));
                                }
                            });
                            view.setDefaultValueObjs(defaultObjs);
                        }
                        break;
                    //单选
                    case "sprint":
                        List<String> sprintStatusCodes = Arrays.asList("sprint_planning", "started");
                        List<SprintNameVO> sprintNameList = modelMapper.map(sprintMapper.queryNameByOptions(Collections.singletonList(projectId), sprintStatusCodes), new TypeToken<List<SprintNameVO>>(){}.getType());
                        Map<Long, SprintNameVO> sprintNameMap = sprintNameList.stream().collect(Collectors.toMap(SprintNameVO::getSprintId, Function.identity()));

                        defaultId = Long.parseLong(view.getDefaultValue().toString());
                        if (sprintNameMap.containsKey(defaultId)) {
                            view.setDefaultValueObj(sprintNameMap.get(defaultId));
                        }
                        break;
                    //单选
                    case "epic":
                        List<EpicDataVO> epicDataList = modelMapper.map(issueMapper.queryEpicList(projectId), new TypeToken<List<EpicDataVO>>(){}.getType());
                        Map<Long, EpicDataVO> epicDataMap = epicDataList.stream().collect(Collectors.toMap(EpicDataVO::getIssueId, Function.identity()));

                        defaultId = Long.parseLong(view.getDefaultValue().toString());
                        if (epicDataMap.containsKey(defaultId)) {
                            view.setDefaultValueObj(epicDataMap.get(defaultId));
                        }
                        break;
                    case "pi":
                        if (agilePluginService != null) {
                            agilePluginService.setBussinessDefaultValueObjs(pageFieldViews, projectId, organizationId);
                        }
                        break;
                    case "backlogClassification":
                    case "backlogType":
                        if (backlogExpandService != null) {
                            backlogExpandService.setBacklogDefaultValueObjs(pageFieldViews, projectId, organizationId);
                        }
                        break;
                    default:
                        break;
                }
            }
        }
    }

    private void updateTemplate(Long projectId,
                                String issueType,
                                IssueTypeFieldVO issueTypeFieldVO,
                                Map<String, Long> issueTypeMap) {
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

    private void updateFieldConfig(Long organizationId,
                                   Long projectId,
                                   String issueType,
                                   List<PageConfigFieldVO> fields,
                                   Map<String, Long> issueTypeMap) {
        boolean onProjectLevel = (projectId != null);
        fields.forEach(f -> {
            Long fieldId = f.getFieldId();
            if (ObjectUtils.isEmpty(f.getRequired())
                    || ObjectUtils.isEmpty(f.getCreated())
                    || ObjectUtils.isEmpty(f.getEdited())) {
                throw new CommonException("error.page.config.field.selectBox.empty");
            }
            //查询字段配置是否存在，存在则更新不存在则创建
            ObjectSchemeFieldExtendDTO dto = new ObjectSchemeFieldExtendDTO();
            dto.setIssueType(issueType);
            dto.setOrganizationId(organizationId);
            dto.setFieldId(fieldId);
            dto.setProjectId(projectId);
            List<ObjectSchemeFieldExtendDTO> result = objectSchemeFieldExtendMapper.select(dto);
            Long issueTypeId = issueTypeMap.get(issueType);
            if (Boolean.FALSE.equals(onProjectLevel)) {
                result = result.stream().filter(v -> ObjectUtils.isEmpty(v.getProjectId())).collect(Collectors.toList());
            }
            //处理字段选项
            if (f.getFieldOptions() != null) {
                List<FieldOptionUpdateVO> fieldOptionUpdateVOList = modelMapper.map(f.getFieldOptions(), new TypeToken<List<FieldOptionUpdateVO>>(){}.getType());
                String defaultIds = fieldOptionService.handleFieldOption(organizationId, fieldId, fieldOptionUpdateVOList);
                f.setDefaultValue(defaultIds);
            }
            String defaultValue = tryDecryptDefaultValue(f.getFieldType(), f.getDefaultValue().toString());
            if (defaultValue != null) {
                f.setDefaultValue(defaultValue);
            }
            if (result.isEmpty() && !ObjectUtils.isEmpty(issueTypeId)) {
                dto.setIssueTypeId(issueTypeId);
                dto.setRequired(f.getRequired());
                dto.setCreated(f.getCreated());
                dto.setEdited(f.getEdited());
                dto.setRank(f.getRank());
                dto.setDefaultValue(f.getDefaultValue().toString());
                dto.setExtraConfig(f.getExtraConfig());
                objectSchemeFieldExtendMapper.insertSelective(dto);
            } else {
                updateObjectSchemeFieldExtend(f, result);
            }
        });
    }

    private void updateObjectSchemeFieldExtend(PageConfigFieldVO field, List<ObjectSchemeFieldExtendDTO> result) {
        ObjectSchemeFieldExtendDTO target = result.get(0);
        target.setRequired(field.getRequired());
        target.setEdited(field.getEdited());
        target.setCreated(field.getCreated());
        target.setRank(field.getRank());
        target.setDefaultValue(field.getDefaultValue().toString());
        target.setObjectVersionNumber(field.getObjectVersionNumber());
        target.setExtraConfig(field.getExtraConfig());
        if (objectSchemeFieldExtendMapper.updateByPrimaryKeySelective(target) != 1) {
            throw new CommonException("error.page.config.field.update");
        }
    }

    private List<IssueTypeVO> queryBacklogIssueType(Long projectId,
                                                    List<IssueTypeVO> issueTypes,
                                                    List<String> contextArray) {
        if (backlogExpandService == null) {
            return new ArrayList<>();
        }
        Boolean backlogEnabled = backlogExpandService.enabled(projectId);
        List<IssueTypeVO> result = new ArrayList<>();
        if (Boolean.TRUE.equals(backlogEnabled)) {
            result.addAll(
                    issueTypes
                            .stream()
                            .filter(i -> ObjectSchemeFieldContext.BACKLOG.equals(i.getTypeCode())
                                    && contextArray.contains(i.getTypeCode()))
                            .collect(Collectors.toList()));
        }
        return result;
    }
}
