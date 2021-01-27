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
    protected static final String CREATED_LEVEL_SYSTEM = "system";
    protected static final String CREATED_LEVEL_ORGANIZATION = "organization";
    protected static final String ISSUE_TYPE_SOURCE_SYSTEM = "system";
    protected static final String ISSUE_TYPE_SOURCE_ORGANIZATION = "organization";
    protected static final String ISSUE_TYPE_SOURCE_PROJECT = "project";

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
    @Autowired
    private IssueLabelService issueLabelService;
    @Autowired
    private IssueTypeMapper issueTypeMapper;

    @Override
    public ObjectSchemeFieldDTO baseCreate(ObjectSchemeFieldDTO field,
                                           List<IssueTypeVO> issueTypes,
                                           Long issueTypeIdForRank) {
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
        insertExtendList(organizationId, projectId, field, issueTypes, issueTypeIdForRank, defaultValue, extraConfig);
        return objectSchemeFieldMapper.selectByPrimaryKey(field.getId());
    }

    private void insertExtendList(Long organizationId,
                                  Long projectId,
                                  ObjectSchemeFieldDTO field,
                                  List<IssueTypeVO> issueTypes,
                                  Long issueTypeIdForRank,
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
                if (Objects.equals(typeId, issueTypeIdForRank)
                        && !StringUtils.isEmpty(rank)) {
                    dto.setRank(rank);
                } else {
                    String minRank = getMinRank(organizationId, projectId, typeId, null);
                    dto.setRank(minRank);
                }
                objectSchemeFieldExtendMapper.insert(dto);
            }
        }
    }

    private String getMinRank(Long organizationId, Long projectId, Long issueTypeId, String minRank) {
        if (ObjectUtils.isEmpty(minRank)) {
            String rank = objectSchemeFieldExtendMapper.selectMinRankByIssueTypeId(organizationId, projectId, issueTypeId);
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
        List<IssueTypeVO> issueTypeVOS = issueTypeService.queryByOrgId(organizationId, projectId);
        List<IssueTypeVO> filterIssueTypeVO = filterIssueType(projectId, issueTypeVOS, issueTypeVOS.stream().map(IssueTypeVO::getId).collect(Collectors.toList()));
        Map<Long, IssueTypeVO> issueTypeVOMap = filterIssueTypeVO.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));
        fields.forEach(f -> {
            ObjectSchemeFieldVO vo = modelMapper.map(f, ObjectSchemeFieldVO.class);
            List<ObjectSchemeFieldExtendDTO> extendList = f.getExtendFields();
            List<Long> issueTypeIds = new ArrayList<>();
            List<String> issueTypeNames = new ArrayList<>();
            List<IssueTypeVO> issueTypeVOList = new ArrayList<>();
            boolean containsAllIssueTypes = containsAllIssueTypes(organizationId, projectId, issueTypeIds);
            String requiredScope =
                    processIssueTyeAndRequiredScope(f,issueTypeIds, issueTypeNames, true, extendList, containsAllIssueTypes, organizationId, projectId);
            vo.setContext(StringUtils.join(issueTypeIds.toArray(), ","));

            if (!CollectionUtils.isEmpty(issueTypeIds)) {
                List<String> contexts = new ArrayList<>();
                issueTypeIds.forEach(issueTypeId -> {
                    IssueTypeVO issueTypeVO = issueTypeVOMap.get(issueTypeId);
                    if (!ObjectUtils.isEmpty(issueTypeVO)) {
                        issueTypeNames.add(issueTypeVO.getName());
                        issueTypeVOList.add(issueTypeVO);
                        contexts.add(issueTypeVO.getTypeCode());
                    }
                });
                vo.setContexts(contexts);
                vo.setIssueTypeVOList(issueTypeVOList);
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
            if (!ObjectUtils.isEmpty(body) && ProjectCategory.checkContainProjectCategory(body.getCategories(),ProjectCategory.MODULE_PROGRAM)) {
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
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setTypeCodes(ObjectSchemeFieldContext.fixDataIssueType());
        issueTypeSearchVO.setSource("system");
        List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, 0L, issueTypeSearchVO);
        List<Long> issueTypeIds = issueTypes.stream().map(IssueTypeVO::getId).collect(Collectors.toList());
        List<ObjectSchemeFieldExtendDTO> result =
                objectSchemeFieldExtendMapper
                        .selectExtendFieldByOptions(
                                issueTypeIds,
                                organizationId,
                                null,
                                null);
        if (result.isEmpty()) {
            ObjectSchemeFieldDTO dto = new ObjectSchemeFieldDTO();
            dto.setSystem(true);
            List<ObjectSchemeFieldDTO> systemFields = objectSchemeFieldMapper.select(dto);
            systemFields.forEach(field -> {
//                String context = getFieldContext(field.getCode());
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
                    extendField.setRank(getMinRank(organizationId, null, issueType.getId(), null));
                    objectSchemeFieldExtendMapper.insertSelective(extendField);
                });

            });
        }
    }

    @Override
    public List<IssueTypeVO> issueTypes(Long organizationId, Long projectId) {
        List<IssueTypeVO> issueTypes = issueTypeService.queryByOrgId(organizationId, projectId);
        List<Long> issueTypeIds = issueTypes.stream().map(IssueTypeVO::getId).collect(Collectors.toList());
        return filterIssueType(projectId, issueTypes, issueTypeIds);
//        List<Long> backlogIssueTypeId = issueTypeMapper.selectIssueTypeIdsByOptions(Arrays.asList(ObjectSchemeFieldContext.BACKLOG), organizationId, null, ISSUE_TYPE_SOURCE_SYSTEM);
//        List<Long> featureIssueTypeId = issueTypeMapper.selectIssueTypeIdsByOptions(Arrays.asList("feature"), organizationId, null, ISSUE_TYPE_SOURCE_SYSTEM);
//        //组织没有backlog类型的数据，则不返回backlog类型
//        boolean containsBacklog =
//                !objectSchemeFieldExtendMapper
//                        .selectExtendFieldByOptions(backlogIssueTypeId, organizationId, null, null)
//                        .isEmpty();
//        //项目没有feature类型的数据，则不返回feature类型
//        boolean containsFeature =
//                !objectSchemeFieldExtendMapper
//                        .selectExtendFieldByOptions(featureIssueTypeId, organizationId, null, projectId)
//                        .isEmpty();
//          List<IssueTypeVO> result = new ArrayList<>();
//        for (IssueTypeVO vo : issueTypes) {
//            String typeCode = vo.getTypeCode();
//            if (ObjectSchemeFieldContext.getIssueTye().contains(typeCode)) {
//                if (ObjectSchemeFieldContext.BACKLOG.equals(typeCode)) {
//                    if (containsBacklog) {
//                        result.add(vo);
//                    }
//                }
//                else if (ObjectSchemeFieldContext.FEATURE.equals(typeCode)) {
//                    if (containsFeature) {
//                        result.add(vo);
//                    }
//                } else {
//                    result.add(vo);
//                }
//            }
//        }
//        return filterBacklog(projectId, result);
    }

    private String processIssueTyeAndRequiredScope(ObjectSchemeFieldDTO fieldDTO,
                                                   List<Long> issueTypeIds,
                                                   List<String> issueTypeNames,
                                                   boolean resetIssueType,
                                                   List<ObjectSchemeFieldExtendDTO> extendList,
                                                   boolean containsAllIssueTypes,
                                                   Long organizationId,
                                                   Long projectId) {
        boolean allIsRequired = true;
        boolean allIsNotRequired = false;
        //系统字段或项目层下的组织字段
        if (Objects.equals(fieldDTO.getCreatedLevel(), CREATED_LEVEL_SYSTEM) ||
                (!Objects.isNull(projectId) && Objects.equals(fieldDTO.getCreatedLevel(), CREATED_LEVEL_ORGANIZATION))) {
            String fieldContext = getFieldContext(fieldDTO.getCode());
            IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
            issueTypeSearchVO.setTypeCodes(Arrays.asList(fieldContext.split(",")));
            List<Long> ids = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO).stream().map(IssueTypeVO::getId).collect(Collectors.toList());
            //项目层的组织字段获取组织层下配置的问题类型
            if (Boolean.FALSE.equals(fieldDTO.getSystem())) {
                fieldDTO = baseQueryById(organizationId, null, fieldDTO.getId());
                ids = fieldDTO.getExtendFields().stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toList());
            }
            issueTypeIds.addAll(ids);
            allIsRequired = allIsRequired && fieldDTO.getRequired();
            allIsNotRequired = allIsNotRequired || fieldDTO.getRequired();
        }else {
            for (ObjectSchemeFieldExtendDTO e : extendList) {
                issueTypeIds.add(e.getIssueTypeId());
//                issueTypeNames.add(e.getIssueTypeName());
                allIsRequired = allIsRequired && e.getRequired();
                allIsNotRequired = allIsNotRequired || e.getRequired();
            }
        }
        if (resetIssueType && containsAllIssueTypes) {
//            issueTypeIds.clear();
//            issueTypeNames.clear();
//            issueTypeIds.add(ObjectSchemeFieldContext.GLOBAL);
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
    public Boolean containsAllIssueTypes(Long organizationId, Long projectId, List<Long> issueTypeIds) {
        List<IssueTypeVO> issueTypeList = issueTypeService.queryByOrgId(organizationId, projectId);
        issueTypeList =
                issueTypeList
                        .stream()
                        .filter(i -> ObjectSchemeFieldContext.getIssueTye().contains(i.getTypeCode()))
                        .collect(Collectors.toList());
        issueTypeList = filterBacklog(projectId, issueTypeList);
        Boolean result = true;
        for (IssueTypeVO vo : issueTypeList) {
            result =  result && issueTypeIds.contains(vo.getId());
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
                                                     Long issueTypeId,
                                                     Long organizationId) {
        Long newProjectId = projectId == null ? 0L : projectId;
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setIssueTypeIds(Arrays.asList(issueTypeId));
        List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, newProjectId, issueTypeSearchVO);
        if (ObjectUtils.isEmpty(issueTypes)) {
            throw new CommonException("error.illegal.issueType");
        }
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
                                            Long issueTypeIdForRank) {
        if (!EnumUtil.contain(FieldType.class, fieldCreateDTO.getFieldType())) {
            throw new CommonException(ERROR_FIELDTYPE_ILLEGAL);
        }
        if (checkName(organizationId, projectId, fieldCreateDTO.getName(), fieldCreateDTO.getSchemeCode())) {
            throw new CommonException(ERROR_FIELD_NAMEEXIST);
        }
        if (checkCode(organizationId, projectId, fieldCreateDTO.getCode(), fieldCreateDTO.getSchemeCode())) {
            throw new CommonException(ERROR_FIELD_CODEEXIST);
        }

        List<Long> issueTypeIds = fieldCreateDTO.getIssueTypeIds();
        if (ObjectUtils.isEmpty(issueTypeIds)) {
            throw new CommonException("error.filed.issue.type.id.empty");
        }
        List<IssueTypeVO> issueTypes = getIssueTypeByIds(organizationId, projectId, issueTypeIds);
        ObjectSchemeFieldDTO field = modelMapper.map(fieldCreateDTO, ObjectSchemeFieldDTO.class);
        field.setContext(Arrays.asList(fieldCreateDTO.getContext()).stream().collect(Collectors.joining(",")));
        field.setOrganizationId(organizationId);
        field.setProjectId(projectId);

        String defaultValue = tryDecryptDefaultValue(field.getFieldType(), field.getDefaultValue());
        if (defaultValue != null) {
            field.setDefaultValue(defaultValue);
        }
        field = baseCreate(field, issueTypes, issueTypeIdForRank);
        //处理字段选项
        if (fieldCreateDTO.getFieldOptions() != null) {
            String defaultIds = fieldOptionService.handleFieldOption(organizationId, field.getId(), fieldCreateDTO.getFieldOptions());
            if (defaultIds != null && !"".equals(defaultIds)) {
                field.setDefaultValue(defaultIds);
                objectSchemeFieldMapper.updateOptional(field, "defaultValue");
                objectSchemeFieldExtendMapper.selectExtendFieldByOptions(null, organizationId, field.getId(), projectId).stream().forEach(f -> {
                    f.setDefaultValue(defaultIds);
                    objectSchemeFieldExtendMapper.updateByPrimaryKey(f);
                });
            }
        }
        if (!ObjectUtils.isEmpty(issueTypeIdForRank)
                && issueTypeIds.contains(issueTypeIdForRank)) {
            Long newProjectId = projectId == null ? 0L : projectId;
            IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
            issueTypeSearchVO.setEnabled(true);
            Map<Long, String> issueTypeMap =
                    issueTypeMapper.selectByOptions(organizationId, newProjectId, issueTypeSearchVO).stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
            insertObjectSchemeFieldExtend(organizationId, projectId, field.getId(), fieldCreateDTO.getRequired(), issueTypeMap, issueTypeIdForRank, fieldCreateDTO.getCreated(), fieldCreateDTO.getEdited(), field.getDefaultValue(), field.getExtraConfig());
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

    protected List<IssueTypeVO> getIssueTypeByIds(Long organizationId,
                                                  Long projectId,
                                                  List<Long> issueTypeIds) {
        Set<String> typeCodes =
                issueTypeMapper.selectByIds(StringUtils.join(issueTypeIds, ","))
                        .stream().map(IssueTypeDTO::getTypeCode).collect(Collectors.toSet());
        String[] contexts = new String[typeCodes.size()];
        new ArrayList(typeCodes).toArray(contexts);
        ObjectSchemeFieldContext.isIllegalContexts(contexts);

        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setEnabled(true);
        List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO);
        List<IssueTypeVO> result = filterIssueType(projectId, issueTypes, issueTypeIds);
        return result;
    }

    private List<IssueTypeVO> filterIssueType(Long projectId, List<IssueTypeVO> issueTypes, List<Long> issueTypeIds) {
        if (ObjectUtils.isEmpty(projectId)) {
            return issueTypes
                    .stream()
                    .filter(i -> ObjectSchemeFieldContext.getIssueTye().contains(i.getTypeCode())
                            && issueTypeIds.contains(i.getId()))
                    .collect(Collectors.toList());
        } else {
            List<IssueTypeVO> backlogs = queryBacklogIssueType(projectId, issueTypes, issueTypeIds);
            List<IssueTypeVO> issueTypeList = queryProjectIssueType(projectId, issueTypes, issueTypeIds);
            issueTypeList.addAll(backlogs);
            return issueTypeList;
        }
    }

    protected List<IssueTypeVO> queryProjectIssueType(Long projectId,
                                                    List<IssueTypeVO> issueTypes,
                                                    List<Long> issueTypeIds) {
            if(agilePluginService != null){
                return agilePluginService.queryProgramIssueType(projectId, issueTypes, issueTypeIds);
            }
            else {
                return issueTypes
                        .stream()
                        .filter(i -> ObjectSchemeFieldContext.NORMAL_PROJECT.contains(i.getTypeCode())
                                && issueTypeIds.contains(i.getId()))
                        .collect(Collectors.toList());
            }
    }

    @Override
    public ObjectSchemeFieldDetailVO queryById(Long organizationId, Long projectId, Long fieldId) {
        ObjectSchemeFieldDTO field = baseQueryById(organizationId, projectId, fieldId);
        List<ObjectSchemeFieldExtendDTO> extendList = field.getExtendFields();
        //不包含已禁用的自定义问题类型
        List<IssueTypeVO> issueTypeVOS = issueTypeService.queryByOrgId(organizationId, projectId);
        Map<Long, IssueTypeVO> issueTypeVOMap = issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));
        List<Long> issueTypeIds = new ArrayList<>();
        List<String> issueTypeNames = new ArrayList<>();
        List<IssueTypeVO> issueTypeVOList = new ArrayList<>();
        boolean containsAllIssueTypes = containsAllIssueTypes(organizationId, projectId, issueTypeIds);
        String requiredScope =
                processIssueTyeAndRequiredScope(field,issueTypeIds, issueTypeNames, false, extendList, containsAllIssueTypes, organizationId, projectId);
        ObjectSchemeFieldDetailVO fieldDetailDTO = modelMapper.map(field, ObjectSchemeFieldDetailVO.class);
//        fieldDetailDTO.setContext(issueTypeIds.toArray(new Long[0]));
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
        if (!CollectionUtils.isEmpty(issueTypeIds)) {
            issueTypeIds.forEach(issueTypeId -> {
                IssueTypeVO issueTypeVO = issueTypeVOMap.get(issueTypeId);
                if (!Objects.isNull(issueTypeVO)) {
                    issueTypeVOList.add(issueTypeVO);
                }
            });
            fieldDetailDTO.setIssueTypeVOList(issueTypeVOList);
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
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setIssueTypeIds(updateDTO.getIssueTypeIds());
        Set<String> typeCodes = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO)
                .stream().map(IssueTypeVO::getTypeCode).collect(Collectors.toSet());
        update.setContext(String.join(",", typeCodes));
        String defaultValue = tryDecryptDefaultValue(update.getFieldType(), update.getDefaultValue());
        updateFieldIssueTypeAndDefaultValue(organizationId, projectId, fieldId, defaultValue, update.getExtraConfig(), updateDTO.getIssueTypeIds());
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
                                                     String defaultValue,
                                                     Boolean extraConfig,
                                                     List<Long> issueTypeIds) {
        if (ObjectUtils.isEmpty(issueTypeIds)) {
            throw new CommonException("error.field.issueTypeIds.empty");
        }

        List<IssueTypeVO> issueTypes = getIssueTypeByIds(organizationId, projectId, issueTypeIds);
        Map<Long, String> issueTypeMap =
                issueTypes.stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
        List<Long> issueTypeIdList = new ArrayList<>(issueTypeMap.keySet());
        ObjectSchemeFieldDTO field =
                selectOneByFieldId(organizationId, projectId, fieldId);
        List<ObjectSchemeFieldExtendDTO> intersection = new ArrayList<>();
        List<ObjectSchemeFieldExtendDTO> deleteList = new ArrayList<>();
        Set<Long> insertSet = new HashSet<>();
        filterByIssueType(intersection, deleteList, insertSet, issueTypeIdList, field);

        dealWithExtendFields(organizationId, projectId, fieldId, deleteList, insertSet, issueTypeMap, defaultValue, extraConfig);
    }

    private void dealWithExtendFields(Long organizationId,
                                      Long projectId,
                                      Long fieldId,
                                      List<ObjectSchemeFieldExtendDTO> deleteList,
                                      Set<Long> insertSet,
                                      Map<Long, String> issueTypeMap,
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
                                          List<Long> issueTypeIds,
                                          Boolean extraConfig) {
        if (!CollectionUtils.isEmpty(issueTypeIds)) {
            //判断需要在组织层或项目层增加的扩展字段
            List<Long> existIssueTypeIds = objectSchemeFieldExtendMapper.selectExtendFieldByOptions(null, organizationId, fieldId, projectId)
                    .stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toList());
            Set<Long> insertSet = new HashSet<>();
            issueTypeIds.forEach(issueTypeId -> {
                if (!existIssueTypeIds.contains(issueTypeId)) {
                    insertSet.add(issueTypeId);
                }
            });
            //增加扩展字段
            if (!CollectionUtils.isEmpty(insertSet)) {
//                List<String> context = issueTypeMapper.queryIssueTypeList(organizationId, new ArrayList<>(insertSet)).stream().map(IssueTypeWithInfoDTO::getTypeCode).collect(Collectors.toList());
                List<IssueTypeVO> issueTypeVOList = getIssueTypeByIds(organizationId, projectId, new ArrayList<>(insertSet));
                Map<Long, String> issueTypeMap =
                        issueTypeVOList.stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
                insertSet.forEach(i ->
                        insertObjectSchemeFieldExtend(organizationId, projectId, fieldId, false, issueTypeMap, i, true, true, defaultValue, extraConfig)
                );
            }
            //同步默认值
            objectSchemeFieldExtendMapper.selectExtendFieldByOptions(null, organizationId, fieldId, projectId)
                    .forEach(i -> {
                        if (issueTypeIds.contains(i.getIssueTypeId())) {
                            i.setDefaultValue(defaultValue);
                            //同步修改默认值时，同步修改额外配置
                            i.setExtraConfig(extraConfig);
                            if (insertSet.contains(i.getIssueTypeId())) {
                                //获取原来引用的组织层或系统内的扩展字段rank值
                                setNewExtendDtoOldRank(organizationId, projectId, fieldId, i);
                            }
                            objectSchemeFieldExtendMapper.updateByPrimaryKey(i);
                        }
                    });
            //如果修改了标签，则启动标签垃圾回收
            ObjectSchemeFieldDTO objectSchemeFieldDTO = getObjectSchemeFieldDTO(FieldCode.LABEL);
            if (Objects.equals(fieldId, objectSchemeFieldDTO.getId())) {
                issueLabelService.labelGarbageCollection(projectId);
            }
        }
    }

    private void setNewExtendDtoOldRank(Long organizationId, Long projectId, Long fieldId, ObjectSchemeFieldExtendDTO newExtendDTO) {
        boolean onProjectLevel = (projectId !=null);
        List<ObjectSchemeFieldExtendDTO> objectSchemeFieldExtendDTOList;
        if (onProjectLevel) {
            objectSchemeFieldExtendDTOList = objectSchemeFieldExtendMapper.selectExtendFieldByOptions(Arrays.asList(newExtendDTO.getIssueTypeId()), organizationId, fieldId, null);
        } else {
            objectSchemeFieldExtendDTOList = objectSchemeFieldExtendMapper.selectExtendFieldByOptions(Arrays.asList(newExtendDTO.getIssueTypeId()), 0L, fieldId, null);
        }
        //项目层引用的组织层扩展字段或组织层引用的系统扩展字段
        ObjectSchemeFieldExtendDTO oldRankDTO;
        if (CollectionUtils.isEmpty(objectSchemeFieldExtendDTOList)) {
            objectSchemeFieldExtendDTOList = objectSchemeFieldExtendMapper.selectExtendFieldByOptions(Arrays.asList(newExtendDTO.getIssueTypeId()), 0L, fieldId, null);
            oldRankDTO = !CollectionUtils.isEmpty(objectSchemeFieldExtendDTOList) ? objectSchemeFieldExtendDTOList.get(0) : null;
        } else {
            oldRankDTO = objectSchemeFieldExtendDTOList.get(0);
        }
        if (!Objects.isNull(oldRankDTO)) {
            newExtendDTO.setRank(oldRankDTO.getRank());
        }
    }

    @Override
    public void syncDefaultValue(Long organizationId, Long projectId, Long fieldId, ObjectSchemeFieldUpdateVO updateDTO) {

        String defaultValue;
        List<Long> issueTypeIds = updateDTO.getIssueTypeIds();
        ObjectSchemeFieldDTO field = new ObjectSchemeFieldDTO();
        field.setId(fieldId);
        //组织层下的组织字段，项目层下的自定义字段查询默认值
        if (Boolean.TRUE.equals(updateDTO.getCustom())) {
            field = baseQueryById(organizationId, projectId, fieldId);
            defaultValue = getObjectSchemeFieldByFieldId(organizationId, projectId, fieldId).getDefaultValue();
        }
        //组织层下的系统字段，项目层下的系统字段和组织字段处理默认值
        else {
            field = objectSchemeFieldMapper.selectOne(field);
            if (Boolean.FALSE.equals(field.getSystem())) {
                field = baseQueryById(organizationId, null, fieldId);
            }
            defaultValue = updateDTO.getDefaultValue();
            String value = tryDecryptDefaultValue(updateDTO.getFieldType(), updateDTO.getDefaultValue());
            if (value != null) {
                defaultValue = value;
            }
        }
        //检查类型是否合法
        checkIssueTypeLegality(organizationId, projectId, field, issueTypeIds);
        updateExtendDefaultValue(organizationId, projectId, fieldId, defaultValue, issueTypeIds, updateDTO.getExtraConfig());
    }

    private void checkIssueTypeLegality(Long organizationId, Long projectId, ObjectSchemeFieldDTO field, List<Long> issueTypeIds) {
        List<Long> legalIssueTypeIds;
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        if (Boolean.TRUE.equals(field.getSystem())) {
            List<String> legalIssueTypes = Arrays.asList(getFieldContext(field.getCode()).split(","));
            issueTypeSearchVO.setTypeCodes(legalIssueTypes);
            issueTypeSearchVO.setEnabled(true);
            legalIssueTypeIds = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO).stream().map(IssueTypeVO::getId).collect(Collectors.toList());
        } else if (!Objects.isNull(projectId) && Objects.equals(field.getCreatedLevel(), CREATED_LEVEL_ORGANIZATION)) {
            legalIssueTypeIds = field.getExtendFields().stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toList());
        } else {
            issueTypeSearchVO.setEnabled(true);
            List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO);
            legalIssueTypeIds = issueTypes.stream().map(IssueTypeVO::getId).collect(Collectors.toList());
        }
        issueTypeIds.forEach(issueTypeId -> {
            if (!legalIssueTypeIds.contains(issueTypeId)) {
                throw new CommonException("error.issue.type.illegal");
            }
        });
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
                                   Set<Long> insertSet,
                                   List<Long> issueTypeIdList,
                                   ObjectSchemeFieldDTO field) {
        List<ObjectSchemeFieldExtendDTO> extendList = field.getExtendFields();
        //交集
        extendList.forEach(e -> {
            if (issueTypeIdList.contains(e.getIssueTypeId())) {
                intersection.add(e);
            }
        });
        Set<Long> intersectionIssueTypes =
                intersection.stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toSet());
        //删除的类型
        extendList.forEach(e -> {
            if (!intersectionIssueTypes.contains(e.getIssueType())) {
                deleteList.add(e);
            }
        });
        //插入的类型
        issueTypeIdList.forEach(c -> {
            if (!intersectionIssueTypes.contains(c)) {
                insertSet.add(c);
            }
        });
    }

    private ObjectSchemeFieldExtendDTO insertObjectSchemeFieldExtend(Long organizationId,
                                                                     Long projectId,
                                                                     Long fieldId,
                                                                     Boolean required,
                                                                     Map<Long, String> issueTypeMap,
                                                                     Long issueTypeId,
                                                                     Boolean created,
                                                                     Boolean edited,
                                                                     String defaultValue,
                                                                     Boolean extraConfig) {
        ObjectSchemeFieldExtendDTO dto = new ObjectSchemeFieldExtendDTO();
        dto.setIssueTypeId(issueTypeId);
        dto.setFieldId(fieldId);
        dto.setOrganizationId(organizationId);

        List<ObjectSchemeFieldExtendDTO> existedList;
        if (ObjectUtils.isEmpty(projectId)) {
            existedList = objectSchemeFieldExtendMapper.selectExtendFieldByOptions(Arrays.asList(issueTypeId), organizationId, fieldId, null);
        } else {
            dto.setProjectId(projectId);
            existedList = objectSchemeFieldExtendMapper.select(dto);
        }
        Long extendId = null;
        if (existedList.isEmpty()) {
            dto.setIssueType(issueTypeMap.get(issueTypeId));
            dto.setRequired(required);
            dto.setCreated(created);
            dto.setEdited(edited);
            dto.setRank(getMinRank(organizationId, projectId, issueTypeId, null));
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
        List<String> fieldCodes = getIssueTypeFieldCodes(issueTypeVO.getId(), organizationId, projectId);
        List<String> existFieldCodes = list.stream().filter(v -> Boolean.TRUE.equals(v.getSystem())).map(ObjectSchemeFieldDetailVO::getCode).collect(Collectors.toList());
        fieldCodes.removeAll(existFieldCodes);
        if (!CollectionUtils.isEmpty(fieldCodes)) {
            list.addAll(objectSchemeFieldMapper.selectFieldsByFieldCodes(fieldCodes));
        }
        return list;
    }

    private List<String> getIssueTypeFieldCodes(Long issueTypeId, Long organizationId, Long projectId) {
        Long newProjectId = projectId == null ? 0L : projectId;
        List<String> fieldCodeS = new ArrayList<>();
        List<AgileSystemFieldContext> agileSystemFieldContexts = Arrays.asList(AgileSystemFieldContext.values());
        agileSystemFieldContexts.forEach(v -> {
            IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
            issueTypeSearchVO.setTypeCodes(Arrays.asList(v.getContext().split(",")));
            issueTypeSearchVO.setSource("system");
            List<Long> issueTypeIds =
                    issueTypeMapper.selectByOptions(organizationId, newProjectId, issueTypeSearchVO)
                            .stream().map(IssueTypeVO::getId).collect(Collectors.toList());
            if (issueTypeIds.contains(issueTypeId)) {
                fieldCodeS.add(v.getFieldCode());
            }
        });
        if (agilePluginService != null) {
            agilePluginService.getIssueTypeFieldCodes(fieldCodeS,issueTypeId);
        }
        if (backlogExpandService != null) {
            backlogExpandService.getBacklogFieldCodes(fieldCodeS,issueTypeId);
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
                    objectSchemeFieldExtendMapper.selectExtendFieldByOptions(null, organizationId, fieldId, projectId);
            if (extendList.isEmpty()) {
                //项目层暂未配置，查组织层并新建
                extendList = objectSchemeFieldExtendMapper.selectExtendFieldByOptions(null, organizationId, fieldId, null);
            }
            Long newProjectId = projectId == null ? 0 : projectId;
            IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
            issueTypeSearchVO.setEnabled(true);
            Map<Long, String> issueTypeMap =
                    issueTypeMapper.selectByOptions(organizationId, newProjectId, issueTypeSearchVO).stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
            ObjectSchemeFieldDTO objectSchemeFieldDTO = getObjectSchemeFieldByFieldId(organizationId, projectId, fieldId);
            extendList.forEach(e ->
                    insertObjectSchemeFieldExtend(organizationId, projectId, fieldId, required, issueTypeMap, e.getIssueTypeId(), e.getCreated(), e.getEdited(), objectSchemeFieldDTO.getDefaultValue(), objectSchemeFieldDTO.getExtraConfig()));
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
    public List<ObjectSchemeFieldVO> unselected(Long organizationId, Long projectId, Long issueTypeId) {
        List<ObjectSchemeFieldVO> unselected = objectSchemeFieldExtendMapper.unselected(organizationId, projectId, issueTypeId);
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
        Long issueTypeId = pageConfigUpdateVO.getIssueTypeId();
        List<PageConfigFieldVO> fields = pageConfigUpdateVO.getFields();
        IssueTypeFieldVO issueTypeFieldVO = pageConfigUpdateVO.getIssueTypeFieldVO();
        Set<Long> deleteIds = pageConfigUpdateVO.getDeleteIds();

        String issueType = issueTypeService.getIssueTypeById(issueTypeId);
        ObjectSchemeFieldContext.isIllegalIssueType(issueType);
        Long newProjectId = projectId == null ? 0L : projectId;
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setEnabled(true);
        Map<Long, String> issueTypeMap =
                issueTypeMapper.selectByOptions(organizationId, newProjectId, issueTypeSearchVO).stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
        if (!ObjectUtils.isEmpty(fields)) {
            updateFieldConfig(organizationId, projectId, issueTypeId, fields, issueTypeMap);
        }
        if (!ObjectUtils.isEmpty(projectId)
                && !ObjectUtils.isEmpty(issueTypeFieldVO)
                && !StringUtils.isEmpty(issueTypeFieldVO.getTemplate())) {
            updateTemplate(projectId, issueTypeId, issueTypeFieldVO, issueTypeMap);
        }
        if (!ObjectUtils.isEmpty(deleteIds)) {
            deleteFieldConfig(organizationId, projectId, deleteIds);
        }
        List<ObjectSchemeFieldCreateVO> createdField = pageConfigUpdateVO.getCreatedFields();
        if (!ObjectUtils.isEmpty(createdField)) {
            createdField.forEach(c -> create(organizationId, projectId, c, issueTypeId));
        }
        List<PageConfigFieldVO> addFields = pageConfigUpdateVO.getAddFields();
        if (!ObjectUtils.isEmpty(addFields)) {
            addFieldConfig(organizationId, projectId, addFields, issueTypeId, issueTypeMap);
        }
    }

    private void addFieldConfig(Long organizationId,
                                Long projectId,
                                List<PageConfigFieldVO> addFields,
                                Long issueTypeId,
                                Map<Long, String> issueTypeMap) {
        addFields.forEach(a -> {
            Long fieldId = a.getFieldId();
            String rank = a.getRank();
            String defaultValue = tryDecryptDefaultValue(a.getFieldType(), a.getDefaultValue().toString());
            if (defaultValue != null) {
                a.setDefaultValue(defaultValue);
            }
            ObjectSchemeFieldExtendDTO result =
                    insertObjectSchemeFieldExtend(organizationId, projectId, fieldId, a.getRequired(), issueTypeMap, issueTypeId, a.getCreated(), a.getEdited() ,a.getDefaultValue().toString(), a.getExtraConfig());
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
                        .selectExtendFieldByOptions(null, organizationId, fieldId, null).size() <= 1) {
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
    public PageConfigVO listConfigs(Long organizationId, Long projectId, Long issueTypeId) {
        PageConfigVO result = new PageConfigVO();
        List<PageConfigFieldVO> pageConfigFields = queryPageConfigFields(organizationId, projectId, issueTypeId);
        result.setFields(pageConfigFields);
        //处理默认值
        processDefaultValue(pageConfigFields, organizationId, projectId);
        //处理字段是否可被编辑
        String issueType = issueTypeService.getIssueTypeById(issueTypeId);
        processFieldEdited(issueType, pageConfigFields);
        if (!ObjectUtils.isEmpty(projectId)) {
//            Map<String, Long> issueTypeMap = issueTypeService.queryIssueTypeMap(organizationId);
//            Long issueTypeId = issueTypeMap.get(issueType);
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

    protected List<PageConfigFieldVO> queryPageConfigFields(Long organizationId, Long projectId, Long issueTypeId) {
        List<PageConfigFieldVO> pageConfigFieldVOS = objectSchemeFieldExtendMapper.listConfigs(organizationId, projectId, issueTypeId);
        // 有新增系统字段未配置到字段扩展表直接添加字段到pageConfigFieldVOS
        addPageFieldVOS(pageConfigFieldVOS,organizationId,projectId,issueTypeId);
        if (agilePluginService != null) {
           return agilePluginService.queryProgramPageConfigFields(projectId,issueTypeId,pageConfigFieldVOS);
        }
        return pageConfigFieldVOS;
    }

    private void addPageFieldVOS(List<PageConfigFieldVO> pageConfigFieldVOS, Long organizationId, Long projectId, Long issueTypeId) {
        List<PageConfigFieldVO> systemFieldS = pageConfigFieldVOS.stream().filter(pageConfigFieldVO -> Objects.equals(pageConfigFieldVO.getCreatedLevel(), "system")).collect(Collectors.toList());
        // 当前问题类型的预定义字段
        List<String> fieldCodeS = getIssueTypeFieldCodes(issueTypeId, organizationId, projectId);
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
                configFieldVO.setIssueTypeId(issueTypeId);
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
        setDefaultValueObjs(pageFieldViews, projectId, organizationId);
        FieldValueUtil.handleDefaultValue(pageFieldViews);
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
                switch (view.getFieldCode()) {
                    //多选
                    case FieldCode.COMPONENT:
                        List<IssueComponentVO> issueComponentList = modelMapper.map(issueComponentMapper.selectByProjectId(projectId), new TypeToken<List<IssueComponentVO>>(){}.getType());
                        Map<Long, Object> issueComponentMap = issueComponentList.stream().collect(Collectors.toMap(IssueComponentVO::getComponentId, Function.identity()));
                        setDefaultValueObjsOfMultiple(defaultValue, issueComponentMap, view);
                        break;
                    //多选
                    case FieldCode.LABEL:
                        List<IssueLabelVO> issueLabelList = modelMapper.map(issueLabelMapper.selectByProjectIds(Collections.singletonList(projectId)), new TypeToken<List<IssueLabelVO>>(){}.getType());
                        Map<Long, Object> issueLabelMap = issueLabelList.stream().collect(Collectors.toMap(IssueLabelVO::getLabelId, Function.identity()));
                        setDefaultValueObjsOfMultiple(defaultValue, issueLabelMap, view);
                        break;
                    //多选
                    case FieldCode.INFLUENCE_VERSION:
                        List<ProductVersionNameVO> influenceVersionList = modelMapper.map(productVersionMapper.queryNameByOptions(projectId, null), new TypeToken<List<ProductVersionNameVO>>(){}.getType());
                        Map<Long, Object> influenceVersionMap = influenceVersionList.stream().collect(Collectors.toMap(ProductVersionNameVO::getVersionId, Function.identity()));
                        setDefaultValueObjsOfMultiple(defaultValue, influenceVersionMap, view);
                        break;
                    //多选
                    case FieldCode.FIX_VERSION:
                        List<ProductVersionNameVO> fixVersionList = modelMapper.map(productVersionMapper.queryNameByOptions(projectId, Collections.singletonList("version_planning")), new TypeToken<List<ProductVersionNameVO>>(){}.getType());
                        Map<Long, Object> fixVersionMap = fixVersionList.stream().collect(Collectors.toMap(ProductVersionNameVO::getVersionId, Function.identity()));
                        setDefaultValueObjsOfMultiple(defaultValue, fixVersionMap, view);
                        break;
                    //单选
                    case FieldCode.SPRINT:
                        List<String> sprintStatusCodes = Arrays.asList("sprint_planning", "started");
                        List<SprintNameVO> sprintNameList = modelMapper.map(sprintMapper.queryNameByOptions(Collections.singletonList(projectId), sprintStatusCodes), new TypeToken<List<SprintNameVO>>(){}.getType());
                        Map<Long, Object> sprintNameMap = sprintNameList.stream().collect(Collectors.toMap(SprintNameVO::getSprintId, Function.identity()));
                        setDefaultValueObjsOfSingle(sprintNameMap, view);
                        break;
                    //单选
                    case FieldCode.EPIC:
                        List<EpicDataVO> epicDataList = modelMapper.map(issueMapper.queryEpicList(projectId), new TypeToken<List<EpicDataVO>>(){}.getType());
                        Map<Long, Object> epicDataMap = epicDataList.stream().collect(Collectors.toMap(EpicDataVO::getIssueId, Function.identity()));
                        setDefaultValueObjsOfSingle(epicDataMap, view);
                        break;
                    case FieldCode.PROGRAM_VERSION:
                        if (agilePluginService != null) {
                            agilePluginService.setBussinessDefaultValueObjs(pageFieldViews, projectId, organizationId);
                        }
                        break;
                    case FieldCode.BACKLOG_TYPE:
                    case FieldCode.BACKLOG_CLASSIFICATION:
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

    @Override
    public void setDefaultValueObjsOfMultiple(Object defaultValue, Map<Long, Object> valueMap, PageFieldViewVO view) {
        List<Object> defaultObjs = new ArrayList<>();
        String[] ids = String.valueOf(defaultValue).split(",");
        List<Long> defaultIds = Arrays.asList((Long[]) ConvertUtils.convert(ids, Long.class));
        if (!CollectionUtils.isEmpty(defaultIds)) {
            defaultIds.forEach(id -> {
                if (valueMap.containsKey(id)) {
                    defaultObjs.add(valueMap.get(id));
                }
            });
            view.setDefaultValueObjs(defaultObjs);
        }
    }

    @Override
    public void setDefaultValueObjsOfSingle(Map<Long, Object> valueMap, PageFieldViewVO view) {
        long defaultId = Long.parseLong(view.getDefaultValue().toString());
        if (valueMap.containsKey(defaultId)) {
            view.setDefaultValueObj(valueMap.get(defaultId));
        }
    }

    private void updateTemplate(Long projectId,
                                Long issueTypeId,
                                IssueTypeFieldVO issueTypeFieldVO,
                                Map<Long, String> issueTypeMap) {
//        String issueType = issueTypeMap.get(issueTypeId);
        if (ObjectUtils.isEmpty(issueTypeId)) {
            throw new CommonException("error.issue.type.not.existed", issueTypeId);
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
                                   Long issueTypeId,
                                   List<PageConfigFieldVO> fields,
                                   Map<Long, String> issueTypeMap) {
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
            dto.setIssueTypeId(issueTypeId);
            dto.setOrganizationId(organizationId);
            dto.setFieldId(fieldId);
            dto.setProjectId(projectId);
            List<ObjectSchemeFieldExtendDTO> result = objectSchemeFieldExtendMapper.select(dto);
            String issueType = issueTypeMap.get(issueTypeId);
            if (Boolean.FALSE.equals(onProjectLevel)) {
                result = result.stream().filter(v -> ObjectUtils.isEmpty(v.getProjectId())).collect(Collectors.toList());
            }
            String defaultValue = tryDecryptDefaultValue(f.getFieldType(), f.getDefaultValue().toString());
            if (defaultValue != null) {
                f.setDefaultValue(defaultValue);
            }
            if (result.isEmpty() && !ObjectUtils.isEmpty(issueType)) {
                dto.setIssueType(issueType);
                dto.setRequired(f.getRequired());
                dto.setCreated(f.getCreated());
                dto.setEdited(f.getEdited());
                dto.setRank(f.getRank());
                dto.setDefaultValue(f.getDefaultValue().toString());
                dto.setExtraConfig(f.getExtraConfig());
                objectSchemeFieldExtendMapper.insertSelective(dto);
            } else {
                updateObjectSchemeFieldExtend(f, result);
                //修改标签，执行标签垃圾回收
                if (Objects.equals(f.getFieldCode(), FieldCode.LABEL)) {
                    issueLabelService.labelGarbageCollection(projectId);
                }
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
                                                    List<Long> issueTypeIds) {
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
                                    && issueTypeIds.contains(i.getId()))
                            .collect(Collectors.toList()));
        }
        return result;
    }

    @Override
    public ObjectSchemeFieldDTO getObjectSchemeFieldDTO(String fieldCode) {
        ObjectSchemeFieldDTO search = new ObjectSchemeFieldDTO();
        search.setCode(fieldCode);
        search.setSchemeCode("agile_issue");
        return objectSchemeFieldMapper.selectOne(search);
    }

    @Override
    public void checkObjectSchemeFieldDefaultValueOfSingle(Long projectId, Long id, String fieldCode) {
        ObjectSchemeFieldDTO fieldDTO = getObjectSchemeFieldDTO(fieldCode);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<ObjectSchemeFieldExtendDTO> objectSchemeFieldExtendList = objectSchemeFieldExtendMapper.selectExtendFields(organizationId, fieldDTO.getId(), projectId);
        objectSchemeFieldExtendList.forEach(f -> {
            String defaultValue = f.getDefaultValue();
            if (!Objects.isNull(defaultValue) && Objects.equals(defaultValue, id.toString())) {
                throw new CommonException("this.is.default.value");
            }
        });
    }

    @Override
    public void checkObjectSchemeFieldDefaultValueOfMultiple(Long projectId, Long id, String fieldCode) {
        ObjectSchemeFieldDTO fieldDTO = getObjectSchemeFieldDTO(fieldCode);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<ObjectSchemeFieldExtendDTO> objectSchemeFieldExtendList = objectSchemeFieldExtendMapper.selectExtendFields(organizationId, fieldDTO.getId(), projectId);
        objectSchemeFieldExtendList.forEach(f -> {
            String defaultValue = f.getDefaultValue();
            if (!Objects.isNull(defaultValue) && !Objects.equals(defaultValue, "")) {
                List<String> componentIds = new ArrayList<>(Arrays.asList(defaultValue.split(",")));
                if (componentIds.contains(id.toString())) {
                    throw new CommonException("this.is.default.value");
                }
            }
        });
    }

    @Override
    public String getIssueSummaryDefaultValue(Long organizationId, Long projectId, Long issueTypeId) {
        ObjectSchemeFieldDTO summaryFieldDTO = new ObjectSchemeFieldDTO();
        summaryFieldDTO.setCode("summary");
        summaryFieldDTO.setSystem(true);
        summaryFieldDTO.setSchemeCode("agile_issue");
        summaryFieldDTO = objectSchemeFieldMapper.selectOne(summaryFieldDTO);
        if (Objects.isNull(summaryFieldDTO)) {
            throw new CommonException("error.summary.field.not.exist");
        }

        List<ObjectSchemeFieldExtendDTO> extendDTO =
                objectSchemeFieldExtendMapper.selectExtendFieldByOptions(Collections.singletonList(issueTypeId), organizationId, summaryFieldDTO.getId(), projectId);
        if (!CollectionUtils.isEmpty(extendDTO)) {
            return extendDTO.get(0).getDefaultValue();
        }
        return null;
    }

}
