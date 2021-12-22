package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
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
    private static final  String[] AGILE_ISSUE_TYPE_LIST = {"story", "issue_epic", "task", "sub_task", "bug"};
    private static final  String[] PROGRAM_ISSUE_TYPE_LIST = {"issue_epic", "feature"};


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
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private FieldValueMapper fieldValueMapper;
    @Autowired
    private FieldDataLogMapper fieldDataLogMapper;
    @Autowired
    private FieldCascadeRuleMapper fieldCascadeRuleMapper;
    @Autowired
    private FieldPermissionMapper fieldPermissionMapper;

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
        Map<Long, String> minRankMap = getMinRankMap(organizationId, projectId);
        List<ObjectSchemeFieldExtendDTO> insertList = new ArrayList<>();
        for (IssueTypeVO issueType : issueTypes) {
            String type = issueType.getTypeCode();
            Long typeId = issueType.getId();
            ObjectSchemeFieldExtendDTO dto = new ObjectSchemeFieldExtendDTO();
            dto.setIssueType(type);
            dto.setIssueTypeId(typeId);
            dto.setProjectId(projectId);
            dto.setOrganizationId(organizationId);
            dto.setFieldId(fieldId);
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
                String minRank = RankUtil.genPre(minRankMap.getOrDefault(typeId, RankUtil.mid()));
                dto.setRank(minRank);
            }
            insertList.add(dto);
        }
        objectSchemeFieldExtendMapper.batchInsert(insertList);
    }

    private Map<Long, String> getMinRankMap(Long organizationId, Long projectId) {
        List<ObjectSchemeFieldExtendDTO> minRankInfoList = objectSchemeFieldExtendMapper.selectIssueTypeMinRank(organizationId, projectId);
        if (CollectionUtils.isEmpty(minRankInfoList)){
            return new HashMap<>(0);
        }
        Map<Long, String> minRankMap = new HashMap<>(minRankInfoList.size());
        minRankInfoList.forEach(proMinRankInfo -> {
            if (minRankMap.get(proMinRankInfo.getId()) != null && proMinRankInfo.getProjectId() == null){
                return;
            }
            minRankMap.put(proMinRankInfo.getIssueTypeId(), proMinRankInfo.getRank());
        });
        return minRankMap;
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
        List<String> issueTypes = getLegalIssueTypes(projectId);
        List<ObjectSchemeFieldDTO> dtoList =
                objectSchemeFieldMapper.selectByOptions(organizationId, projectId, ObjectSchemeCode.AGILE_ISSUE, fieldId, null, issueTypes);
        if (dtoList.isEmpty()) {
            throw new CommonException(ERROR_FIELD_NOTFOUND);
        } else {
            return dtoList.get(0);
        }
    }

    private List<String> getLegalIssueTypes(Long projectId) {
        List<String> issueTypes = new ArrayList<>();
        if (!ObjectUtils.isEmpty(projectId)) {
            ProjectVO body = ConvertUtil.queryProject(projectId);
            if (!ObjectUtils.isEmpty(body) && ProjectCategory.checkContainProjectCategory(body.getCategories(),ProjectCategory.MODULE_PROGRAM)) {
                if(agilePluginService != null){
                    issueTypes = agilePluginService.addProgramIssueType();
                }
            } else {
                issueTypes = new ArrayList<>(ObjectSchemeFieldContext.NORMAL_PROJECT);
            }
            if (backlogExpandService != null && backlogExpandService.enabled(projectId)) {
                issueTypes.add(ObjectSchemeFieldContext.BACKLOG);
            }
        } else {
            issueTypes = ObjectSchemeFieldContext.getIssueTye();
        }
        return issueTypes;
    }

    @Override
    public List<ObjectSchemeFieldDTO> listQuery(Long organizationId, Long projectId, ObjectSchemeFieldSearchVO searchDTO) {
        return objectSchemeFieldMapper.listQuery(organizationId, new HashSet<>(Arrays.asList(projectId)), searchDTO, null);
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
        List<Long> filterIssueTypeIds = filterIssueTypeVO.stream().map(IssueTypeVO::getId).collect(Collectors.toList());
        Map<Long, IssueTypeVO> issueTypeVOMap = filterIssueTypeVO.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));
        fields.forEach(f -> {
            ObjectSchemeFieldVO vo = modelMapper.map(f, ObjectSchemeFieldVO.class);
            List<ObjectSchemeFieldExtendDTO> extendList = f.getExtendFields();
            List<Long> issueTypeIds = new ArrayList<>();
            List<String> issueTypeNames = new ArrayList<>();
            List<IssueTypeVO> issueTypeVOList = new ArrayList<>();
            boolean containsAllIssueTypes = containsAllIssueTypes(organizationId, projectId, issueTypeIds);
            String requiredScope =
                    processIssueTyeAndRequiredScope(f,issueTypeIds, filterIssueTypeIds,  issueTypeNames, true, extendList, containsAllIssueTypes, organizationId, projectId);
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
            } else {
                return;
            }
            List<IssueTypeVO> sortIssueTypeList = sortIssueTypeList(issueTypeVOList);
            vo.setIssueTypeVOList(sortIssueTypeList);
            vo.setContextName(sortIssueTypeList.stream().map(IssueTypeVO::getName).collect(Collectors.joining(",")));
            vo.setRequiredScope(requiredScope);
            fieldViews.add(vo);
        });
        return fieldViews;
    }

    private List<IssueTypeVO> sortIssueTypeList(List<IssueTypeVO> issueTypeVOList) {
        if(CollectionUtils.isEmpty(issueTypeVOList)){
            return new ArrayList<>();
        }
        Collections.sort(issueTypeVOList, (o1,o2) -> o2.getId().compareTo(o1.getId()));
        List<IssueTypeVO> list = new ArrayList<>();
        IssueTypeVO backlog = null;
        for (IssueTypeVO typeVO : issueTypeVOList) {
            if(Objects.equals("backlog" ,typeVO.getTypeCode())){
                backlog = typeVO;
            } else {
                list.add(typeVO);
            }
        }
        if (!ObjectUtils.isEmpty(backlog)) {
            list.add(backlog);
        }
        return list;
    }

    @Override
    public List<ObjectSchemeFieldVO> listPageFieldWithOption(Long organizationId,
                                                             Long projectId,
                                                             String schemeCode,
                                                             List<Long> issueTypeIds) {
        List<ObjectSchemeFieldVO> fieldVOS = generateFieldViews(organizationId, projectId, schemeCode);
        List<ObjectSchemeFieldDetailVO> objectSchemeFieldDetailVOList = objectSchemeFieldMapper.selectCustomFieldList(ConvertUtil.getOrganizationId(projectId), projectId, null);
        if (CollectionUtils.isEmpty(objectSchemeFieldDetailVOList)) {
            return fieldVOS.stream()
                    .filter(vo -> CollectionUtils.isEmpty(issueTypeIds)
                            || issueTypeIds.stream().anyMatch(
                                    item -> vo.getIssueTypeVOList()
                                            .stream()
                                            .map(IssueTypeVO::getId)
                                            .collect(Collectors.toList())
                                            .contains(item)))
                    .collect(Collectors.toList());
        }
        Map<Long, ObjectSchemeFieldDetailVO> map =
                objectSchemeFieldDetailVOList.stream().collect(Collectors.toMap(ObjectSchemeFieldDetailVO::getId,
                        Function.identity()));
        return fieldVOS.stream()
                .filter(vo -> CollectionUtils.isEmpty(issueTypeIds)
                        || issueTypeIds.stream().anyMatch(
                                item -> vo.getIssueTypeVOList()
                                        .stream()
                                        .map(IssueTypeVO::getId)
                                        .collect(Collectors.toList())
                                        .contains(item)))
                .peek(vo -> vo.setFieldOptions(map.getOrDefault(vo.getId(), new ObjectSchemeFieldDetailVO()).getFieldOptions()))
                .collect(Collectors.toList());
    }

    protected List<ObjectSchemeFieldDTO> selectFieldsByOptions(Long organizationId,
                                                               Long projectId,
                                                               String schemeCode,
                                                               Long fieldId,
                                                               Long issueTypeId) {
        List<String> issueTypes = new ArrayList<>();
        Boolean isProgram = false;
        boolean includeBacklogSystemField = false;
        if (!ObjectUtils.isEmpty(projectId)) {
            ProjectVO body = ConvertUtil.queryProject(projectId);
            if (!ObjectUtils.isEmpty(body) && ProjectCategory.checkContainProjectCategory(body.getCategories(),ProjectCategory.MODULE_PROGRAM)) {
                isProgram = true;
                if(agilePluginService != null){
                    issueTypes = agilePluginService.addProgramIssueType();
                }
            } else {
                issueTypes = new ArrayList<>(ObjectSchemeFieldContext.NORMAL_PROJECT);
            }
            if (backlogExpandService != null && backlogExpandService.enabled(projectId)) {
                issueTypes.add(ObjectSchemeFieldContext.BACKLOG);
                includeBacklogSystemField = true;
            }
        } else {
            //判断组织下如果没有开启需求池的项目，组织层不展示需求类型的系统字段
            List<ProjectVO> projectVOList = baseFeignClient.listProjectsByOrgId(organizationId).getBody();
            if (backlogExpandService != null && !CollectionUtils.isEmpty(projectVOList)) {
                List<Long> projectIds = projectVOList.stream().map(ProjectVO::getId).collect(Collectors.toList());
                for (Long id : projectIds) {
                    if (Boolean.TRUE.equals(backlogExpandService.enabled(id))) {
                        includeBacklogSystemField = true;
                        break;
                    }
                }
            }

        }
        List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS = objectSchemeFieldMapper.selectByOptions(organizationId, projectId, schemeCode, fieldId, issueTypeId, issueTypes);
        addNotSyncedField(objectSchemeFieldDTOS, issueTypes, includeBacklogSystemField);
        Collections.sort(objectSchemeFieldDTOS, Comparator.comparing(ObjectSchemeFieldDTO::getId));
        if(isProgram && agilePluginService != null){
           return agilePluginService.filterProgramEpic(objectSchemeFieldDTOS);
        }
        return filterFieldsByProjectCategories(objectSchemeFieldDTOS, projectId);
    }

    private List filterFieldsByProjectCategories(List list,
                                                 Long projectId) {
        if (projectId == null || ObjectUtils.isEmpty(list)) {
            return list;
        }
        Set<String> codes = getProjectCategoryCodes(projectId);
        boolean doFilter =
                codes.contains(ProjectCategory.MODULE_AGILE)
                        && !codes.contains(ProjectCategory.MODULE_DEVOPS);
        if (doFilter) {
            Object obj = list.get(0);
            List result = list;
            if (obj instanceof ObjectSchemeFieldDTO) {
                result =
                        ((List<ObjectSchemeFieldDTO>) list)
                                .stream()
                                .filter(x -> !FieldCode.TAG.equals(x.getCode()))
                                .collect(Collectors.toList());
            } else if (obj instanceof PageConfigFieldVO) {
                result =
                        ((List<PageConfigFieldVO>) list)
                                .stream()
                                .filter(x -> !FieldCode.TAG.equals(x.getFieldCode()))
                                .collect(Collectors.toList());
            } else if (obj instanceof ObjectSchemeFieldDetailVO) {
                result =
                        ((List<ObjectSchemeFieldDetailVO>) list)
                                .stream()
                                .filter(x -> !FieldCode.TAG.equals(x.getCode()))
                                .collect(Collectors.toList());
            }
            return result;
        } else {
            return list;
        }
    }

    private void addNotSyncedField(List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS, List<String> issueTypes, boolean includeBacklogSystemField) {
        List<Long> systemFieldIds = objectSchemeFieldDTOS.stream().filter(v -> Boolean.TRUE.equals(v.getSystem())).map(ObjectSchemeFieldDTO::getId).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(systemFieldIds)) {
            return;
        }
        List<ObjectSchemeFieldDTO> notSyncFields = objectSchemeFieldMapper.selectNotSyncField(systemFieldIds, includeBacklogSystemField);
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
        issueTypeSearchVO.setSource(CREATED_LEVEL_SYSTEM);
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
                String context = getFieldContext(field.getCode());
                // 过滤当前字段的问题类型
                String[] contexts = context.split(",");
                List<IssueTypeVO> issueTypeVOS = issueTypes.stream().filter(v -> Arrays.asList(contexts).contains(v.getTypeCode())).collect(Collectors.toList());
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
                issueTypeVOS.forEach(issueType -> {
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
    }

    private String processIssueTyeAndRequiredScope(ObjectSchemeFieldDTO fieldDTO,
                                                   List<Long> issueTypeIds,
                                                   List<Long> filterIssueTypeIds,
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
            Set<Long> ids = new HashSet<>();
            //系统字段
            if (Boolean.TRUE.equals(fieldDTO.getSystem())) {
                String fieldContext = getFieldContext(fieldDTO.getCode());
                IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
                issueTypeSearchVO.setTypeCodes(Arrays.asList(fieldContext.split(",")));
                ids = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO).stream().map(IssueTypeVO::getId).collect(Collectors.toSet());
            }
            //项目层的组织字段获取组织层下配置的问题类型
            else if (Boolean.FALSE.equals(fieldDTO.getSystem())) {
                List<ObjectSchemeFieldExtendDTO> fieldExtendDTOList = objectSchemeFieldExtendMapper.selectExtendFields(organizationId, fieldDTO.getId(), projectId, filterIssueTypeIds);
                ids = fieldExtendDTOList.stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toSet());
            }
            issueTypeIds.addAll(ids);
            allIsRequired = allIsRequired && fieldDTO.getRequired();
            allIsNotRequired = allIsNotRequired || fieldDTO.getRequired();
        }else {
            for (ObjectSchemeFieldExtendDTO e : extendList) {
                issueTypeIds.add(e.getIssueTypeId());
                allIsRequired = allIsRequired && e.getRequired();
                allIsNotRequired = allIsNotRequired || e.getRequired();
            }
        }
        if (resetIssueType && containsAllIssueTypes) {
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
        List<IssueTypeVO> issueTypes = getIssueTypeByIds(organizationId, projectId, issueTypeIds, true);
        ObjectSchemeFieldDTO field = modelMapper.map(fieldCreateDTO, ObjectSchemeFieldDTO.class);
        Set<String> typeCodes = issueTypes.stream().map(IssueTypeVO::getTypeCode).collect(Collectors.toSet());
        field.setContext(String.join(",", typeCodes));
        field.setOrganizationId(organizationId);
        field.setProjectId(projectId);
        decryptAndSetDefaultValue(field);
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
            Map<Long, String> issueTypeMap = getIssueTypeList(organizationId, projectId).stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
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

    private List<IssueTypeVO> getIssueTypeByIds(Long organizationId,
                                                Long projectId,
                                                List<Long> issueTypeIds,
                                                Boolean enabled) {
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setEnabled(enabled);
        List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO);
        Map<Long, IssueTypeVO> typeMap = issueTypes.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));
        Set<String> contextList = issueTypeIds
                .stream()
                .map(typeId -> typeMap.getOrDefault(typeId, new IssueTypeVO()).getTypeCode())
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        String[] contexts =  contextList.toArray(new String[0]);
        ObjectSchemeFieldContext.isIllegalContexts(contexts);
        return filterIssueType(projectId, issueTypes, issueTypeIds);
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
                processIssueTyeAndRequiredScope(field,issueTypeIds, null, issueTypeNames, false, extendList, containsAllIssueTypes, organizationId, projectId);
        ObjectSchemeFieldDetailVO fieldDetailDTO = modelMapper.map(field, ObjectSchemeFieldDetailVO.class);
        fieldDetailDTO.setRequiredScope(requiredScope);
        //获取字段选项，并设置默认值
        List<FieldOptionVO> fieldOptions = fieldOptionService.queryByFieldId(organizationId, fieldId);
        if (!fieldOptions.isEmpty()) {
            if (!ObjectUtils.isEmpty(field.getDefaultValue())) {
                List<String> defaultIds = Arrays.asList(field.getDefaultValue().split(","));
                fieldOptions.forEach(fieldOption -> fieldOption.setIsDefault(defaultIds.contains(fieldOption.getId().toString())));
                List<String> encryptList = EncryptionUtils.encryptListToStr(defaultIds);
                fieldDetailDTO.setDefaultValue(StringUtils.join(encryptList.toArray(), ","));
            } else {
                fieldOptions.forEach(fieldOption -> fieldOption.setIsDefault(false));
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
        isOrganizationIllegal(projectId, organizationId);
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
        if (Boolean.TRUE.equals(field.getSystem())) {
            throw new CommonException(ERROR_FIELD_ILLEGAL);
        }
        if (projectId == null) {
            //组织层判断项目是否使用了该字段，如果使用则不能删除
            List<Long> projectIds =
                    baseFeignClient.listProjectsByOrgId(organizationId)
                            .getBody()
                            .stream()
                            .map(ProjectVO::getId)
                            .collect(Collectors.toList());
            if (!projectIds.isEmpty()) {
                isFieldDeleted(projectIds, fieldId, null, null);
            }
        }
        fieldCascadeRuleMapper.deleteByFieldId(organizationId, projectId, fieldId, null);
        fieldPermissionMapper.deleteByFieldId(organizationId, projectId, fieldId, null);
        objectSchemeFieldMapper.cascadeDelete(organizationId, projectId, fieldId);
        //删除字段值
        fieldValueService.deleteByFieldId(fieldId);
        //删除日志
        fieldDataLogService.deleteByFieldId(projectId, fieldId);
    }

    @Override
    public ObjectSchemeFieldDetailVO update(Long organizationId, Long projectId, Long fieldId, ObjectSchemeFieldUpdateVO updateDTO) {
        ObjectSchemeFieldDTO update = modelMapper.map(updateDTO, ObjectSchemeFieldDTO.class);
        //处理context
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setIssueTypeIds(updateDTO.getIssueTypeIds());
        Set<String> typeCodes = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO)
                .stream().map(IssueTypeVO::getTypeCode).collect(Collectors.toSet());
        update.setContext(String.join(",", typeCodes));
        decryptAndSetDefaultValue(update);
        updateFieldIssueTypeAndDefaultValue(organizationId, projectId, fieldId, update.getDefaultValue(), updateDTO);
        update.setId(fieldId);
        baseUpdate(update);
        updateNumberTypeExtraConfig(organizationId, projectId, fieldId, updateDTO);
        return queryById(organizationId, projectId, fieldId);
    }

    private void updateNumberTypeExtraConfig(Long organizationId, Long projectId, Long fieldId, ObjectSchemeFieldUpdateVO updateDTO) {
        if (FieldType.NUMBER.equals(updateDTO.getFieldType())) {
            //数字类型字段，额外配置关联到所有问题类型
            ObjectSchemeFieldExtendDTO dto = new ObjectSchemeFieldExtendDTO();
            dto.setProjectId(projectId);
            dto.setOrganizationId(organizationId);
            dto.setFieldId(fieldId);
            boolean extraConfig = Boolean.TRUE.equals(updateDTO.getExtraConfig());
            objectSchemeFieldExtendMapper.select(dto).forEach(x -> {
                x.setExtraConfig(extraConfig);
                objectSchemeFieldExtendMapper.updateByPrimaryKeySelective(x);
            });
        }
    }

    private void decryptAndSetDefaultValue(ObjectSchemeFieldDTO field) {
        String defaultValue = tryDecryptDefaultValue(field.getFieldType(), field.getDefaultValue());
        if (defaultValue != null) {
            field.setDefaultValue(defaultValue);
        }
    }

    private void updateFieldIssueTypeAndDefaultValue(Long organizationId,
                                                     Long projectId,
                                                     Long fieldId,
                                                     String defaultValue,
                                                     ObjectSchemeFieldUpdateVO updateDTO) {
        if (ObjectUtils.isEmpty(updateDTO.getIssueTypeIds())) {
            throw new CommonException("error.field.issueTypeIds.empty");
        }

        List<IssueTypeVO> issueTypes = getIssueTypeByIds(organizationId, projectId, updateDTO.getIssueTypeIds(), null);
        Map<Long, String> issueTypeMap =
                issueTypes.stream().filter(issueType -> Boolean.TRUE.equals(issueType.getEnabled())).collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
        List<Long> disabledIssueTypeIds =
                issueTypes.stream().filter(issueType -> Boolean.FALSE.equals(issueType.getEnabled())).map(IssueTypeVO::getId).collect(Collectors.toList());
        List<Long> issueTypeIdList = new ArrayList<>(issueTypeMap.keySet());
        ObjectSchemeFieldDTO field =
                selectOneByFieldId(organizationId, projectId, fieldId);
        List<ObjectSchemeFieldExtendDTO> intersection = new ArrayList<>();
        List<ObjectSchemeFieldExtendDTO> deleteList = new ArrayList<>();
        Set<Long> insertSet = new HashSet<>();
        filterByIssueTypeId(intersection, deleteList, insertSet, issueTypeIdList, field, disabledIssueTypeIds);

        dealWithExtendFields(organizationId, projectId, fieldId, deleteList, insertSet, issueTypeMap, defaultValue, updateDTO.getExtraConfig());
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
            List<Long> projectIdList =
                    baseFeignClient.listProjectsByOrgId(organizationId)
                            .getBody()
                            .stream()
                            .map(ProjectVO::getId)
                            .collect(Collectors.toList());
            //组织层新增或删除，项目层数据同时新增或删除
            deleteList.forEach(d -> {
                Long issueTypeId = d.getIssueTypeId();
                String schemeCode = getSchemeCodeByIssueTypeId(issueTypeId);
                isFieldDeleted(projectIdList, fieldId, schemeCode, issueTypeId);
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
        ObjectSchemeFieldDTO field = objectSchemeFieldMapper.selectByPrimaryKey(fieldId);
        if (FieldType.NUMBER.equals(field.getFieldType())) {
            //数字类型，不更新extraConfig，只能通过update字段更新
            extraConfig = field.getExtraConfig();
        }
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
                List<IssueTypeVO> issueTypeVOList = getIssueTypeByIds(organizationId, projectId, new ArrayList<>(insertSet), true);
                Map<Long, String> issueTypeMap =
                        issueTypeVOList.stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
                for (Long i : insertSet) {
                    insertObjectSchemeFieldExtend(organizationId, projectId, fieldId, false, issueTypeMap, i, true, true, defaultValue, extraConfig);
                }
            }
            //同步默认值
            List<ObjectSchemeFieldExtendDTO> dtoList =
                    objectSchemeFieldExtendMapper.selectExtendFieldByOptions(null, organizationId, fieldId, projectId);
            for (ObjectSchemeFieldExtendDTO i : dtoList) {
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
            if (Boolean.FALSE.equals(field.getSystem())) {
                field = baseQueryById(organizationId, null, fieldId);
            } else {
                field = objectSchemeFieldMapper.selectOne(field);
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
        Set<Long> legalIssueTypeIds;
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        List<IssueTypeVO> issueTypeVOS = issueTypeService.queryByOrgId(organizationId, projectId);
        List<IssueTypeVO> filterIssueTypeVO = filterIssueType(projectId, issueTypeVOS, issueTypeVOS.stream().map(IssueTypeVO::getId).collect(Collectors.toList()));
        List<Long> filterIssueTypeIds = filterIssueTypeVO.stream().map(IssueTypeVO::getId).collect(Collectors.toList());
        if (Boolean.TRUE.equals(field.getSystem())) {
            List<String> legalIssueTypes = Arrays.asList(getFieldContext(field.getCode()).split(","));
            issueTypeSearchVO.setTypeCodes(legalIssueTypes);
            issueTypeSearchVO.setEnabled(true);
            legalIssueTypeIds = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO).stream().map(IssueTypeVO::getId).collect(Collectors.toSet());
        } else if (!Objects.isNull(projectId) && Objects.equals(field.getCreatedLevel(), CREATED_LEVEL_ORGANIZATION)) {
            List<ObjectSchemeFieldExtendDTO> fieldExtendDTOList = objectSchemeFieldExtendMapper.selectExtendFields(organizationId, field.getId(), projectId, filterIssueTypeIds);
            legalIssueTypeIds = fieldExtendDTOList.stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toSet());
        } else {
            issueTypeSearchVO.setEnabled(true);
            List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO);
            legalIssueTypeIds = issueTypes.stream().map(IssueTypeVO::getId).collect(Collectors.toSet());
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

    private void filterByIssueTypeId(List<ObjectSchemeFieldExtendDTO> intersection,
                                   List<ObjectSchemeFieldExtendDTO> deleteList,
                                   Set<Long> insertSet,
                                   List<Long> issueTypeIdList,
                                   ObjectSchemeFieldDTO field,
                                   List<Long> disabledIssueTypeIds) {
        List<ObjectSchemeFieldExtendDTO> extendList = field.getExtendFields();
        //交集
        extendList.forEach(e -> {
            if (issueTypeIdList.contains(e.getIssueTypeId())) {
                intersection.add(e);
            }
        });
        Set<Long> intersectionIssueTypeIds =
                intersection.stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toSet());
        //删除的类型
        extendList.forEach(e -> {
            //禁用的问题类型不做处理
            boolean disabled = !CollectionUtils.isEmpty(disabledIssueTypeIds) && disabledIssueTypeIds.contains(e.getIssueTypeId());
            if (!intersectionIssueTypeIds.contains(e.getIssueTypeId()) && !disabled) {
                deleteList.add(e);
            }
        });
        //插入的类型
        issueTypeIdList.forEach(c -> {
            //禁用的问题类型不做处理
            boolean disabled = !CollectionUtils.isEmpty(disabledIssueTypeIds) && disabledIssueTypeIds.contains(c);
            if (!intersectionIssueTypeIds.contains(c) && !disabled) {
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
            try {
                if (FieldType.multipleFieldType.contains(fieldType) && !ObjectUtils.isEmpty(defaultValue)) {
                    String[] splits = defaultValue.split(",");
                    List<String> list = new ArrayList<>();
                    for (String split : splits) {
                        list.add(EncryptionUtils.decrypt(split));
                    }
                    return list.stream().collect(Collectors.joining(","));
                } else {
                    return EncryptionUtils.decrypt(defaultValue);
                }
            } catch (Exception e) {
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
        IssueTypeVO issueTypeVO = issueTypeService.queryById(issueTypeId, projectId);
        List<String> fieldCodes = getIssueTypeFieldCodes(issueTypeVO.getId(), organizationId, projectId);
        List<String> existFieldCodes = list.stream().filter(v -> Boolean.TRUE.equals(v.getSystem())).map(ObjectSchemeFieldDetailVO::getCode).collect(Collectors.toList());
        fieldCodes.removeAll(existFieldCodes);
        if (!CollectionUtils.isEmpty(fieldCodes)) {
            List<ObjectSchemeFieldDetailVO> addFields = modelMapper.map(objectSchemeFieldMapper.selectFieldsByFieldCodes(fieldCodes),  new TypeToken<List<ObjectSchemeFieldDetailVO>>(){}.getType());
            list.addAll(addFields);
        }
        return filterFieldsByProjectCategories(list, projectId);
    }

    private Set<String> getProjectCategoryCodes(Long projectId) {
        ProjectVO project = baseFeignClient.queryProject(projectId).getBody();
        return project
                .getCategories()
                .stream()
                .map(ProjectCategoryDTO::getCode)
                .collect(Collectors.toSet());
    }

    private List<String> getIssueTypeFieldCodes(Long issueTypeId, Long organizationId, Long projectId) {
        Long newProjectId = projectId == null ? 0L : projectId;
        List<String> fieldCodeS = new ArrayList<>();
        List<AgileSystemFieldContext> agileSystemFieldContexts = Arrays.asList(AgileSystemFieldContext.values());
        agileSystemFieldContexts.forEach(v -> {
            IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
            issueTypeSearchVO.setTypeCodes(Arrays.asList(v.getContext().split(",")));
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
            Map<Long, String> issueTypeMap = getIssueTypeList(organizationId, projectId).stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
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
            // single,radio,checkbox,multiple
            if (!fieldOptions.isEmpty()) {
                if (!ObjectUtils.isEmpty(v.getDefaultValue())) {
                    List<String> defaultIds = Arrays.asList(v.getDefaultValue().split(","));
                    List<String> encryptList = EncryptionUtils.encryptListToStr(defaultIds);
                    v.setDefaultValue(StringUtils.join(encryptList.toArray(), ","));
                }
                v.setFieldOptions(fieldOptions);
            }
            //member
            else if (FieldType.MEMBER.equals(v.getFieldType())) {
                if (v.getDefaultValue() != null && !"".equals(v.getDefaultValue())) {
                    Long defaultValue = Long.valueOf(String.valueOf(v.getDefaultValue()));
                    v.setDefaultValue(EncryptionUtils.encrypt(defaultValue));
                    List<UserDTO> list = baseFeignClient.listUsersByIds(Arrays.asList(defaultValue).toArray(new Long[1]), false).getBody();
                    if (!list.isEmpty()) {
                        v.setDefaultValueObj(list.get(0));
                    }
                }
            }
            //multiMember
            else if (FieldType.MULTI_MEMBER.equals(v.getFieldType()) && v.getDefaultValue() != null && !"".equals(v.getDefaultValue())) {
                List<String> defaultIds = Arrays.asList(v.getDefaultValue().split(","));
                List<String> encryptList = EncryptionUtils.encryptListToStr(defaultIds);
                v.setDefaultValue(StringUtils.join(encryptList.toArray(), ","));
                List<UserDTO> list = baseFeignClient.listUsersByIds(defaultIds.stream().map(Long::valueOf).toArray(Long[]::new), false).getBody();
                if (!CollectionUtils.isEmpty(list)) {
                    v.setDefaultValueObj(list);
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
        isOrganizationIllegal(projectId, organizationId);
        Long issueTypeId = pageConfigUpdateVO.getIssueTypeId();
        List<PageConfigFieldVO> fields = pageConfigUpdateVO.getFields();
        IssueTypeFieldVO issueTypeFieldVO = pageConfigUpdateVO.getIssueTypeFieldVO();
        Set<Long> deleteIds = pageConfigUpdateVO.getDeleteIds();

        String issueType = issueTypeService.getIssueTypeById(issueTypeId);
        ObjectSchemeFieldContext.isIllegalIssueType(issueType);
        Map<Long, String> issueTypeMap = getIssueTypeList(organizationId, projectId).stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
        if (!ObjectUtils.isEmpty(fields)) {
            updateFieldConfig(organizationId, projectId, issueTypeId, fields, issueTypeMap);
        }
        if (!ObjectUtils.isEmpty(projectId)
                && !ObjectUtils.isEmpty(issueTypeFieldVO)) {
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

    private void isOrganizationIllegal(Long projectId, Long organizationId) {
        if (projectId != null
                && !Objects.equals(organizationId, ConvertUtil.getOrganizationId(projectId))) {
            throw new CommonException("error.project.not.belong.organization");
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
                    //删除字段值
                    fieldValueService.deleteByFieldId(fieldId);
                    //删除日志
                    fieldDataLogService.deleteByFieldId(projectId, fieldId);
                }
                Long issueTypeId = extend.getIssueTypeId();
                fieldCascadeRuleMapper.deleteByFieldId(organizationId, projectId, fieldId, issueTypeId);
                fieldPermissionMapper.deleteByFieldId(organizationId, projectId, fieldId, issueTypeId);
                objectSchemeFieldExtendMapper.deleteByPrimaryKey(d);
            });
        } else {
            //获取组织下所有项目
            List<Long> projectIds =
                    baseFeignClient.listProjectsByOrgId(organizationId)
                            .getBody()
                            .stream()
                            .map(ProjectVO::getId)
                            .collect(Collectors.toList());
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
                deleteFieldValueAndDataLog(projectIds, extend.getIssueTypeId(), fieldId, false);
            });
        }
    }

    private void deleteFieldValueAndDataLog(List<Long> projectIds,
                                            Long issueTypeId,
                                            Long fieldId,
                                            boolean editOnProjectLevel) {
        if (ObjectUtils.isEmpty(projectIds)) {
            return;
        }
        String schemeCode = getSchemeCodeByIssueTypeId(issueTypeId);
        if (ObjectSchemeCode.BACKLOG.equals(schemeCode)) {
            if (editOnProjectLevel) {
                Long projectId = projectIds.get(0);
                FieldValueDTO fieldValueDTO = new FieldValueDTO();
                fieldValueDTO.setFieldId(fieldId);
                fieldValueDTO.setProjectId(projectId);
                fieldValueDTO.setSchemeCode(schemeCode);
                fieldValueMapper.delete(fieldValueDTO);

                FieldDataLogDTO fieldDataLogDTO = new FieldDataLogDTO();
                fieldDataLogDTO.setProjectId(projectId);
                fieldDataLogDTO.setSchemeCode(schemeCode);
                fieldDataLogDTO.setFieldId(fieldId);
                fieldDataLogMapper.delete(fieldDataLogDTO);
            } else {
                isFieldDeleted(projectIds, fieldId, schemeCode, issueTypeId);
            }
        } else if (ObjectSchemeCode.AGILE_ISSUE.equals(schemeCode)) {
            List<Long> issueIds = issueMapper.selectIdsByIssueTypeIdsAndProjectIds(projectIds, issueTypeId);
            if (!issueIds.isEmpty()) {
                if (editOnProjectLevel) {
                    Long projectId = projectIds.get(0);
                    fieldValueMapper.deleteByInstanceIds(projectId, issueIds, schemeCode, fieldId);
                    fieldDataLogMapper.deleteByInstanceIdsAndFieldIds(projectId, issueIds, schemeCode, Arrays.asList(fieldId));
                } else {
                    //组织下的项目如果有相关的field_value值，如果有不允许删除
                    isFieldDeleted(projectIds, fieldId, schemeCode, issueTypeId);
                }
            }
        } else {
            throw new CommonException("error.illegal.schemeCode");
        }
    }

    private void isFieldDeleted(List<Long> projectIds,
                                Long fieldId,
                                String schemeCode,
                                Long issueTypeId) {
        if (ObjectUtils.isEmpty(projectIds)) {
            return;
        }
        List<Long> instanceIds = fieldValueMapper.queryInstanceByFieldIdAndIssueTypeId(projectIds, schemeCode, fieldId, issueTypeId);
        List<FieldCascadeRuleDTO> fieldCascadeRules = fieldCascadeRuleMapper.selectByOptions(projectIds, new HashSet<>(Arrays.asList(fieldId)), issueTypeId);
        List<FieldPermissionDTO> fieldPermissions = fieldPermissionMapper.selectByOptions(projectIds, new HashSet<>(Arrays.asList(fieldId)), issueTypeId);
        boolean isDeleted =
                instanceIds.isEmpty()
                        && fieldCascadeRules.isEmpty()
                        && fieldPermissions.isEmpty();
        if (!isDeleted) {
            throw new CommonException("error.organization.field.can.not.delete");
        }
    }

    private String getSchemeCodeByIssueTypeId(Long issueTypeId) {
        IssueTypeDTO dto = issueTypeMapper.selectByPrimaryKey(issueTypeId);
        if (dto != null) {
            String typeCode = dto.getTypeCode();
            if (IssueTypeCode.BACKLOG.value().equals(typeCode)) {
                return ObjectSchemeCode.BACKLOG;
            } else {
                return ObjectSchemeCode.AGILE_ISSUE;
            }
        }
        return null;
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
        // 统计组织字段项目中的使用情况
        setInstanceCount(organizationId, projectId, issueTypeId, pageConfigFields);
        return result;
    }

    private void setInstanceCount(Long organizationId, Long projectId, Long issueTypeId, List<PageConfigFieldVO> pageConfigFields) {
        boolean editOnProjectLevel = (projectId != null);
        // 过滤组织字段
        List<Long> fieldIds = pageConfigFields.stream()
                .filter(v -> Objects.equals(CREATED_LEVEL_ORGANIZATION, v.getCreatedLevel()))
                .map(PageConfigFieldVO::getFieldId)
                .collect(Collectors.toList());
        if (!editOnProjectLevel && !CollectionUtils.isEmpty(fieldIds)) {
            // 获取组织下所有项目
            List<Long> projectIds =
                    baseFeignClient.listProjectsByOrgId(organizationId)
                            .getBody()
                            .stream()
                            .map(ProjectVO::getId)
                            .collect(Collectors.toList());
            String schemeCode = getSchemeCodeByIssueTypeId(issueTypeId);
            Map<Long, FieldInstanceCountVO> fieldInstanceCountMap =
                    fieldValueMapper.queryInstanceCountByFieldIds(projectIds, schemeCode, fieldIds, issueTypeId)
                            .stream().collect(Collectors.toMap(FieldInstanceCountVO::getFieldId, Function.identity()));
            Map<Long, Integer> fieldCascadeRulesMap = processFieldCascadeRulesMap(issueTypeId, fieldIds, projectIds);
            Map<Long, Integer> fieldPermissionsMap = processFieldPermissionsMap(issueTypeId, fieldIds, projectIds);
            pageConfigFields.forEach(field -> {
                Long fieldId = field.getFieldId();
                Integer instanceCount = 0;
                FieldInstanceCountVO fieldInstanceCountVO = fieldInstanceCountMap.get(field.getFieldId());
                if (fieldInstanceCountVO != null) {
                    instanceCount = Optional.ofNullable(fieldInstanceCountVO.getInstanceCount()).orElse(0);
                }
                Integer fieldCascadeRuleCount = Optional.ofNullable(fieldCascadeRulesMap.get(fieldId)).orElse(0);
                Integer fieldPermissionCount = Optional.ofNullable(fieldPermissionsMap.get(fieldId)).orElse(0);
                int total = instanceCount + fieldCascadeRuleCount + fieldPermissionCount;
                if (total == 0) {
                    field.setInstanceCount(null);
                } else {
                    field.setInstanceCount(total);
                }
            });
        }
    }

    private Map<Long, Integer> processFieldPermissionsMap(Long issueTypeId,
                                                          List<Long> fieldIds,
                                                          List<Long> projectIds) {
        Map<Long, Integer> fieldPermissionsMap = new HashMap<>();
        List<FieldPermissionDTO> fieldPermissions = fieldPermissionMapper.selectByOptions(projectIds, new HashSet<>(fieldIds), issueTypeId);
        fieldPermissions.forEach(fieldPermission -> {
            Long fieldId = fieldPermission.getFieldId();
            fieldPermissionsMap.put(fieldId, 1);
        });
        return fieldPermissionsMap;
    }

    private Map<Long, Integer> processFieldCascadeRulesMap(Long issueTypeId,
                                                           List<Long> fieldIds,
                                                           List<Long> projectIds) {
        Map<Long, Integer> fieldCascadeRulesMap = new HashMap<>();
        List<FieldCascadeRuleDTO> fieldCascadeRules = fieldCascadeRuleMapper.selectByOptions(projectIds, new HashSet<>(fieldIds), issueTypeId);
        fieldCascadeRules.forEach(fieldCascadeRule -> {
            Long fieldId = fieldCascadeRule.getFieldId();
            Long cascadeFieldId = fieldCascadeRule.getCascadeFieldId();
            fieldCascadeRulesMap.put(fieldId, 1);
            fieldCascadeRulesMap.put(cascadeFieldId, 1);
        });
        return fieldCascadeRulesMap;
    }

    @Override
    public List<PageConfigFieldVO> queryPageConfigFields(Long organizationId, Long projectId, Long issueTypeId) {
        List<PageConfigFieldVO> pageConfigFieldVOS = objectSchemeFieldExtendMapper.listConfigs(organizationId, projectId, issueTypeId);
        // 有新增系统字段未配置到字段扩展表直接添加字段到pageConfigFieldVOS
        addPageFieldVOS(pageConfigFieldVOS,organizationId,projectId,issueTypeId);
        if (agilePluginService != null) {
           return filterFieldsByProjectCategories(
                   agilePluginService.queryProgramPageConfigFields(projectId,issueTypeId,pageConfigFieldVOS),
                   projectId);
        }
        return filterFieldsByProjectCategories(pageConfigFieldVOS, projectId);
    }

    @Override
    public List<ObjectSchemeFieldVO> getAllField(Long organizationId, Long projectId, String schemeCode, String issueTypeList) {
        List<ObjectSchemeFieldVO> objectSchemeFieldVOS = generateFieldViews(organizationId, projectId, schemeCode);
        objectSchemeFieldVOS = filterByIssueTypeList(objectSchemeFieldVOS, issueTypeList);
        return objectSchemeFieldVOS;
    }

    @Override
    public List<ObjectSchemeFieldDetailVO> queryCustomFieldListWithOutOption(Long projectId, String issueTypeList) {
        List<ObjectSchemeFieldDetailVO> objectSchemeFieldDetailVOList = objectSchemeFieldMapper.selectCustomFieldListWithOutOption(ConvertUtil.getOrganizationId(projectId), projectId, issueTypeList);
        if (objectSchemeFieldDetailVOList != null && !objectSchemeFieldDetailVOList.isEmpty()) {
            return objectSchemeFieldDetailVOList;
        } else {
            return new ArrayList<>();
        }
    }

    private List<ObjectSchemeFieldVO> filterByIssueTypeList(List<ObjectSchemeFieldVO> objectSchemeFieldVOS, String issueTypeList) {
        if (CollectionUtils.isEmpty(objectSchemeFieldVOS)) {
            return new ArrayList<>();
        }

        List<ObjectSchemeFieldVO> list = new ArrayList<>();
        List<String> issueTypes = new ArrayList<>();
        if (Objects.equals("agileIssueType", issueTypeList)) {
            issueTypes.addAll(Arrays.asList(AGILE_ISSUE_TYPE_LIST));
        } else if (Objects.equals("programIssueType", issueTypeList)) {
            issueTypes.addAll(Arrays.asList(PROGRAM_ISSUE_TYPE_LIST));
        } else if (Objects.equals("backlogIssueType", issueTypeList)) {
            issueTypes.add("backlog");
        }
        objectSchemeFieldVOS.forEach(v -> {
            Boolean needAdd = false;
            List<String> contexts = v.getContexts();
            if (!CollectionUtils.isEmpty(contexts)) {
                contexts.retainAll(issueTypes);
                if (!issueTypes.isEmpty()) {
                    needAdd = true;
                }
            }
            if (Boolean.TRUE.equals(needAdd)) {
                ObjectSchemeFieldVO objectSchemeFieldVO = new ObjectSchemeFieldVO();
                objectSchemeFieldVO.setId(v.getId());
                objectSchemeFieldVO.setFieldType(v.getFieldType());
                objectSchemeFieldVO.setCode(v.getCode());
                objectSchemeFieldVO.setName(v.getName());
                objectSchemeFieldVO.setSystem(v.getSystem());
                list.add(objectSchemeFieldVO);
            }
        });
        return list;
    }

    private void addPageFieldVOS(List<PageConfigFieldVO> pageConfigFieldVOS, Long organizationId, Long projectId, Long issueTypeId) {
        List<PageConfigFieldVO> systemFieldS = pageConfigFieldVOS.stream().filter(pageConfigFieldVO -> Objects.equals(pageConfigFieldVO.getCreatedLevel(), CREATED_LEVEL_SYSTEM)).collect(Collectors.toList());
        // 当前问题类型的预定义字段
        List<String> fieldCodeS = getIssueTypeFieldCodes(issueTypeId, organizationId, projectId);
        if (!CollectionUtils.isEmpty(fieldCodeS)) {
            List<String> existField = systemFieldS.stream().map(PageConfigFieldVO::getFieldCode).collect(Collectors.toList());
            fieldCodeS.removeAll(existField);
            if (CollectionUtils.isEmpty(fieldCodeS)) {
                return;
            }
            List<IssueTypeVO> issueTypeList = getIssueTypeList(organizationId, projectId);
            Map<Long, String> issueTypeCodeMap = issueTypeList.stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
            Map<Long, IssueTypeVO> issueTypeMap = issueTypeList.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));
            List<PageConfigFieldVO> configFieldVOS = objectSchemeFieldExtendMapper.listConfigsByFieldCodes(fieldCodeS);
            for (PageConfigFieldVO configFieldVO : configFieldVOS) {
                if (!Objects.isNull(issueTypeMap.get(issueTypeId))) {
                    SystemFieldPageConfig.CommonField commonField = SystemFieldPageConfig.CommonField.queryByField(configFieldVO.getFieldCode());
                    if (!ObjectUtils.isEmpty(commonField)) {
                        configFieldVO.setCreated(commonField.created());
                        configFieldVO.setEdited(commonField.edited());
                    }
                    configFieldVO.setIssueTypeId(issueTypeId);
                    ObjectSchemeFieldExtendDTO extendDTO = insertExtendFieldByConfig(organizationId, issueTypeMap.get(issueTypeId).getProjectId(), configFieldVO, issueTypeCodeMap);
                    configFieldVO.setRank(extendDTO.getRank());
                    pageConfigFieldVOS.add(configFieldVO);
                }
            }
        }
    }

    private ObjectSchemeFieldExtendDTO insertExtendFieldByConfig(Long organizationId, Long projectId, PageConfigFieldVO vo, Map<Long, String> issueTypeMap) {
        Long newProjectId = Objects.equals(0L, projectId) ? null : projectId;
        String defaultValue = Objects.isNull(vo.getDefaultValue()) ? null : vo.getDefaultValue().toString();
        return insertObjectSchemeFieldExtend(organizationId, newProjectId,
                vo.getFieldId(), vo.getRequired(), issueTypeMap,
                vo.getIssueTypeId(), vo.getCreated(), vo.getEdited(),
                defaultValue, vo.getExtraConfig());
    }

    private List<IssueTypeVO> getIssueTypeList(Long organizationId, Long projectId) {
        Long newProjectId = projectId == null ? 0L : projectId;
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setEnabled(true);
        List<IssueTypeVO> issueTypeList =
                issueTypeMapper.selectByOptions(organizationId, newProjectId, issueTypeSearchVO);
        return issueTypeList;
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
                    SystemFieldPageConfig.CommonField commonField = SystemFieldPageConfig.CommonField.queryByField(fieldCode);
                    if (!ObjectUtils.isEmpty(commonField) && !ObjectUtils.isEmpty(fieldEdited)) {
                        if (Boolean.TRUE.equals(fieldEdited.getCreatedFieldCanNotEdit())) {
                            p.setCreated(commonField.created());
                        }
                        if (Boolean.TRUE.equals(fieldEdited.getEditedFieldCanNotEdit())) {
                            p.setEdited(commonField.edited());
                        }
                    }
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
            List<Long> newDefaultIds = new ArrayList<>();
            defaultIds.forEach(id -> {
                if (valueMap.containsKey(id)) {
                    defaultObjs.add(valueMap.get(id));
                    newDefaultIds.add(id);
                }
            });
            view.setDefaultValue(StringUtils.join(newDefaultIds, ","));
            view.setDefaultValueObjs(defaultObjs);
        }
    }

    @Override
    public void setDefaultValueObjsOfSingle(Map<Long, Object> valueMap, PageFieldViewVO view) {
        long defaultId = Long.parseLong(view.getDefaultValue().toString());
        if (valueMap.containsKey(defaultId)) {
            view.setDefaultValueObj(valueMap.get(defaultId));
        } else {
            view.setDefaultValue("");
        }
    }

    private void updateTemplate(Long projectId,
                                Long issueTypeId,
                                IssueTypeFieldVO issueTypeFieldVO,
                                Map<Long, String> issueTypeMap) {
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
        if (CollectionUtils.isEmpty(extendDTO)) {
            extendDTO = objectSchemeFieldExtendMapper.selectExtendFieldByOptions(Collections.singletonList(issueTypeId), organizationId, summaryFieldDTO.getId(), null);
        }
        if (!CollectionUtils.isEmpty(extendDTO)) {
            return extendDTO.get(0).getDefaultValue();
        }
        return null;
    }

}
