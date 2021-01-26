package io.choerodon.agile.app.service.impl;

import  io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.agile.infra.utils.ProjectCategoryUtil;
import io.choerodon.agile.infra.utils.RankUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.agile.api.vo.*;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author shinan.chen
 * @Date 2018/8/8
 */
@Service
@RefreshScope
@Transactional(rollbackFor = Exception.class)
public class IssueTypeServiceImpl implements IssueTypeService {

    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private StateMachineSchemeConfigService stateMachineSchemeConfigService;
    @Autowired
    private StateMachineService stateMachineService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    private static final Long ZERO = 0L;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private ProjectConfigMapper projectConfigMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueTypeSchemeConfigMapper issueTypeSchemeConfigMapper;
    @Autowired
    private IssueTypeExtendMapper issueTypeExtendMapper;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired
    private StatusMachineSchemeConfigMapper statusMachineSchemeConfigMapper;

    private static final String ORGANIZATION = "organization";

    private static final String PROJECT = "project";

    private static final List<String> AGILE_CREATE_ISSUE_TYPES =
            Arrays.asList(
                    IssueTypeCode.STORY.value(),
                    IssueTypeCode.SUB_TASK.value(),
                    IssueTypeCode.TASK.value(),
                    IssueTypeCode.BUG.value()
            );

    private static final List<String> AGILE_ISSUE_TYPES =
            Arrays.asList(
                    IssueTypeCode.STORY.value(),
                    IssueTypeCode.SUB_TASK.value(),
                    IssueTypeCode.TASK.value(),
                    IssueTypeCode.BUG.value(),
                    IssueTypeCode.ISSUE_EPIC.value()
            );

    private static final List<String> PROGRAM_ISSUE_TYPES =
            Arrays.asList(
                    IssueTypeCode.ISSUE_EPIC.value(),
                    IssueTypeCode.FEATURE.value());

    private static final Map<String, List<String>> TYPE_CODE_CATEGORY_MAP;

    static {
        TYPE_CODE_CATEGORY_MAP = new HashMap<>();
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.BUG.value(), Arrays.asList(ProjectCategory.MODULE_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.FEATURE.value(), Arrays.asList(ProjectCategory.MODULE_PROGRAM));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.ISSUE_EPIC.value(), Arrays.asList(ProjectCategory.MODULE_PROGRAM, ProjectCategory.MODULE_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.STORY.value(), Arrays.asList(ProjectCategory.MODULE_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.SUB_TASK.value(), Arrays.asList(ProjectCategory.MODULE_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.TASK.value(), Arrays.asList(ProjectCategory.MODULE_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.BACKLOG.value(), Arrays.asList(ProjectCategory.MODULE_BACKLOG));
    }


    @Override
    public IssueTypeVO queryById(Long organizationId, Long issueTypeId) {
        IssueTypeDTO issueType = issueTypeMapper.selectByPrimaryKey(issueTypeId);
        if (issueType != null) {
            return modelMapper.map(issueType, IssueTypeVO.class);
        }
        return null;
    }

    @Override
    public IssueTypeVO create(Long organizationId,
                              Long projectId,
                              IssueTypeVO issueTypeVO) {
        issueTypeVO.setProjectId(projectId);
        issueTypeVO.setOrganizationId(organizationId);
        String typeCode = issueTypeVO.getTypeCode();
        Set<String> categoryCodes = validateTypeCode(typeCode, projectId);
        if (nameExisted(organizationId, projectId, issueTypeVO.getName(), null)) {
            throw new CommonException("error.issue.type.name.existed");
        }
        issueTypeVO.setInitialize(false);
        issueTypeVO.setReferenced(ZERO.equals(projectId));
        String source = issueTypeVO.getSource();
        if (!StringUtils.hasText(source)) {
            source = ZERO.equals(projectId) ? ORGANIZATION : PROJECT;
        }
        issueTypeVO.setSource(source);
        IssueTypeDTO issueType = modelMapper.map(issueTypeVO, IssueTypeDTO.class);
        IssueTypeVO result = modelMapper.map(createIssueType(issueType), IssueTypeVO.class);
        if (!ZERO.equals(projectId)) {
            updateExtendEnabled(result.getId(), projectId, organizationId, true);
            //项目层创建问题类型,初始化默认状态机
            initDefaultStateMachine(organizationId, projectId, result, categoryCodes);
            //初始化项目系统字段与问题类型关系
            initIssueTypeAndFieldRel(organizationId, projectId, result);
        } else {
            //初始化组织系统字段与问题类型关系
            initIssueTypeAndFieldRel(organizationId, null, result);
        }
        return result;
    }

    private void initIssueTypeAndFieldRel(Long organizationId,
                                          Long projectId,
                                          IssueTypeVO result) {
        ObjectSchemeFieldDTO field = new ObjectSchemeFieldDTO();
        field.setSystem(true);
        String typeCode = result.getTypeCode();
        List<ObjectSchemeFieldDTO> fields =
                objectSchemeFieldMapper.select(field)
                        .stream()
                        .filter(x -> {
                            String context = AgileSystemFieldContext.getContextByFieldCode(x.getCode());
                            if (context != null) {
                                for (String str : context.split(",")) {
                                    if (str.trim().equals(typeCode)) {
                                        return true;
                                    }
                                }
                            }
                            return false;
                        }).collect(Collectors.toList());
        Long issueTypeId = result.getId();
        fields.forEach(x -> {
            Boolean required = x.getRequired();
            SystemFieldPageConfig.CommonField commonField = SystemFieldPageConfig.CommonField.queryByField(x.getCode());
            Boolean created = false;
            Boolean edited = false;
            if (!ObjectUtils.isEmpty(commonField)) {
                created = commonField.created();
                edited = commonField.edited();
            }
            String rank = objectSchemeFieldExtendMapper.selectMinRankByIssueTypeId(organizationId, projectId, issueTypeId);
            if (rank == null) {
                rank = RankUtil.mid();
            } else {
                rank = RankUtil.genPre(rank);
            }
            Long fieldId = x.getId();
            ObjectSchemeFieldExtendDTO extendDTO = new ObjectSchemeFieldExtendDTO();
            if (objectSchemeFieldExtendMapper.selectExtendFieldByOptions(Arrays.asList(issueTypeId), organizationId, fieldId, projectId).isEmpty()) {
                extendDTO.setIssueTypeId(issueTypeId);
                extendDTO.setIssueType(typeCode);
                extendDTO.setOrganizationId(organizationId);
                extendDTO.setProjectId(projectId);
                extendDTO.setFieldId(fieldId);
                extendDTO.setRequired(required);
                extendDTO.setCreated(created);
                extendDTO.setEdited(edited);
                extendDTO.setRank(rank);
                extendDTO.setDefaultValue(x.getDefaultValue());
                extendDTO.setExtraConfig(x.getExtraConfig());
                objectSchemeFieldExtendMapper.insertSelective(extendDTO);
            }
        });
    }

    private void updateExtendEnabled(Long issueTypeId,
                                     Long projectId,
                                     Long organizationId,
                                     Boolean enabled) {
        IssueTypeExtendDTO dto = new IssueTypeExtendDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setIssueTypeId(issueTypeId);
        List<IssueTypeExtendDTO> list = issueTypeExtendMapper.select(dto);
        if (list.isEmpty()) {
            dto.setEnabled(enabled);
            issueTypeExtendMapper.insert(dto);
        } else {
            dto = list.get(0);
            dto.setEnabled(enabled);
            issueTypeExtendMapper.updateByPrimaryKey(dto);
        }
    }

    private Set<String> validateTypeCode(String typeCode, Long projectId) {
        if (!IssueTypeCode.contains(typeCode)) {
            throw new CommonException("error.illegal.issue.type.code");
        }
        if (ZERO.equals(projectId)) {
            return new HashSet<>();
        }
        Set<String> codes = getProjectCategoryCodes(projectId);
        List<String> issueTypes = new ArrayList<>(AGILE_CREATE_ISSUE_TYPES);
//        issueTypes.add(IssueTypeCode.BACKLOG.value());
        if (codes.contains(ProjectCategory.MODULE_AGILE)
                && !issueTypes.contains(typeCode)) {
            throw new CommonException("error.illegal.type.code");
        }
        if (codes.contains(ProjectCategory.MODULE_PROGRAM)) {
            throw new CommonException("error.program.can.not.create.custom.issue.type");
        }
//        issueTypes.clear();
//        issueTypes.addAll(PROGRAM_ISSUE_TYPES);
//        issueTypes.add(IssueTypeCode.BACKLOG.value());
//        if (codes.contains(ProjectCategory.MODULE_PROGRAM)
//                && !issueTypes.contains(typeCode)) {
//            throw new CommonException("error.illegal.type.code");
//        }
//        if (!codes.contains(ProjectCategory.MODULE_BACKLOG)
//                && IssueTypeCode.BACKLOG.value().equals(typeCode)) {
//            throw new CommonException("error.project.backlog.not.open");
//        }
        return codes;
    }

    private Set<String> getProjectCategoryCodes(Long projectId) {
        ProjectVO project = baseFeignClient.queryProject(projectId).getBody();
        if (project == null) {
            throw new CommonException("error.project.not.existed");
        }
        return ProjectCategoryUtil.getCategoryCodeAndValidate(project.getCategories());
    }

    private void initDefaultStateMachine(Long organizationId,
                                         Long projectId,
                                         IssueTypeVO issueType,
                                         Set<String> categoryCodes) {
        Long issueTypeId = issueType.getId();
        String typeCode = issueType.getTypeCode();
        String applyType = getApplyTypeByCategoryCodes(categoryCodes, typeCode);
        Long stateMachineSchemeId = getSchemeIdByOption(projectId, applyType, SchemeType.STATE_MACHINE);
        stateMachineSchemeConfigService.queryStatusMachineBySchemeIdAndIssueType(organizationId, stateMachineSchemeId, issueTypeId);
        //初始化问题类型方案配置
        initIssueTypeSchemeConfig(organizationId, projectId, issueTypeId, applyType);
    }

    private String getApplyTypeByCategoryCodes(Set<String> categoryCodes, String typeCode) {
        String applyType;
        if (categoryCodes.contains(ProjectCategory.MODULE_AGILE)) {
            applyType = "agile";
        } else {
            applyType = "program";
        }
        if (IssueTypeCode.BACKLOG.value().equals(typeCode)) {
            applyType = "backlog";
        }
        return applyType;
    }

    private void initIssueTypeSchemeConfig(Long organizationId, Long projectId, Long issueTypeId, String applyType) {
        Long issueTypeSchemeId = getSchemeIdByOption(projectId, applyType, SchemeType.ISSUE_TYPE);
        IssueTypeSchemeConfigDTO issueTypeSchemeConfig = new IssueTypeSchemeConfigDTO();
        issueTypeSchemeConfig.setOrganizationId(organizationId);
        issueTypeSchemeConfig.setIssueTypeId(issueTypeId);
        issueTypeSchemeConfig.setSchemeId(issueTypeSchemeId);
        if (issueTypeSchemeConfigMapper.select(issueTypeSchemeConfig).isEmpty()) {
            BigDecimal sequence = issueTypeSchemeConfigMapper.selectMaxSequence(issueTypeSchemeId, organizationId);
            if (sequence == null) {
                sequence = new BigDecimal(0);
            }
            issueTypeSchemeConfig.setSequence(sequence);
            issueTypeSchemeConfigMapper.insert(issueTypeSchemeConfig);
        }
    }

    private boolean nameExisted(Long organizationId,
                                Long projectId,
                                String name,
                                Long issueTypeId) {
        List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, projectId, null);
        Map<String, Long> nameMap =
                issueTypes.stream().collect(Collectors.toMap(IssueTypeVO::getName, IssueTypeVO::getId));
        Long id = nameMap.get(name);
        if (issueTypeId == null) {
            //新增
            return (id != null);
        } else {
            //编辑
            if (id == null) {
                return false;
            } else {
                return !id.equals(issueTypeId);
            }
        }
    }

    @Override
    public IssueTypeVO update(IssueTypeVO issueTypeVO) {
        Long projectId = issueTypeVO.getProjectId();
        Long organizationId = issueTypeVO.getOrganizationId();
        Long issueTypeId = issueTypeVO.getId();

        IssueTypeDTO issueTypeDTO = selectOne(organizationId, projectId, issueTypeId);
        if (issueTypeDTO == null) {
            throw new CommonException("error.issue.type.not.existed");
        }
        String name = issueTypeVO.getName();
        if (nameExisted(organizationId, projectId, name, issueTypeId)) {
            throw new CommonException("error.issue.type.name.existed");
        }
        issueTypeDTO.setColour(issueTypeVO.getColour());
        issueTypeDTO.setDescription(issueTypeVO.getDescription());
        issueTypeDTO.setName(name);
        issueTypeDTO.setIcon(issueTypeVO.getIcon());
        issueTypeDTO.setReferenced(issueTypeVO.getReferenced());
        if (issueTypeMapper.updateByPrimaryKeySelective(issueTypeDTO) != 1) {
            throw new CommonException("error.issue.type.update");
        }
        return modelMapper.map(issueTypeMapper.selectByPrimaryKey(issueTypeId), IssueTypeVO.class);
    }

    @Override
    public void delete(Long organizationId,
                       Long projectId,
                       Long issueTypeId) {
        IssueTypeDTO result = selectOne(organizationId, projectId, issueTypeId);
        if (result == null) {
            return;
        }
        IssueTypeVO vo = modelMapper.map(result, IssueTypeVO.class);
        setDeleted(Arrays.asList(vo), organizationId, projectId);
        if (Boolean.FALSE.equals(vo.getDeleted())) {
            throw new CommonException("error.issue.type.not.deleted");
        }
        issueTypeMapper.deleteByPrimaryKey(result.getId());
        if (!ZERO.equals(projectId)) {
            deleteIssueTypeExtend(organizationId, projectId, issueTypeId);
            deleteIssueTypeAndFieldRel(organizationId, projectId, issueTypeId);
            deleteStateMachineAndIssueTypeConfig(organizationId, projectId, result);
        } else {
            deleteIssueTypeAndFieldRel(organizationId, null, issueTypeId);
        }
    }

    private void deleteStateMachineAndIssueTypeConfig(Long organizationId,
                                                      Long projectId,
                                                      IssueTypeDTO issueTypeDTO) {
        String typeCode = issueTypeDTO.getTypeCode();
        Long issueTypeId = issueTypeDTO.getId();
        Set<String> codes = getProjectCategoryCodes(projectId);
        String applyType = getApplyTypeByCategoryCodes(codes, typeCode);
        Long stateMachineSchemeId = getSchemeIdByOption(projectId, applyType, SchemeType.STATE_MACHINE);
        StatusMachineSchemeConfigDTO statusMachineSchemeConfig = new StatusMachineSchemeConfigDTO();
        statusMachineSchemeConfig.setIssueTypeId(issueTypeId);
        statusMachineSchemeConfig.setOrganizationId(organizationId);
        statusMachineSchemeConfig.setSchemeId(stateMachineSchemeId);
        statusMachineSchemeConfigMapper.delete(statusMachineSchemeConfig);

        Long issueTypeSchemeId = getSchemeIdByOption(projectId, applyType, SchemeType.ISSUE_TYPE);
        IssueTypeSchemeConfigDTO issueTypeSchemeConfig = new IssueTypeSchemeConfigDTO();
        issueTypeSchemeConfig.setOrganizationId(organizationId);
        issueTypeSchemeConfig.setIssueTypeId(issueTypeId);
        issueTypeSchemeConfig.setSchemeId(issueTypeSchemeId);
        issueTypeSchemeConfigMapper.delete(issueTypeSchemeConfig);
    }

    private Long getSchemeIdByOption(Long projectId,
                                     String applyType,
                                     String schemeType) {
        ProjectConfigDTO configDTO = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, schemeType, applyType);
        if (ObjectUtils.isEmpty(configDTO)) {
            throw new CommonException("error.scheme.config.not.found." + schemeType);
        }
        return configDTO.getSchemeId();
    }

    private void deleteIssueTypeAndFieldRel(Long organizationId,
                                            Long projectId,
                                            Long issueTypeId) {
        objectSchemeFieldExtendMapper
                .selectExtendFieldByOptions(Arrays.asList(issueTypeId), organizationId, null, projectId)
                .forEach(x -> objectSchemeFieldExtendMapper.deleteByPrimaryKey(x.getId()));
    }

    private void deleteIssueTypeExtend(Long organizationId, Long projectId, Long issueTypeId) {
        IssueTypeExtendDTO extend = new IssueTypeExtendDTO();
        extend.setProjectId(projectId);
        extend.setOrganizationId(organizationId);
        extend.setIssueTypeId(issueTypeId);
        issueTypeExtendMapper.delete(extend);
    }

    @Override
    public Page<IssueTypeVO> pagedQuery(PageRequest pageRequest,
                                        Long organizationId,
                                        Long projectId,
                                        IssueTypeSearchVO issueTypeSearchVO) {
        issueTypeSearchVO.setOrganizationId(organizationId);
        issueTypeSearchVO.setProjectId(projectId);
        if (!ZERO.equals(projectId)) {
            //项目群暂时不可以配置问题类型，过滤掉测试/自动化测试/特性
            issueTypeSearchVO.setTypeCodes(AGILE_ISSUE_TYPES);
        }
        Page<IssueTypeVO> result =
                PageHelper.doPage(pageRequest, () -> issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO));
        if (ZERO.equals(projectId)) {
            //组织层统计使用情况
            statisticsUsage(result.getContent(), organizationId);
        }
        setDeleted(result.getContent(), organizationId, projectId);
        return result;
    }

    private void setDeleted(List<IssueTypeVO> result, Long organizationId, Long projectId) {
        if (ObjectUtils.isEmpty(result)) {
            return;
        }
        if (ZERO.equals(projectId)) {
            setOrganizationLevelDeleted(result, organizationId);
        } else {
            setProjectLevelDeleted(result, projectId);
        }
    }

    private void setProjectLevelDeleted(List<IssueTypeVO> result,
                                        Long projectId) {
        Set<Long> issueTypeIds = setSystemIssueTypeDeletedFalse(result);
        if (!issueTypeIds.isEmpty()) {
            Map<Long, Integer> countMap =
                    issueMapper.selectCountGroupByIssueTypeId(projectId, issueTypeIds)
                            .stream()
                            .collect(Collectors.toMap(IssueTypeCountVO::getIssueTypeId, IssueTypeCountVO::getCount));
            result.forEach(x -> {
                if (Boolean.FALSE.equals(x.getInitialize())) {
                    Integer count = countMap.get(x.getId());
                    boolean deleted = (count == null || count == 0);
                    x.setDeleted(deleted);
                }
            });
        }
    }

    private void setOrganizationLevelDeleted(List<IssueTypeVO> result,
                                             Long organizationId) {
        Set<Long> issueTypeIds = setSystemIssueTypeDeletedFalse(result);
        //组织层如果被引用，不能删除
        if (!issueTypeIds.isEmpty()) {
            Map<Long, List<IssueTypeDTO>> map =
                    issueTypeMapper.selectByReferenceId(issueTypeIds, organizationId).stream()
                            .collect(Collectors.groupingBy(IssueTypeDTO::getReferenceId));
            result.forEach(x -> {
                if (Boolean.FALSE.equals(x.getInitialize())) {
                    boolean deleted = ObjectUtils.isEmpty(map.get(x.getId()));
                    x.setDeleted(deleted);
                }
            });
        }
    }

    private Set<Long> setSystemIssueTypeDeletedFalse(List<IssueTypeVO> result) {
        Set<Long> issueTypeIds = new HashSet<>();
        result.forEach(x -> {
            if (Boolean.TRUE.equals(x.getInitialize())) {
                x.setDeleted(false);
            } else {
                issueTypeIds.add(x.getId());
            }
        });
        return issueTypeIds;
    }

    @Override
    public Page<ProjectIssueTypeVO> usageDetail(Long organizationId, Long issueTypeId,
                                                PageRequest pageRequest) {
        IssueTypeDTO dto = selectOne(organizationId, ZERO, issueTypeId);
        if (dto == null) {
            throw new CommonException("error.issue.type.not.existed");
        }
        Page<ProjectIssueTypeVO> emptyPage = PageUtil.emptyPageInfo(pageRequest.getPage(), pageRequest.getSize());
        if (Boolean.TRUE.equals(dto.getInitialize())) {
            List<String> categories = TYPE_CODE_CATEGORY_MAP.get(dto.getTypeCode());
            if (ObjectUtils.isEmpty(categories)) {
                return emptyPage;
            }
            ProjectSearchVO projectSearchVO = new ProjectSearchVO();
            projectSearchVO.setEnable(true);
            projectSearchVO.setCategoryCodes(categories);
            Page<ProjectVO> page =
                    baseFeignClient.listWithCategoryByOrganizationIds(organizationId, projectSearchVO, pageRequest.getPage(), pageRequest.getSize())
                            .getBody();
            List<ProjectVO> projects = page.getContent();
            Set<Long> ids = projects.stream().map(ProjectVO::getId).collect(Collectors.toSet());
            if (ids.isEmpty()) {
                return emptyPage;
            }
            IssueTypeExtendDTO issueTypeExtendDTO = new IssueTypeExtendDTO();
            issueTypeExtendDTO.setOrganizationId(organizationId);
            issueTypeExtendDTO.setIssueTypeId(issueTypeId);
            Map<Long, Boolean> projectEnableMap =
                    issueTypeExtendMapper.select(issueTypeExtendDTO)
                            .stream()
                            .collect(Collectors.toMap(IssueTypeExtendDTO::getProjectId, IssueTypeExtendDTO::getEnabled));
            List<ProjectIssueTypeVO> result = buildProjectIssueType(projects, projectEnableMap);
            return PageUtils.copyPropertiesAndResetContent(page, result);
        } else {
            IssueTypeExtendDTO issueTypeExtendDTO = new IssueTypeExtendDTO();
            issueTypeExtendDTO.setOrganizationId(organizationId);
            issueTypeExtendDTO.setIssueTypeId(issueTypeId);
            Page<IssueTypeExtendDTO> page =
                    PageHelper.doPageAndSort(pageRequest, () -> issueTypeExtendMapper.select(issueTypeExtendDTO));
            List<IssueTypeExtendDTO> list = page.getContent();
            if (list.isEmpty()) {
                return emptyPage;
            }
            Map<Long, Boolean> projectEnableMap =
                    list.stream()
                            .collect(Collectors.toMap(IssueTypeExtendDTO::getProjectId, IssueTypeExtendDTO::getEnabled));
            Set<Long> projectIds =
                    list.stream().map(IssueTypeExtendDTO::getProjectId).collect(Collectors.toSet());
            List<ProjectVO> projects = baseFeignClient.queryByIds(projectIds).getBody();
            List<ProjectIssueTypeVO> result = buildProjectIssueType(projects, projectEnableMap);
            return PageUtils.copyPropertiesAndResetContent(page, result);
        }
    }

    @Override
    public Page<IssueTypeVO> pageQueryReference(PageRequest pageRequest, Long organizationId, Long projectId) {
        Set<String> codes = getProjectCategoryCodes(projectId);
        if (codes.contains(ProjectCategory.MODULE_AGILE)) {
            return PageHelper.doPage(pageRequest, () -> issueTypeMapper.selectEnableReference(organizationId, projectId));
        } else {
            return PageUtil.emptyPageInfo(pageRequest.getPage(), pageRequest.getSize());
        }
    }

    @Override
    public void reference(Long projectId,
                          Long organizationId,
                          Long referenceId,
                          IssueTypeVO issueTypeVO) {
        IssueTypeDTO issueType = selectOne(organizationId, ZERO, referenceId);
        if (issueType == null) {
            throw new CommonException("error.issue.type.not.existed");
        }
        if (Boolean.TRUE.equals(issueType.getInitialize())) {
            throw new CommonException("error.system.issue.type.can.not.be.referenced");
        }
        IssueTypeDTO dto = new IssueTypeDTO();
        dto.setOrganizationId(organizationId);
        dto.setProjectId(projectId);
        dto.setReferenceId(referenceId);
        dto.setSource(ORGANIZATION);
        dto.setInitialize(false);
        if (issueTypeMapper.select(dto).isEmpty()) {
            String name = issueTypeVO.getName();
            if (StringUtils.hasText(name)) {
                issueType.setName(name);
            }
            issueType.setId(null);
            issueType.setInitialize(false);
            issueType.setProjectId(projectId);
            issueType.setReferenced(false);
            issueType.setReferenceId(referenceId);
            IssueTypeVO vo = modelMapper.map(issueType, IssueTypeVO.class);
            create(organizationId, projectId, vo);
        } else {
            throw new CommonException("error.issue.type.is.already.referenced");
        }
    }

    private List<ProjectIssueTypeVO> buildProjectIssueType(List<ProjectVO> projects, Map<Long, Boolean> projectEnableMap) {
        List<ProjectIssueTypeVO> result = new ArrayList<>();
        projects.forEach(x -> {
            ProjectIssueTypeVO projectIssueTypeVO = new ProjectIssueTypeVO();
            BeanUtils.copyProperties(x, projectIssueTypeVO);
            boolean enabled = Optional.ofNullable(projectEnableMap.get(x.getId())).orElse(true);
            projectIssueTypeVO.setIssueTypeEnabled(enabled);
            result.add(projectIssueTypeVO);
        });
        return result;
    }

    private void statisticsUsage(List<IssueTypeVO> result, Long organizationId) {
        if (result.isEmpty()) {
            return;
        }
        boolean containsPredefined = false;
        Set<Long> ids = new HashSet<>();
        for (IssueTypeVO vo : result) {
            ids.add(vo.getId());
            if (Boolean.TRUE.equals(vo.getInitialize())) {
                containsPredefined = true;
            }
        }
        if (containsPredefined) {
            Map<String, Set<Long>> categoryProjectMap = listProjectIdsGroupByCategory(organizationId);
            setPredefinedUsageCount(result, categoryProjectMap);
        }
        setCustomIssueTypeUsageCount(organizationId, ids, result);
    }

    private void setCustomIssueTypeUsageCount(Long organizationId,
                                              Set<Long> ids,
                                              List<IssueTypeVO> result) {
        List<IssueTypeDTO> issueTypeList = issueTypeMapper.selectByReferenceId(ids, organizationId);
        Map<Long, Set<Long>> referenceMap = new HashMap<>();
        issueTypeList.forEach(x -> {
            if (Boolean.TRUE.equals(x.getInitialize())) {
                Long referenceId = x.getReferenceId();
                Long projectId = x.getProjectId();
                Set<Long> projectIds = referenceMap.computeIfAbsent(referenceId, k -> new HashSet<>());
                projectIds.add(projectId);
            }
        });
        result.forEach(x -> {
            if (Boolean.FALSE.equals(x.getInitialize())) {
                int count = Optional.ofNullable(referenceMap.get(x.getId())).map(y -> y.size()).orElse(0);
                x.setUsageCount(count);
            }
        });
    }

    private void setPredefinedUsageCount(List<IssueTypeVO> result,
                                         Map<String, Set<Long>> categoryProjectMap) {
        int agileProjectCount = Optional.ofNullable(categoryProjectMap.get(ProjectCategory.MODULE_AGILE)).map(x -> x.size()).orElse(0);
        int programProjectCount = Optional.ofNullable(categoryProjectMap.get(ProjectCategory.MODULE_PROGRAM)).map(x -> x.size()).orElse(0);
        int total = agileProjectCount + programProjectCount;
        int backlogProjectCount = queryBacklogProjectCount(categoryProjectMap.get(ProjectCategory.MODULE_BACKLOG));
        result.forEach(x -> {
            if (Boolean.TRUE.equals(x.getInitialize())) {
                String typeCode = x.getTypeCode();
                if (AGILE_ISSUE_TYPES.contains(typeCode)) {
                    x.setUsageCount(agileProjectCount);
                }
                if (IssueTypeCode.ISSUE_EPIC.value().equals(typeCode)) {
                    x.setUsageCount(total);
                }
                if (IssueTypeCode.BACKLOG.value().equals(typeCode)) {
                    x.setUsageCount(backlogProjectCount);
                }
                if (IssueTypeCode.FEATURE.value().equals(typeCode)) {
                    x.setUsageCount(programProjectCount);
                }
            }
        });
    }

    private int queryBacklogProjectCount(Set<Long> projectIds) {
        if (ObjectUtils.isEmpty(projectIds)) {
            return 0;
        }
        if (backlogExpandService == null) {
            return 0;
        } else {
            return backlogExpandService.listProjectIdsWhichEnableBacklog(projectIds).size();
        }
    }

    private Map<String, Set<Long>> listProjectIdsGroupByCategory(Long organizationId) {
        ProjectSearchVO projectSearchVO = new ProjectSearchVO();
        projectSearchVO.setEnable(true);
        Page<ProjectVO> page = baseFeignClient.listWithCategoryByOrganizationIds(organizationId, projectSearchVO, 1, 0).getBody();
        List<ProjectVO> projects = page.getContent();
        Map<String, Set<Long>> map = new HashMap<>();
        projects.forEach(p -> {
            Long projectId = p.getId();
            List<ProjectCategoryDTO> categories = p.getCategories();
            if (!ObjectUtils.isEmpty(categories)) {
                Set<String> codes = categories.stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toSet());
                if (codes.contains(ProjectCategory.MODULE_AGILE)) {
                    Set<Long> projectIds = map.computeIfAbsent(ProjectCategory.MODULE_AGILE, k -> new HashSet<>());
                    projectIds.add(projectId);
                } else if (codes.contains(ProjectCategory.MODULE_PROGRAM)) {
                    Set<Long> projectIds = map.computeIfAbsent(ProjectCategory.MODULE_PROGRAM, k -> new HashSet<>());
                    projectIds.add(projectId);
                }
                if (codes.contains(ProjectCategory.MODULE_BACKLOG)) {
                    Set<Long> projectIds = map.computeIfAbsent(ProjectCategory.MODULE_BACKLOG, k -> new HashSet<>());
                    projectIds.add(projectId);
                }
            }
        });
        return map;
    }


    @Override
    public Boolean checkName(Long organizationId,
                             Long projectId,
                             String name,
                             Long id) {
        return nameExisted(organizationId, projectId, name, id);
    }

    @Override
    public List<IssueTypeVO> queryByOrgId(Long organizationId, Long projectId) {
        return issueTypeMapper.selectByOptions(organizationId, projectId, null);
    }

    @Override
    public List<IssueTypeVO> queryIssueTypeByStateMachineSchemeId(Long organizationId, Long schemeId) {
        List<IssueTypeVO> issueTypeVOS = queryByOrgId(organizationId, null);
        List<StatusMachineSchemeConfigVO> configVOS = stateMachineSchemeConfigService.queryBySchemeId(true, organizationId, schemeId);
        Map<Long, StatusMachineSchemeConfigVO> configMap = configVOS.stream().collect(Collectors.toMap(StatusMachineSchemeConfigVO::getIssueTypeId, x -> x));
        for (IssueTypeVO issueTypeVO : issueTypeVOS) {
            StatusMachineSchemeConfigVO configVO = configMap.get(issueTypeVO.getId());
            if (configVO != null) {
                StatusMachineVO statusMachineVO = stateMachineService.queryStateMachineById(organizationId, configVO.getStateMachineId());
                issueTypeVO.setStateMachineName(statusMachineVO.getName());
                issueTypeVO.setStateMachineId(statusMachineVO.getId());
            }
        }
        return issueTypeVOS;
    }

    @Override
    public void initIssueTypeByConsumeCreateOrganization(Long organizationId) {
        for (InitIssueType initIssueType : InitIssueType.values()) {
            if (agilePluginService == null && Objects.equals("feature", initIssueType.getTypeCode())) {
                continue;
            }
            //创建默认问题类型
            IssueTypeDTO dto = new IssueTypeDTO();
            dto.setIcon(initIssueType.getIcon());
            dto.setName(initIssueType.getName());
            dto.setDescription(initIssueType.getDescription());
            dto.setColour(initIssueType.getColour());
            dto.setOrganizationId(organizationId);
            dto.setTypeCode(initIssueType.getTypeCode());
            dto.setInitialize(true);
            dto.setProjectId(ZERO);
            dto.setReferenced(true);
            dto.setSource("system");
            createIssueType(dto);
        }
    }


    @Override
    public IssueTypeDTO createIssueType(IssueTypeDTO issueType) {
        //保证幂等性
        List<IssueTypeDTO> issueTypes = issueTypeMapper.select(issueType);
        if (!issueTypes.isEmpty()) {
            return issueTypes.get(0);
        }

        if (issueTypeMapper.insert(issueType) != 1) {
            throw new CommonException("error.issueType.create");
        }
        return issueTypeMapper.selectByPrimaryKey(issueType);
    }

    @Override
    public Map<Long, String> queryIssueTypeMap(Long organizationId) {
        IssueTypeDTO dto = new IssueTypeDTO();
        dto.setOrganizationId(organizationId);
        return issueTypeMapper.select(dto).stream().collect(Collectors.toMap(IssueTypeDTO::getId, IssueTypeDTO::getTypeCode));
    }

    @Override
    public IssueTypeVO query(Long organizationId, Long projectId, Long issueTypeId) {
        IssueTypeDTO dto = new IssueTypeDTO();
        dto.setId(issueTypeId);
        dto.setOrganizationId(organizationId);
        dto.setProjectId(projectId);
        IssueTypeDTO result = issueTypeMapper.selectOne(dto);
        if (!ZERO.equals(projectId) && result == null) {
            //查组织层
            dto.setProjectId(ZERO);
            result = issueTypeMapper.selectOne(dto);
        }
        if (result != null) {
            return modelMapper.map(result, IssueTypeVO.class);
        } else {
            return null;
        }
    }

    @Override
    public Boolean canDisable(Long organizationId, Long projectId, Long issueTypeId) {
        if (ZERO.equals(projectId)) {
            return false;
        }
        IssueTypeDTO dto = issueTypeMapper.selectByPrimaryKey(issueTypeId);
        if (dto == null) {
            throw new CommonException("error.issue.type.not.existed");
        }
        List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, projectId, null);
        //子任务至少要有一个，故事和任务至少要有一个
        Map<String, Integer> typeCodeCountMap = new HashMap<>();
        issueTypes.forEach(x -> {
            if (Boolean.TRUE.equals(x.getEnabled())) {
                String typeCode = x.getTypeCode();
                Integer count = typeCodeCountMap.computeIfAbsent(typeCode, y -> 0);
                count++;
                typeCodeCountMap.put(typeCode, count);
            }
        });
        String dtoTypeCode = dto.getTypeCode();
        Optional.ofNullable(typeCodeCountMap.get(dtoTypeCode)).orElse(0);
        if (IssueTypeCode.SUB_TASK.value().equals(dtoTypeCode)) {
            int count = Optional.ofNullable(typeCodeCountMap.get(dtoTypeCode)).orElse(0);
            return count > 1;
        }
        if (IssueTypeCode.TASK.value().equals(dtoTypeCode)
                || IssueTypeCode.STORY.value().equals(dtoTypeCode)) {
            int taskCount = Optional.ofNullable(typeCodeCountMap.get(IssueTypeCode.TASK.value())).orElse(0);
            int storyCount = Optional.ofNullable(typeCodeCountMap.get(IssueTypeCode.STORY.value())).orElse(0);
            return (taskCount + storyCount > 1);
        }
        return true;
    }

    @Override
    public void updateEnabled(Long organizationId,
                              Long projectId,
                              Long issueTypeId,
                              Boolean enabled) {
        //只有项目层问题类型可以被启停用
        if (ZERO.equals(projectId)) {
            return;
        }
        if (Boolean.FALSE.equals(enabled)
                && !canDisable(organizationId, projectId, issueTypeId)) {
            throw new CommonException("error.issue.type.can.not.disable");
        }
        updateExtendEnabled(issueTypeId, projectId, organizationId, enabled);
    }

    @Override
    public void updateReferenced(Long organizationId, Long issueTypeId, Boolean referenced) {
        IssueTypeDTO issueType = selectOne(organizationId, ZERO, issueTypeId);
        if (issueType == null) {
            throw new CommonException("error.issue.type.not.existed");
        }
        if (Boolean.TRUE.equals(issueType.getInitialize())) {
            throw new CommonException("error.system.issue.type.can.not.edit");
        }
        issueType.setReferenced(referenced);
        issueTypeMapper.updateByPrimaryKey(issueType);
    }

    private IssueTypeDTO selectOne(Long organizationId,
                                   Long projectId,
                                   Long issueTypeId) {
        IssueTypeDTO dto = new IssueTypeDTO();
        dto.setId(issueTypeId);
        dto.setOrganizationId(organizationId);
        dto.setProjectId(projectId);
        return issueTypeMapper.selectOne(dto);
    }

    @Override
    public Map<Long, IssueTypeVO> listIssueTypeMap(Long organizationId) {
        IssueTypeDTO issueType = new IssueTypeDTO();
        issueType.setOrganizationId(organizationId);
        List<IssueTypeDTO> issueTypes = issueTypeMapper.select(issueType);
        Map<Long, IssueTypeVO> issueTypeVOMap = new HashMap<>();
        for (IssueTypeDTO iType : issueTypes) {
            issueTypeVOMap.put(iType.getId(), modelMapper.map(iType, new TypeToken<IssueTypeVO>() {
            }.getType()));
        }
        return issueTypeVOMap;
    }

    @Override
    public Map<Long, Map<String, Long>> initIssueTypeData(Long organizationId, List<Long> orgIds) {
        Map<Long, Map<String, Long>> result = new HashMap<>();
        for (Long orgId : orgIds) {
            Map<String, Long> temp = new HashMap<>();
            for (InitIssueType initIssueType : InitIssueType.values()) {
                if (agilePluginService == null && Objects.equals("feature", initIssueType.getTypeCode())) {
                    continue;
                }
                IssueTypeDTO issueType = createIssueType(new IssueTypeDTO(initIssueType.getIcon(), initIssueType.getName(), initIssueType.getDescription(), orgId, initIssueType.getColour(), initIssueType.getTypeCode(), true));
                temp.put(initIssueType.getTypeCode(), issueType.getId());
            }
            result.put(orgId, temp);
        }
        return result;
    }

    @Override
    public String getIssueTypeById(Long issueTypeId) {
        IssueTypeDTO issueTypeDTO = issueTypeMapper.selectByPrimaryKey(issueTypeId);
        if (Objects.isNull(issueTypeDTO)) {
            throw new CommonException("error.issue.type.not.exist");
        }
        return issueTypeDTO.getTypeCode();
    }
}
