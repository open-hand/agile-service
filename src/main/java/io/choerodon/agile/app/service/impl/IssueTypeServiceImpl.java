package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.domain.Page;
import io.choerodon.agile.api.vo.*;
import io.choerodon.core.oauth.DetailsHelper;
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
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
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
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private IssueTypeFieldMapper issueTypeFieldMapper;

    private static final String ORGANIZATION = "organization";

    private static final String PROJECT = "project";

    private static final String SYSTEM = "system";

    private static final String AGILE_AND_PROGRAM = "agileAndProgram";

    private static final String ERROR_ISSUE_TYPE_NAME_EXISTED = "error.issue.type.name.existed";

    private static final String ERROR_ISSUE_TYPE_ICON_EXISTED = "error.issue.type.icon.existed";

    private static final String ERROR_ISSUE_TYPE_NOT_EXISTED = "error.issue.type.not.existed";

    @Autowired
    private OrganizationConfigService organizationConfigService;

    @Autowired
    private StateMachineNodeService stateMachineNodeService;
    
    @Autowired
    private IssueStatusService issueStatusService;

    @Autowired
    private StatusMapper statusMapper;

    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private StatusMachineNodeMapper nodeDeployMapper;
    @Autowired
    private StatusService statusService;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;

    private static final List<String> AGILE_CREATE_ISSUE_TYPES =
            Arrays.asList(
                    IssueTypeCode.STORY.value(),
                    IssueTypeCode.SUB_TASK.value(),
                    IssueTypeCode.TASK.value(),
                    IssueTypeCode.BUG.value()
            );


    private static final List<String> IGNORED_ISSUE_TYPES =
            Arrays.asList(
                    IssueTypeCode.BACKLOG.value(),
                    IssueTypeCode.ISSUE_AUTO_TEST.value(),
                    IssueTypeCode.ISSUE_TEST.value()
            );

    private static final List<String> AGILE_ISSUE_TYPES =
            Arrays.asList(
                    IssueTypeCode.STORY.value(),
                    IssueTypeCode.SUB_TASK.value(),
                    IssueTypeCode.TASK.value(),
                    IssueTypeCode.BUG.value(),
                    IssueTypeCode.ISSUE_EPIC.value()
            );

    private static final List<String> WATERFALL_ISSUE_TYPES =
            Arrays.asList(
                    IssueTypeCode.STAGE.value(),
                    IssueTypeCode.MILESTONE.value(),
                    IssueTypeCode.ACTIVITY.value()
            );

    private static final Map<String, List<String>> TYPE_CODE_CATEGORY_MAP;

    static {
        TYPE_CODE_CATEGORY_MAP = new HashMap<>();
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.BUG.value(), Arrays.asList(ProjectCategory.MODULE_AGILE, ProjectCategory.MODULE_WATERFALL_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.FEATURE.value(), Arrays.asList(ProjectCategory.MODULE_PROGRAM));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.ISSUE_EPIC.value(), Arrays.asList(ProjectCategory.MODULE_PROGRAM, ProjectCategory.MODULE_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.STORY.value(), Arrays.asList(ProjectCategory.MODULE_AGILE, ProjectCategory.MODULE_WATERFALL_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.SUB_TASK.value(), Arrays.asList(ProjectCategory.MODULE_AGILE, ProjectCategory.MODULE_WATERFALL_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.TASK.value(), Arrays.asList(ProjectCategory.MODULE_AGILE, ProjectCategory.MODULE_WATERFALL_AGILE));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.BACKLOG.value(), Arrays.asList(ProjectCategory.MODULE_BACKLOG));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.STAGE.value(), Arrays.asList(ProjectCategory.MODULE_WATERFALL));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.MILESTONE.value(), Arrays.asList(ProjectCategory.MODULE_WATERFALL));
        TYPE_CODE_CATEGORY_MAP.put(IssueTypeCode.ACTIVITY.value(), Arrays.asList(ProjectCategory.MODULE_WATERFALL));
    }


    @Override
    public IssueTypeVO queryById(Long issueTypeId, Long projectId) {
        IssueTypeDTO issueType = issueTypeMapper.selectWithAlias(issueTypeId, projectId);
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
            throw new CommonException(ERROR_ISSUE_TYPE_NAME_EXISTED);
        }
        if (iconExisted(organizationId, projectId, issueTypeVO.getIcon(), null)) {
            throw new CommonException(ERROR_ISSUE_TYPE_ICON_EXISTED);
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
            initDefaultStateMachine(organizationId, projectId, result, categoryCodes, issueTypeVO.getCopyStatusMachine());
            //初始化项目系统字段与问题类型关系
            initIssueTypeAndFieldRel(organizationId, projectId, result, issueTypeVO.getCopyCustomField());
        } else {
            //初始化组织系统字段与问题类型关系
            initIssueTypeAndFieldRel(organizationId, null, result, issueTypeVO.getCopyCustomField());
        }
        return result;
    }

    private void initIssueTypeAndFieldRel(Long organizationId,
                                          Long projectId,
                                          IssueTypeVO result,
                                          Boolean copyField) {
        if (Boolean.TRUE.equals(copyField)) {
            copyFieldRel(organizationId, projectId, result);
        } else {
            initFieldRel(organizationId, projectId, result);
        }
    }

    private void copyFieldRel(Long organizationId, Long projectId, IssueTypeVO result) {
        // 查询要复制的系统问题类型
        IssueTypeDTO issueTypeDTO = querySystemIssueTypeByCode(organizationId, result.getTypeCode());
        // 查询问题类型的所有字段
        List<PageConfigFieldVO> fields  = objectSchemeFieldService.queryPageConfigFields(organizationId, projectId, issueTypeDTO.getId());
        if (!CollectionUtils.isEmpty(fields)) {
            String rank = RankUtil.mid();
            for (PageConfigFieldVO field : fields) {
                ObjectSchemeFieldExtendDTO extendDTO = new ObjectSchemeFieldExtendDTO();
                if (objectSchemeFieldExtendMapper.selectExtendFieldByOptions(Arrays.asList(result.getId()), organizationId, field.getFieldId(), projectId).isEmpty()) {
                    extendDTO.setIssueTypeId(result.getId());
                    extendDTO.setIssueType(result.getTypeCode());
                    extendDTO.setOrganizationId(organizationId);
                    extendDTO.setProjectId(projectId);
                    extendDTO.setFieldId(field.getFieldId());
                    extendDTO.setRequired(field.getRequired());
                    extendDTO.setCreated(field.getCreated());
                    extendDTO.setEdited(field.getEdited());
                    extendDTO.setRank(rank);
                    extendDTO.setDefaultValue(!ObjectUtils.isEmpty(field.getDefaultValue()) ? field.getDefaultValue().toString() : "");
                    extendDTO.setExtraConfig(field.getExtraConfig());
                    objectSchemeFieldExtendMapper.insertSelective(extendDTO);
                    rank = RankUtil.genPre(rank);
                }
            }
        }
        // 复制原有问题类型描述的默认值
        if (!ObjectUtils.isEmpty(projectId)) {
            IssueTypeFieldDTO issueTypeFieldDTO = new IssueTypeFieldDTO();
            issueTypeFieldDTO.setProjectId(projectId);
            issueTypeFieldDTO.setIssueTypeId(issueTypeDTO.getId());
            List<IssueTypeFieldDTO> issueTypeField = issueTypeFieldMapper.select(issueTypeFieldDTO);
            if (!CollectionUtils.isEmpty(issueTypeField)) {
                IssueTypeFieldDTO typeFieldDTO = issueTypeField.get(0);
                IssueTypeFieldDTO newIssueTypeField = new IssueTypeFieldDTO();
                newIssueTypeField.setProjectId(projectId);
                newIssueTypeField.setTemplate(typeFieldDTO.getTemplate());
                newIssueTypeField.setIssueTypeId(result.getId());
                if (issueTypeFieldMapper.insertSelective(newIssueTypeField) != 1) {
                    throw new CommonException("error.insert.issue.type.field");
                }
            }
        }
    }

    private void initFieldRel(Long organizationId,
                              Long projectId,
                              IssueTypeVO result){
        ObjectSchemeFieldDTO field = new ObjectSchemeFieldDTO();
        field.setSystem(true);
        String typeCode = result.getTypeCode();
        List<ObjectSchemeFieldDTO> fields;
        Long issueTypeId = result.getId();
        if (!Objects.isNull(result.getReferenceId())) {
            fields = objectSchemeFieldMapper.selectByOptions(organizationId, null, null, null, result.getReferenceId(),Arrays.asList(typeCode));
        } else {
            fields =
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
        }
        Map<Long, ObjectSchemeFieldExtendDTO> referencedSystemFieldExtendMap = objectSchemeFieldExtendMapper.selectExtendFieldByOptions(Arrays.asList(result.getReferenceId()), organizationId, null, null)
                        .stream().collect(Collectors.toMap(ObjectSchemeFieldExtendDTO::getFieldId, Function.identity()));
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
                if (!Objects.isNull(result.getReferenceId())) {
                    extendDTO.setDefaultValue(referencedSystemFieldExtendMap.get(x.getId()).getDefaultValue());
                } else {
                    extendDTO.setDefaultValue(x.getDefaultValue());
                }
                extendDTO.setExtraConfig(x.getExtraConfig());
                objectSchemeFieldExtendMapper.insertSelective(extendDTO);
            }
        });
    }

    private IssueTypeDTO querySystemIssueTypeByCode(Long organizationId, String typeCode){
        // 查询系统问题类型的状态机id
        List<IssueTypeDTO> issueTypeDTOS = issueTypeMapper.selectSystemIssueTypeByOrganizationIds(new HashSet<>(Arrays.asList(organizationId)));
        IssueTypeDTO issueTypeDTO = issueTypeDTOS.stream().filter(v -> Objects.equals(typeCode, v.getTypeCode())).findAny().orElse(null);
        if(ObjectUtils.isEmpty(issueTypeDTO)){
            throw new CommonException("error.system.issueType.not.found");
        }
        return issueTypeDTO;
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
            String rank = issueTypeExtendMapper.selectMaxRank(organizationId, projectId, null);
            if (!StringUtils.hasText(rank)) {
                rank = RankUtil.mid();
            }else {
                rank = RankUtil.genNext(rank);
            }
            dto.setRank(rank);
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
        ProjectVO project = ConvertUtil.queryProject(projectId);
        Set<String> codes = new HashSet<>(ProjectCategory.getProjectCategoryCodes(project));
        List<String> issueTypes = new ArrayList<>(AGILE_CREATE_ISSUE_TYPES);
        if (ProjectCategory.containsAgile(new ArrayList<>(codes))
                && !issueTypes.contains(typeCode)) {
            throw new CommonException("error.illegal.type.code");
        }
        return codes;
    }

    private void initDefaultStateMachine(Long organizationId,
                                         Long projectId,
                                         IssueTypeVO issueType,
                                         Set<String> categoryCodes,
                                         Boolean copyStatusMachine) {
        Long issueTypeId = issueType.getId();
        String typeCode = issueType.getTypeCode();
        String applyType = projectConfigService.getApplyTypeByTypeCode(projectId, typeCode);
        Long stateMachineSchemeId = getSchemeIdByOption(projectId, applyType, SchemeType.STATE_MACHINE);
        ProjectVO projectVO = ConvertUtil.queryProject(projectId);
        String stateMachineName = projectVO.getCode() + "-状态机-" + issueType.getName();
        Long statusMachineId = null;
        if (Boolean.TRUE.equals(copyStatusMachine)) {
            if (Objects.equals(ORGANIZATION, issueType.getSource()) && !ObjectUtils.isEmpty(issueType.getReferenceId())) {
                Long stateMachineTemplateId = organizationConfigService.queryIssueTypeStatusMachineId(organizationId, issueType.getReferenceId());
                statusMachineId = !ObjectUtils.isEmpty(stateMachineTemplateId) ? stateMachineTemplateId : defaultHandlerStateMachine(organizationId, typeCode, stateMachineSchemeId);
                // 处理状态机里面有,而项目没有的状态机
                handlerStatus(organizationId, projectId, statusMachineId, applyType);
            } else {
                statusMachineId = defaultHandlerStateMachine(organizationId, typeCode, stateMachineSchemeId);
            }
        }
        stateMachineSchemeConfigService.initStatusMachineAndSchemeConfig(organizationId, stateMachineName, stateMachineSchemeId, issueTypeId, projectVO, applyType, statusMachineId);
        //初始化问题类型方案配置
        initIssueTypeSchemeConfig(organizationId, projectId, issueTypeId, applyType);
    }

    private void handlerStatus(Long organizationId, Long projectId, Long statusMachineId, String applyType) {
        List<StatusMachineNodeVO> statusMachineNodeVOS = stateMachineNodeService.queryByStateMachineId(organizationId, statusMachineId, false);
        if (!CollectionUtils.isEmpty(statusMachineNodeVOS)) {
            List<Long> statusIds = statusMachineNodeVOS.stream().map(StatusMachineNodeVO::getStatusId).collect(Collectors.toList());
            IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
            issueStatusDTO.setProjectId(projectId);
            List<IssueStatusVO> issueStatusVOS = issueStatusService.queryIssueStatusList(projectId);
            List<Long> projectStatusIds = new ArrayList<>();
            if (!CollectionUtils.isEmpty(issueStatusVOS)) {
                projectStatusIds.addAll(issueStatusVOS.stream().map(IssueStatusVO::getStatusId).collect(Collectors.toList()));
            }
            List<Long> needAddIds = new ArrayList<>();
            statusIds.forEach(v -> {
                if (!projectStatusIds.contains(v)) {
                    needAddIds.add(v);
                }
            });
            if (!CollectionUtils.isEmpty(needAddIds)) {
                createIssueStatus(needAddIds, projectId, organizationId);
            }
        }
    }

    private void createIssueStatus(List<Long> needAddIds, Long projectId, Long organizationId) {
        List<StatusWithInfoDTO> statusWithInfoDTOS = statusMapper.queryStatusList(organizationId, needAddIds);
        Map<Long, StatusWithInfoDTO> infoDTOMap = statusWithInfoDTOS.stream().collect(Collectors.toMap(StatusWithInfoDTO::getId, Function.identity()));
        for (Long needAddId : needAddIds) {
            StatusWithInfoDTO statusWithInfoDTO = infoDTOMap.getOrDefault(needAddId, null);
            if (!ObjectUtils.isEmpty(statusWithInfoDTO)) {
                IssueStatusDTO issueStatus = new IssueStatusDTO();
                issueStatus.setName(statusWithInfoDTO.getName());
                issueStatus.setProjectId(projectId);
                issueStatus.setCompleted(false);
                issueStatus.setEnable(false);
                issueStatus.setCategoryCode(statusWithInfoDTO.getType());
                issueStatus.setStatusId(statusWithInfoDTO.getId());
                issueStatusService.insertIssueStatus(issueStatus);
            }
        }
    }

    private Long defaultHandlerStateMachine(Long organizationId, String typeCode, Long stateMachineSchemeId){
        // 查询系统问题类型的状态机id
        IssueTypeDTO issueTypeDTO = querySystemIssueTypeByCode(organizationId, typeCode);
        // 查询系统问题类型的状态机
        return stateMachineSchemeConfigService.queryStatusMachineBySchemeIdAndIssueType(organizationId, stateMachineSchemeId, issueTypeDTO.getId());
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
            } else {
                sequence = sequence.add(new BigDecimal(1));
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
        return existed(issueTypeId, id);
    }

    @Override
    public IssueTypeVO update(IssueTypeVO issueTypeVO, List<String> fieldList) {
        Long projectId = issueTypeVO.getProjectId();
        Long organizationId = issueTypeVO.getOrganizationId();
        Long issueTypeId = issueTypeVO.getId();

        IssueTypeDTO issueTypeDTO = selectOne(organizationId, projectId, issueTypeId);
        if (issueTypeDTO == null) {
            throw new CommonException(ERROR_ISSUE_TYPE_NOT_EXISTED);
        }
        if (SYSTEM.equals(issueTypeDTO.getSource())
                && !ZERO.equals(projectId)) {
            throw new CommonException("error.project.can.not.edit.system.issue.type");
        }
        String name = issueTypeVO.getName();
        if (nameExisted(organizationId, projectId, name, issueTypeId)) {
            throw new CommonException(ERROR_ISSUE_TYPE_NAME_EXISTED);
        }
        if (iconExisted(organizationId, projectId, issueTypeVO.getIcon(), issueTypeId)) {
            throw new CommonException(ERROR_ISSUE_TYPE_ICON_EXISTED);
        }
        issueTypeDTO.setColour(issueTypeVO.getColour());
        issueTypeDTO.setDescription(issueTypeVO.getDescription());
        issueTypeDTO.setName(name);
        issueTypeDTO.setIcon(issueTypeVO.getIcon());
        issueTypeDTO.setReferenced(issueTypeVO.getReferenced());
        if (issueTypeMapper.updateOptional(issueTypeDTO, fieldList.toArray(new String[fieldList.size()])) != 1) {
            throw new CommonException("error.issue.type.update");
        }
        return modelMapper.map(issueTypeMapper.selectWithAlias(issueTypeId, projectId), IssueTypeVO.class);
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
        setDeletedAndReference(Arrays.asList(vo), organizationId, projectId);
        if (Boolean.FALSE.equals(vo.getDeleted())) {
            throw new CommonException("error.issue.type.not.deleted");
        }
        if (!ZERO.equals(projectId)) {
            deleteIssueTypeExtend(organizationId, projectId, issueTypeId);
            deleteIssueTypeAndFieldRel(organizationId, projectId, issueTypeId);
            deleteStateMachineAndIssueTypeConfig(organizationId, projectId, result);
        } else {
            deleteIssueTypeAndFieldRel(organizationId, null, issueTypeId);
        }
        issueTypeMapper.deleteByPrimaryKey(result.getId());
    }

    private void deleteStateMachineAndIssueTypeConfig(Long organizationId,
                                                      Long projectId,
                                                      IssueTypeDTO issueTypeDTO) {
        //如果是状态关联的最后一个问题类型，同时把问题类型和项目的关联删除
        Long issueTypeId = issueTypeDTO.getId();
        deleteIssueStatusIfLastOne(organizationId, projectId, issueTypeId);

        String applyType = projectConfigService.getApplyType(projectId, issueTypeId);
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

    private void deleteIssueStatusIfLastOne(Long organizationId,
                                            Long projectId,
                                            Long issueTypeId) {
        String applyType = projectConfigService.getApplyType(projectId, issueTypeId);
        ProjectConfigDetailVO projectConfigDetailVO = projectConfigService.queryById(projectId);
        StateMachineSchemeVO stateMachineSchemeVO = projectConfigDetailVO.getStateMachineSchemeMap().get(applyType);
        Long schemeId = stateMachineSchemeVO.getId();
        Set<Long> issueTypeList = new HashSet<>();
        issueTypeList.add(issueTypeId);
        List<Long> statusIds =
                nodeDeployMapper.selectStatusIdsByIssueTypeIds(organizationId, schemeId, issueTypeList, applyType);
        if (!statusIds.isEmpty()) {
            Map<Long, Set<Long>> statusIssueTypeMap = new HashMap<>();
            nodeDeployMapper.countIssueTypeByStatusIds(organizationId, schemeId, statusIds, applyType)
                    .forEach(x -> {
                        Long statusId = x.getId();
                        Long thisIssueTypeId = x.getIssueTypeId();
                        Set<Long> issueTypeIds = statusIssueTypeMap.computeIfAbsent(statusId, y -> new HashSet<>());
                        issueTypeIds.add(thisIssueTypeId);
                    });
            statusIssueTypeMap.forEach((x, y) -> {
                if (y.size() == 1 && y.contains(issueTypeId)) {
                    statusService.deleteStatus(projectId, x, applyType, new ArrayList<>());
                }
            });
        }
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
            List<IssueTypeVO> issueTypes = objectSchemeFieldService.issueTypes(organizationId, projectId);
            List<String> typeCodes = new ArrayList<>();
            issueTypes.forEach(issueType -> {
                String typeCode = issueType.getTypeCode();
                if (!IGNORED_ISSUE_TYPES.contains(typeCode)) {
                    typeCodes.add(typeCode);
                }
            });
            issueTypeSearchVO.setTypeCodes(typeCodes);
        }
        Page<IssueTypeVO> result =
                PageHelper.doPage(pageRequest, () -> issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO));
        if (ZERO.equals(projectId)) {
            //组织层统计使用情况
            statisticsUsage(result.getContent(), organizationId);
        }
        setDeletedAndReference(result.getContent(), organizationId, projectId);
        return result;
    }

    private void setDeletedAndReference(List<IssueTypeVO> result,
                                        Long organizationId,
                                        Long projectId) {
        if (ObjectUtils.isEmpty(result)) {
            return;
        }
        if (ZERO.equals(projectId)) {
            setOrganizationLevelDeleted(result, organizationId);
        } else {
            setProjectLevelDeletedAndReference(result, projectId);
        }
    }

    private void setProjectLevelDeletedAndReference(List<IssueTypeVO> result,
                                                    Long projectId) {
        Set<Long> referenceIds =
                result.stream()
                        .filter(x -> x.getReferenceId() != null)
                        .map(IssueTypeVO::getReferenceId)
                        .collect(Collectors.toSet());
        Set<Long> issueTypeIds = setSystemIssueTypeDeletedFalse(result);
        if (!issueTypeIds.isEmpty()) {
            Map<Long, Integer> countMap =
                    issueMapper.selectCountGroupByIssueTypeId(projectId, issueTypeIds)
                            .stream()
                            .collect(Collectors.toMap(IssueTypeCountVO::getIssueTypeId, IssueTypeCountVO::getCount));
            Map<Long, IssueTypeDTO> issueTypeMap = new HashMap<>();
            if (!referenceIds.isEmpty()) {
                List<Long> list = new ArrayList<>(referenceIds);
                Map<Long, IssueTypeDTO> map =
                        issueTypeMapper.selectByIds(StringUtils.collectionToDelimitedString(list, ","))
                                .stream()
                                .collect(Collectors.toMap(IssueTypeDTO::getId, Function.identity()));
                issueTypeMap.putAll(map);
            }
            result.forEach(x -> {
                if (Boolean.FALSE.equals(x.getInitialize())) {
                    Integer count = countMap.get(x.getId());
                    boolean deleted = (count == null || count == 0);
                    x.setDeleted(deleted);
                    Long referenceId = x.getReferenceId();
                    if (referenceId != null) {
                        IssueTypeDTO dto = issueTypeMap.get(referenceId);
                        if (dto != null) {
                            x.setReferenceIssueType(modelMapper.map(dto, IssueTypeVO.class));
                        }
                    }
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
            throw new CommonException(ERROR_ISSUE_TYPE_NOT_EXISTED);
        }
        Page<ProjectIssueTypeVO> emptyPage = PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
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
            Set<Long> issueTypeIds =
                    issueTypeMapper.selectByReferenceId(new HashSet<>(Arrays.asList(issueTypeId)), organizationId)
                            .stream().map(IssueTypeDTO::getId).collect(Collectors.toSet());
            Page<IssueTypeExtendDTO> page =
                    PageHelper.doPageAndSort(pageRequest, () -> issueTypeExtendMapper.selectByIssueTypeIds(issueTypeIds, null, organizationId));
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
        ProjectVO project = ConvertUtil.queryProject(projectId);
        Set<String> categories = new HashSet<>(ProjectCategory.getProjectCategoryCodes(project));
        if (categories.contains(ProjectCategory.MODULE_AGILE) || categories.contains(ProjectCategory.MODULE_WATERFALL_AGILE)) {
            return PageHelper.doPage(pageRequest, () -> issueTypeMapper.selectEnableReference(organizationId, projectId));
        } else {
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
    }

    @Override
    public void reference(Long projectId,
                          Long organizationId,
                          Long referenceId,
                          IssueTypeVO issueTypeVO) {
        IssueTypeDTO issueType = selectOne(organizationId, ZERO, referenceId);
        if (issueType == null) {
            throw new CommonException(ERROR_ISSUE_TYPE_NAME_EXISTED);
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
            String icon = issueTypeVO.getIcon();
            if (StringUtils.hasText(icon)) {
                issueType.setIcon(icon);
            }
            issueType.setId(null);
            issueType.setInitialize(false);
            issueType.setProjectId(projectId);
            issueType.setReferenced(false);
            issueType.setReferenceId(referenceId);
            IssueTypeVO vo = modelMapper.map(issueType, IssueTypeVO.class);
            if (!ObjectUtils.isEmpty(issueTypeVO.getCopyStatusMachine())) {
                vo.setCopyStatusMachine(issueTypeVO.getCopyStatusMachine());
            }

            if (!ObjectUtils.isEmpty(issueTypeVO.getCopyCustomField())) {
                vo.setCopyCustomField(issueTypeVO.getCopyCustomField());
            }
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
            Long referenceId = x.getReferenceId();
            Long projectId = x.getProjectId();
            Set<Long> projectIds = referenceMap.computeIfAbsent(referenceId, k -> new HashSet<>());
            projectIds.add(projectId);
        });
        result.forEach(x -> {
            if (Boolean.FALSE.equals(x.getInitialize())) {
                int count = Optional.ofNullable(referenceMap.get(x.getId())).map(Set::size).orElse(0);
                x.setUsageCount(count);
            }
        });
    }

    private void setPredefinedUsageCount(List<IssueTypeVO> result,
                                         Map<String, Set<Long>> categoryProjectMap) {
        int agileProjectCount = Optional.ofNullable(categoryProjectMap.get(ProjectCategory.MODULE_AGILE)).map(Set::size).orElse(0);
        int programProjectCount = Optional.ofNullable(categoryProjectMap.get(ProjectCategory.MODULE_PROGRAM)).map(Set::size).orElse(0);
        int agileAndProgramCount = Optional.ofNullable(categoryProjectMap.get(AGILE_AND_PROGRAM)).map(Set::size).orElse(0);
        int waterfallCount = Optional.ofNullable(categoryProjectMap.get(ProjectCategory.MODULE_WATERFALL)).map(Set::size).orElse(0);
        int waterfallAndAgileCount = Optional.ofNullable(categoryProjectMap.get(ProjectCategory.MODULE_WATERFALL_AGILE)).map(Set::size).orElse(0);
        int total = agileProjectCount + programProjectCount - agileAndProgramCount - waterfallAndAgileCount;
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
                if (WATERFALL_ISSUE_TYPES.contains(typeCode)) {
                    x.setUsageCount(waterfallCount);
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
                boolean containsAgile = ProjectCategory.containsAgile(new ArrayList<>(codes));
                if (containsAgile) {
                    Set<Long> projectIds = map.computeIfAbsent(ProjectCategory.MODULE_AGILE, k -> new HashSet<>());
                    projectIds.add(projectId);
                }
                boolean containsProgram = codes.contains(ProjectCategory.MODULE_PROGRAM);
                if (codes.contains(ProjectCategory.MODULE_PROGRAM)) {
                    Set<Long> projectIds = map.computeIfAbsent(ProjectCategory.MODULE_PROGRAM, k -> new HashSet<>());
                    projectIds.add(projectId);
                }
                boolean containsBacklog = codes.contains(ProjectCategory.MODULE_BACKLOG);
                if (containsBacklog) {
                    Set<Long> projectIds = map.computeIfAbsent(ProjectCategory.MODULE_BACKLOG, k -> new HashSet<>());
                    projectIds.add(projectId);
                }
                boolean isAgileAndProgram = containsAgile && containsProgram;
                if (isAgileAndProgram) {
                    Set<Long> projectIds = map.computeIfAbsent(AGILE_AND_PROGRAM, k -> new HashSet<>());
                    projectIds.add(projectId);
                }
                boolean isWaterfall = codes.contains(ProjectCategory.MODULE_WATERFALL);
                if (isWaterfall) {
                    Set<Long> projectIds = map.computeIfAbsent(ProjectCategory.MODULE_WATERFALL, k -> new HashSet<>());
                    projectIds.add(projectId);
                }
                boolean isWaterfallAndAgile = codes.contains(ProjectCategory.MODULE_WATERFALL_AGILE);
                if (isWaterfallAndAgile) {
                    Set<Long> projectIds = map.computeIfAbsent(ProjectCategory.MODULE_WATERFALL_AGILE, k -> new HashSet<>());
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
        if (projectId == null) {
            projectId = ZERO;
        }
        return issueTypeMapper.selectByOptions(organizationId, projectId, null);
    }

    @Override
    public void initIssueTypeByConsumeCreateOrganization(Long organizationId) {
        for (InitIssueType initIssueType : InitIssueType.values()) {
            String typeCode = initIssueType.getTypeCode();
            if (agilePluginService == null && Objects.equals("feature", typeCode)) {
                continue;
            }
            if (agileWaterfallService == null && WATERFALL_ISSUE_TYPES.contains(typeCode)) {
                continue;
            }
            //创建默认问题类型
            IssueTypeDTO dto = buildIssueTypeFromInitIssueType(initIssueType, organizationId);
            createIssueType(dto);
        }
    }

    private IssueTypeDTO buildIssueTypeFromInitIssueType(InitIssueType initIssueType,
                                                         Long organizationId) {
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
        dto.setSource(SYSTEM);
        return dto;
    }


    @Override
    public IssueTypeDTO createIssueType(IssueTypeDTO issueType) {
        Long projectId = issueType.getProjectId();
        //保证幂等性
        List<IssueTypeDTO> issueTypes = issueTypeMapper.select(issueType);
        if (!issueTypes.isEmpty()) {
            return issueTypes.get(0);
        }

        if (issueTypeMapper.insert(issueType) != 1) {
            throw new CommonException("error.issueType.create");
        }
        return issueTypeMapper.selectWithAlias(issueType.getId(), projectId);
    }

    @Override
    public IssueTypeVO query(Long organizationId, Long projectId, Long issueTypeId) {
        IssueTypeDTO result =  issueTypeMapper.selectWithAlias(issueTypeId, projectId);
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
        IssueTypeDTO dto = issueTypeMapper.selectWithAlias(issueTypeId, projectId);
        if (dto == null) {
            throw new CommonException(ERROR_ISSUE_TYPE_NOT_EXISTED);
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
            throw new CommonException(ERROR_ISSUE_TYPE_NOT_EXISTED);
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
        return issueTypeMapper.selectWithAlias(issueTypeId, projectId);
    }

    @Override
    public Map<Long, IssueTypeVO> listIssueTypeMap(Long organizationId,
                                                   Long projectId) {
        if (projectId == null) {
            projectId = ZERO;
        }
        return issueTypeMapper.selectByOptions(organizationId, projectId, null)
                .stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));
    }

    @Override
    public Map<Long, List<IssueTypeVO>> listIssueTypeMapByProjectIds(Long organizationId, List<Long> projectIds) {
        return issueTypeMapper.selectByProjectIds(organizationId, projectIds)
                .stream().collect(Collectors.groupingBy(IssueTypeVO::getId));
    }

    @Override
    public void updateRank(Long projectId,
                           Long organizationId,
                           Long issueTypeId,
                           IssueTypeRankVO issueTypeRankVO) {
        Map<Long, IssueTypeVO> issueTypeMap = initRankIfNull(organizationId, projectId);
        IssueTypeVO issueTypeVO = issueTypeMap.get(issueTypeId);
        AssertUtilsForCommonException.notNull(issueTypeVO, "error.issue.type.null");
        Long frontId = issueTypeRankVO.getFrontId();
        Long backId = issueTypeRankVO.getBackId();
        if (ObjectUtils.isEmpty(frontId)
                && ObjectUtils.isEmpty(backId)) {
            throw new CommonException("error.update.issue.type.rank.all.null");
        }
        IssueTypeVO frontIssueType = issueTypeMap.get(frontId);
        IssueTypeVO backIssueType = issueTypeMap.get(backId);
        String frontRank = null;
        String backRank = null;
        if (frontIssueType != null) {
            frontRank = frontIssueType.getRank();
        }
        if (backIssueType != null) {
            backRank = backIssueType.getRank();
        }
        backRank = getMaxBackRankIfExisted(frontRank, backRank, organizationId, projectId);
        String rank = between(frontRank, backRank);
        IssueTypeExtendDTO example = new IssueTypeExtendDTO();
        example.setProjectId(projectId);
        example.setOrganizationId(organizationId);
        example.setIssueTypeId(issueTypeId);
        IssueTypeExtendDTO extend = issueTypeExtendMapper.selectOne(example);
        insertOrUpdateRank(issueTypeVO, extend, rank, projectId);
    }

    @Override
    public Page<IssueTypeVO> pagingProjectIssueTypes(PageRequest pageRequest, Long organizationId, IssueTypeSearchVO issueTypeSearchVO) {
        List<Long> projectIds = issueTypeSearchVO.getProjectIds();
        if (CollectionUtils.isEmpty(projectIds)) {
            projectIds = new ArrayList<>();
            Long userId = DetailsHelper.getUserDetails().getUserId();
            List<ProjectVO> list = baseFeignClient.listProjectsByUserIdForSimple(organizationId, userId,  null, true).getBody();
            if (CollectionUtils.isEmpty(list)) {
                return new Page<>();
            }
            projectIds.addAll(list.stream().map(ProjectVO::getId).collect(Collectors.toList()));
        }
        List<Long> finalProjectIds = projectIds;
        Page<IssueTypeVO> page = PageHelper.doPage(pageRequest, () -> issueTypeMapper.selectProjectIssueTypeByOptions(organizationId, finalProjectIds, issueTypeSearchVO));
        List<IssueTypeVO> list = new ArrayList<>();
        List<Long> filterIssueTypeIds = issueTypeSearchVO.getFilterIssueTypeIds();
        if (!CollectionUtils.isEmpty(filterIssueTypeIds)) {
            String ids = filterIssueTypeIds.stream().map(String::valueOf).collect(Collectors.joining(","));
            List<IssueTypeDTO> issueTypeDTOS = issueTypeMapper.selectByIds(ids);
            list.addAll(modelMapper.map(issueTypeDTOS, new TypeToken<List<IssueTypeDTO>>() {
            }.getType()));
        }
        if (!CollectionUtils.isEmpty(page.getContent())) {
            list.addAll(page.getContent());
        }
        return PageUtil.buildPageInfoWithPageInfoList(page, list);
    }

    @Override
    public void initIssueTypeIfNotExisted(Long organizationId) {
        IssueTypeDTO query = new IssueTypeDTO();
        query.setOrganizationId(organizationId);
        query.setInitialize(true);
        List<IssueTypeDTO> issueTypeDTOS = issueTypeMapper.select(query);
        if (CollectionUtils.isEmpty(issueTypeDTOS)) {
            initIssueTypeByConsumeCreateOrganization(organizationId);
        }
    }

    private String getMaxBackRankIfExisted(String frontRank,
                                           String backRank,
                                           Long organizationId,
                                           Long projectId) {
        //处理分页的情况拖动到最后的情况
        if (StringUtils.hasText(frontRank) && !StringUtils.hasText(backRank)) {
            return issueTypeExtendMapper.selectMaxRank(organizationId, projectId, frontRank);
        } else {
            return backRank;
        }
    }

    private String between(String frontRank, String backRank) {
        if (!StringUtils.hasText(frontRank) && StringUtils.hasText(backRank)) {
            return RankUtil.genNext(backRank);
        } else if (StringUtils.hasText(frontRank) && !StringUtils.hasText(backRank)) {
            return RankUtil.genPre(frontRank);
        } else {
            return RankUtil.between(frontRank, backRank);
        }
    }


    private Map<Long, IssueTypeVO> initRankIfNull(Long organizationId, Long projectId) {
        List<String> typeCodes =
                objectSchemeFieldService.issueTypes(organizationId, projectId)
                        .stream()
                        .filter(x -> !IGNORED_ISSUE_TYPES.contains(x.getTypeCode()))
                        .map(IssueTypeVO::getTypeCode)
                        .collect(Collectors.toList());
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setOrganizationId(organizationId);
        issueTypeSearchVO.setProjectId(projectId);
        issueTypeSearchVO.setTypeCodes(typeCodes);
        List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO);
        int size = issueTypes.size();
        if (size == 0) {
            return new HashMap<>();
        }
        Map<Long, IssueTypeVO> issueTypeMap =
                issueTypes
                        .stream()
                        .collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));
        Set<Long> issueTypeIds = issueTypeMap.keySet();
        Map<Long, IssueTypeExtendDTO> issueTypeExtendMap =
                issueTypeExtendMapper.selectByIssueTypeIds(issueTypeIds, projectId, organizationId)
                        .stream()
                        .collect(Collectors.toMap(IssueTypeExtendDTO::getIssueTypeId, Function.identity()));
        for (int i = 0; i < size; i++) {
            IssueTypeVO issueType = issueTypes.get(i);
            Long issueTypeId = issueType.getId();
            if (StringUtils.hasText(issueType.getRank())) {
                continue;
            }
            IssueTypeVO previous = getPreviousIssueType(i, issueTypes);
            String rank;
            if (ObjectUtils.isEmpty(previous)) {
                rank = RankUtil.mid();
            } else {
                String previousRank = previous.getRank();
                if (previousRank == null) {
                    previousRank = RankUtil.mid();
                    insertOrUpdateRank(previous, issueTypeExtendMap.get(previous.getId()), previousRank, projectId);
                }
                rank = RankUtil.genPre(previousRank);
            }
            insertOrUpdateRank(issueType, issueTypeExtendMap.get(issueTypeId), rank, projectId);
            issueType.setRank(rank);
        }
        return issueTypeMap;
    }

    private void insertOrUpdateRank(IssueTypeVO issueType,
                                    IssueTypeExtendDTO issueTypeExtendDTO,
                                    String rank,
                                    Long projectId) {
        if (ObjectUtils.isEmpty(issueTypeExtendDTO)) {
            IssueTypeExtendDTO dto = new IssueTypeExtendDTO();
            dto.setIssueTypeId(issueType.getId());
            dto.setProjectId(projectId);
            dto.setOrganizationId(issueType.getOrganizationId());
            dto.setEnabled(true);
            dto.setRank(rank);
            issueTypeExtendMapper.insertSelective(dto);
        } else {
            issueTypeExtendDTO.setRank(rank);
            issueTypeExtendMapper.updateByPrimaryKeySelective(issueTypeExtendDTO);
        }
    }

    private IssueTypeVO getPreviousIssueType(int current, List<IssueTypeVO> issueTypes) {
        if (current == 0) {
            return null;
        }
        return issueTypes.get(current - 1);
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
                IssueTypeDTO dto = buildIssueTypeFromInitIssueType(initIssueType, organizationId);
                IssueTypeDTO issueType = createIssueType(dto);
                temp.put(initIssueType.getTypeCode(), issueType.getId());
            }
            result.put(orgId, temp);
        }
        return result;
    }

    @Override
    public String getIssueTypeById(Long issueTypeId) {
        IssueTypeDTO issueTypeDTO = issueTypeMapper.selectByPrimaryKey(issueTypeId);
        if (issueTypeDTO == null) {
            throw new CommonException(ERROR_ISSUE_TYPE_NOT_EXISTED);
        }
        return issueTypeDTO.getTypeCode();
    }

    @Override
    public void updateSystemIssueType(Long organizationId,
                                      Long projectId,
                                      Long issueTypeId,
                                      IssueTypeVO issueTypeVO) {
        //只有项目层问题类型可以被通过改接口重命名
        if (ZERO.equals(projectId)) {
            return;
        }
        IssueTypeDTO dto = issueTypeMapper.selectWithAlias(issueTypeId, projectId);
        if (dto == null) {
            throw new CommonException(ERROR_ISSUE_TYPE_NOT_EXISTED);
        }
        String name = issueTypeVO.getName();
        String icon = issueTypeVO.getIcon();
        String description = issueTypeVO.getDescription();
        String colour = issueTypeVO.getColour();
        updateSystemIssueTypeValidator(organizationId, projectId, issueTypeId, dto, issueTypeVO);
        IssueTypeExtendDTO extend = new IssueTypeExtendDTO();
        extend.setIssueTypeId(issueTypeId);
        extend.setProjectId(projectId);
        extend.setOrganizationId(organizationId);
        List<IssueTypeExtendDTO> list = issueTypeExtendMapper.select(extend);
        if (list.isEmpty()) {
            extend.setEnabled(true);
            extend.setName(name);
            extend.setIcon(icon);
            extend.setDescription(description);
            extend.setColour(colour);
            issueTypeExtendMapper.insert(extend);
        } else {
            extend = list.get(0);
            extend.setName(name);
            extend.setIcon(icon);
            extend.setDescription(description);
            extend.setColour(colour);
            issueTypeExtendMapper.updateByPrimaryKey(extend);
        }
    }

    private void updateSystemIssueTypeValidator(Long organizationId,
                                                Long projectId,
                                                Long issueTypeId,
                                                IssueTypeDTO issueTypeDTO,
                                                IssueTypeVO issueTypeVO) {
        String name = issueTypeVO.getName();
        String icon = issueTypeVO.getIcon();
        String colour = issueTypeVO.getColour();
        //非系统问题类型不可用这个接口重命名
        if (!SYSTEM.equals(issueTypeDTO.getSource())) {
            throw new CommonException("error.can.not.edit");
        }
        if (!StringUtils.hasText(name)) {
            throw new CommonException("error.issue.type.name.empty");
        }
        if (!StringUtils.hasText(icon)) {
            throw new CommonException("error.issue.type.icon.empty");
        }
        if (!StringUtils.hasText(colour)) {
            throw new CommonException("error.issue.type.colour.empty");
        }
        if (nameExisted(organizationId, projectId, name, issueTypeId)) {
            throw new CommonException(ERROR_ISSUE_TYPE_NAME_EXISTED);
        }
        if (iconExisted(organizationId, projectId, icon, issueTypeId)) {
            throw new CommonException(ERROR_ISSUE_TYPE_ICON_EXISTED);
        }
    }

    @Override
    public Boolean checkIcon(Long organizationId,
                             Long projectId,
                             String icon,
                             Long issueTypeId) {
        return iconExisted(organizationId, projectId, icon, issueTypeId);
    }

    private Boolean iconExisted(Long organizationId, Long projectId, String icon, Long issueTypeId) {
        List<IssueTypeVO> issueTypes = issueTypeMapper.selectByOptions(organizationId, projectId, null);
        Map<String, Long> iconMap =
                issueTypes.stream().collect(Collectors.toMap(IssueTypeVO::getIcon, IssueTypeVO::getId));
        Long id = iconMap.get(icon);
        return existed(issueTypeId, id);
    }

    private Boolean existed(Long issueTypeId, Long id) {
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
}
