package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.IssueTypeExtendDTO;
import io.choerodon.agile.infra.dto.IssueTypeSchemeConfigDTO;
import io.choerodon.agile.infra.dto.ProjectConfigDTO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.enums.SchemeType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ProjectCategoryUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.dto.IssueTypeDTO;
import io.choerodon.agile.infra.enums.InitIssueType;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

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

    private static final String ORGANIZATION ="organization";

    private static final String PROJECT ="project";

    private static final List<String> AGILE_ISSUE_TYPES =
            Arrays.asList(
//                    IssueTypeCode.BUG.value(),
//                    IssueTypeCode.ISSUE_EPIC.value(),
                    IssueTypeCode.STORY.value(),
                    IssueTypeCode.SUB_TASK.value(),
                    IssueTypeCode.TASK.value()
                    );

    private static final List<String> PROGRAM_ISSUE_TYPES =
            Arrays.asList(
                    IssueTypeCode.ISSUE_EPIC.value(),
                    IssueTypeCode.FEATURE.value());


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
        String source = ZERO.equals(projectId) ? ORGANIZATION : PROJECT;
        issueTypeVO.setSource(source);
        IssueTypeDTO issueType = modelMapper.map(issueTypeVO, IssueTypeDTO.class);
        IssueTypeVO result = modelMapper.map(createIssueType(issueType), IssueTypeVO.class);
        if (!ZERO.equals(projectId)) {
            updateEnabled(result.getId(), projectId, true);
            //项目层创建问题类型,初始化默认状态机
            initDefaultStateMachine(organizationId, projectId, result, categoryCodes);
        }
        return result;
    }

    private void updateEnabled(Long issueTypeId, Long projectId, boolean enabled) {
        IssueTypeExtendDTO dto = new IssueTypeExtendDTO();
        dto.setProjectId(projectId);
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
        ProjectVO project = baseFeignClient.queryProject(projectId).getBody();
        if (project == null) {
            throw new CommonException("error.project.not.existed");
        }
        Set<String> codes = ProjectCategoryUtil.getCategoryCodeAndValidate(project.getCategories());
        List<String> issueTypes = new ArrayList<>(AGILE_ISSUE_TYPES);
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

    private void initDefaultStateMachine(Long organizationId,
                                         Long projectId,
                                         IssueTypeVO issueType,
                                         Set<String> categoryCodes) {
        Long issueTypeId = issueType.getId();
        String typeCode = issueType.getTypeCode();
        String applyType;
        if (categoryCodes.contains(ProjectCategory.MODULE_AGILE)) {
            applyType = "agile";
        } else {
            applyType = "program";
        }
        if (IssueTypeCode.BACKLOG.value().equals(typeCode)) {
            applyType = "backlog";
        }
        ProjectConfigDTO configDTO = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.STATE_MACHINE, applyType);
        if (ObjectUtils.isEmpty(configDTO)) {
            throw new CommonException("error.stateMachine.scheme.config.not.found");
        }
        Long stateMachineSchemeId = configDTO.getSchemeId();
        stateMachineSchemeConfigService.queryStatusMachineBySchemeIdAndIssueType(organizationId, stateMachineSchemeId, issueTypeId);
        //初始化问题类型方案配置
        initIssueTypeSchemeConfig(organizationId, projectId, issueTypeId, applyType);
    }

    private void initIssueTypeSchemeConfig(Long organizationId, Long projectId, Long issueTypeId, String applyType) {
        ProjectConfigDTO issueTypeConfig = projectConfigMapper.queryBySchemeTypeAndApplyType(projectId, SchemeType.ISSUE_TYPE, applyType);
        if (ObjectUtils.isEmpty(issueTypeConfig)) {
            throw new CommonException("error.issueType.scheme.config.not.found");
        }
        Long issueTypeSchemeId = issueTypeConfig.getSchemeId();
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

        IssueTypeDTO dto = new IssueTypeDTO();
        dto.setId(issueTypeId);
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        IssueTypeDTO issueTypeDTO = issueTypeMapper.selectOne(dto);
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
        IssueTypeDTO dto = new IssueTypeDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setId(issueTypeId);
        IssueTypeDTO result = issueTypeMapper.selectOne(dto);
        if (result == null) {
            return;
        }
        IssueTypeVO vo = modelMapper.map(result, IssueTypeVO.class);
        setDeleted(Arrays.asList(vo), organizationId, projectId);
        if (Boolean.FALSE.equals(vo.getDeleted())) {
            throw new CommonException("error.issue.type.not.deleted");
        }
        issueTypeMapper.delete(dto);
        if (!ZERO.equals(projectId)) {
            IssueTypeExtendDTO extend = new IssueTypeExtendDTO();
            extend.setProjectId(projectId);
            extend.setIssueTypeId(issueTypeId);
            issueTypeExtendMapper.delete(extend);
        }
        //todo 删除关联数据
    }

    @Override
    public Page<IssueTypeVO> pagedQuery(PageRequest pageRequest,
                                        Long organizationId,
                                        Long projectId,
                                        IssueTypeSearchVO issueTypeSearchVO) {
        issueTypeSearchVO.setOrganizationId(organizationId);
        issueTypeSearchVO.setProjectId(projectId);
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
        List<ProjectVO> projects = baseFeignClient.listWithCategoryByOrganizationIds(organizationId, true).getBody();
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
    public List<IssueTypeVO> queryByOrgId(Long organizationId) {
        List<IssueTypeDTO> issueTypes = issueTypeMapper.queryByOrgId(organizationId);
        return modelMapper.map(issueTypes, new TypeToken<List<IssueTypeVO>>() {
        }.getType());
    }

    @Override
    public List<IssueTypeVO> queryIssueTypeByStateMachineSchemeId(Long organizationId, Long schemeId) {
        List<IssueTypeVO> issueTypeVOS = queryByOrgId(organizationId);
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
    public Map<String, Long> queryIssueTypeMap(Long organizationId) {
        IssueTypeDTO dto = new IssueTypeDTO();
        dto.setOrganizationId(organizationId);
        return issueTypeMapper.select(dto).stream().collect(Collectors.toMap(IssueTypeDTO::getTypeCode, IssueTypeDTO::getId));
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
            throw new CommonException("error.issue.type.not.found");
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
        updateEnabled(issueTypeId, projectId, enabled);
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
}
