package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.IssueTypeSchemeConfigDTO;
import io.choerodon.agile.infra.dto.ProjectConfigDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
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

    private static final String ORGANIZATION ="organization";

    private static final String PROJECT ="project";

    private static final List<String> AGILE_ISSUE_TYPES =
            Arrays.asList(
                    IssueTypeCode.BUG.value(),
                    IssueTypeCode.STORY.value(),
                    IssueTypeCode.SUB_TASK.value(),
                    IssueTypeCode.TASK.value(),
                    IssueTypeCode.ISSUE_EPIC.value()
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
        issueTypeVO.setEnabled(true);
        String source = ZERO.equals(projectId) ? ORGANIZATION : PROJECT;
        issueTypeVO.setSource(source);
        IssueTypeDTO issueType = modelMapper.map(issueTypeVO, IssueTypeDTO.class);
        IssueTypeVO result = modelMapper.map(createIssueType(issueType), IssueTypeVO.class);
        if (!ZERO.equals(projectId)) {
            //项目层创建问题类型,初始化默认状态机
            initDefaultStateMachine(organizationId, projectId, result, categoryCodes);
        }
        return result;
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
        issueTypes.add(IssueTypeCode.BACKLOG.value());
        if (codes.contains(ProjectCategory.MODULE_AGILE)
                && !issueTypes.contains(typeCode)) {
            throw new CommonException("error.illegal.type.code");
        }
        issueTypes.clear();
        issueTypes.addAll(PROGRAM_ISSUE_TYPES);
        issueTypes.add(IssueTypeCode.BACKLOG.value());
        if (codes.contains(ProjectCategory.MODULE_PROGRAM)
                && !issueTypes.contains(typeCode)) {
            throw new CommonException("error.illegal.type.code");
        }
        if (!codes.contains(ProjectCategory.MODULE_BACKLOG)
                && IssueTypeCode.BACKLOG.value().equals(typeCode)) {
            throw new CommonException("error.project.backlog.not.open");
        }
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
        IssueTypeDTO example = new IssueTypeDTO();
        example.setOrganizationId(organizationId);
        example.setName(name);
        example.setProjectId(projectId);
        List<IssueTypeDTO> list = issueTypeMapper.select(example);
        if (issueTypeId == null) {
            //新增
            return !list.isEmpty();
        } else {
            //编辑
            if (list.isEmpty()) {
                return false;
            } else {
                IssueTypeDTO dto = list.get(0);
                if (issueTypeId.equals(dto.getId())) {
                    return false;
                } else {
                    return true;
                }
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
    public Map<String, Object> checkDelete(Long organizationId, Long issueTypeId) {
        Map<String, Object> result = new HashMap<>();
        result.put("canDelete", true);
        IssueTypeDTO issueType = issueTypeMapper.selectByPrimaryKey(issueTypeId);
        if (issueType == null) {
            throw new CommonException("error.base.notFound");
        } else if (!issueType.getOrganizationId().equals(organizationId)) {
            throw new CommonException("error.issueType.illegal");
        }
        //判断要删除的issueType是否有使用中的issue【toDo】

        return result;
    }

    @Override
    public void delete(Long organizationId,
                          Long projectId,
                          Long issueTypeId) {
        if (!deleted(organizationId, projectId, issueTypeId)) {
            throw new CommonException("error.issue.type.not.deleted");
        }
        IssueTypeDTO dto = new IssueTypeDTO();
        dto.setOrganizationId(organizationId);
        dto.setProjectId(projectId);
        dto.setId(issueTypeId);
        issueTypeMapper.delete(dto);
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
            createIssueType(new IssueTypeDTO(initIssueType.getIcon(), initIssueType.getName(), initIssueType.getDescription(), organizationId, initIssueType.getColour(), initIssueType.getTypeCode(), true));
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
        if (result != null) {
            return modelMapper.map(result, IssueTypeVO.class);
        } else {
            return null;
        }
    }

    @Override
    public boolean deleted(Long organizationId, Long projectId, Long issueTypeId) {
        IssueTypeDTO dto = issueTypeMapper.selectByPrimaryKey(issueTypeId);
        if (dto == null) {
            throw new CommonException("error.issue.type.not.existed");
        }
        if (!dto.getProjectId().equals(projectId)
                || !dto.getOrganizationId().equals(organizationId)) {
            return false;
        }
        if (Boolean.TRUE.equals(dto.getInitialize())) {
            return false;
        }
        if (!ZERO.equals(projectId)) {
            IssueDTO issue = new IssueDTO();
            issue.setProjectId(projectId);
            issue.setIssueTypeId(issueTypeId);
            int count = issueMapper.selectCount(issue);
            int backlogCount = 0;
            if (backlogExpandService != null) {
                //todo 判断需求池能不能删除
            }
            return count == 0 && backlogCount == 0;
        }
        return true;
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
