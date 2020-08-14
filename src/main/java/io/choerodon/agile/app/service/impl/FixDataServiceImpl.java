package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.StateMachineNodeVO;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import org.apache.commons.collections.MapIterator;
import org.apache.commons.collections.map.MultiKeyMap;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.slf4j.LoggerFactory.getLogger;

@Service
@Transactional(rollbackFor = Exception.class)
public class FixDataServiceImpl implements FixDataService {

    private static final Logger LOGGER = getLogger(FixDataServiceImpl.class);

    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private ProjectConfigMapper projectConfigMapper;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired
    private IssueLinkTypeMapper issueLinkTypeMapper;
    @Autowired
    private BoardMapper boardMapper;
    @Autowired
    private BoardColumnMapper boardColumnMapper;
    @Autowired
    private ColumnStatusRelMapper columnStatusRelMapper;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired
    private ProjectInfoService projectInfoService;
    @Autowired
    private IssueLinkTypeService issueLinkTypeService;
    @Autowired
    private StateMachineSchemeService stateMachineSchemeService;
    @Autowired
    private IssueTypeSchemeService issueTypeSchemeService;
    @Autowired
    private StateMachineMapper stateMachineMapper;
    @Autowired
    private StateMachineNodeService stateMachineNodeService;
    @Autowired
    private StateMachineTransformMapper stateMachineTransformMapper;
    @Autowired
    private StateMachineSchemeConfigService stateMachineSchemeConfigService;
    @Autowired
    private IssueTypeSchemeConfigMapper issueTypeSchemeConfigMapper;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private PageMapper pageMapper;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;

    private List<String> fieldIssueTypes = Arrays.asList(ObjectSchemeFieldContext.ISSUE_TYPES);

    @Override
    public void fixCreateProject() {
        // 查询有问题的项目id列表
        List<Long> configProjectIds = projectConfigMapper.selectAll().stream().map(ProjectConfigDTO::getProjectId).collect(Collectors.toList());
        List<Long> projectIds = projectInfoMapper.selectAll().stream().map(ProjectInfoDTO::getProjectId).collect(Collectors.toList());
        projectIds.removeAll(configProjectIds);
        Collections.sort(projectIds, Comparator.reverseOrder());
        LOGGER.info("查询出有问题的项目共有{}个，开始修复数据", projectIds.size());
        int count = 0;
        for (Long projectId : projectIds) {
            ProjectVO project = baseFeignClient.queryProject(projectId).getBody();
            LOGGER.info("项目id:{}，项目信息:{}", projectId, project);
            if (!project.getCode().equals("def-ops-proj") || !project.getCategory().equals(ProjectCategory.GENERAL) || !project.getCreatedBy().equals(0L)) {
                LOGGER.info("项目id:{}，该项目不符合规定，跳过", projectId);
                continue;
            }
            singleFix(projectId, project);
            count++;
        }
        LOGGER.info("完成修复数据，共计修复项目{}个", count);
    }

    @Override
    public void fixCreateProjectSingle(Long projectId) {
        ProjectInfoDTO projectInfo = new ProjectInfoDTO();
        projectInfo.setProjectId(projectId);
        if (projectInfoMapper.select(projectInfo).isEmpty()) {
            LOGGER.info("项目id:{}，该项目不符合规定，跳过", projectId);
            return;
        }
        ProjectConfigDTO projectConfig = new ProjectConfigDTO();
        projectConfig.setProjectId(projectId);
        if (!projectConfigMapper.select(projectConfig).isEmpty()) {
            LOGGER.info("项目id:{}，该项目不符合规定，跳过", projectId);
            return;
        }
        ProjectVO project = baseFeignClient.queryProject(projectId).getBody();
        LOGGER.info("项目id:{}，项目信息:{}", projectId, project.toString());
        if (!project.getCode().equals("def-ops-proj") || !project.getCategory().equals(ProjectCategory.GENERAL) || !project.getCreatedBy().equals(0L)) {
            LOGGER.info("项目id:{}，该项目不符合规定，跳过", projectId);
            return;
        }
        singleFix(projectId, project);
    }

    private void singleFix(Long projectId, ProjectVO project) {
        // 删除project_info
        ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
        projectInfoDTO.setProjectId(projectId);
        projectInfoMapper.delete(projectInfoDTO);
        // 删除agile_issue_link_type
        IssueLinkTypeDTO issueLinkTypeDTO = new IssueLinkTypeDTO();
        issueLinkTypeDTO.setProjectId(projectId);
        issueLinkTypeMapper.delete(issueLinkTypeDTO);
        // 删除agile_board
        BoardDTO boardDTO = new BoardDTO();
        boardDTO.setProjectId(projectId);
        boardMapper.delete(boardDTO);
        // 删除agile_board_column
        BoardColumnDTO boardColumnDTO = new BoardColumnDTO();
        boardColumnDTO.setProjectId(projectId);
        boardColumnMapper.delete(boardColumnDTO);
        // 删除agile_board_column_status_rel
        ColumnStatusRelDTO columnStatusRelDTO = new ColumnStatusRelDTO();
        columnStatusRelDTO.setProjectId(projectId);
        columnStatusRelMapper.delete(columnStatusRelDTO);
        // 删除agile_issue_status
        IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
        issueStatusDTO.setProjectId(projectId);
        issueStatusMapper.delete(issueStatusDTO);
        LOGGER.info("已清理脏数据，项目id:{}", projectId);

        ProjectEvent projectEvent = new ProjectEvent();
        projectEvent.setProjectCategory(ProjectCategory.GENERAL);
        projectEvent.setProjectCode(project.getCode());
        projectEvent.setProjectName(project.getName());
        projectEvent.setProjectId(project.getId());
        //创建projectInfo
        projectInfoService.initializationProjectInfo(projectEvent);
        //创建项目初始化issueLinkType
        issueLinkTypeService.initIssueLinkType(projectEvent.getProjectId());
        //创建项目时创建默认状态机方案
        stateMachineSchemeService.initByConsumeCreateProject(projectEvent);
        //创建项目时创建默认问题类型方案
        issueTypeSchemeService.initByConsumeCreateProject(projectEvent.getProjectId(), projectEvent.getProjectCode());
        LOGGER.info("已修复数据，项目id:{}", projectId);
    }

    public void fixDateStateMachine(){
        long l = System.currentTimeMillis();
        LOGGER.info("开始修复数据");

        fixStateMachineTransform();

        fixStateMachineByIssueTypeId();
        LOGGER.info("修复数据完成,耗时{}",System.currentTimeMillis() - l);
    }

    @Override
    public void fixPage() {
        String createPageCode = "agile_issue_create";
        String editPageCode = "agile_issue_edit";
        Long createPageId = getPageIdByCode(createPageCode);
        Long editPageId = getPageIdByCode(editPageCode);
        if (ObjectUtils.isEmpty(createPageId) || ObjectUtils.isEmpty(editPageId)) {
            return;
        }

        String schemeCode = "agile_issue";
        //key1 fieldId, key2 organizationId, key3 projectId, key4 issueType
        MultiKeyMap dataMap = new MultiKeyMap();

        ObjectSchemeFieldDTO objectSchemeField = new ObjectSchemeFieldDTO();
        objectSchemeField.setSchemeCode(schemeCode);
        objectSchemeField.setSystem(true);

        generateDataMap(createPageId, editPageId, dataMap, objectSchemeField);

        objectSchemeField.setSystem(false);
        generateDataMap(createPageId, editPageId, dataMap, objectSchemeField);
        List<ObjectSchemeFieldExtendDTO> insertList = new ArrayList<>();
        MapIterator mapIterator = dataMap.mapIterator();
        while (mapIterator.hasNext()) {
            mapIterator.next();
            ObjectSchemeFieldExtendDTO dto = (ObjectSchemeFieldExtendDTO)mapIterator.getValue();
            dto.setCreated(Optional.ofNullable(dto.getCreated()).orElse(false));
            dto.setEdited(Optional.ofNullable(dto.getEdited()).orElse(false));
            insertList.add(dto);
        }
        int total = insertList.size();
        int step = 5000;
        int totalPage = total / step + 1;
        for (int i = 0; i < totalPage; i++) {
            int startLine = i * step;
            int endLine =  (i + 1) * step > total ? total : (i + 1) * step;
            objectSchemeFieldExtendMapper.batchInsert(insertList.subList(startLine, endLine));
        }
    }

    private void generateDataMap(Long createPageId, Long editPageId, MultiKeyMap dataMap, ObjectSchemeFieldDTO objectSchemeField) {
        List<ObjectSchemeFieldDTO> fields = objectSchemeFieldMapper.selectFieldsWithPages(objectSchemeField);
        List<PageFieldDTO> pages = new ArrayList<>();
        fields.forEach(s -> pages.addAll(s.getPages()));
        Set<Long> organizationIds = pages.stream().map(PageFieldDTO::getOrganizationId).collect(Collectors.toSet());
        List<IssueTypeDTO> issueTypeList = issueTypeMapper.selectByOrganizationIds(organizationIds);
        Map<Long, List<IssueTypeDTO>> issueTypeMap = issueTypeList.stream().collect(Collectors.groupingBy(IssueTypeDTO::getOrganizationId));

        fields.forEach(s -> {
            Boolean required = s.getRequired();
            String context = s.getContext();
            String[] contextArray = context.split(",");
            Long fieldId = s.getId();
            List<PageFieldDTO> pageFields = s.getPages();
            pageFields.forEach(p -> {
                Long organizationId = p.getOrganizationId();
                List<IssueTypeDTO> issueTypes = getIssueType(organizationId, issueTypeMap, contextArray);
                ObjectSchemeFieldExtendDTO carrier =
                        buildCarrier(createPageId, editPageId, required, fieldId, p, organizationId);
                issueTypes.forEach(i -> {
                    carrier.setIssueType(i.getTypeCode());
                    carrier.setIssueTypeId(i.getId());
                    getAndPutDataMap(dataMap, carrier);
                });
            });
        });
    }

    private List<IssueTypeDTO> getIssueType(Long organizationId,
                                            Map<Long, List<IssueTypeDTO>> issueTypeMap,
                                            String[] contextArray) {
        List<IssueTypeDTO> result = new ArrayList<>();
        List<String> contextList = Arrays.asList(contextArray);
        List<IssueTypeDTO> issueTypeList = issueTypeMap.get(organizationId);
        if (ObjectSchemeFieldContext.isGlobal(contextArray)) {
            issueTypeList.forEach(i -> {
                if (fieldIssueTypes.contains(i.getTypeCode())) {
                    result.add(i);
                }
            });
        } else {
            issueTypeList.forEach(i -> {
                if (contextList.contains(i.getTypeCode())) {
                    result.add(i);
                }
            });
        }
        return result;
    }

    private ObjectSchemeFieldExtendDTO buildCarrier(Long createPageId, Long editPageId, Boolean required, Long fieldId, PageFieldDTO p, Long organizationId) {
        Long projectId = p.getProjectId();
        Long pageId = p.getPageId();
        Boolean created = null;
        Boolean edited = null;
        String rank = null;
        if (createPageId.equals(pageId)) {
            created = p.getDisplay();
            rank = p.getRank();
        }
        if (editPageId.equals(pageId)) {
            edited = p.getDisplay();
            rank = p.getRank();
        }

        ObjectSchemeFieldExtendDTO carrier = new ObjectSchemeFieldExtendDTO();
        carrier.setOrganizationId(organizationId);
        carrier.setProjectId(projectId);
        carrier.setFieldId(fieldId);
        carrier.setRequired(required);
        carrier.setCreated(created);
        carrier.setEdited(edited);
        carrier.setRank(rank);
        return carrier;
    }

    private void getAndPutDataMap(MultiKeyMap dataMap, ObjectSchemeFieldExtendDTO carrier) {
        Long issueTypeId = carrier.getIssueTypeId();
        Long fieldId = carrier.getFieldId();
        Long organizationId = carrier.getOrganizationId();
        Long projectId = carrier.getProjectId();
        String issueType = carrier.getIssueType();
        Boolean required = carrier.getRequired();
        Boolean created = carrier.getCreated();
        Boolean edited = carrier.getEdited();
        String rank = carrier.getRank();
        ObjectSchemeFieldExtendDTO dto =
                (ObjectSchemeFieldExtendDTO) dataMap.get(fieldId, organizationId, projectId, issueType);
        if (ObjectUtils.isEmpty(dto)) {
            dto = new ObjectSchemeFieldExtendDTO();
            dto.setIssueTypeId(issueTypeId);
            dto.setIssueType(issueType);
            dto.setOrganizationId(organizationId);
            dto.setProjectId(projectId);
            dto.setFieldId(fieldId);
            dto.setRequired(required);
            dto.setCreated(created);
            dto.setEdited(edited);
            dto.setRank(rank);
            dataMap.put(fieldId, organizationId, projectId, issueType, dto);
        } else {
            if (!ObjectUtils.isEmpty(rank)) {
                dto.setRank(rank);
            }
            if (!ObjectUtils.isEmpty(created)) {
                dto.setCreated(created);
            }
            if (!ObjectUtils.isEmpty(edited)) {
                dto.setEdited(edited);
            }
            dto.setRequired(required);
        }
    }

    private Long getPageIdByCode(String code) {
        PageDTO pageExample = new PageDTO();
        pageExample.setPageCode(code);
        List<PageDTO> pages = pageMapper.select(pageExample);
        if (pages.isEmpty()) {
            LOGGER.error("【迁移页面配置数据失败】fd_page中不存在code={}的数据", code);
            return null;
        }
        return pages.get(0).getId();
    }

    private  void fixStateMachineTransform(){
        LOGGER.info("开始修复状态机转换");
        // 查所有的状态机
        List<StateMachineDTO> stateMachines = stateMachineMapper.selectAll();
        // 遍历修改
        for (StateMachineDTO stateMachineDTO : stateMachines) {
            // 查询当前状态机所有的node
            List<StateMachineNodeVO> stateMachineNodeVOS = stateMachineNodeService.queryByStateMachineId(stateMachineDTO.getOrganizationId(), stateMachineDTO.getId(), false);
            if (CollectionUtils.isEmpty(stateMachineNodeVOS)) {
                continue;
            }
            List<StateMachineNodeVO> machineNodeVOS = stateMachineNodeVOS.stream().filter(v -> !NodeType.START.equals(v.getType())).collect(Collectors.toList());
            Map<Long, StateMachineNodeVO> nodeVOMap = machineNodeVOS.stream().collect(Collectors.toMap(StateMachineNodeVO::getId, Function.identity()));
            // 将转换到所有转换为多个transform
            List<StateMachineTransformDTO> stateMachineTransformDTOS = stateMachineTransformMapper.queryByStateMachineIds(stateMachineDTO.getOrganizationId(), Arrays.asList(stateMachineDTO.getId()));
            List<StateMachineTransformDTO> allTransforms = stateMachineTransformDTOS.stream().filter(x -> x.getType().equals(TransformType.ALL)).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(allTransforms)) {
                continue;
            }
            // 对tansform_all进行转换
            Map<Long, List<Long>> nodeMap = stateMachineTransformDTOS.stream().filter(x -> x.getType().equals(TransformType.CUSTOM)).collect(Collectors.groupingBy(StateMachineTransformDTO::getStartNodeId, Collectors.mapping(StateMachineTransformDTO::getEndNodeId, Collectors.toList())));
            List<StateMachineTransformDTO> addTransform = new ArrayList<>();
            allTransforms.forEach(v -> {
                Long startNode = v.getEndNodeId();
                List<Long> endNodes = nodeMap.get(startNode);
                if (CollectionUtils.isEmpty(endNodes)) {
                    endNodes = new ArrayList<>();
                }
                for (StateMachineNodeVO node : machineNodeVOS) {
                    if (Boolean.FALSE.equals(endNodes.contains(node.getId()))) {
                        StateMachineNodeVO nodeVO = nodeVOMap.get(startNode);
                        StateMachineNodeVO endNodeVO = nodeVOMap.get(node.getId());
                        StateMachineTransformDTO stateMachineTransformDTO = new StateMachineTransformDTO();
                        stateMachineTransformDTO.setOrganizationId(stateMachineDTO.getOrganizationId());
                        stateMachineTransformDTO.setStartNodeId(startNode);
                        stateMachineTransformDTO.setEndNodeId(node.getId());
                        stateMachineTransformDTO.setName(nodeVO.getStatusVO().getName() + "转换到" + endNodeVO.getStatusVO().getName());
                        stateMachineTransformDTO.setStateMachineId(v.getStateMachineId());
                        stateMachineTransformDTO.setType(TransformType.CUSTOM);
                        stateMachineTransformDTO.setConditionStrategy("condition_all");
                        addTransform.add(stateMachineTransformDTO);
                    }
                }
            });
            // 批量增加
            stateMachineTransformMapper.batchInsert(addTransform);
        }
        // 删除所有type为transform_all的转换
        StateMachineTransformDTO stateMachineTransformDTO = new StateMachineTransformDTO();
        stateMachineTransformDTO.setType(TransformType.ALL);
        stateMachineTransformMapper.delete(stateMachineTransformDTO);
        LOGGER.info("修复状态机转换完成,共计修复:{}条", stateMachines.size());
    }

    private void fixStateMachineByIssueTypeId(){
        LOGGER.info("开始修复问题类型的状态机");
        // 查询所有的项目
        List<Long> projectIds = projectInfoMapper.selectAll().stream().map(ProjectInfoDTO::getProjectId).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(projectIds)) {
            return;
        }
        int total = projectIds.size();
        int size = 100;
        int totalPage = (int) Math.ceil(projectIds.size() / (size * 1.0));
        int totalSize = 0;
        for (int page = 0; page < totalPage; page++) {
            List<Long> list = projectIds.subList(page * size > total ? total : page * size, (page + 1) * size > total ? total : (page + 1) * size);
            List<ProjectVO> projectVOS = baseFeignClient.queryByIds(new HashSet<>(list)).getBody();
            for (ProjectVO projectVO : projectVOS) {
                String applyType = "PROGRAM".equals(projectVO.getCategory()) ? "program" : "agile";
                // 查询单个项目的问题类型(故事、特性、任务、子任务、bug)
                fixStateMachineApplyType(projectVO,applyType);
                // 修复需求池的状态机
                fixStateMachineApplyType(projectVO,"backlog");
            }
            totalSize = totalSize + projectVOS.size();
        }
        LOGGER.info("项目的状态机修复完成:预计项目{}个", projectIds.size());
        LOGGER.info("项目的状态机修复完成:实际修复项目{}个", totalSize);
    }

    private void fixStateMachineApplyType(ProjectVO projectVO,String applyType){
        ProjectConfigDTO projectConfigDTO = projectConfigMapper.queryBySchemeTypeAndApplyType(projectVO.getId(), SchemeType.ISSUE_TYPE, applyType);
        ProjectConfigDTO configDTO = projectConfigMapper.queryBySchemeTypeAndApplyType(projectVO.getId(), SchemeType.STATE_MACHINE, applyType);
        if (ObjectUtils.isEmpty(projectConfigDTO)) {
            return;
        }
        if (ObjectUtils.isEmpty(configDTO)) {
            return;
        }
        Long stateMachineSchemeId = configDTO.getSchemeId();
        Long schemeId = projectConfigDTO.getSchemeId();
        IssueTypeSchemeConfigDTO issueTypeSchemeConfigDTO = new IssueTypeSchemeConfigDTO();
        issueTypeSchemeConfigDTO.setSchemeId(schemeId);
        issueTypeSchemeConfigDTO.setOrganizationId(projectVO.getOrganizationId());
        List<IssueTypeSchemeConfigDTO> issueTypeSchemeConfigDTOS = issueTypeSchemeConfigMapper.select(issueTypeSchemeConfigDTO);
        if (CollectionUtils.isEmpty(issueTypeSchemeConfigDTOS)) {
            return;
        }
        List<Long> issueTypeIds = issueTypeSchemeConfigDTOS.stream().map(IssueTypeSchemeConfigDTO::getIssueTypeId).collect(Collectors.toList());
        for (Long issueTypeId : issueTypeIds) {
            stateMachineSchemeConfigService.queryStatusMachineBySchemeIdAndIssueType(projectVO.getOrganizationId(), stateMachineSchemeId, issueTypeId);
        }
    }

}
