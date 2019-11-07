package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
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
            LOGGER.info("项目id:{}，项目信息:{}", projectId, project.toString());
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

}
