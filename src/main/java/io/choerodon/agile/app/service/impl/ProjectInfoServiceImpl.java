package io.choerodon.agile.app.service.impl;

import java.util.ArrayList;
import java.util.List;

import io.choerodon.agile.api.vo.ProjectInfoFixVO;
import io.choerodon.agile.api.vo.ProjectInfoVO;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.app.service.AgilePluginService;
import io.choerodon.agile.app.service.BacklogExpandService;
import io.choerodon.agile.app.service.ProjectInfoService;
import io.choerodon.agile.infra.dto.ProjectInfoDTO;
import io.choerodon.agile.infra.mapper.ProjectInfoMapper;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/30
 */
@Component
@Transactional(rollbackFor = Exception.class)
public class ProjectInfoServiceImpl implements ProjectInfoService {

    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;

    @Override
    public void initializationProjectInfo(ProjectEvent projectEvent) {

        ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
        projectInfoDTO.setProjectId(projectEvent.getProjectId());
        List<ProjectInfoDTO> projectDTOS = projectInfoMapper.select(projectInfoDTO);
        if (!CollectionUtils.isEmpty(projectDTOS)) {
            return;
        }
        projectInfoDTO.setProjectCode(projectEvent.getProjectCode());
        projectInfoDTO.setFeedbackMaxNum(0L);
        projectInfoDTO.setIssueMaxNum(0L);
        int result = projectInfoMapper.insert(projectInfoDTO);
        if (result != 1) {
            throw new CommonException("error.projectInfo.initializationProjectInfo");
        }
        if (backlogExpandService != null) {
            backlogExpandService.initBacklogMaxNum(projectEvent.getProjectId(),0L);
        }
    }

    @Override
    public Boolean checkProjectCode(String projectName) {
        ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
        projectInfoDTO.setProjectCode(projectName);
        return projectInfoMapper.selectOne(projectInfoDTO) != null;
    }

    @Override
    public ProjectInfoVO updateProjectInfo(ProjectInfoVO projectInfoVO) {
        ProjectInfoDTO projectInfoDTO = modelMapper.map(projectInfoVO, ProjectInfoDTO.class);
        if (ObjectUtils.isEmpty(agilePluginService)) {
            if (projectInfoMapper.updateByPrimaryKeySelective(projectInfoDTO) != 1) {
                throw new CommonException("error.projectInfo.update");
            }
            return projectInfoVO;
        } else {
            return agilePluginService.updateProjectInfo(projectInfoVO);
        }
    }

    @Override
    public ProjectInfoVO queryProjectInfoByProjectId(Long projectId) {
        ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
        projectInfoDTO.setProjectId(projectId);
        ProjectInfoDTO dto = projectInfoMapper.selectOne(projectInfoDTO);
        if (ObjectUtils.isEmpty(dto)) {
            return null;
        } else {
            return modelMapper.map(dto, ProjectInfoVO.class);
        }
    }

    /**
     * 更新MaxNum方法，在高并发的情况下，可能更新的maxNum已经不是最大的maxNum，因此不需要判断是否更新成功
     *
     * @param projectId   projectId
     * @param issueMaxNum issueMaxNum
     */
    @Override
    public void updateIssueMaxNum(Long projectId, String issueMaxNum) {
        projectInfoMapper.updateIssueMaxNum(projectId, issueMaxNum);
    }

    @Override
    public List<ProjectInfoFixVO> queryAllProjectInfo() {
        List<ProjectInfoDTO> projectInfoDTOList = projectInfoMapper.selectAll();
        if (projectInfoDTOList != null && !projectInfoDTOList.isEmpty()) {
            return modelMapper.map(projectInfoDTOList, new TypeToken<List<ProjectInfoFixVO>>(){}.getType());
        } else {
            return new ArrayList<>();
        }
    }

}
