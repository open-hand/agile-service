package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.validator.StoryMapValidator;
import io.choerodon.agile.api.vo.business.StoryMapDragVO;
import io.choerodon.agile.app.assembler.StoryMapAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.StoryMapStoryDTO;
import io.choerodon.agile.infra.mapper.IssueStatusMapper;
import io.choerodon.agile.infra.mapper.StoryMapMapper;
import io.choerodon.agile.infra.mapper.StoryMapWidthMapper;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/5/31.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class StoryMapServiceImpl implements StoryMapService {

    @Autowired
    private StoryMapMapper storyMapMapper;

    @Autowired
    private IssueAccessDataService issueAccessDataService;

    @Autowired
    private StoryMapValidator storyMapValidator;

    @Autowired
    private VersionIssueRelService versionIssueRelService;

    @Autowired
    private StoryMapWidthMapper storyMapWidthMapper;
    @Autowired
    private StoryMapAssembler storyMapAssembler;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;

    protected List<StoryMapWidthVO> setStoryMapWidth(Long projectId) {
        List<StoryMapWidthDTO> storyMapWidthDTOList = storyMapWidthMapper.selectByProjectId(projectId);
        if (storyMapWidthDTOList != null && !storyMapWidthDTOList.isEmpty()) {
            return modelMapper.map(storyMapWidthDTOList, new TypeToken<List<StoryMapWidthVO>>(){}.getType());
        } else {
            return new ArrayList<>();
        }
    }

    @Override
    public StoryMapVO queryStoryMap(Long projectId, Long organizationId, SearchVO searchVO) {
        StoryMapVO storyMap = new StoryMapVO();
        List<Long> epicIds = new ArrayList<>();
        // get project completed status
        getStatusIdByIsCompleted(projectId, searchVO);
        // get project epic
        List<Long> projectEpicIds = storyMapMapper.selectEpicIdsByProject(projectId, searchVO.getAdvancedSearchArgs());
        if (projectEpicIds != null && !projectEpicIds.isEmpty()) {
            epicIds.addAll(projectEpicIds);
        }
        if (agilePluginService != null) {
            storyMap = agilePluginService.handlerBusinessQueryStoryMap(projectId, epicIds, searchVO);
        }
        else {
            if (epicIds.isEmpty()) {
                storyMap.setEpics(new ArrayList<>());
            } else {
                List<EpicWithInfoDTO> epicWithInfoDTOList = storyMapMapper.selectEpicList(projectId, epicIds, searchVO.getAdvancedSearchArgs());
                storyMap.setEpics(epicWithInfoDTOList);
            }
            List<EpicWithInfoDTO> epicWithInfoDTOList = storyMap.getEpics();
            List<StoryMapStoryDTO> storyMapStoryDTOS = storyMapMapper.selectStoryList(projectId, epicIds, searchVO);
            // 查询故事的问题数
            if (!CollectionUtils.isEmpty(storyMapStoryDTOS)) {
                List<Long> resultStoryIds = storyMapStoryDTOS.stream().map(StoryMapStoryDTO::getIssueId).collect(Collectors.toList());
                List<IssueProgressVO> storyCounts = storyMapMapper.countStoryProgress(projectId, resultStoryIds);
                Map<Long, IssueProgressVO> progressVOMap = storyCounts.stream().collect(Collectors.toMap(IssueProgressVO::getId, Function.identity()));
                for (StoryMapStoryDTO storyMapStoryDTO : storyMapStoryDTOS) {
                    storyMapStoryDTO.setIssueProgressVO(progressVOMap.get(storyMapStoryDTO.getIssueId()));
                }
            }
            // 查询史诗下的问题数
            List<IssueProgressVO> epicCounts = storyMapMapper.countEpicProgress(projectId,epicIds);
            Map<Long, IssueProgressVO> epicProgressVOMap = epicCounts.stream().collect(Collectors.toMap(IssueProgressVO::getId, Function.identity()));
            for (EpicWithInfoDTO epicWithInfoDTO : epicWithInfoDTOList) {
                epicWithInfoDTO.setIssueProgressVO(epicProgressVOMap.get(epicWithInfoDTO.getIssueId()));
            }
            storyMap.setStoryList(!epicIds.isEmpty() ? storyMapStoryDTOS : new ArrayList<>());
        }
        storyMap.setStoryMapWidth(setStoryMapWidth(projectId));
        return storyMap;
    }

    protected void getStatusIdByIsCompleted(Long projectId, SearchVO searchVO) {
        Boolean completedFlag = (Boolean) Optional.ofNullable(searchVO.getAdvancedSearchArgs())
                .map(map -> map.get("isCompleted")).orElse(null);
        if (Objects.nonNull(completedFlag)){
            List<IssueStatusDTO> statusIdList = issueStatusMapper.selectByCondition(Condition.builder(IssueStatusDTO.class)
                    .andWhere(Sqls.custom().andEqualTo(IssueStatusDTO.FIELD_PROJECT_ID, projectId)
                            .andEqualTo(IssueStatusDTO.FILED_IS_COMPLETED, completedFlag)).build());
            searchVO.getAdvancedSearchArgs().put("statusIdList",
                    statusIdList.stream().map(IssueStatusDTO::getStatusId).collect(Collectors.toList()));
        }
    }

    @Override
    public StoryMapVO queryStoryMapDemand(Long projectId, SearchVO searchVO) {
        StoryMapVO storyMap = new StoryMapVO();
        List<StoryMapStoryDTO> storyMapStoryDTOList = storyMapMapper.selectDemandStoryList(projectId, searchVO);
        storyMap.setDemandStoryList(storyMapAssembler.storyMapStoryDTOToVO(projectId, storyMapStoryDTOList));
        return storyMap;
    }

    protected void dragToEpic(Long projectId, Long epicId, StoryMapDragVO storyMapDragVO) {
        storyMapValidator.checkEpicExist(epicId);
        List<Long> issueIds = storyMapDragVO.getEpicIssueIds();
        if (issueIds != null && !issueIds.isEmpty()) {
            issueAccessDataService.batchIssueToEpic(projectId, epicId, issueIds);
        }
    }

    protected void dragToVersion(Long projectId, Long versionId, StoryMapDragVO storyMapDragVO) {
        List<VersionIssueRelVO> versionIssueRelVOList = storyMapDragVO.getVersionIssueRelVOList();
        if (versionIssueRelVOList != null && !versionIssueRelVOList.isEmpty()) {
            for (VersionIssueRelVO versionIssueRelVO : versionIssueRelVOList) {
                VersionIssueRelDTO versionIssueRelDTO = new VersionIssueRelDTO();
                versionIssueRelDTO.setIssueId(versionIssueRelVO.getIssueId());
                versionIssueRelDTO.setVersionId(versionIssueRelVO.getVersionId());
                versionIssueRelDTO.setRelationType("fix");
                versionIssueRelDTO.setProjectId(projectId);
                versionIssueRelService.delete(versionIssueRelDTO);
            }
        }
        storyMapValidator.checkVersionExist(versionId);
        List<Long> issueIds = storyMapDragVO.getVersionIssueIds();
        if (issueIds == null || issueIds.isEmpty()) {
            return;
        }
        if (!Objects.equals(versionId, 0L)) {
            VersionIssueRelDTO versionIssueRelDTO = new VersionIssueRelDTO();
            versionIssueRelDTO.createBatchIssueToVersionDTO(projectId, versionId, issueIds);
            issueAccessDataService.batchIssueToVersion(versionIssueRelDTO);
        }
    }

    @Override
    public void storyMapMove(Long projectId, StoryMapDragVO storyMapDragVO) {
        Long epicId = storyMapDragVO.getEpicId();
        Long versionId = storyMapDragVO.getVersionId();
        if (epicId != null) {
            dragToEpic(projectId, epicId, storyMapDragVO);
        }
        if (versionId != null) {
            dragToVersion(projectId, versionId, storyMapDragVO);
        }
        if (agilePluginService != null) {
            agilePluginService.handlerStoryMapMoveFeature(projectId,storyMapDragVO);
        }
    }

}
