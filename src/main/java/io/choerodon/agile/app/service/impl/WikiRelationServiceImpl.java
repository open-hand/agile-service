package io.choerodon.agile.app.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import io.choerodon.agile.api.vo.KnowledgeRelationVO;
import io.choerodon.agile.api.vo.WikiRelationVO;
import io.choerodon.agile.api.vo.WorkSpaceVO;
import io.choerodon.agile.app.service.AgileWaterfallService;
import io.choerodon.agile.app.service.IWikiRelationService;
import io.choerodon.agile.app.service.WikiRelationService;
import io.choerodon.agile.infra.dto.WikiRelationDTO;
import io.choerodon.agile.infra.feign.operator.KnowledgebaseClientOperator;
import io.choerodon.agile.infra.mapper.WikiRelationMapper;
import io.choerodon.agile.infra.utils.BaseFieldUtil;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/12/03.
 * Email: fuqianghuang01@gmail.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WikiRelationServiceImpl implements WikiRelationService {

    @Autowired
    private WikiRelationMapper wikiRelationMapper;

    @Autowired
    private KnowledgebaseClientOperator knowledgebaseClientOperator;

    @Autowired
    private IWikiRelationService iWikiRelationService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;

    private Boolean checkRepeat(WikiRelationDTO wikiRelationDTO) {
        WikiRelationDTO wikiRelation = new WikiRelationDTO();
        wikiRelation.setProjectId(wikiRelationDTO.getProjectId());
        wikiRelation.setIssueId(wikiRelationDTO.getIssueId());
        wikiRelation.setSpaceId(wikiRelationDTO.getSpaceId());
        WikiRelationDTO res = wikiRelationMapper.selectOne(wikiRelation);
        return res != null;
    }

    @Override
    public void create(Long projectId, List<WikiRelationVO> wikiRelationVOList) {
        List<WikiRelationDTO> wikiRelationDTOList = modelMapper.map(wikiRelationVOList, new TypeToken<List<WikiRelationDTO>>() {
        }.getType());
        if (wikiRelationDTOList != null && !wikiRelationDTOList.isEmpty()) {
            for (WikiRelationDTO wikiRelationDTO : wikiRelationDTOList) {
                if (!checkRepeat(wikiRelationDTO)) {
                    iWikiRelationService.createBase(wikiRelationDTO);
                    BaseFieldUtil.updateIssueLastUpdateInfo(wikiRelationDTO.getIssueId(), wikiRelationDTO.getProjectId());
                }
            }
        }
    }

    @Override
    public KnowledgeRelationVO queryByIssueId(Long projectId, Long issueId) {
        WikiRelationDTO wikiRelationDTO = new WikiRelationDTO();
        wikiRelationDTO.setIssueId(issueId);
        List<WikiRelationDTO> wikiRelationDTOList = wikiRelationMapper.select(wikiRelationDTO);
        List<WikiRelationVO> result = new ArrayList<>();
        if (wikiRelationDTOList != null && !wikiRelationDTOList.isEmpty()) {
            List<Long> spaceIds = wikiRelationDTOList.stream().map(WikiRelationDTO::getSpaceId).collect(Collectors.toList());
            List<WorkSpaceVO> workSpaceVOS = knowledgebaseClientOperator.querySpaceByIds(projectId, spaceIds);
            Map<Long, WorkSpaceVO> workSpaceMap = CollectionUtils.isEmpty(workSpaceVOS) ? new HashMap<>()
                    : new HashMap<>(workSpaceVOS.stream().collect(Collectors.toMap(WorkSpaceVO::getId, Function.identity())));
            for (WikiRelationDTO wikiRelation : wikiRelationDTOList) {
                WorkSpaceVO workSpaceVO = workSpaceMap.get(wikiRelation.getSpaceId());
                if (!ObjectUtils.isEmpty(workSpaceVO)) {
                    WikiRelationVO wikiRelationVO = new WikiRelationVO();
                    BeanUtils.copyProperties(wikiRelation, wikiRelationVO);
                    wikiRelationVO.setWorkSpaceVO(workSpaceVO);
                    result.add(wikiRelationVO);
                }
            }
        }
        return KnowledgeRelationVO.of(result);
    }

    @Override
    public void deleteByWorkSpaceId(Long projectId, Long workSpaceId) {
        WikiRelationDTO wikiRelationDTO = new WikiRelationDTO();
        wikiRelationDTO.setSpaceId(workSpaceId);
        List<WikiRelationDTO> wikiRelationDTOS = wikiRelationMapper.select(wikiRelationDTO);
        if (!CollectionUtils.isEmpty(wikiRelationDTOS)) {
            wikiRelationDTOS.forEach(v -> iWikiRelationService.deleteBase(wikiRelationDTO));
        }
        if (agileWaterfallService != null) {
            agileWaterfallService.deleteByWorkSpaceId(projectId, workSpaceId);
        }
    }

    @Override
    public void deleteById(Long projectId, Long id) {
        WikiRelationDTO wikiRelationDTO = new WikiRelationDTO();
        wikiRelationDTO.setProjectId(projectId);
        wikiRelationDTO.setId(id);
        iWikiRelationService.deleteBase(wikiRelationDTO);
        BaseFieldUtil.updateIssueLastUpdateInfo(wikiRelationDTO.getIssueId(), projectId);
    }

    @Override
    public void copyIssueKnowledgeRelations(Long projectId, Long issueId, Long newIssueId) {
        WikiRelationDTO wikiRelation = new WikiRelationDTO();
        wikiRelation.setProjectId(projectId);
        wikiRelation.setIssueId(issueId);
        List<WikiRelationDTO> list = wikiRelationMapper.select(wikiRelation);
        if (ObjectUtils.isEmpty(list)) {
            return;
        }
        wikiRelation.setIssueId(newIssueId);
        List<WikiRelationDTO> existList = wikiRelationMapper.select(wikiRelation);
        if (!ObjectUtils.isEmpty(existList)) {
            return;
        }
        List<WikiRelationVO> createList = new ArrayList<>();
        list.forEach(v -> {
            WikiRelationVO create = new WikiRelationVO();
            create.setIssueId(newIssueId);
            create.setProjectId(newIssueId);
            create.setSpaceId(v.getSpaceId());
            create.setWikiName(v.getWikiName());
            create.setWikiUrl(v.getWikiUrl());
            createList.add(create);
        });
        create(projectId, createList);
    }
}
