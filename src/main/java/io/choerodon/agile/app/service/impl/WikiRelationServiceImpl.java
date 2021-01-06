package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.KnowledgeRelationVO;
import io.choerodon.agile.api.vo.WikiRelationVO;
import io.choerodon.agile.api.vo.WorkSpaceVO;
import io.choerodon.agile.app.service.IWikiRelationService;
import io.choerodon.agile.app.service.WikiRelationService;
import io.choerodon.agile.infra.dto.WikiRelationDTO;
import io.choerodon.agile.infra.feign.KnowledgebaseClient;
import io.choerodon.agile.infra.mapper.WikiRelationMapper;
import io.choerodon.agile.infra.utils.BaseFieldUtil;
import io.choerodon.agile.infra.utils.FeignUtil;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/12/03.
 * Email: fuqianghuang01@gmail.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WikiRelationServiceImpl implements WikiRelationService {

    private static final Logger LOGGER = LoggerFactory.getLogger(WikiRelationServiceImpl.class);

    @Autowired
    private WikiRelationMapper wikiRelationMapper;

    @Autowired
    private KnowledgebaseClient knowledgebaseClient;

    @Autowired
    private IWikiRelationService iWikiRelationService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private FeignUtil feignUtil;

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
        List<WikiRelationDTO> wikiRelationDTOList = modelMapper.map(wikiRelationVOList, new TypeToken<List<WikiRelationDTO>>(){}.getType());
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
            Map<Long, WorkSpaceVO> workSpaceMap = new HashMap<>();
            if (feignUtil.isExist(FeignUtil.KNOWLEDGEBASE_SERVICE)) {
                workSpaceMap.putAll(knowledgebaseClient.querySpaceByIds(projectId, spaceIds).getBody().stream().collect(Collectors.toMap(WorkSpaceVO::getId, Function.identity())));
            }
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
        KnowledgeRelationVO knowledgeRelation = new KnowledgeRelationVO();
        knowledgeRelation.setKnowledgeRelationList(result);
        return knowledgeRelation;
    }

    @Override
    public void deleteByWorkSpaceId(Long projectId, Long workSpaceId) {
        WikiRelationDTO wikiRelationDTO = new WikiRelationDTO();
        wikiRelationDTO.setSpaceId(workSpaceId);
        List<WikiRelationDTO> wikiRelationDTOS = wikiRelationMapper.select(wikiRelationDTO);
        if(!CollectionUtils.isEmpty(wikiRelationDTOS)){
            wikiRelationDTOS.forEach(v -> iWikiRelationService.deleteBase(wikiRelationDTO));
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
}
