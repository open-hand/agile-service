package io.choerodon.agile.app.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.yqcloud.core.oauth.ZKnowDetailsHelper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

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

    private static final String AGILE_ISSUE_ID_IS_NULL = "agile.issue.id.is.null";
    private static final String AGILE_WIKI_TYPE_IS_NULL = "agile.wiki.type.is.null";

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
        wikiRelation.setSourceType(wikiRelationDTO.getSourceType());
        wikiRelation.setKnowledgeElementId(wikiRelationDTO.getKnowledgeElementId());
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
                    wikiRelationDTO.setSourceType(wikiRelationDTO.getSourceType() == null ? ZKnowDetailsHelper.VALUE_CHOERODON : wikiRelationDTO.getSourceType());
                    iWikiRelationService.createBase(wikiRelationDTO);
                    BaseFieldUtil.updateIssueLastUpdateInfo(wikiRelationDTO.getIssueId(), wikiRelationDTO.getProjectId());
                }
            }
        }
    }

    @Override
    public List<WikiRelationVO> queryByIssueId(Long projectId, Long issueId) {
        WikiRelationDTO wikiRelationDTO = new WikiRelationDTO();
        wikiRelationDTO.setIssueId(issueId);
        List<WikiRelationDTO> wikiRelationDTOList = wikiRelationMapper.select(wikiRelationDTO);

        List<WikiRelationVO> result = new ArrayList<>();
        if (!CollectionUtils.isEmpty(wikiRelationDTOList)) {
            result.addAll(handlerC7nSpace(projectId, wikiRelationDTOList));
            result.addAll(handlerZknowSpace(projectId, wikiRelationDTOList));
        }

        return result;
    }

    /**
     * 交由子类实现
     *
     * @param projectId
     * @param wikiRelationDTOList
     */
    protected List<WikiRelationVO> handlerZknowSpace(Long projectId, List<WikiRelationDTO> wikiRelationDTOList) {
        return new ArrayList<>();
    }

    private List<WikiRelationVO> handlerC7nSpace(Long projectId, List<WikiRelationDTO> wikiRelationDTOList) {
        List<WikiRelationDTO> c7nWikiRelationDTOList = wikiRelationDTOList
                .stream()
                .filter(w -> ZKnowDetailsHelper.VALUE_CHOERODON.equals(w.getSourceType()))
                .collect(Collectors.toList());
        List<WikiRelationVO> result = new ArrayList<>();
        if (!CollectionUtils.isEmpty(c7nWikiRelationDTOList)) {
            List<Long> c7nSpaceIds = c7nWikiRelationDTOList
                    .stream()
                    .map(WikiRelationDTO::getSpaceId)
                    .collect(Collectors.toList());
            List<WorkSpaceVO> workSpaceVOS = knowledgebaseClientOperator.querySpaceByIds(projectId, c7nSpaceIds);
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
        return result;
    }

    @Override
    public List<WikiRelationDTO> baseQueryByIssueIdAndType(Long issueId, String type) {
        Assert.notNull(issueId, AGILE_ISSUE_ID_IS_NULL);
        Assert.notNull(type, AGILE_WIKI_TYPE_IS_NULL);

        WikiRelationDTO wikiRelationDTO = new WikiRelationDTO();
        wikiRelationDTO.setIssueId(issueId);
        wikiRelationDTO.setSourceType(type);

        return wikiRelationMapper.select(wikiRelationDTO);
    }

    @Override
    public void deleteByWorkSpaceId(Long projectId, Long workSpaceId) {
        WikiRelationDTO wikiRelationDTO = new WikiRelationDTO();
        wikiRelationDTO.setSpaceId(workSpaceId);
        List<WikiRelationDTO> wikiRelationDTOS = wikiRelationMapper.select(wikiRelationDTO);
        if (!CollectionUtils.isEmpty(wikiRelationDTOS)) {
            for (WikiRelationDTO wikiRelation : wikiRelationDTOS) {
                iWikiRelationService.deleteBase(wikiRelation);
            }
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
            createList.add(create);
        });
        create(projectId, createList);
    }
}
