package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.RankVO;
import io.choerodon.agile.api.validator.RankValidator;
import io.choerodon.agile.app.service.AgilePluginService;
import io.choerodon.agile.app.service.RankService;
import io.choerodon.agile.infra.utils.RankUtil;
import io.choerodon.agile.infra.dto.RankDTO;
import io.choerodon.agile.infra.mapper.RankMapper;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.core.exception.CommonException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;


/**
 * Created by HuangFuqiang@choerodon.io on 2019/6/24.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class RankServiceImpl implements RankService {

    private static final String RANK_TYPE_EPIC = "epic";

    @Autowired
    private RankValidator rankValidator;

    @Autowired
    private RankMapper rankMapper;

    @Autowired
    private UserService userService;

    @Autowired(required = false)
    private AgilePluginService agilePluginService;


    private List<Long> getEpicIds(Long projectId) {
        List<Long> epicIds = new ArrayList<>();
        if (agilePluginService != null) {
            agilePluginService.getProgramEpicIds(epicIds, projectId);
        }
        List<Long> projectEpicIds = rankMapper.selectEpicIdsByProject(projectId);
        if (projectEpicIds != null && !projectEpicIds.isEmpty()) {
            epicIds.addAll(projectEpicIds);
        }
        Collections.sort(epicIds, Comparator.reverseOrder());
        return epicIds;
    }

    protected void insertRankByBatch(Long projectId, List<Long> issueIds, String type) {
        List<RankDTO> insertRankList = new ArrayList<>();
        String rank = RankUtil.mid();
        for (Long issueId : issueIds) {
            insertRankList.add(new RankDTO(issueId, rank));
            rank = RankUtil.genNext(rank);
        }
        if (!insertRankList.isEmpty()) {
            rankMapper.batchInsertRank(projectId, type, insertRankList);
        }
    }

    @Override
    public RankDTO getReferenceRank(Long projectId, String type, Long referenceIssueId) {
        RankDTO rankDTO = rankMapper.selectRankByIssueId(projectId, type, referenceIssueId);
        if (rankDTO == null) {
            if (RANK_TYPE_EPIC.equals(type)) {
                List<Long> epicIds = getEpicIds(projectId);
                List<Long> epicRankDOList = rankMapper.checkRankEmpty(projectId, RANK_TYPE_EPIC);
                List<Long> emptyEpicIds = epicIds.stream().filter(epicId -> !epicRankDOList.contains(epicId)).collect(Collectors.toList());
                if (emptyEpicIds != null && !emptyEpicIds.isEmpty()) {
                    insertRankByBatch(projectId, emptyEpicIds, RANK_TYPE_EPIC);
                }
            }
            else {
                if (agilePluginService != null) {
                    agilePluginService.handlerFeatureRank(projectId,type);
                }
            }
            RankDTO newRank = rankMapper.selectRankByIssueId(projectId, type, referenceIssueId);
            if (newRank == null) {
                throw new CommonException("error.rank.get");
            }
            return newRank;
        } else {
            return rankDTO;
        }
    }

    private RankDTO getOrinitReferenceRank(Long projectId, String type, Long referenceIssueId, Long issueId) {
        RankDTO rankReference = rankMapper.selectRankByIssueId(projectId, type, referenceIssueId);
        RankDTO rankCurrent = rankMapper.selectRankByIssueId(projectId, type, issueId);
        if (rankReference == null || rankCurrent == null) {
            if (RANK_TYPE_EPIC.equals(type)) {
                List<Long> epicIds = getEpicIds(projectId);
                List<Long> epicRankDOList = rankMapper.checkRankEmpty(projectId, RANK_TYPE_EPIC);
                List<Long> emptyEpicIds = epicIds.stream().filter(epicId -> !epicRankDOList.contains(epicId)).collect(Collectors.toList());
                if (emptyEpicIds != null && !emptyEpicIds.isEmpty()) {
                    insertRankByBatch(projectId, emptyEpicIds, RANK_TYPE_EPIC);
                }
            }
            else {
                if (agilePluginService != null) {
                    agilePluginService.handlerFeatureRank(projectId,type);
                }
            }
            RankDTO newRank = rankMapper.selectRankByIssueId(projectId, type, referenceIssueId);
            if (newRank == null) {
                throw new CommonException("error.rank.get");
            }
            return newRank;
        } else {
            return rankReference;
        }
    }

    @Override
    public void epicAndFeatureRank(Long projectId, RankVO rankVO) {
        rankValidator.checkEpicAndFeatureRank(rankVO);
        Long referenceIssueId = rankVO.getReferenceIssueId();
        RankDTO referenceRank = getOrinitReferenceRank(projectId, rankVO.getType(), referenceIssueId, rankVO.getIssueId());
        if (Boolean.TRUE.equals(rankVO.getBefore())) {
            String leftRank = rankMapper.selectLeftRank(projectId, rankVO.getType(), referenceRank.getRank());
            String rank = (leftRank == null ? RankUtil.genPre(referenceRank.getRank()) : RankUtil.between(leftRank, referenceRank.getRank()));
            RankDTO rankDTO = rankMapper.selectRankByIssueId(projectId, rankVO.getType(), rankVO.getIssueId());
            Long objectVersionNumber = rankVO.getObjectVersionNumber() == null ? rankDTO.getObjectVersionNumber() : rankVO.getObjectVersionNumber();
            rankMapper.updateByPrimaryKeySelective(new RankDTO(rankDTO.getId(), rank, objectVersionNumber));
        } else {
            String rightRank = rankMapper.selectRightRank(projectId, rankVO.getType(), referenceRank.getRank());
            String rank = (rightRank == null ? RankUtil.genNext(referenceRank.getRank()) : RankUtil.between(referenceRank.getRank(), rightRank));
            RankDTO rankDTO = rankMapper.selectRankByIssueId(projectId, rankVO.getType(), rankVO.getIssueId());
            Long objectVersionNumber = rankVO.getObjectVersionNumber() == null ? rankDTO.getObjectVersionNumber() : rankVO.getObjectVersionNumber();
            rankMapper.updateByPrimaryKeySelective(new RankDTO(rankDTO.getId(), rank, objectVersionNumber));
        }
    }
}
