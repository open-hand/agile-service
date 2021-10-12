package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.IssueParticipantRelService;
import io.choerodon.agile.infra.annotation.DataLog;
import io.choerodon.agile.infra.dto.IssueParticipantRelDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.IssueParticipantRelMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-10-12 15:23
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueParticipantRelServiceImpl implements IssueParticipantRelService {

    @Autowired
    private IssueParticipantRelMapper issueParticipantRelMapper;

    @Autowired
    private IssueMapper issueMapper;

    @Override
    @DataLog(type = "createParticipant")
    public void createParticipantRel(Long issueId, Long projectId, List<Long> participantIds) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        participantIds.forEach(v -> {
            IssueParticipantRelDTO issueParticipantRelDTO = new IssueParticipantRelDTO();
            issueParticipantRelDTO.setIssueId(issueId);
            issueParticipantRelDTO.setIssueTypeId(issueDTO.getIssueTypeId());
            issueParticipantRelDTO.setParticipantId(v);
            issueParticipantRelDTO.setProjectId(projectId);
            issueParticipantRelDTO.setOrganizationId(organizationId);
            baseCreate(issueParticipantRelDTO);
        });
    }

    private void baseCreate(IssueParticipantRelDTO issueParticipantRelDTO) {
        if (issueParticipantRelMapper.insertSelective(issueParticipantRelDTO) != 1) {
            throw new CommonException("error.participant.insert");
        }
    }

    @Override
    @DataLog(type = "updateParticipant")
    public void updateParticipantRel(Long issueId, Long projectId, List<Long> participantIds) {
        // 获取当前的参与人
        IssueParticipantRelDTO issueParticipantRelDTO = new IssueParticipantRelDTO();
        issueParticipantRelDTO.setIssueId(issueId);
        List<IssueParticipantRelDTO> issueParticipantRelDTOS = issueParticipantRelMapper.select(issueParticipantRelDTO);
        List<Long> currentUser = new ArrayList<>();
        if (!CollectionUtils.isEmpty(issueParticipantRelDTOS)) {
            currentUser.addAll(issueParticipantRelDTOS.stream().map(IssueParticipantRelDTO::getParticipantId).collect(Collectors.toList()));
        }
        // 删除无用的参与人
        List<Long> needDelete = currentUser.stream().filter(v -> !participantIds.contains(v)).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(needDelete)) {
            issueParticipantRelMapper.deleteByIssueIdAndParticipantIds(projectId, issueId, participantIds);
        }
        // 新增参与人
        List<Long> needAdd = participantIds.stream().filter(v -> !currentUser.contains(v)).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(needAdd)) {
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
            needAdd.forEach(participantId -> {
                IssueParticipantRelDTO participantRelDTO = new IssueParticipantRelDTO();
                participantRelDTO.setIssueId(issueId);
                participantRelDTO.setOrganizationId(organizationId);
                participantRelDTO.setProjectId(projectId);
                participantRelDTO.setIssueTypeId(issueDTO.getIssueTypeId());
                participantRelDTO.setParticipantId(participantId);
                baseCreate(participantRelDTO);
            });
        }
    }

    @Override
    @DataLog(type = "deleteParticipant")
    public void deleteParticipantRel(Long issueId, Long projectId) {
        issueParticipantRelMapper.batchDeleteByIssueId(issueId);
    }
}
