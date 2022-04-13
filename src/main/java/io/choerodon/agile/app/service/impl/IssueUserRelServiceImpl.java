package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.IssueUserRelService;
import io.choerodon.agile.infra.dto.IssueUserRelDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.IssueUserRelMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/12
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueUserRelServiceImpl implements IssueUserRelService {

    private static final String RELATED_PARTY = "relatedParty";

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private IssueUserRelMapper issueUserRelMapper;

    @Override
    public void createUserRel(Long projectId, Long issueId, List<Long> userIds, String userType) {
        if (!RELATED_PARTY.equals(userType)) {
            throw new CommonException("error.issue.user.rel.type.illegal");
        }
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (Objects.isNull(issueDTO)) {
            throw new CommonException("error.issue.not.exist");
        }
        if (ObjectUtils.isEmpty(userIds)) {
            return;
        }
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        userIds.forEach(v -> {
            IssueUserRelDTO issueUserRelDTO = new IssueUserRelDTO();
            issueUserRelDTO.setIssueId(issueId);
            issueUserRelDTO.setUserType(userType);
            issueUserRelDTO.setUserId(v);
            issueUserRelDTO.setProjectId(projectId);
            issueUserRelDTO.setOrganizationId(organizationId);
            baseCreate(issueUserRelDTO);
        });
    }

    private void baseCreate(IssueUserRelDTO issueUserRelDTO) {
        if (issueUserRelMapper.insertSelective(issueUserRelDTO) != 1) {
            throw new CommonException("error.issue.user.rel.insert");
        }
    }

    @Override
    public void updateUserRel(Long projectId, Long issueId, List<Long> userIds, String userType) {
        // 清空人员关联
        if (ObjectUtils.isEmpty(userIds)) {
            deleteUserRel(projectId, issueId, userType);
            return;
        }
        List<Long> userIdList = userIds.stream().filter(Objects::nonNull).distinct().collect(Collectors.toList());
        // 获取当前类型的人员关联
        IssueUserRelDTO issueUserRelDTO = new IssueUserRelDTO();
        issueUserRelDTO.setIssueId(issueId);
        issueUserRelDTO.setUserType(userType);
        List<IssueUserRelDTO> issueUserRelDTOS = issueUserRelMapper.select(issueUserRelDTO);
        List<Long> currentUser = new ArrayList<>();
        if (!ObjectUtils.isEmpty(issueUserRelDTOS)) {
            currentUser.addAll(issueUserRelDTOS.stream().map(IssueUserRelDTO::getUserId).collect(Collectors.toList()));
        }
        // 删除无用的人员关联
        List<Long> needDelete = currentUser.stream().filter(v -> !userIdList.contains(v)).collect(Collectors.toList());
        if (!ObjectUtils.isEmpty(needDelete)) {
            issueUserRelMapper.deleteByIssueIdAndUserIdsWithType(projectId, issueId, needDelete, userType);
        }
        // 新增人员关联
        List<Long> needAdd = userIdList.stream().filter(v -> !currentUser.contains(v)).collect(Collectors.toList());
        if (!ObjectUtils.isEmpty(needAdd)) {
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            needAdd.forEach(userId -> {
                IssueUserRelDTO userRelDTO = new IssueUserRelDTO();
                issueUserRelDTO.setIssueId(issueId);
                issueUserRelDTO.setUserType(userType);
                issueUserRelDTO.setUserId(userId);
                issueUserRelDTO.setProjectId(projectId);
                issueUserRelDTO.setOrganizationId(organizationId);
                baseCreate(userRelDTO);
            });
        }
    }

    public void deleteUserRel(Long projectId, Long issueId, String userType) {
        IssueUserRelDTO delete = new IssueUserRelDTO();
        delete.setIssueId(issueId);
        delete.setProjectId(projectId);
        delete.setUserType(userType);
        issueUserRelMapper.delete(delete);
    }
}
