package io.choerodon.agile.app.assembler;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.choerodon.agile.api.vo.IssueLinkVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.PriorityVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.app.service.IssueTypeService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.IssueLinkDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.utils.ConvertUtil;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/14
 */
@Component
public class IssueLinkAssembler extends AbstractAssembler {

    @Autowired
    private UserService userService;
    @Autowired
    private IssueTypeService issueTypeService;

    public List<IssueLinkVO> issueLinkDTOToVO(Long projectId, List<IssueLinkDTO> issueLinkDTOList) {
        List<IssueLinkVO> issueLinkVOList = new ArrayList<>(issueLinkDTOList.size());
        if (!issueLinkDTOList.isEmpty()) {
            Set<Long> projectIds = issueLinkDTOList.stream().map(IssueLinkDTO::getLinkedIssueProjectId).collect(Collectors.toSet());
            projectIds.add(projectId);
            List<IssueTypeVO> issueTypeList = issueTypeService.listIssueType(ConvertUtil.getOrganizationId(projectId), projectIds);
            Map<Long, Map<Long, IssueTypeVO>> issueTypeVOMappedByIdGroupingByIssueTypeExtendProjectId = issueTypeList.stream().filter(v -> v.getIssueTypeExtendProjectId() != null).collect(Collectors.groupingBy(IssueTypeVO::getIssueTypeExtendProjectId, Collectors.toMap(IssueTypeVO::getId, Function.identity())));
            Map<Long, Map<Long, IssueTypeVO>> issueTypeVOMappedByIdGroupingByIssueTypeProjectId = issueTypeList.stream().filter(v -> v.getProjectId() != 0).collect(Collectors.groupingBy(IssueTypeVO::getProjectId, Collectors.toMap(IssueTypeVO::getId, Function.identity())));
            Map<Long, IssueTypeVO> issueTypeVOMappedByIdFilterByZeroProjectId = issueTypeList.stream().filter(issueTypeVO -> issueTypeVO.getProjectId() == 0 && issueTypeVO.getIssueTypeExtendProjectId() == null).collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));


            Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
            Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
            List<Long> assigneeIds = issueLinkDTOList.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueLinkDTO::getAssigneeId).distinct().collect(Collectors.toList());
            Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
            issueLinkDTOList.forEach(issueLinkDO -> {
                String assigneeName = usersMap.get(issueLinkDO.getAssigneeId()) != null ? usersMap.get(issueLinkDO.getAssigneeId()).getName() : null;
                String imageUrl = assigneeName != null ? usersMap.get(issueLinkDO.getAssigneeId()).getImageUrl() : null;
                IssueLinkVO issueLinkVO = new IssueLinkVO();
                BeanUtils.copyProperties(issueLinkDO, issueLinkVO);
                issueLinkVO.setIssueTypeVO(getIssueTypeVO(issueLinkVO.getLinkedIssueProjectId(), issueLinkDO.getIssueTypeId(), issueTypeVOMappedByIdGroupingByIssueTypeExtendProjectId, issueTypeVOMappedByIdGroupingByIssueTypeProjectId, issueTypeVOMappedByIdFilterByZeroProjectId));
                issueLinkVO.setStatusVO(statusMapDTOMap.get(issueLinkDO.getStatusId()));
                issueLinkVO.setPriorityVO(priorityDTOMap.get(issueLinkDO.getPriorityId()));
                issueLinkVO.setAssigneeName(assigneeName);
                issueLinkVO.setImageUrl(imageUrl);
                issueLinkVOList.add(issueLinkVO);
            });
        }
        return issueLinkVOList;
    }

    private IssueTypeVO getIssueTypeVO(Long issueProjectId, Long issueTypeId, Map<Long, Map<Long, IssueTypeVO>> issueTypeVOMappedByIdGroupingByIssueTypeExtendProjectId, Map<Long, Map<Long, IssueTypeVO>> issueTypeVOMappedByIdGroupingByIssueTypeProjectId, Map<Long, IssueTypeVO> issueTypeVOMappedByIdFilterByZeroProjectId) {
        Map<Long, IssueTypeVO> issueTypeVOMap = issueTypeVOMappedByIdGroupingByIssueTypeExtendProjectId.get(issueProjectId);
        if (issueTypeVOMap != null && issueTypeVOMap.get(issueTypeId) != null) {
            return issueTypeVOMap.get(issueTypeId);
        } else {
            issueTypeVOMap = issueTypeVOMappedByIdGroupingByIssueTypeProjectId.get(issueProjectId);
            if (issueTypeVOMap != null && issueTypeVOMap.get(issueTypeId) != null) {
                return issueTypeVOMap.get(issueTypeId);
            } else {
                return issueTypeVOMappedByIdFilterByZeroProjectId.get(issueTypeId);
            }
        }
    }
}
