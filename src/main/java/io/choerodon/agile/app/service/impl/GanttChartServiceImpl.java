package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.IssueTypeVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.app.assembler.BoardAssembler;
import io.choerodon.agile.app.service.GanttChartService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.IssueTypeService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author superlee
 * @since 2020-11-24
 */
@Service
public class GanttChartServiceImpl implements GanttChartService {

    @Autowired
    private IssueService issueService;
    @Autowired
    private BoardAssembler boardAssembler;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private UserService userService;

    @Override
    public List<GanttChartVO> list(Long projectId, SearchVO searchVO) {
        Boolean condition = issueService.handleSearchUser(searchVO, projectId);
        if (condition) {
            String filterSql;
            List<Long> quickFilterIds = searchVO.getQuickFilterIds();
            if (!ObjectUtils.isEmpty(quickFilterIds)) {
                filterSql = issueService.getQuickFilter(quickFilterIds);
            } else {
                filterSql = null;
            }
            boardAssembler.handleOtherArgs(searchVO);
//            Map<String, String> order = new HashMap<>(1);
//            order.put("issueId", "issue_issue_id");
//            String orderStr = PageableHelper.getSortSql(PageUtil.sortResetOrder(sort, null, order));
            List<IssueDTO> issues = issueMapper.queryIssueIdsListWithSub(projectId, searchVO, filterSql, searchVO.getAssigneeFilterIds(), null);
            List<Long> issueIds = issues.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
            if (!ObjectUtils.isEmpty(issueIds)) {
                Set<Long> childrenIds = issueMapper.queryChildrenIdByParentId(issueIds, projectId, searchVO, filterSql, searchVO.getAssigneeFilterIds());
                List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(issueIds, childrenIds, false);
                List<GanttChartVO> result = buildFromIssueDto(issueDTOList, projectId);
                return result;
            } else {
                return new ArrayList<>();
            }
        } else {
            return new ArrayList<>();
        }
    }

    private List<GanttChartVO> buildFromIssueDto(List<IssueDTO> issueList, Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId);
        Set<Long> userIds = new HashSet<>();
        for (IssueDTO dto : issueList) {
            if (!ObjectUtils.isEmpty(dto.getReporterId()) && !Objects.equals(0L, dto.getReporterId())) {
                userIds.add(dto.getReporterId());
            }
            if (!ObjectUtils.isEmpty(dto.getAssigneeId()) && !Objects.equals(0L, dto.getAssigneeId())) {
                userIds.add(dto.getAssigneeId());
            }
        }
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(new ArrayList<>(userIds), true);
        List<GanttChartVO> result = new ArrayList<>(issueList.size());
        issueList.forEach(i -> {
            GanttChartVO ganttChart = new GanttChartVO();
            result.add(ganttChart);
            BeanUtils.copyProperties(i, ganttChart);
            ganttChart.setIssueTypeVO(issueTypeDTOMap.get(i.getIssueTypeId()));
            Long assigneeId = i.getAssigneeId();
            if (!ObjectUtils.isEmpty(assigneeId)) {
                UserMessageDTO assignee = usersMap.get(assigneeId);
                if (!ObjectUtils.isEmpty(assignee)) {
                    ganttChart.setAssignee(assignee);
                }
            }
            setParentId(ganttChart, i);
        });
        return result;
    }

    private void setParentId(GanttChartVO ganttChartVO, IssueDTO dto) {
        Long relateIssueId = dto.getRelateIssueId();
        Long parentIssueId = dto.getParentIssueId();
        if (!ObjectUtils.isEmpty(relateIssueId) && !Objects.equals(0L, relateIssueId)) {
            ganttChartVO.setParentId(relateIssueId);
            return;
        }
        if (!ObjectUtils.isEmpty(parentIssueId) && !Objects.equals(0L, parentIssueId)) {
            ganttChartVO.setParentId(parentIssueId);
        }
    }
}
