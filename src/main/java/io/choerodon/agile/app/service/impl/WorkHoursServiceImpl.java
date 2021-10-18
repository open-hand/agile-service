package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.WorkHoursMapper;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-10-15 13:35
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WorkHoursServiceImpl implements WorkHoursService {

    @Autowired
    private StatusService statusService;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private UserService userService;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private IssueService issueService;
    @Autowired
    private WorkHoursMapper workHoursMapper;

    @Override
    public Page<WorkHoursLogVO> pageWorkHoursLogByProjectIds(Long organizationId,
                                                             List<Long> projectIds,
                                                             PageRequest pageRequest,
                                                             WorkHoursSearchVO workHoursSearchVO) {
        // 校验时间范围
        checkTimeRange(workHoursSearchVO);
        Page<WorkHoursLogVO> page = PageHelper.doPage(pageRequest,
                () -> workHoursMapper.listByProjectIds(projectIds, workHoursSearchVO));
        List<WorkHoursLogVO> workHoursLogVOS = page.getContent();
        if (CollectionUtils.isEmpty(workHoursLogVOS)) {
            return new Page<>();
        }
        List<ProjectVO> projectVOS = baseFeignClient.queryByIds(new HashSet<>(projectIds)).getBody();
        Map<Long, ProjectVO> projectMap = projectVOS.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        // 获取创建人
        List<Long> userIds = workHoursLogVOS.stream().map(WorkHoursLogVO::getUserId).collect(Collectors.toList());
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(userIds, true);
        // 获取状态
        Map<Long, StatusVO> statusVOMap = statusService.queryAllStatusMap(organizationId);
        // 获取问题类型
        Map<Long, List<IssueTypeVO>> issueTypeMap = issueTypeService.listIssueTypeMapByProjectIds(organizationId, projectIds);
        for (WorkHoursLogVO workHoursLogVO : workHoursLogVOS) {
            workHoursLogVO.setUser(usersMap.get(workHoursLogVO.getUserId()));
            workHoursLogVO.setStatusVO(statusVOMap.get(workHoursLogVO.getStatusId()));
            workHoursLogVO.setProjectVO(projectMap.get(workHoursLogVO.getProjectId()));
            List<IssueTypeVO> issueTypeVOS = issueTypeMap.get(workHoursLogVO.getProjectId());
            if (!CollectionUtils.isEmpty(issueTypeVOS)) {
                Map<Long, IssueTypeVO> typeVOMap = issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getId, Function.identity()));
                workHoursLogVO.setIssueTypeVO(typeVOMap.get(workHoursLogVO.getIssueTypeId()));
            }
        }
        return PageUtil.buildPageInfoWithPageInfoList(page, workHoursLogVOS);
    }

    @Override
    public Page<WorkHoursLogVO> pageWorkHoursLogByOrgId(Long organizationId, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO) {
        List<Long> projectIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        handlerProject(organizationId, projectIds, userId, workHoursSearchVO);
        return pageWorkHoursLogByProjectIds(organizationId, projectIds, pageRequest, workHoursSearchVO);
    }

    private void handlerProject(Long organizationId, List<Long> projectIds, Long userId, WorkHoursSearchVO workHoursSearchVO){
        if (CollectionUtils.isEmpty(workHoursSearchVO.getProjectIds())) {
            // 查询有权限的项目
            List<ProjectVO> projects = new ArrayList<>();
            issueService.queryUserProjects(organizationId, null, projectIds, projects, userId, null);
        } else {
            projectIds.addAll(workHoursSearchVO.getProjectIds());
        }
    }

    private void checkTimeRange(WorkHoursSearchVO workHoursSearchVO) {
        if (ObjectUtils.isEmpty(workHoursSearchVO.getStartTime()) || ObjectUtils.isEmpty(workHoursSearchVO.getStartTime())) {
            throw new CommonException("error.search.time.not.null");
        }
        if (workHoursSearchVO.getStartTime().after(workHoursSearchVO.getEndTime())) {
            throw new CommonException("error.search.time.illegal");
        }
        if (diffTime(workHoursSearchVO.getStartTime(), workHoursSearchVO.getEndTime()) > 31) {
            throw new CommonException("error.search.time.illegal");
        }
    }

    private Long diffTime(Date startTime, Date endTime) {
        long diff = endTime.getTime() - startTime.getTime();//这样得到的差值是毫秒级别
        long days = diff / (1000 * 60 * 60 * 24);
        return days;
    }
}
