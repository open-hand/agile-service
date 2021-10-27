package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.WorkHoursMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.hzero.core.base.BaseConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
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
        //
        Map<String, String> order = new HashMap<>();
        order.put("issueNum", "issue_num_convert");
        pageRequest.setSort(PageUtil.sortResetOrder(pageRequest.getSort(), null, order));
        Page<WorkHoursLogVO> page = PageHelper.doPageAndSort(pageRequest,
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
            List<IssueTypeVO> issueTypeVOS = issueTypeMap.get(workHoursLogVO.getIssueTypeId());
            if (!CollectionUtils.isEmpty(issueTypeVOS)) {
                Map<Long, IssueTypeVO> typeVOMap = issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getProjectId, Function.identity()));
                IssueTypeVO issueTypeVO = typeVOMap.get(workHoursLogVO.getProjectId());
                if (ObjectUtils.isEmpty(issueTypeVO)) {
                    issueTypeVO = typeVOMap.get(0L);
                }
                workHoursLogVO.setIssueTypeVO(issueTypeVO);
            }
        }
        return PageUtil.buildPageInfoWithPageInfoList(page, workHoursLogVOS);
    }

    @Override
    public Page<WorkHoursLogVO> pageWorkHoursLogByOrgId(Long organizationId, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO) {
        List<Long> projectIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        handlerProject(organizationId, projectIds, userId, workHoursSearchVO);
        if (CollectionUtils.isEmpty(projectIds)) {
            return new Page<>();
        }
        return pageWorkHoursLogByProjectIds(organizationId, projectIds, pageRequest, workHoursSearchVO);
    }

    @Override
    public Page<WorkHoursCalendarVO> workHoursCalendar(Long organizationId, List<Long> projectIds, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO, Boolean isOrg) {
        checkTimeRange(workHoursSearchVO);
        Page<UserMessageDTO> page = getUserPage(organizationId, projectIds, pageRequest, workHoursSearchVO, isOrg);
        if (CollectionUtils.isEmpty(page.getContent())) {
            return new Page<>();
        }
        List<Long> userIds = page.getContent().stream().map(UserMessageDTO::getId).collect(Collectors.toList());
        workHoursSearchVO.setUserIds(userIds);
        List<WorkHoursLogVO> workHoursLogVOS = workHoursMapper.listByProjectIds(projectIds, workHoursSearchVO);
        Map<Long, List<WorkHoursLogVO>> workHoursGroup = new HashMap<>();
        if (!CollectionUtils.isEmpty(workHoursLogVOS)) {
            workHoursGroup = workHoursLogVOS.stream().collect(Collectors.groupingBy(WorkHoursLogVO::getUserId));
        }
        List<WorkHoursCalendarVO> list = new ArrayList<>();
        Map<Long, UserMessageDTO> userMessageDTOMap = page.getContent().stream().collect(Collectors.toMap(UserMessageDTO::getId, Function.identity()));
        DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATE);
        for (UserMessageDTO userMessageDTO : page.getContent()) {
            Long userId = userMessageDTO.getId();
            WorkHoursCalendarVO workHoursCalendarVO = new WorkHoursCalendarVO();
            workHoursCalendarVO.setUserId(userId);
            workHoursCalendarVO.setUserMessageDTO(userMessageDTOMap.get(userId));
            List<WorkHoursLogVO> workHoursLogS = workHoursGroup.get(userId);
            Map<String, BigDecimal> countMap = new HashMap<>();
            // 按时间分组
            if(!CollectionUtils.isEmpty(workHoursLogS)){
                Map<String, List<WorkHoursLogVO>> dateMap = workHoursLogS.stream().collect(Collectors.groupingBy(v -> df.format(v.getStartDate())));
                BigDecimal allEstimateTime = BigDecimal.ZERO;
                for (Map.Entry<String, List<WorkHoursLogVO>> entry : dateMap.entrySet()) {
                    String key = entry.getKey();
                    List<WorkHoursLogVO> value = entry.getValue();
                    BigDecimal count = value.stream().map(WorkHoursLogVO::getWorkTime).reduce(BigDecimal.ZERO, BigDecimal::add);
                    allEstimateTime = allEstimateTime.add(count);
                    countMap.put(key, count);
                }
                workHoursCalendarVO.setAllEstimateTime(allEstimateTime);
            }
            workHoursCalendarVO.setCountMap(countMap);
            list.add(workHoursCalendarVO);
        }
        return PageUtil.buildPageInfoWithPageInfoList(page, list);
    }

    private Page<UserMessageDTO> getUserPage(Long organizationId,
                                             List<Long> projectIds,
                                             PageRequest pageRequest,
                                             WorkHoursSearchVO workHoursSearchVO,
                                             Boolean isOrg) {
        Page<UserMessageDTO> page = new Page<>();
        if(!CollectionUtils.isEmpty(workHoursSearchVO.getUserIds())){
            List<UserMessageDTO> userMessageDTOMap = userService.queryUsers(workHoursSearchVO.getUserIds(), true);
            if(CollectionUtils.isEmpty(userMessageDTOMap)){
                userMessageDTOMap = new ArrayList<>();
            }
            // 手动分页
            page = buildPage(pageRequest, userMessageDTOMap);
        } else {
            if (Boolean.TRUE.equals(isOrg)) {
                page = userService.queryUserByOrganizationId(organizationId, pageRequest.getPage(), pageRequest.getSize(), true);
            } else {
                page = userService.queryUserByProjectId(projectIds.get(0), pageRequest.getPage(), pageRequest.getSize(), true);
            }
        }
        return page;
    }

    private Page<UserMessageDTO> buildPage(PageRequest pageRequest, List<UserMessageDTO> userMessageDTOS) {
        Page<UserMessageDTO> page = new Page<>();
        page.setTotalPages(userMessageDTOS.size());
        page.setSize(pageRequest.getSize());
        page.setNumber(pageRequest.getPage());
        List<UserMessageDTO> list = new ArrayList<>();
        if (!CollectionUtils.isEmpty(userMessageDTOS)) {
            int total = userMessageDTOS.size();
            int size = pageRequest.getSize();
            int fromIndex = pageRequest.getPage() * size;
            int totalPage = (int) Math.ceil(total / (size * 1.0));
            page.setTotalPages(totalPage);
            page.setNumberOfElements(total);
            if (fromIndex < total) {
                int toIndex = (pageRequest.getPage() + 1) * size;
                list = userMessageDTOS.subList(fromIndex, (toIndex > total) ? total : toIndex);
            }
        }
        page.setContent(list);
        return page;
    }

    @Override
    public Page<WorkHoursCalendarVO> workHoursCalendarByOrg(Long organizationId, PageRequest pageRequest, WorkHoursSearchVO workHoursSearchVO) {
        List<Long> projectIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        handlerProject(organizationId, projectIds, userId, workHoursSearchVO);
        if (CollectionUtils.isEmpty(projectIds)) {
            return new Page<>();
        }
        return workHoursCalendar(organizationId, projectIds, pageRequest, workHoursSearchVO, true);
    }

    @Override
    public Map<String, List<WorkHoursLogVO>> workHoursCalendarInfoByUserId(Long organizationId, List<Long> projectIds, Long userId, WorkHoursSearchVO workHoursSearchVO) {
        checkTimeRange(workHoursSearchVO);
        List<Long> userIds = new ArrayList<>();
        userIds.add(userId);
        workHoursSearchVO.setUserIds(userIds);
        List<WorkHoursLogVO> workHoursLogVOS = workHoursMapper.listByProjectIds(projectIds, workHoursSearchVO);
        if (CollectionUtils.isEmpty(workHoursLogVOS)) {
            return new HashMap<>();
        }
        DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATE);
        return workHoursLogVOS.stream().collect(Collectors.groupingBy(v -> df.format(v.getStartDate())));
    }

    @Override
    public Map<String, List<WorkHoursLogVO>> workHoursCalendarOrgInfoByUserId(Long organizationId, Long userId, WorkHoursSearchVO workHoursSearchVO) {
        List<Long> projectIds = new ArrayList<>();
        handlerProject(organizationId, projectIds, userId, workHoursSearchVO);
        if (CollectionUtils.isEmpty(projectIds)) {
            return new HashMap<>();
        }
        return workHoursCalendarInfoByUserId(organizationId, projectIds, userId, workHoursSearchVO);
    }

    @Override
    public Page<IssueVO> queryIssue(Long projectId, PageRequest pageRequest, String params) {
        Page<IssueVO> page = PageHelper.doPageAndSort(pageRequest,
                                                        () -> workHoursMapper.queryIssue(projectId, params));
        if(CollectionUtils.isEmpty(page.getContent())){
            return new Page<>();
        }
        List<IssueVO> content = page.getContent();
        Map<Long, IssueTypeVO> issueTypeMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.AGILE);
        for (IssueVO issueVO : content) {
            issueVO.setIssueTypeVO(issueTypeMap.get(issueVO.getIssueTypeId()));
        }
        return PageUtil.buildPageInfoWithPageInfoList(page, content);
    }

    private void handlerProject(Long organizationId, List<Long> projectIds, Long userId, WorkHoursSearchVO workHoursSearchVO){
        if (CollectionUtils.isEmpty(workHoursSearchVO.getProjectIds())) {
            // 查询有权限的项目
            Page<ProjectVO> page = baseFeignClient.pagingProjectsByUserId(organizationId, userId, 0, 0, true, "N_AGILE").getBody();
            if (!CollectionUtils.isEmpty(page.getContent())) {
                projectIds.addAll(page.getContent().stream().map(ProjectVO::getId).collect(Collectors.toList()));
            }
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
        return diff / (1000 * 60 * 60 * 24);
    }
}
