package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.WorkHoursMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.DateUtil;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.lang3.time.DateUtils;
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
    @Autowired
    private DateUtil dateUtil;

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
        List<WorkHoursLogVO> workHoursLogVOS = workHoursMapper.listGroupDataByProjectIds(projectIds, workHoursSearchVO);
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
            BigDecimal allEstimateTime = BigDecimal.ZERO;
            if(!CollectionUtils.isEmpty(workHoursLogS)){
                Map<String, List<WorkHoursLogVO>> dateMap = workHoursLogS.stream().collect(Collectors.groupingBy(v -> df.format(v.getStartDate())));
                for (Map.Entry<String, List<WorkHoursLogVO>> entry : dateMap.entrySet()) {
                    String key = entry.getKey();
                    List<WorkHoursLogVO> value = entry.getValue();
                    BigDecimal count = value.stream().map(WorkHoursLogVO::getWorkTime).reduce(BigDecimal.ZERO, BigDecimal::add);
                    allEstimateTime = allEstimateTime.add(count);
                    countMap.put(key, count);
                }
            }
            workHoursCalendarVO.setAllEstimateTime(allEstimateTime);
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
        Page<UserMessageDTO> page = null;
        if (!CollectionUtils.isEmpty(workHoursSearchVO.getUserIds())){
            page = queryUserAndBuildPage(workHoursSearchVO.getUserIds(), pageRequest);
        } else {
            if (Boolean.TRUE.equals(isOrg)) {
                Set<Long> userIds = workHoursMapper.selectUserIds(projectIds, workHoursSearchVO);
                page = queryUserAndBuildPage(new ArrayList<>(userIds), pageRequest);
            } else {
                page = userService.queryUserByProjectId(projectIds.get(0), pageRequest.getPage(), pageRequest.getSize(), true);
            }
        }
        return page;
    }

    private Page<UserMessageDTO> queryUserAndBuildPage(List<Long> userIds, PageRequest pageRequest){
        if (CollectionUtils.isEmpty(userIds)) {
            return new Page<>();
        }
        List<UserMessageDTO> userMessageDTOS = userService.queryUsers(userIds, true);
        return buildPage(pageRequest, userMessageDTOS);
    }

    private Page<UserMessageDTO> buildPage(PageRequest pageRequest, List<UserMessageDTO> userMessageDTOS) {
        Page<UserMessageDTO> page = new Page<>();
        page.setSize(pageRequest.getSize());
        page.setNumber(pageRequest.getPage());
        page.setNumberOfElements(userMessageDTOS.size());
        page.setTotalElements(userMessageDTOS.size());
        List<UserMessageDTO> list = new ArrayList<>();
        if (!CollectionUtils.isEmpty(userMessageDTOS)) {
            int total = userMessageDTOS.size();
            int size = pageRequest.getSize();
            int fromIndex = pageRequest.getPage() * size;
            int totalPage = (int) Math.ceil(total / (size * 1.0));
            page.setTotalPages(totalPage);
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
        List<WorkHoursLogVO> workHoursLogVOS = workHoursMapper.listGroupDataByProjectIds(projectIds, workHoursSearchVO);
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

    @Override
    public Map<String, BigDecimal> countWorkHours(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO) {
        checkTimeRange(workHoursSearchVO);
        List<WorkHoursLogVO> workHoursLogVOS = workHoursMapper.listGroupDataByProjectIds(projectIds, workHoursSearchVO);
        if (CollectionUtils.isEmpty(workHoursLogVOS)) {
            return new HashMap<>();
        }
        DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATE);
        Map<String, List<WorkHoursLogVO>> map = workHoursLogVOS.stream().collect(Collectors.groupingBy(v -> df.format(v.getStartDate())));
        Map<String, BigDecimal> countMap = new HashMap<>();
        for (Map.Entry<String, List<WorkHoursLogVO>> entry : map.entrySet()) {
            String key = entry.getKey();
            List<WorkHoursLogVO> value = entry.getValue();
            countMap.put(key, value.stream().map(WorkHoursLogVO::getWorkTime).reduce(BigDecimal.ZERO, BigDecimal::add));
        }
        return countMap;
    }

    @Override
    public Map<String, BigDecimal> countWorkHoursOnOrganizationLevel(Long organizationId, WorkHoursSearchVO workHoursSearchVO) {
        List<Long> projectIds = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        handlerProject(organizationId, projectIds, userId, workHoursSearchVO);
        return countWorkHours(organizationId, projectIds, workHoursSearchVO);
    }

    @Override
    public Map<Long, WorkHoursCountVO> countWorkHoursCalendar(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO) {
        Map<Long, WorkHoursCountVO> resultMap = new HashMap<>();
        List<WorkHoursLogVO> workHoursLogVOS = workHoursMapper.countUserWorkTime(projectIds, workHoursSearchVO);
        if (CollectionUtils.isEmpty(workHoursLogVOS)) {
            return resultMap;
        }
        Map<Long, List<WorkHoursLogVO>> workHoursGroup = workHoursLogVOS.stream().collect(Collectors.groupingBy(WorkHoursLogVO::getUserId));
        // 统计人数
        List<Long> userIds = workHoursLogVOS.stream().map(WorkHoursLogVO::getUserId).distinct().collect(Collectors.toList());
        // 根据开始时间和结束时间 计算需要经历的时间，排除节假日日期
        Set<Date> allDate = dateUtil.getWorkDays(organizationId, workHoursSearchVO.getStartTime(), workHoursSearchVO.getEndTime());
        DateFormat df = new SimpleDateFormat(BaseConstants.Pattern.DATE);
        List<String> dataStrings = allDate.stream().map(df::format).collect(Collectors.toList());
        Long days = diffTime(workHoursSearchVO.getStartTime(), DateUtils.addDays(workHoursSearchVO.getEndTime(), 1));
        for (Long id : userIds) {
           handleCountData(days, id, dataStrings, resultMap, workHoursGroup, workHoursSearchVO, df);
        }
        return resultMap;
    }

    private void handleCountData(Long days,
                                 Long id,
                                 List<String> dataStrings,
                                 Map<Long, WorkHoursCountVO> resultMap,
                                 Map<Long, List<WorkHoursLogVO>> workHoursGroup,
                                 WorkHoursSearchVO workHoursSearchVO,
                                 DateFormat df) {
        WorkHoursCountVO workHoursCountVO = new WorkHoursCountVO(0, 0);
        List<WorkHoursLogVO> workHoursLogVOList = workHoursGroup.getOrDefault(id, new ArrayList<>());
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(workHoursSearchVO.getStartTime());
        boolean isUnsaturated = false;
        int unsaturatedTimes = 0;
        if (!CollectionUtils.isEmpty(workHoursLogVOList)) {
            Map<String, BigDecimal> workTimeMap = workHoursLogVOList.stream().collect(Collectors.toMap(v -> df.format(v.getStartDate()), WorkHoursLogVO::getWorkTime));
            for (int i = 0; i < days; i++) {
                String dateString = df.format(calendar.getTime());
                calendar.add(Calendar.DATE, 1);
                if (dataStrings.contains(dateString)) {
                    continue;
                }
                BigDecimal bigDecimal = workTimeMap.getOrDefault(dateString, BigDecimal.ZERO);
                if (bigDecimal.intValue() < 8) {
                    unsaturatedTimes += 1;
                    if (Boolean.FALSE.equals(isUnsaturated)) {
                        isUnsaturated = true;
                    }
                }
            }
        }
        workHoursCountVO.setUnsaturatedTimes(unsaturatedTimes);
        workHoursCountVO.setUnsaturatedUserCount(isUnsaturated ? 1 : 0);
        resultMap.put(id, workHoursCountVO);
    }

    private void handlerProject(Long organizationId, List<Long> projectIds, Long userId, WorkHoursSearchVO workHoursSearchVO){
        if (CollectionUtils.isEmpty(workHoursSearchVO.getProjectIds())) {
            // 查询有权限的项目
            ProjectSearchVO projectSearchVO = new ProjectSearchVO();
            projectSearchVO.setEnable(true);
            projectSearchVO.setCategoryCodes(Arrays.asList("N_AGILE"));
            Page<ProjectVO> page = baseFeignClient.listWithCategoryByOrganizationIds(organizationId, projectSearchVO, 0, 0).getBody();
            if (!CollectionUtils.isEmpty(page.getContent())) {
                projectIds.addAll(page.getContent().stream().map(ProjectVO::getId).collect(Collectors.toList()));
            }
        } else {
            projectIds.addAll(workHoursSearchVO.getProjectIds());
        }
    }

    private void checkTimeRange(WorkHoursSearchVO workHoursSearchVO) {
        if (ObjectUtils.isEmpty(workHoursSearchVO.getStartTime()) || ObjectUtils.isEmpty(workHoursSearchVO.getEndTime())) {
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
