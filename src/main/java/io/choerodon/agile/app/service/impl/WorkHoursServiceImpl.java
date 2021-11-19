package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.business.TagVO;
import io.choerodon.agile.app.assembler.IssueAssembler;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
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

    protected static final String USER_MAP = "userMap";
    protected static final String ISSUE_TYPE_MAP = "issueTypeMap";
    protected static final String STATUS_MAP = "statusMap";
    protected static final String PRIORITY_MAP = "priorityMap";
    protected static final String CLOSE_SPRINT_MAP = "closeSprintMap";
    protected static final String FIX_VERSION_MAP = "fixVersionMap";
    protected static final String INFLUENCE_VERSION_MAP = "influenceVersionMap";
    protected static final String LABEL_MAP = "labelMap";
    protected static final String COMPONENT_MAP = "componentMap";
    protected static final String FOUNDATION_CODE_VALUE_MAP = "foundationCodeValueMap";
    protected static final String ENV_MAP = "envMap";
    protected static final String WORK_LOG_MAP = "workLogMap";
    protected static final String RELATED_ISSUE_MAP = "relatedIssueMap";

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
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private PageFieldService pageFieldService;
    @Autowired
    private IssueAssembler issueAssembler;
    @Autowired
    private WorkLogMapper workLogMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private WorkGroupUserRelMapper workGroupUserRelMapper;
    @Autowired
    private LookupValueService lookupValueService;
    @Autowired
    private IssueLinkMapper issueLinkMapper;
    @Autowired
    private WorkGroupService workGroupService;

    @Override
    public Page<WorkHoursLogVO> pageWorkHoursLogByProjectIds(Long organizationId,
                                                             List<Long> projectIds,
                                                             PageRequest pageRequest,
                                                             WorkHoursSearchVO workHoursSearchVO) {
        // 校验时间范围
        checkTimeRange(workHoursSearchVO);
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

    private void getUserIdsByWorkGroupIds(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO) {
        if (!CollectionUtils.isEmpty(workHoursSearchVO.getWorkGroupIds()) && CollectionUtils.isEmpty(workHoursSearchVO.getUserIds())) {
            List<Long> workGroupIds = workGroupService.listChildrenWorkGroup(organizationId, workHoursSearchVO.getWorkGroupIds());
            Set<Long> userIds = new HashSet<>();
            if (!CollectionUtils.isEmpty(workGroupIds)) {
                userIds.addAll(workGroupUserRelMapper.listUserIdsByWorkGroupIds(organizationId, workGroupIds));
            }
            if (!CollectionUtils.isEmpty(projectIds) && workHoursSearchVO.getWorkGroupIds().contains(0L)) {
                // 查询登记过工时但未分配工作组的人员
                Set<Long> noGroupUserIds = workGroupUserRelMapper.selectNoGroupUsers(organizationId, projectIds, workHoursSearchVO.getStartTime(), workHoursSearchVO.getEndTime());
                List<UserDTO> userDTOS = baseFeignClient.listUsersByIds(noGroupUserIds.toArray(new Long[1]), true).getBody();
                if (!CollectionUtils.isEmpty(userDTOS)) {
                    userIds.addAll(userDTOS.stream().map(UserDTO::getId).collect(Collectors.toList()));
                }
            }
            if (CollectionUtils.isEmpty(userIds)) {
                userIds.add(0L);
            }
            workHoursSearchVO.setUserIds(new ArrayList<>(userIds));
        }
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
        getUserIdsByWorkGroupIds(organizationId, projectIds, workHoursSearchVO);
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
            if (Boolean.TRUE.equals(isOrg)) {
                Set<Long> userIds = workHoursMapper.selectUserIds(projectIds, workHoursSearchVO);
                workHoursSearchVO.getUserIds().removeIf(v -> !userIds.contains(v));
            }
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
        getUserIdsByWorkGroupIds(organizationId, projectIds, workHoursSearchVO);
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

    @Override
    public Page<IssueListFieldKVVO> pageQueryIssues(Long organizationId, List<Long> projectIds, PageRequest pageRequest, Boolean containsSubIssue, SearchVO searchVO) {
        //处理用户搜索
        Map<String, Object> sortMap = issueService.processSortMap(pageRequest, projectIds.get(0), organizationId);
        String filterSql = null;
        //处理自定义搜索
        if (!CollectionUtils.isEmpty(searchVO.getQuickFilterIds())) {
            filterSql = issueService.getQuickFilter(searchVO.getQuickFilterIds());
        }
        issueService.splitIssueNumProjectCodePrefix(searchVO, new HashSet<>(projectIds));
        String finalFilterSql = filterSql;
        Page<IssueDTO> page = PageHelper.doPage(pageRequest, () -> workHoursMapper.queryParentIssueIdsList(new HashSet<>(projectIds), searchVO, finalFilterSql, searchVO.getAssigneeFilterIds(), sortMap));
        List<IssueDTO> content = page.getContent();
        if(CollectionUtils.isEmpty(content)){
            return new Page<>();
        }
        Set<Long> projects = content.stream().map(IssueDTO::getProjectId).collect(Collectors.toSet());
        List<ProjectVO> projectVOS = baseFeignClient.queryByIds(projects).getBody();
        Map<Long, ProjectVO> projectMap = projectVOS.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        List<Long> parentIds = content.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        List<IssueDTO> childIssues = issueMapper.queryChildrenIdByParentId(parentIds, new HashSet<>(projectIds), searchVO, finalFilterSql, searchVO.getAssigneeFilterIds(), null);
        Set<Long> childrenIds  = childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toSet());
        List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(parentIds, childrenIds, false, true);
        Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
        Map<Long, List<IssueTypeVO>> issueTypeDTOMap = issueTypeService.listIssueTypeMapByProjectIds(organizationId, projectIds);
        Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
        List<Long> allIssueIds = issueDTOList.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, projectIds, allIssueIds, false);
        Map<Long, List<WorkLogVO>> workLogVOMap = workLogMapper.queryByIssueIds(projectIds , allIssueIds).stream().collect(Collectors.groupingBy(WorkLogVO::getIssueId));
        List<IssueListFieldKVVO> issueListFieldKVVOS = issueAssembler.issueDoToIssueListFieldKVDTO(issueDTOList, priorityMap, statusMapDTOMap, null, foundationCodeValue, workLogVOMap);
        if (!ObjectUtils.isEmpty(agilePluginService) && !CollectionUtils.isEmpty(issueListFieldKVVOS)) {
            agilePluginService.doToIssueListFieldKVDTO(projectIds ,issueListFieldKVVOS);
        }
        // 查询登记工时、累计登记工时以及计算偏差率
        List<WorkLogDTO> workTimes = workHoursMapper.countWorkTime(projectIds, allIssueIds, searchVO);
        Map<Long, BigDecimal> workTimeMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(workTimes)){
            workTimeMap = workTimes.stream().collect(Collectors.toMap(WorkLogDTO::getIssueId, WorkLogDTO::getWorkTime));
        }
        List<WorkLogDTO> allWorkTimes = workHoursMapper.countWorkTime(projectIds, allIssueIds, null);
        Map<Long, BigDecimal> allWorkTimeMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(allWorkTimes)){
            allWorkTimeMap = allWorkTimes.stream().collect(Collectors.toMap(WorkLogDTO::getIssueId, WorkLogDTO::getWorkTime));
        }
        Map<Long, List<Long>> issueMap = issueListFieldKVVOS.stream()
                .filter(v -> !ObjectUtils.isEmpty(v.getParentId()))
                .collect(Collectors.groupingBy(IssueListFieldKVVO::getParentId, Collectors.mapping(IssueListFieldKVVO::getIssueId, Collectors.toList())));
        Map<Long, BigDecimal> estimateTimeMap = issueListFieldKVVOS.stream()
                .filter(v -> !ObjectUtils.isEmpty(v.getEstimateTime()))
                .collect(Collectors.toMap(IssueListFieldKVVO::getIssueId, IssueListFieldKVVO::getEstimateTime));
        for (IssueListFieldKVVO issueListFieldKVVO : issueListFieldKVVOS) {
            List<IssueTypeVO> issueTypeVOS = issueTypeDTOMap.get(issueListFieldKVVO.getIssueTypeId());
            if (!CollectionUtils.isEmpty(issueTypeVOS)) {
                Map<Long, IssueTypeVO> typeVOMap = issueTypeVOS.stream().collect(Collectors.toMap(IssueTypeVO::getProjectId, Function.identity()));
                IssueTypeVO issueTypeVO = typeVOMap.get(issueListFieldKVVO.getProjectId());
                if (ObjectUtils.isEmpty(issueTypeVO)) {
                    issueTypeVO = typeVOMap.get(0L);
                }
                issueListFieldKVVO.setIssueTypeVO(issueTypeVO);
            }
            issueListFieldKVVO.setProjectVO(projectMap.get(issueListFieldKVVO.getProjectId()));
            // 计算工时
            statisticalWorkHours(issueListFieldKVVO, issueMap, estimateTimeMap, allWorkTimeMap, workTimeMap, containsSubIssue);
        }
        return PageUtil.buildPageInfoWithPageInfoList(page,issueListFieldKVVOS);
    }

    @Override
    public Page<IssueDTO> pageIssue(Long organizationId, List<Long> projectIds, PageRequest pageRequest, SearchVO searchVO){
        //处理用户搜索
        Map<String, Object> sortMap = issueService.processSortMap(pageRequest, projectIds.get(0), organizationId);
        String filterSql = null;
        //处理自定义搜索
        if (!CollectionUtils.isEmpty(searchVO.getQuickFilterIds())) {
            filterSql = issueService.getQuickFilter(searchVO.getQuickFilterIds());
        }
        issueService.splitIssueNumProjectCodePrefix(searchVO, new HashSet<>(projectIds));
        String finalFilterSql = filterSql;
        Page<IssueDTO> page = PageHelper.doPage(pageRequest, () -> workHoursMapper.queryParentIssueIdsList(new HashSet<>(projectIds), searchVO, finalFilterSql, searchVO.getAssigneeFilterIds(), sortMap));
        List<IssueDTO> content = page.getContent();
        if(CollectionUtils.isEmpty(content)){
            return new Page<>();
        }
        List<Long> parentIds = content.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        List<IssueDTO> childIssues = issueMapper.queryChildrenIdByParentId(parentIds, new HashSet<>(projectIds), searchVO, finalFilterSql, searchVO.getAssigneeFilterIds(), null);
        Set<Long> childrenIds  = childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toSet());
        List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(parentIds, childrenIds, true, true);
        return PageUtil.buildPageInfoWithPageInfoList(page, issueDTOList);
    }
    @Override
    public void buildIssueValueMap(Long organizationId, List<Long> projectIds, List<IssueDTO> issues, Map<String, Object> issueValueMap, SearchVO searchVO) {
        List<Long> allIssueIds = issues.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        Set<Long> userIds = new HashSet<>();
        Set<Long> featureIds = new HashSet<>();
        issues.forEach(i -> {
            Long assigneeId = i.getAssigneeId();
            Long reporterId = i.getReporterId();
            Long createdUser = i.getCreatedBy();
            Long updatedUser = i.getLastUpdatedBy();
            Long mainResponsibleId = i.getMainResponsibleId();
            if (!ObjectUtils.isEmpty(assigneeId) && !Objects.equals(assigneeId, 0L)) {
                userIds.add(assigneeId);
            }
            if (!ObjectUtils.isEmpty(reporterId) && !Objects.equals(reporterId, 0L)) {
                userIds.add(reporterId);
            }
            if (!ObjectUtils.isEmpty(i.getFeatureId())) {
                featureIds.add(i.getFeatureId());
            }
            Long parentIssueId = i.getParentIssueId();
            Long relateIssueId = i.getRelateIssueId();
            if (!ObjectUtils.isEmpty(parentIssueId)
                    && !Objects.equals(parentIssueId, 0L)) {
                featureIds.add(parentIssueId);
            }
            if (!ObjectUtils.isEmpty(relateIssueId)
                    && !Objects.equals(relateIssueId, 0L)) {
                featureIds.add(relateIssueId);
            }
            if (!ObjectUtils.isEmpty(createdUser) && !Objects.equals(createdUser, 0L)) {
                userIds.add(createdUser);
            }
            if (!ObjectUtils.isEmpty(updatedUser) && !Objects.equals(updatedUser, 0L)) {
                userIds.add(updatedUser);
            }
            if (!ObjectUtils.isEmpty(mainResponsibleId) && !Objects.equals(mainResponsibleId, 0L)) {
                userIds.add(mainResponsibleId);
            }
        });
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(new ArrayList<>(userIds), true);
        Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
        Map<Long, List<IssueTypeVO>> issueTypeDTOMap = issueTypeService.listIssueTypeMapByProjectIds(organizationId, projectIds);
        Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
        Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, projectIds, allIssueIds, false);
        Map<Long, List<WorkLogVO>> workLogVOMap = workLogMapper.queryByIssueIds(projectIds, allIssueIds).stream().collect(Collectors.groupingBy(WorkLogVO::getIssueId));
        Map<Long, List<SprintNameDTO>> closeSprintNames = issueMapper.querySprintNameByIssueIds(projectIds, allIssueIds).stream().collect(Collectors.groupingBy(SprintNameDTO::getIssueId));
        Map<Long, List<VersionIssueRelDTO>> fixVersionNames = issueMapper.queryVersionNameByIssueIds(projectIds, allIssueIds, "fix").stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
        Map<Long, List<VersionIssueRelDTO>> influenceVersionNames = issueMapper.queryVersionNameByIssueIds(projectIds, allIssueIds, "influence").stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
        Map<Long, List<LabelIssueRelDTO>> labelNames = issueMapper.queryLabelIssueByIssueIds(projectIds, allIssueIds).stream().collect(Collectors.groupingBy(LabelIssueRelDTO::getIssueId));
        Map<Long, List<ComponentIssueRelDTO>> componentMap = issueMapper.queryComponentIssueByIssueIds(projectIds, allIssueIds).stream().collect(Collectors.groupingBy(ComponentIssueRelDTO::getIssueId));
        Map<String, String> envMap = lookupValueService.queryMapByTypeCode(FieldCode.ENVIRONMENT);
        Map<Long, Set<TagVO>> tagMap = new HashMap<>();
        if (agilePluginService != null) {
            tagMap.putAll(agilePluginService.listTagMap(organizationId, new HashSet<>(projectIds), allIssueIds));
            agilePluginService.handleProgramIssueValueMap(organizationId, projectIds, allIssueIds, featureIds, issueValueMap);
        }
        Map<Long, List<IssueLinkDTO>> relatedIssueMap =
                issueLinkMapper.queryIssueLinkByIssueId(new HashSet<>(allIssueIds), new HashSet<>(projectIds), false)
                        .stream()
                        .collect(Collectors.groupingBy(IssueLinkDTO::getKeyIssueId));
        List<WorkLogDTO> workTimes = workHoursMapper.countWorkTime(projectIds, allIssueIds, searchVO);
        Map<Long, BigDecimal> workTimeMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(workTimes)){
            workTimeMap = workTimes.stream().collect(Collectors.toMap(WorkLogDTO::getIssueId, WorkLogDTO::getWorkTime));
        }
        List<WorkLogDTO> allWorkTimes = workHoursMapper.countWorkTime(projectIds, allIssueIds, null);
        Map<Long, BigDecimal> allWorkTimeMap = new HashMap<>();
        if(!CollectionUtils.isEmpty(allWorkTimes)){
            allWorkTimeMap = allWorkTimes.stream().collect(Collectors.toMap(WorkLogDTO::getIssueId, WorkLogDTO::getWorkTime));
        }
        Map<Long, BigDecimal> estimateTimeMap = issues.stream()
                .filter(v -> !ObjectUtils.isEmpty(v.getEstimateTime()))
                .collect(Collectors.toMap(IssueDTO::getIssueId, IssueDTO::getEstimateTime));
        issueValueMap.put(USER_MAP, usersMap);
        issueValueMap.put("issueTypesMap", issueTypeDTOMap);
        issueValueMap.put(STATUS_MAP, statusMapDTOMap);
        issueValueMap.put(PRIORITY_MAP, priorityMap);
        issueValueMap.put(CLOSE_SPRINT_MAP, closeSprintNames);
        issueValueMap.put(FIX_VERSION_MAP, fixVersionNames);
        issueValueMap.put(INFLUENCE_VERSION_MAP, influenceVersionNames);
        issueValueMap.put(LABEL_MAP, labelNames);
        issueValueMap.put(COMPONENT_MAP, componentMap);
        issueValueMap.put(FOUNDATION_CODE_VALUE_MAP, foundationCodeValue);
        issueValueMap.put(ENV_MAP, envMap);
        issueValueMap.put(WORK_LOG_MAP, workLogVOMap);
        issueValueMap.put(RELATED_ISSUE_MAP, relatedIssueMap);
        issueValueMap.put("workTimeMap", workTimeMap);
        issueValueMap.put("allWorkTimeMap", allWorkTimeMap);
        issueValueMap.put("estimateTimeMap", estimateTimeMap);
        issueValueMap.put("tagMap", tagMap);
    }

    @Override
    public List<IssueWorkHoursVO> listProjectAssigneeWorkHours(Long organizationId, List<Long> projects, SearchVO searchVO) {
        // 查询issue
        List<Long> allIssueId = queryAllIssueIds(organizationId, projects, searchVO);
        if (CollectionUtils.isEmpty(allIssueId)) {
            return new ArrayList<>();
        }
        List<IssueWorkHoursVO> projectAssignee = workHoursMapper.queryProjectAssigneeIds(projects, allIssueId);
        if (CollectionUtils.isEmpty(projectAssignee)) {
            return new ArrayList<>();
        }
        List<Long> userIds = projectAssignee.stream().map(IssueWorkHoursVO::getUserId).collect(Collectors.toList());
        List<UserDTO> userList = baseFeignClient.listUsersByIds(userIds.toArray(new Long[userIds.size()]), false).getBody();
        Map<Long, UserDTO> userMap = userList.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity()));
        // 统计工时
        List<IssueWorkHoursVO> workTimes = workHoursMapper.countProjectAssigneeWorkTime(projects, allIssueId, searchVO);
        Map<Long, Map<Long, BigDecimal>> workTimeMap = workTimes.stream().collect(Collectors.groupingBy(IssueWorkHoursVO::getUserId, Collectors.toMap(IssueWorkHoursVO::getProjectId, IssueWorkHoursVO::getWorkTime)));
        // 统计累计工时
        List<IssueWorkHoursVO> cumulativeWorkTimes = workHoursMapper.countProjectAssigneeWorkTime(projects, allIssueId, null);
        Map<Long, Map<Long, BigDecimal>> cumulativeWorkTimeMap = cumulativeWorkTimes.stream().collect(Collectors.groupingBy(IssueWorkHoursVO::getUserId, Collectors.toMap(IssueWorkHoursVO::getProjectId, IssueWorkHoursVO::getWorkTime)));
        for (IssueWorkHoursVO issueWorkHoursVO : projectAssignee) {
            Long userId = issueWorkHoursVO.getUserId();
            issueWorkHoursVO.setUserDTO(userMap.get(userId));
            Map<Long, BigDecimal> projectWorkTimeMap = workTimeMap.getOrDefault(userId, new HashMap<>());
            Map<Long, BigDecimal> projectCumulativeWorkTimeMap = cumulativeWorkTimeMap.getOrDefault(userId, new HashMap<>());
            Long projectId = issueWorkHoursVO.getProjectId();
            issueWorkHoursVO.setWorkTime(projectWorkTimeMap.getOrDefault(projectId, BigDecimal.ZERO));
            issueWorkHoursVO.setCumulativeWorkTime(projectCumulativeWorkTimeMap.getOrDefault(projectId, BigDecimal.ZERO));
            BigDecimal deviationRate = BigDecimal.ZERO;
            BigDecimal estimateTime = ObjectUtils.isEmpty(issueWorkHoursVO.getEstimateTime()) ? BigDecimal.ZERO : issueWorkHoursVO.getEstimateTime();
            int compare = BigDecimal.ZERO.compareTo(estimateTime);
            if (!Objects.equals(0, compare)) {
                deviationRate = issueWorkHoursVO.getCumulativeWorkTime().subtract(estimateTime).divide(estimateTime, 2,BigDecimal.ROUND_HALF_UP);
            }
            issueWorkHoursVO.setDeviationRate(deviationRate);
        }
        return projectAssignee;
    }

    @Override
    public Page<IssueWorkHoursVO> pageQueryAssignee(Long organizationId, List<Long> projectIds, PageRequest pageRequest, SearchVO searchVO) {
        List<Long> allIssueIds = queryAllIssueIds(organizationId, projectIds, searchVO);
        if (CollectionUtils.isEmpty(allIssueIds)) {
            return new Page<>();
        }
        Page<Long> page = PageHelper.doPage(pageRequest, () -> workHoursMapper.queryIds(projectIds, allIssueIds, "assignee"));
        if (CollectionUtils.isEmpty(page.getContent())) {
            return new Page<>();
        }
        List<Long> assigneeIds = page.getContent();
        // 查询用户信息
        List<UserDTO> userDTOS = baseFeignClient.listUsersByIds(assigneeIds.toArray(new Long[allIssueIds.size()]), false).getBody();
        Map<Long, UserDTO> userMap = userDTOS.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity()));
        // 统计登记工时
        List<IssueWorkHoursVO>  workTimes = workHoursMapper.countAssigneeWorkTime(projectIds, allIssueIds, searchVO);
        Map<Long, BigDecimal> workTimeMap = workTimes.stream().collect(Collectors.toMap(IssueWorkHoursVO::getUserId, IssueWorkHoursVO::getWorkTime));
        // 统计累计工时
        List<IssueWorkHoursVO> cumulativeWorkTimes = workHoursMapper.countAssigneeWorkTime(projectIds, allIssueIds, null);
        Map<Long, IssueWorkHoursVO> cumulativeWorkTimeMap = cumulativeWorkTimes.stream().collect(Collectors.toMap(IssueWorkHoursVO::getUserId, Function.identity()));
        List<IssueWorkHoursVO> issueWorkHoursVOS = new ArrayList<>();
        for (Long assigneeId : assigneeIds) {
            IssueWorkHoursVO issueWorkHoursVO = new IssueWorkHoursVO();
            issueWorkHoursVO.setUserId(assigneeId);
            issueWorkHoursVO.setUserDTO(userMap.get(assigneeId));
            workHoursCount(assigneeId, workTimeMap, cumulativeWorkTimeMap, issueWorkHoursVO);
            issueWorkHoursVOS.add(issueWorkHoursVO);
        }
        return PageUtil.buildPageInfoWithPageInfoList(page, issueWorkHoursVOS);
    }

    private void workHoursCount(Long id,
                                Map<Long, BigDecimal> workTimeMap,
                                Map<Long, IssueWorkHoursVO> cumulativeWorkTimeMap,
                                IssueWorkHoursVO issueWorkHoursVO) {
        // 计算工时
        BigDecimal workTime = workTimeMap.getOrDefault(id, BigDecimal.ZERO);
        BigDecimal estimateTime = BigDecimal.ZERO;
        BigDecimal cumulativeWorkTime = BigDecimal.ZERO;
        IssueWorkHoursVO issueWorkHoursAssignee = cumulativeWorkTimeMap.get(id);
        if (!ObjectUtils.isEmpty(issueWorkHoursAssignee)) {
            cumulativeWorkTime = ObjectUtils.isEmpty(issueWorkHoursAssignee.getWorkTime()) ? BigDecimal.ZERO : issueWorkHoursAssignee.getWorkTime();
            estimateTime = ObjectUtils.isEmpty(issueWorkHoursAssignee.getEstimateTime()) ? BigDecimal.ZERO :issueWorkHoursAssignee.getEstimateTime();
        }
        // 计算偏差率
        BigDecimal deviationRate = BigDecimal.ZERO;
        int equalZero = BigDecimal.ZERO.compareTo(estimateTime);
        if (equalZero != 0) {
            deviationRate = cumulativeWorkTime.subtract(estimateTime).divide(estimateTime, 2, BigDecimal.ROUND_HALF_UP);
        }
        issueWorkHoursVO.setWorkTime(workTime);
        issueWorkHoursVO.setCumulativeWorkTime(cumulativeWorkTime);
        issueWorkHoursVO.setEstimateTime(estimateTime);
        issueWorkHoursVO.setDeviationRate(deviationRate);
    }

    @Override
    public Page<IssueWorkHoursVO> pageProjectLatitude(Long organizationId, PageRequest pageRequest, SearchVO searchVO) {
        List<Long> projectIds = transformProjectIds(searchVO);
        return pageQueryProject(organizationId, projectIds, pageRequest, searchVO);
    }

    @Override
    public Page<IssueWorkHoursVO> pageQueryProject(Long organizationId,
                                                   List<Long> projectIds,
                                                   PageRequest pageRequest,
                                                   SearchVO searchVO) {
        // 查询有权限的项目
        List<ProjectVO> projectVOS = new ArrayList<>();
        if (CollectionUtils.isEmpty(projectIds)) {
            projectIds = new ArrayList<>();
            Long userId = DetailsHelper.getUserDetails().getUserId();
            handlePermissionProject(organizationId, projectIds,  projectVOS,  userId);
        } else {
            projectVOS.addAll(baseFeignClient.queryByIds(new HashSet<>(projectIds)).getBody());
        }
        List<Long> allIssueIds = queryAllIssueIds(organizationId, projectIds, searchVO);
        if (CollectionUtils.isEmpty(allIssueIds)) {
            return new Page<>();
        }
        List<Long> finalProjectIds = projectIds;
        Page<Long> page = PageHelper.doPage(pageRequest, () -> workHoursMapper.queryIds(finalProjectIds, allIssueIds, "project"));
        if(CollectionUtils.isEmpty(page.getContent())) {
            return new Page<>();
        }
        List<Long> currentProjectIds = page.getContent();
        // 查询项目详情
        Map<Long, ProjectVO> projectVOMap = projectVOS.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        // 统计登记工时
        List<IssueWorkHoursVO>  workTimes = workHoursMapper.countProjectWorkTime(currentProjectIds, allIssueIds, searchVO);
        Map<Long, BigDecimal> workTimeMap = workTimes.stream().collect(Collectors.toMap(IssueWorkHoursVO::getProjectId, IssueWorkHoursVO::getWorkTime));
        // 统计累计工时
        List<IssueWorkHoursVO> cumulativeWorkTimes = workHoursMapper.countProjectWorkTime(currentProjectIds, allIssueIds, null);
        Map<Long, IssueWorkHoursVO> cumulativeWorkTimeMap = cumulativeWorkTimes.stream().collect(Collectors.toMap(IssueWorkHoursVO::getProjectId, Function.identity()));
        List<IssueWorkHoursVO> issueWorkHoursVOS = new ArrayList<>();
        for (Long currentProjectId : currentProjectIds) {
            IssueWorkHoursVO issueWorkHoursVO = new IssueWorkHoursVO();
            issueWorkHoursVO.setProjectId(currentProjectId);
            issueWorkHoursVO.setProjectVO(projectVOMap.get(currentProjectId));
            // 计算工时
            workHoursCount(currentProjectId, workTimeMap, cumulativeWorkTimeMap, issueWorkHoursVO);
            issueWorkHoursVOS.add(issueWorkHoursVO);
        }
        return PageUtil.buildPageInfoWithPageInfoList(page, issueWorkHoursVOS);
    }

    @Override
    public Page<IssueWorkHoursVO> pageQueryAssigneeOnOrganizationLevel(Long organizationId, PageRequest pageRequest, SearchVO searchVO) {
        List<Long> projectIds = transformProjectIds(searchVO);
        if (CollectionUtils.isEmpty(projectIds)) {
            Long userId = DetailsHelper.getUserDetails().getUserId();
            handlePermissionProject(organizationId, projectIds,  new ArrayList<>(),  userId);
        }
        return pageQueryAssignee(organizationId, projectIds, pageRequest, searchVO);
    }

    private List<Long> transformProjectIds(SearchVO searchVO) {
        List<String> projectStringList = (List<String>) searchVO.getSearchArgs().getOrDefault("projectIds", new ArrayList<>());
        if (!CollectionUtils.isEmpty(projectStringList)) {
            return projectStringList.stream().map(Long::valueOf).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    @Override
    public void handlePermissionProject(Long organizationId, List<Long> projectIds, List<ProjectVO> projectVOS, Long userId) {
        Page<ProjectVO> page = baseFeignClient.pagingProjectsByUserId(organizationId, userId, 0, 0, true, "N_AGILE").getBody();
        if (!CollectionUtils.isEmpty(page.getContent())) {
            projectIds.addAll(page.getContent().stream().map(ProjectVO::getId).collect(Collectors.toList()));
            projectVOS.addAll(page.getContent());
        }
    }

    @Override
    public Page<IssueListFieldKVVO> pageQueryIssuesOnOrganizationLevel(Long organizationId, PageRequest pageRequest, Boolean containsSubIssue, SearchVO searchVO) {
        List<Long> projectIds = transformProjectIds(searchVO);
        if (CollectionUtils.isEmpty(projectIds)) {
            Long userId = DetailsHelper.getUserDetails().getUserId();
            handlePermissionProject(organizationId, projectIds,  new ArrayList<>(),  userId);
        }
        return pageQueryIssues(organizationId, projectIds, pageRequest, containsSubIssue, searchVO);
    }

    @Override
    public BigDecimal countIssueWorkHours(Long organizationId, List<Long> projectIds, SearchVO searchVO) {
        List<Long> allIssueIds = queryAllIssueIds(organizationId, projectIds, searchVO);
        if (CollectionUtils.isEmpty(allIssueIds)) {
            return BigDecimal.ZERO;
        }
        return workHoursMapper.countWorkTime(projectIds, allIssueIds, searchVO)
                .stream().map(WorkLogDTO::getWorkTime).reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    @Override
    public BigDecimal countIssueWorkHoursOnOrganizationLevel(Long organizationId, SearchVO searchVO) {
        List<Long> projectIds = transformProjectIds(searchVO);
        if (CollectionUtils.isEmpty(projectIds)) {
            Long userId = DetailsHelper.getUserDetails().getUserId();
            handlePermissionProject(organizationId, projectIds,  new ArrayList<>(),  userId);
        }
        return countIssueWorkHours(organizationId, projectIds, searchVO);
    }

    private void statisticalWorkHours(IssueListFieldKVVO issueListFieldKVVO,
                                        Map<Long, List<Long>> issueMap,
                                        Map<Long, BigDecimal> estimateTimeMap,
                                        Map<Long, BigDecimal> allWorkTimeMap,
                                        Map<Long, BigDecimal> workTimeMap,
                                        Boolean containsSubIssue) {
        BigDecimal workTime = workTimeMap.getOrDefault(issueListFieldKVVO.getIssueId(), BigDecimal.ZERO);
        BigDecimal allWorkTime = allWorkTimeMap.getOrDefault(issueListFieldKVVO.getIssueId(), BigDecimal.ZERO);
        BigDecimal estimateTime = issueListFieldKVVO.getEstimateTime();
        if (ObjectUtils.isEmpty(estimateTime)) {
            estimateTime = BigDecimal.ZERO;
        }
        if (containsSubIssue) {
            List<Long> childrens = issueMap.getOrDefault(issueListFieldKVVO.getIssueId(), new ArrayList<>());
            for (Long children : childrens) {
                BigDecimal childrenWorkTime = workTimeMap.getOrDefault(children, BigDecimal.ZERO);
                BigDecimal childrenAllWorkTime = allWorkTimeMap.getOrDefault(children, BigDecimal.ZERO);
                BigDecimal childrenEstimateTime = estimateTimeMap.getOrDefault(children, BigDecimal.ZERO);
                workTime = workTime.add(childrenWorkTime);
                allWorkTime = allWorkTime.add(childrenAllWorkTime);
                estimateTime = estimateTime.add(childrenEstimateTime);
            }
        }
        // 计算偏差率
        BigDecimal deviationRate = BigDecimal.ZERO;
        if (!Objects.equals(BigDecimal.ZERO, estimateTime)) {
            deviationRate = allWorkTime.subtract(estimateTime).divide(estimateTime, 2,BigDecimal.ROUND_HALF_UP);
        }
        issueListFieldKVVO.setWorkTime(workTime);
        issueListFieldKVVO.setEstimateTime(estimateTime);
        issueListFieldKVVO.setCumulativeWorkTime(allWorkTime);
        issueListFieldKVVO.setDeviationRate(deviationRate);
    }

    private List<Long> queryAllIssueIds(Long organizationId, List<Long> projectIds, SearchVO searchVO) {
        //处理用户搜索
        String filterSql = null;
        //处理自定义搜索
        if (!CollectionUtils.isEmpty(searchVO.getQuickFilterIds())) {
            filterSql = issueService.getQuickFilter(searchVO.getQuickFilterIds());
        }
        issueService.splitIssueNumProjectCodePrefix(searchVO, new HashSet<>(projectIds));
        List<IssueDTO> issueDTOS = workHoursMapper.queryParentIssueIdsList(new HashSet<>(projectIds), searchVO, filterSql, searchVO.getAssigneeFilterIds(), null);
        if (CollectionUtils.isEmpty(issueDTOS)) {
            return new ArrayList<>();
        }
        List<Long> parentIds = issueDTOS.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        List<IssueDTO> childIssues = issueMapper.queryChildrenIdByParentId(parentIds, new HashSet<>(projectIds), searchVO, filterSql, searchVO.getAssigneeFilterIds(), null);
        if (!CollectionUtils.isEmpty(childIssues)) {
            parentIds.addAll(childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toList()));
        }
        return parentIds;
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

    @Override
    public void handlerProject(Long organizationId, List<Long> projectIds, Long userId, WorkHoursSearchVO workHoursSearchVO){
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
