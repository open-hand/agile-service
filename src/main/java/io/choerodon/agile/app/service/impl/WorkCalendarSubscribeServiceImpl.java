 package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.WorkItemSearchVO;
import io.choerodon.agile.api.vo.WorkItemVO;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.IssueParticipantRelMapper;
import io.choerodon.agile.infra.utils.ICal4jUtil;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.app.service.WorkCalendarSubscribeService;
import io.choerodon.agile.infra.dto.WorkCalendarSubscribeDTO;
import io.choerodon.agile.infra.mapper.WorkCalendarSubscribeMapper;
import io.choerodon.agile.infra.utils.MultipartICalendar;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import net.fortuna.ical4j.model.*;
import net.fortuna.ical4j.model.Date;
import net.fortuna.ical4j.model.component.VAlarm;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.property.*;
import org.apache.commons.compress.utils.IOUtils;
import org.hzero.boot.file.FileClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.CacheControl;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.concurrent.TimeUnit;

/**
 * @author huaxin.deng@hand-china.com 2021-10-11 14:35:33
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WorkCalendarSubscribeServiceImpl implements WorkCalendarSubscribeService {

    private static final String URL_TEMPLATE1 = "#/agile/work-list/issue?type=project&id=";
    private static final String URL_TEMPLATE2 = "&name=";
    private static final String URL_TEMPLATE3 = "&paramName=";
    private static final String URL_TEMPLATE4 = "&paramIssueId=";
    private static final String URL_TEMPLATE5 = "&paramOpenIssueId=";
    private static final String URL_TEMPLATE6 = "&organizationId=";
    private static final String URL_TEMPLATE7 = "&orgId=";

    private static final String MULTIPART_NAME = "file";
    private static final String ORIGINAL_FILE_NAME = ".ics";
    private static final String BUCKET_NAME = "agile-service";
    private static final String CALENDAR_CONTENT_TYPE = "text/calendar";
    private static final String ASSIGNEE = "assignee";
    private static final String PARTICIPANT = "participant";

    @Autowired
    private WorkCalendarSubscribeMapper workCalendarSubscribeMapper;

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private IssueParticipantRelMapper issueParticipantRelMapper;

    @Autowired
    private IssueService issueService;

    @Autowired
    private FileClient fileClient;

    @Value("${services.domain.url}")
    private String domainUrl;

    @Value("${services.attachment.url}")
    private String attachmentUrl;

    @Override
    public String subscribe(Long organizationId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        WorkCalendarSubscribeDTO select = new WorkCalendarSubscribeDTO(organizationId, userId, null, null);
        WorkCalendarSubscribeDTO dto = workCalendarSubscribeMapper.selectOne(select);
        String uuid;
        if (Objects.isNull(dto)) {
            // 未订阅，生成订阅文件，并返回uuid
            uuid = generateCalendarFile(organizationId, userId);
        } else {
            // 已订阅，直接获取uuid
            uuid = dto.getUuid();
        }
        return uuid;
    }

    private String generateCalendarFile(Long organizationId, Long userId) {
        String url;
        // 生成MultipartICalendar文件
        MultipartFile multipartFile = createMultipartICalendar(organizationId, userId);
        try {
            // 文件上传到minio
            url = fileClient.uploadFile(organizationId, BUCKET_NAME, null, ORIGINAL_FILE_NAME, multipartFile);
        } catch (Exception e) {
            throw new CommonException("error.generateCalendarFile.uploadFile");
        }
        return create(organizationId, userId, url);
    }

    private MultipartFile createMultipartICalendar(Long organizationId, Long userId) {
        // 生成事件
        List<VEvent> events = initEvents(organizationId, userId);
        // 生成日历文件并获取二进制流
        byte[] content = ICal4jUtil.getCalendarFileByteArray(events);
        return new MultipartICalendar(MULTIPART_NAME, ORIGINAL_FILE_NAME, CALENDAR_CONTENT_TYPE, content);
    }

    private String create(Long organizationId, Long userId, String url) {
        WorkCalendarSubscribeDTO dto = new WorkCalendarSubscribeDTO(organizationId, userId, getRelativeUrl(url), getUuid());
        if (workCalendarSubscribeMapper.insertSelective(dto) != 1) {
            throw new CommonException("error.workCalendarSubscribe.insert");
        }
        return dto.getUuid();
    }

    private String getRelativeUrl(String url) {
        // 获取相对路径
        String relativeUrl = url.substring(url.indexOf(BUCKET_NAME) + BUCKET_NAME.length() + 1);
        if (relativeUrl.length() < 1) {
            throw new CommonException("error.illegal.url");
        }
        return relativeUrl;
    }

    private String getUuid() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    private List<VEvent> initEvents(Long organizationId, Long userId) {
        List<Long> projectIds = new ArrayList<>();
        List<ProjectVO> projects = new ArrayList<>();
        //查询有权限的项目
        issueService.queryUserProjects(organizationId, null, projectIds, projects, userId, null);
        List<VEvent> events = new ArrayList<>();
        if (!CollectionUtils.isEmpty(projectIds)) {
            WorkItemSearchVO workItemSearchVO = createWorkItemSearchVO();
            int page = 0;
            int size = 1000;
            int sequence = 0;
            // 分段处理
            while (true) {
                PageRequest pageRequest = new PageRequest(page, size);
                Page<WorkItemVO> issuePage = PageHelper.doPage(pageRequest, ()-> issueMapper.queryAssigneeIssueList(projectIds, userId, workItemSearchVO));
                if (CollectionUtils.isEmpty(issuePage.getContent())) {
                    break;
                }
                List<WorkItemVO> issueList = issuePage.getContent();
                for (WorkItemVO workItemVO : issueList) {
                    events.add(createEventVO(workItemVO, sequence++));
                }
                boolean hasNextPage = (++page) < issuePage.getTotalPages();
                // 清空集合
                issueList.clear();
                if (!hasNextPage) {
                    break;
                }
            }
        }
        return events;
    }

    private WorkItemSearchVO createWorkItemSearchVO () {
        WorkItemSearchVO workItemSearchVO = new WorkItemSearchVO();

        // 只查询当月第1天0时以后的工作项
        java.util.Calendar calendar = java.util.Calendar.getInstance();
        calendar.setTime(new Date());
        calendar.set(java.util.Calendar.DAY_OF_MONTH, 1);
        calendar.set(java.util.Calendar.HOUR, 0);
        java.util.Date firstDayOfMonth = calendar.getTime();
        workItemSearchVO.setStartTime(firstDayOfMonth);
        workItemSearchVO.setOnlyStartTime(true);

        // 查询我经办和我参与的
        List<String> assigneeFilter = new ArrayList<>();
        assigneeFilter.add(ASSIGNEE);
        assigneeFilter.add(PARTICIPANT);
        workItemSearchVO.setAssigneeFilter(assigneeFilter);
        return workItemSearchVO;
    }

    private VEvent createEventVO(WorkItemVO issue, Integer sequence) {
        // 事件主题
        String summary = issue.getIssueNum() + "-" + issue.getSummary();
        // 开始时间
        DateTime start = new DateTime(issue.getEstimatedStartTime());
        // 开始时间转换为UTC时间（UTC ＋ 时区差 ＝ 本地时间 ）
        start.setUtc(true);
        // 结束时间
        DateTime end = new DateTime(issue.getEstimatedEndTime());
        // 结束时间设置成UTC时间（UTC ＋ 时区差 ＝ 本地时间 ）
        end.setUtc(true);
        // 新建普通事件
         VEvent event = new VEvent(start, end, summary);
        // 生成唯一标示
        event.getProperties().add(new Uid(String.valueOf(issue.getIssueId())));
        // 生成sequence
        event.getProperties().add(new Sequence(sequence));
        // 添加描述：工作项链接
        event.getProperties().add(new Description("[" + getIssueUrl(issue) +"]"));
        // 提醒,提前15分钟
        VAlarm valarm = new VAlarm(new Dur(0, 0, -15, 0));
        valarm.getProperties().add(new Summary("Event Alarm"));
        valarm.getProperties().add(Action.DISPLAY);
        valarm.getProperties().add(new Description("该日程将于15分钟后开始"));
        // 将VAlarm加入VEvent
        event.getAlarms().add(valarm);
        return event;
    }

    private String getIssueUrl(WorkItemVO issue) {
        ProjectVO projectVO = ConvertUtil.queryProject(issue.getProjectId());
        return domainUrl + URL_TEMPLATE1 + projectVO.getId()
                + URL_TEMPLATE2 + convertProjectName(projectVO)
                + URL_TEMPLATE6 + projectVO.getOrganizationId()
                + URL_TEMPLATE7 + projectVO.getOrganizationId()
                + URL_TEMPLATE3 + issue.getIssueNum()
                + URL_TEMPLATE4 + issue.getIssueId()
                + URL_TEMPLATE5 + issue.getIssueId();
    }

    private String convertProjectName(ProjectVO projectVO) {
        String projectName = projectVO.getName();
        return projectName.replace(" ", "%20");
    }

    @Override
    public void handleWorkCalendarSubscribeChanged(Long projectId, Long issueId, boolean estimatedTimeChanged, List<Long> oldUserIds) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Set<Long> userIds = getUserIds(projectId, issueId);
        addUserIds(userIds, oldUserIds);

        if (Boolean.TRUE.equals(estimatedTimeChanged)) {
            setWorkCalendarSubscribeChanged(organizationId, userIds);
        } else {
            IssueDTO issue = issueMapper.selectByPrimaryKey(issueId);
            if (!Objects.isNull(issue.getEstimatedStartTime()) && !Objects.isNull(issue.getEstimatedEndTime())) {
                setWorkCalendarSubscribeChanged(organizationId, userIds);
            }
        }
    }

    private void setWorkCalendarSubscribeChanged(Long organizationId, Set<Long> userIds) {
        if (!CollectionUtils.isEmpty(userIds)) {
            List<WorkCalendarSubscribeDTO> list = workCalendarSubscribeMapper.queryByUserIds(organizationId, userIds);
            if (!CollectionUtils.isEmpty(list)) {
                list.forEach(dto -> {
                    // 已订阅日历且内容未更新时，标记为已更新
                    if (!Objects.isNull(dto) && Boolean.FALSE.equals(dto.getChanged())) {
                        dto.setChanged(true);
                        update(dto);
                    }
                });
            }
        }
    }

    private void addUserIds(Set<Long> userIds, List<Long> userIdList) {
        if (!CollectionUtils.isEmpty(userIdList)) {
            userIdList.forEach(userId -> {
                if (userId != null && userId != 0) {
                    userIds.add(userId);
                }
            });
        }
    }

    private Set<Long> getUserIds(Long projectId, Long issueId) {
        Set<Long> userIds = new HashSet<>();
        //经办人
        IssueDTO issue = issueMapper.selectByPrimaryKey(issueId);
        if (issue.getAssigneeId() != null && issue.getAssigneeId() != 0) {
            userIds.add(issue.getAssigneeId());
        }
        //参与人
        List<Long> participants = issueParticipantRelMapper.listByIssueId(projectId, issueId);
        if (!CollectionUtils.isEmpty(participants)) {
            userIds.addAll(participants);
        }
        return userIds;
    }

    @Override
    public ResponseEntity<byte[]> downloadFile(Long organizationId, String uuid, HttpServletResponse httpResponse) {

        WorkCalendarSubscribeDTO select = new WorkCalendarSubscribeDTO(organizationId, null, null, uuid);

        WorkCalendarSubscribeDTO dto = workCalendarSubscribeMapper.selectOne(select);
        // 未订阅不处理
        if (Objects.isNull(dto)) {
            return ResponseEntity.ok().body(new byte[0]);
        }
        String realUrl = getRealUrl(dto.getFileUrl());
        // 日历内容未更新，不更新文件
        if (Boolean.TRUE.equals(dto.getChanged())) {
            MultipartFile multipartFile = createMultipartICalendar(organizationId, dto.getUserId());
            try {
                fileClient.updateFile(organizationId, BUCKET_NAME, realUrl, null, multipartFile);
            } catch (Exception e) {
                throw new CommonException("error.generateCalendarFile.updateFile");
            }
            dto.setChanged(false);
            update(dto);
        }
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(CALENDAR_CONTENT_TYPE))
                .cacheControl(CacheControl.maxAge(1, TimeUnit.DAYS).cachePublic())
                .body(getFileByteArray(organizationId, realUrl));
    }

    @Override
    public String query(Long organizationId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        WorkCalendarSubscribeDTO select = new WorkCalendarSubscribeDTO(organizationId, userId, null, null);
        WorkCalendarSubscribeDTO dto = workCalendarSubscribeMapper.selectOne(select);
        String uuid = null;
        if (!Objects.isNull(dto)) {
            uuid = dto.getUuid();
        }
        return uuid;
    }

    private String getRealUrl(String url) {
        return attachmentUrl + "/" + BUCKET_NAME + "/" + url;
    }

    private byte[] getFileByteArray(Long organizationId, String url){
        InputStream inputStream =
                fileClient.downloadFile(organizationId, BUCKET_NAME, url);
        try {
            return IOUtils.toByteArray(inputStream);
        } catch (IOException e) {
            throw new CommonException(e.getMessage(), e);
        }
    }

    private void update(WorkCalendarSubscribeDTO dto) {
        if (workCalendarSubscribeMapper.updateByPrimaryKeySelective(dto) != 1) {
            throw new CommonException("error.update.workCalendarSubscribe");
        }
    }
}
