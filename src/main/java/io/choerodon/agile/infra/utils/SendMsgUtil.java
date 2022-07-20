package io.choerodon.agile.infra.utils;

import java.util.*;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.IssueCommentVO;
import io.choerodon.agile.api.vo.IssueMoveVO;
import io.choerodon.agile.api.vo.IssueSubVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.NoticeService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.ProjectInfoDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.IssueConstant;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.mapper.IssueStatusMapper;
import io.choerodon.agile.infra.mapper.ProjectInfoMapper;
import io.choerodon.agile.infra.mapper.StarBeaconMapper;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.boot.message.entity.MessageSender;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

/**
 * Created by HuangFuqiang@choerodon.io on 2019/4/29.
 * Email: fuqianghuang01@gmail.com
 */
@Component
public class SendMsgUtil {

    private static final String URL_TEMPLATE1 = "#/agile/work-list/issue?type=project&id=";
    private static final String URL_TEMPLATE9 = "#/waterfall/wbs?type=project&id=";
    private static final String URL_TEMPLATE10 = "#/agile/pro-risk?type=project&id=";
    private static final String URL_TEMPLATE2 = "&name=";
    private static final String URL_TEMPLATE3 = "&paramName=";
    private static final String URL_TEMPLATE4 = "&paramIssueId=";
    private static final String URL_TEMPLATE5 = "&paramOpenIssueId=";
    private static final String URL_TEMPLATE6 = "&organizationId=";
    private static final String URL_TEMPLATE7 = "&orgId=";
    private static final String URL_TEMPLATE8 = "&detailTab=comment";
    private static final String ERROR_PROJECT_NOTEXIST = "error.project.notExist";
    private static final String SUB_TASK = "sub_task";
    private static final String STATUS_ID = "statusId";
    private static final String INSERT = "insert";
    private static final String IMAGE = "image";
    private static final String IMG = "img";

    private static final String FEATURE_URL_TEMPLATE1 = "#/agile/feature?type=project&id=";
    private static final String FEATURE_URL_TEMPLATE2 = "&name=";
    private static final String FEATURE_URL_TEMPLATE4 = "&organizationId=";
    private static final String ISSUE_SOLVE = "ISSUESOLVE";
    private static final String ISSUE_CREATE = "ISSUECREATE";
    private static final String ISSUE_SET_PARTICIPANT = "ISSUE_SET_PARTICIPANT";
    @Autowired
    private SiteMsgUtil siteMsgUtil;

    @Autowired
    private NoticeService noticeService;

    @Autowired
    private UserService userService;

    @Autowired
    private IssueStatusMapper issueStatusMapper;

    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private StarBeaconMapper starBeaconMapper;
    @Value("${services.domain.url}")
    private String domainUrl;

    private String convertProjectName(ProjectVO projectVO) {
        String projectName = projectVO.getName();
        return projectName.replace(" ", "%20");
    }

    @Async
    public void sendMsgByIssueCreate(Long projectId, IssueVO result, Long operatorId) {
        //发送消息
        if (checkApplyType(result.getApplyType())) {
            List<Long> userIds = noticeService.queryUserIdsByProjectId(projectId, ISSUE_CREATE, result);
            String summary = result.getIssueNum() + "-" + result.getSummary();
            String reporterName = result.getReporterName();
            ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
            String url = getIssueUrl(result, projectVO, result.getIssueId());
            siteMsgUtil.issueCreate(userIds, reporterName, summary, url, operatorId, projectId, false);
            if (result.getAssigneeId() != null) {
                List<Long> assigneeIds = new ArrayList<>();
                assigneeIds.add(result.getAssigneeId());
                siteMsgUtil.issueAssignee(assigneeIds, result.getAssigneeName(), summary, url, projectId, reporterName, operatorId);
            }
            if (CollectionUtils.isNotEmpty(result.getParticipants())) {
                List<Long> sendUserIds = noticeService.queryUserIdsByProjectId(projectId, ISSUE_SET_PARTICIPANT, result);
                List<Long> participantIds = result.getParticipants().stream().map(UserMessageDTO::getId).collect(Collectors.toList());
                siteMsgUtil.issueParticipant(summary, url, projectId, operatorId, sendUserIds, reporterName, participantIds);
            }
        }
    }

    public String getIssueUrl(IssueVO result, ProjectVO projectVO, Long paramIssueId) {
        StringBuilder url = new StringBuilder();
        if (SchemeApplyType.WATERFALL.equals(result.getApplyType())) {
            url.append(URL_TEMPLATE9);
        } else if (SchemeApplyType.RISK.equals(result.getApplyType())) {
            url.append(URL_TEMPLATE10);
        } else {
            url.append(URL_TEMPLATE1);
        }
        url.append(projectVO.getId())
            .append(URL_TEMPLATE2).append(convertProjectName(projectVO))
            .append(URL_TEMPLATE6).append(projectVO.getOrganizationId())
            .append(URL_TEMPLATE7).append(projectVO.getOrganizationId())
            .append(URL_TEMPLATE3).append(result.getIssueNum())
            .append(URL_TEMPLATE4).append(paramIssueId)
            .append(URL_TEMPLATE5).append(result.getIssueId());
        return url.toString();
    }

    public String getFeatureUrl(IssueVO result, ProjectVO projectVO, Long paramIssueId) {
        return FEATURE_URL_TEMPLATE1 + projectVO.getId()
                + FEATURE_URL_TEMPLATE2 + convertProjectName(projectVO)
                + FEATURE_URL_TEMPLATE4 + projectVO.getOrganizationId()
                + URL_TEMPLATE3 + result.getIssueNum()
                + URL_TEMPLATE4 + paramIssueId
                + URL_TEMPLATE5 + result.getIssueId();

    }

    @Async
    public void sendMsgBySubIssueCreate(Long projectId, IssueSubVO result, Long operatorId) {
        // 发送消息
        if (SchemeApplyType.AGILE.equals(result.getApplyType())) {
            IssueVO issueVO = new IssueVO();
            issueVO.setReporterId(result.getReporterId());
            issueVO.setIssueId(result.getIssueId());
            List<Long> userIds = noticeService.queryUserIdsByProjectId(projectId, ISSUE_CREATE, issueVO);
            String summary = result.getIssueNum() + "-" + result.getSummary();
            String reporterName = result.getReporterName();
            ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
            String projectName = convertProjectName(projectVO);
            String url = URL_TEMPLATE1 + projectId + URL_TEMPLATE2 + projectName + URL_TEMPLATE6 + projectVO.getOrganizationId() + URL_TEMPLATE7 + projectVO.getOrganizationId() + URL_TEMPLATE3 + result.getIssueNum() + URL_TEMPLATE4 + result.getParentIssueId() + URL_TEMPLATE5 + result.getIssueId();
            siteMsgUtil.issueCreate(userIds, reporterName, summary, url, operatorId, projectId, false);
            Long getAssigneeId = result.getAssigneeId();
            if (!ObjectUtils.isEmpty(getAssigneeId)) {
                siteMsgUtil.issueAssignee(Arrays.asList(getAssigneeId), result.getAssigneeName(), summary, url, projectId, reporterName, operatorId);
            }
            if (CollectionUtils.isNotEmpty(result.getParticipantIds())) {
                List<Long> sendUserIds = noticeService.queryUserIdsByProjectId(projectId, ISSUE_SET_PARTICIPANT, issueVO);
                siteMsgUtil.issueParticipant(summary, url, projectId, operatorId, sendUserIds, reporterName, result.getParticipantIds());
            }
        }
    }

    @Async
    public void sendMsgByIssueAssignee(Long projectId,
                                       List<String> fieldList,
                                       IssueVO result,
                                       Long operatorId) {
        if (fieldList.contains("assigneeId") && result.getAssigneeId() != null && checkApplyType(result.getApplyType())) {
            List<Long> userIds = noticeService.queryUserIdsByProjectId(projectId, "ISSUEASSIGNEE", result);
            String summary = result.getIssueNum() + "-" + result.getSummary();
            String assigneeName = result.getAssigneeName();
            ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
            StringBuilder url = new StringBuilder();
            if (SUB_TASK.equals(result.getTypeCode())) {
                url.append(getIssueUrl(result, projectVO, result.getParentIssueId()));
            } else {
                url.append(getIssueUrl(result, projectVO, result.getIssueId()));
            }
            siteMsgUtil.issueAssignee(userIds, assigneeName, summary, url.toString(), projectId, getOperatorNameFromUserDetail(), operatorId);
        }
    }

    private String getOperatorNameFromUserDetail() {
        CustomUserDetails userDetails = DetailsHelper.getUserDetails();
        if (ObjectUtils.isEmpty(userDetails)) {
            throw new CommonException("error.user.not.login");
        }
        Long userId = userDetails.getUserId();
        Map<Long, UserMessageDTO> userMap = userService.queryUsersMap(Arrays.asList(userId), true);
        UserMessageDTO user = userMap.get(userId);
        if (ObjectUtils.isEmpty(user)) {
            return null;
        } else {
            return user.getName();
        }
    }

    @Async
    public void sendMsgByIssueComplete(Long projectId,
                                       List<String> fieldList,
                                       IssueVO result,
                                       Long operatorId) {
        Boolean completed = issueStatusMapper.selectByStatusId(projectId, result.getStatusId()).getCompleted();
        if (fieldList.contains(STATUS_ID) && completed != null && completed && result.getAssigneeId() != null && checkApplyType(result.getApplyType())) {
            List<Long> userIds = noticeService.queryUserIdsByProjectId(projectId, ISSUE_SOLVE, result);
            ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
            StringBuilder url = new StringBuilder();
            if (SUB_TASK.equals(result.getTypeCode())) {
                url.append(getIssueUrl(result, projectVO, result.getParentIssueId()));
            } else {
                url.append(getIssueUrl(result, projectVO, result.getIssueId()));
            }
            String userName = result.getAssigneeName();
            String summary = result.getIssueNum() + "-" + result.getSummary();
            siteMsgUtil.issueSolve(userIds, userName, summary, url.toString(), projectId, getOperatorNameFromUserDetail(), operatorId);
        }
    }

    @Async
    public void sendMsgByIssueMoveComplete(Long projectId,
                                           IssueMoveVO issueMoveVO,
                                           IssueDTO issueDTO,
                                           Long operatorId) {
        // 发送消息
        Boolean completed = issueStatusMapper.selectByStatusId(projectId, issueMoveVO.getStatusId()).getCompleted();
        if (completed != null && completed && issueDTO.getAssigneeId() != null && SchemeApplyType.AGILE.equals(issueDTO.getApplyType())) {
            List<Long> userIds = noticeService.queryUserIdsByProjectId(projectId, ISSUE_SOLVE, modelMapper.map(issueDTO, IssueVO.class));
            ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
            StringBuilder url = new StringBuilder();
            ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
            projectInfoDTO.setProjectId(projectId);
            List<ProjectInfoDTO> pioList = projectInfoMapper.select(projectInfoDTO);
            ProjectInfoDTO pio = null;
            if (pioList != null && !pioList.isEmpty()) {
                pio = pioList.get(0);
            }
            String pioCode = (pio == null ? "" : pio.getProjectCode());
            issueDTO.setIssueNum(pioCode + "-" + issueDTO.getIssueNum());
            IssueVO issueVO = modelMapper.map(issueDTO, IssueVO.class);
            if (SUB_TASK.equals(issueDTO.getTypeCode())) {
                url.append(getIssueUrl(issueVO, projectVO, issueVO.getParentIssueId()));
            } else {
                url.append(getIssueUrl(issueVO, projectVO, issueVO.getIssueId()));
            }
            String summary = issueDTO.getIssueNum() + "-" + issueDTO.getSummary();
            List<Long> assigneeIds = new ArrayList<>();
            assigneeIds.add(issueDTO.getAssigneeId());
            Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
            String userName = Optional.ofNullable(usersMap.get(issueDTO.getAssigneeId())).map(UserMessageDTO::getName).orElse("");
            siteMsgUtil.issueSolve(userIds, userName, summary, url.toString(), projectId, getOperatorNameFromUserDetail(), operatorId);
        }
    }

    @Async
    public void noticeIssueStatus(Long projectId,
                                  Set<Long> userSet,
                                  List<String> noticeTypeList,
                                  IssueDTO issueDTO,
                                  CustomUserDetails userDetails) {
        boolean onlyWebHook = CollectionUtils.isNotEmpty(noticeTypeList) && noticeTypeList.size() == 1 && noticeTypeList.contains("WEB_HOOK");
        if (!onlyWebHook && CollectionUtils.isEmpty(userSet)){
            return;
        }
        Map<String, String> templateArgsMap = new HashMap<>();
        // 设置经办人
        Long[] ids = new Long[3];
        ids[0] = issueDTO.getAssigneeId();
        ids[1] = userDetails.getUserId();
        ids[2] = issueDTO.getReporterId();
        Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(Arrays.asList(ids), true);
        ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
        Boolean isProgram = Objects.equals(issueDTO.getApplyType(), "program");
        String actionType = Boolean.TRUE.equals(isProgram) ? "报告的" : "经办的";
        UserMessageDTO assignee = Boolean.TRUE.equals(isProgram) ? usersMap.get(issueDTO.getReporterId()) : usersMap.get(issueDTO.getAssigneeId());
        String assigneeName = !Objects.isNull(assignee) ? assignee.getName() : "";
        // 设置概要
        String summary = projectVO.getCode() + "-" + issueDTO.getIssueNum() + "-" + issueDTO.getSummary();
        // 设置操作人
        UserMessageDTO operator = usersMap.get(userDetails.getUserId());
        String operatorName = !Objects.isNull(operator) ? operator.getName() : "";
        // 设置状态
        String status = ConvertUtil.getIssueStatusMap(projectId).get(issueDTO.getStatusId()).getName();
        // 设置url
        String url;
        IssueVO issueVO = modelMapper.map(issueDTO, IssueVO.class);
        if (isProgram) {
            url = getFeatureUrl(issueVO, projectVO, issueDTO.getIssueId());
        } else {
            url = getIssueUrl(issueVO, projectVO, issueDTO.getIssueId());
        }
        templateArgsMap.put("actionType", Objects.equals("", assigneeName) ? "" : actionType);
        templateArgsMap.put("assigneeName", assigneeName);
        templateArgsMap.put("summary", summary);
        templateArgsMap.put("operatorName", operatorName);
        templateArgsMap.put("status", status);
        templateArgsMap.put("url", url);
        templateArgsMap.put("link", domainUrl + "/" + url);
        siteMsgUtil.sendChangeIssueStatus(projectId, userSet, noticeTypeList, templateArgsMap, userDetails.getUserId(), onlyWebHook);
    }


    public String queryUserName(Long userId) {
        Map<Long, UserMessageDTO> userMessageDOMap = userService.queryUsersMap(Collections.singletonList(userId), true);
        return Optional.ofNullable(userMessageDOMap.get(userId)).map(UserMessageDTO::getName).orElse("");
    }


    public MessageSender generateIssueAsigneeSender(Long projectId,
                                                    Set<String> fieldList,
                                                    IssueDTO issue,
                                                    Long operatorId) {
        if (!SchemeApplyType.AGILE.equals(issue.getApplyType())) {
            return null;
        }
        if (Objects.isNull(issue.getAssigneeId())) {
            return null;
        }
        if (Objects.nonNull(fieldList) && !fieldList.contains("assigneeId")){
            return null;
        }
        IssueVO result = modelMapper.map(issue, IssueVO.class);
        String summary = result.getIssueNum() + "-" + result.getSummary();
        String reporterName = queryUserName(result.getReporterId());
        String assigneeName = queryUserName(result.getAssigneeId());
        ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
        String url = getIssueUrl(result, projectVO, result.getIssueId());
        return siteMsgUtil.issueAssigneeSender(Collections.singletonList(result.getAssigneeId()),
                assigneeName, summary, url, projectId, reporterName, operatorId);
    }

    public MessageSender generateIssueResolvSender(Long projectId,
                                                   Set<String> fieldList,
                                                   IssueDTO issue,
                                                   Long operatorId) {
        IssueVO result = modelMapper.map(issue, IssueVO.class);
        Boolean completed = issueStatusMapper.selectByStatusId(projectId, result.getStatusId()).getCompleted();
        if ((Objects.nonNull(fieldList) && !fieldList.contains(STATUS_ID))
                || completed == null 
                || !completed 
                || result.getAssigneeId() == null 
                || !SchemeApplyType.AGILE.equals(result.getApplyType())) {
            return null;
        }
        List<Long> userIds = noticeService.queryUserIdsByProjectId(projectId, ISSUE_SOLVE, result);
        ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
        StringBuilder url = new StringBuilder();
        if (SUB_TASK.equals(result.getTypeCode())) {
            url.append(getIssueUrl(result, projectVO, result.getParentIssueId()));
        } else {
            url.append(getIssueUrl(result, projectVO, result.getIssueId()));
        }
        String userName = queryUserName(result.getAssigneeId());
        String summary = result.getIssueNum() + "-" + result.getSummary();
        return siteMsgUtil.issueSolveSender(userIds, userName, summary, url.toString(), projectId, getOperatorNameFromUserDetail(), operatorId);
    }

    public MessageSender generateNoticeIssueStatusSender(Long projectId,
                                                         Set<Long> userSet,
                                                         List<String> noticeTypeList,
                                                         IssueDTO issueDTO,
                                                         CustomUserDetails userDetails,
                                                         Set<String> fieldList,
                                                         Long operatorId) {
        if (CollectionUtils.isEmpty(userSet)){
            return null;
        }
        if (Objects.nonNull(fieldList) && !fieldList.contains(STATUS_ID)){
            return null;
        }
        Map<String, String> templateArgsMap = new HashMap<>();
        // 设置经办人
        Map<Long, UserMessageDTO> userMap = userService.queryUsersMap(Arrays.asList(issueDTO.getAssigneeId(), userDetails.getUserId()), true);
        String assigneeName = Optional.ofNullable(userMap.get(issueDTO.getAssigneeId())).map(UserMessageDTO::getName).orElse("");
        // 设置操作人
        String operatorName = Optional.ofNullable(userMap.get(userDetails.getUserId())).map(UserMessageDTO::getName).orElse("");
        // 设置概要
        String summary = issueDTO.getIssueNum() + "-" + issueDTO.getSummary();
        // 设置状态
        String status = ConvertUtil.getIssueStatusMap(projectId).get(issueDTO.getStatusId()).getName();
        templateArgsMap.put("assigneeName", assigneeName);
        templateArgsMap.put("summary", summary);
        templateArgsMap.put("operatorName", operatorName);
        templateArgsMap.put("status", status);
        return siteMsgUtil.sendChangeIssueStatusSender(projectId, userSet, noticeTypeList, templateArgsMap, operatorId);
    }

    public ProjectVO getProjectVO(Long projectId, String errorProjectNotexist) {
        ProjectVO projectVO = userService.queryProject(projectId);
        if (projectVO == null) {
            throw new CommonException(errorProjectNotexist);
        }
        return projectVO;
    }

    @Async
    public void sendMsgByIssueComment(Long projectId,
                                      IssueDetailDTO issueDTO,
                                      IssueCommentVO issueCommentVO,
                                      Long operatorId) {
        IssueVO issueVO = modelMapper.map(issueDTO, IssueVO.class);
        Map<Long, String> actionMap = new HashMap<>(3);
        String url;
        String issueType;

        ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
        if ("feature".equals(issueVO.getTypeCode())) {
            issueType = "特性";
            url = getFeatureUrl(issueVO, projectVO, issueVO.getIssueId()) + URL_TEMPLATE8;
        } else {
            issueType = IssueConstant.ISSUE_CN;
            url = getIssueUrl(issueVO, projectVO, issueVO.getIssueId()) + URL_TEMPLATE8;
        }
        //设置动作与发送人
        List<Long> userIds = noticeService.queryUserIdsByProjectId(projectId, "ISSUE_COMMENT", issueVO);
        setIssueCommentMessageActionAndUser(actionMap, issueCommentVO.getUserId(), issueVO, projectVO, userIds);
        String summary = String.join("-", issueVO.getIssueNum(), issueVO.getSummary());
        String comment = Optional.ofNullable(issueCommentVO.getCommentText()).map(SendMsgUtil::getText).orElse("无");

        if (CollectionUtils.isNotEmpty(actionMap.keySet())) {
            siteMsgUtil.sendIssueComment(actionMap, projectVO, summary, url, comment, issueCommentVO, issueType, operatorId);
        }
    }

    private void setIssueCommentMessageActionAndUser(Map<Long, String> actionMap, Long userId, IssueVO issueVO, ProjectVO projectVO, List<Long> userIds) {
        Map<Long, String> map = new HashMap<>();
        List<Long> starUsers = starBeaconMapper.selectUsersByInstanceId(projectVO.getId(), issueVO.getIssueId());
        //邮件和站内信有接收人的消息中的action
        if (!CollectionUtils.isEmpty(starUsers)) {
            starUsers.forEach(starUserId -> {
                if (userIds.contains(starUserId)) {
                    map.put(starUserId, "关注的");
                }
            });
        }
        map.put(issueVO.getMainResponsibleId(), "负责的");
        map.put(issueVO.getReporterId(), "负责的");
        map.put(issueVO.getAssigneeId(), "处理的");
        userIds.forEach(sendUserId -> {
            if(sendUserId.equals(userId)){
                return;
            }
            actionMap.put(sendUserId, map.getOrDefault(sendUserId, "管理的"));
        });
        //webhook等无接收人的消息中的默认action
        if (CollectionUtils.isNotEmpty(actionMap.keySet())) {
            actionMap.put(null, "管理的");
        }
    }

    @Async
    public void sendMsgByIssueCommentReply(Long projectId,
                                           IssueDetailDTO issueDTO,
                                           IssueCommentVO issueCommentVO,
                                           Long operatorId) {
        Map<Long, String> actionMap = new HashMap<>(1);
        actionMap.put(issueCommentVO.getReplyToUserId(), "评论的");
        IssueVO issueVO = modelMapper.map(issueDTO, IssueVO.class);
        String url;
        String issueType;

        ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
        if ("feature".equals(issueVO.getTypeCode())) {
            issueType = "特性";
            url = getFeatureUrl(issueVO, projectVO, issueVO.getIssueId()) + URL_TEMPLATE8;
        } else {
            issueType = IssueConstant.ISSUE_CN;
            url = getIssueUrl(issueVO, projectVO, issueVO.getIssueId()) + URL_TEMPLATE8;
        }

        String summary = String.join("-", issueVO.getIssueNum(), issueVO.getSummary());
        String comment = Optional.ofNullable(issueCommentVO.getCommentText()).map(SendMsgUtil::getText).orElse("无");
        siteMsgUtil.sendIssueComment(actionMap, projectVO, summary, url, comment, issueCommentVO, issueType, operatorId);
    }

    public static String getText(String rawText) {
        if (StringUtils.isEmpty(rawText)) {
            return null;
        }
        StringBuilder result = new StringBuilder();
        try {
            JSONArray root = JSON.parseArray(rawText);
            for (Object o : root) {
                JSONObject object = (JSONObject) o;
                if (!(object.get(INSERT) instanceof JSONObject)) {
                    result.append(object.getString(INSERT));
                } else if (!(((JSONObject) object.get(INSERT)).get(IMAGE) instanceof JSONObject)) {
                    result.append("<img style=\"width: auto;height: auto;max-width: 650px;\" src=\"").append(((JSONObject) object.get(INSERT)).getString(IMAGE)).append("\"></img>");
                }
            }
        } catch (Exception e) {
            Document doc = Jsoup.parse(rawText);
            doc.body().children().forEach(SendMsgUtil::setImgSize);
            return doc.body().html();
        }
        if (result.length() == 0) {
            return null;
        }
        StringUtils.replace(rawText, "\n", "<br >");
        return result.toString();
    }

    private static void setImgSize(Element element) {
        String tagName = element.tag().getName();
        if (IMG.equals(tagName)) {
            element.removeAttr("style");
            element.attr("style", "width: auto; height: auto; max-width: 650px; text-align:center;");
        }
        if (element.childrenSize() > 0) {
            element.children().forEach(SendMsgUtil::setImgSize);
        }
    }

    @Async
    public void sendMsgToCustomFieldUsersByIssueCreate(Long projectId, IssueVO result, Long operatorId) {
        //问题创建通知自定义字段人员（普通创建、快速创建、问题导入）
        if (checkApplyType(result.getApplyType())) {
            List<Long> userIds = noticeService.queryCustomFieldUserIdsByProjectId(projectId, ISSUE_CREATE, result);
            if (CollectionUtils.isEmpty(userIds)) {
                return;
            }
            String summary = result.getIssueNum() + "-" + result.getSummary();
            String reporterName = result.getReporterName();
            ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
            String url = getIssueUrl(result, projectVO, result.getIssueId());
            siteMsgUtil.issueCreate(userIds, reporterName, summary, url, operatorId, projectId, true);
        }
    }

    @Async
    public void sendMsgByIssueParticipant(Long projectId, List<String> fieldList, IssueVO result, Long operatorId) {
        if (fieldList.contains("participantIds") && checkApplyType(result.getApplyType())) {
            if (CollectionUtils.isEmpty(result.getParticipants())){
              return;
            }
            List<Long> userIds = noticeService.queryUserIdsByProjectId(projectId, ISSUE_SET_PARTICIPANT, result);
            String summary = result.getIssueNum() + "-" + result.getSummary();
            String reporterName = result.getReporterName();
            ProjectVO projectVO = getProjectVO(projectId, ERROR_PROJECT_NOTEXIST);
            StringBuilder url = new StringBuilder();
            if (SUB_TASK.equals(result.getTypeCode())) {
                url.append(getIssueUrl(result, projectVO, result.getParentIssueId()));
            } else {
                url.append(getIssueUrl(result, projectVO, result.getIssueId()));
            }
            List<Long> participantIds = result.getParticipants().stream().map(UserMessageDTO::getId).collect(Collectors.toList());
            siteMsgUtil.issueParticipant(summary, url.toString(), projectId, operatorId, userIds, reporterName, participantIds);
        }
    }

    private boolean checkApplyType(String applyType) {
        return SchemeApplyType.AGILE.equals(applyType) || SchemeApplyType.WATERFALL.equals(applyType) || SchemeApplyType.RISK.equals(applyType);
    }
}
