package io.choerodon.agile.infra.aspect;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.RuleLogRelVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.annotation.DataLog;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.RedisUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.modelmapper.ModelMapper;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 日志切面
 *
 * @author dinghuang123@gmail.com
 * @since 2018/7/23
 */
@Aspect
@Component
@Transactional(rollbackFor = Exception.class)
public class DataLogAspect {

    private static final String ISSUE = "issue";
    private static final String ISSUE_CREATE = "issueCreate";
    private static final String SPRINT = "sprint";
    private static final String VERSION_CREATE = "versionCreate";
    private static final String COMPONENT_CREATE = "componentCreate";
    private static final String COMPONENT_DELETE = "componentDelete";
    private static final String LABEL_DELETE = "labelDelete";
    private static final String LABEL_CREATE = "labelCreate";
    private static final String VERSION_DELETE = "versionDelete";
    private static final String BATCH_DELETE_VERSION = "batchDeleteVersion";
    private static final String BATCH_DELETE_BY_VERSIONID = "batchDeleteByVersionId";
    private static final String BATCH_UPDATE_ISSUE_EPIC_ID = "batchUpdateIssueEpicId";
    private static final String BATCH_VERSION_DELETE_BY_IN_COMPLETE_ISSUE = "batchVersionDeleteByIncompleteIssue";
    private static final String BATCH_DELETE_VERSION_BY_VERSION = "batchDeleteVersionByVersion";
    private static final String BATCH_COMPONENT_DELETE = "batchComponentDelete";
    private static final String BATCH_TO_VERSION = "batchToVersion";
    private static final String BATCH_REMOVE_VERSION = "batchRemoveVersion";
    private static final String BATCH_REMOVE_SPRINT_TO_TARGET = "batchRemoveSprintToTarget";
    private static final String BATCH_TO_EPIC = "batchToEpic";
    private static final String BATCH_REMOVE_SPRINT = "batchRemoveSprint";
    private static final String BATCH_REMOVE_SPRINT_BY_SPRINT_ID = "batchRemoveSprintBySprintId";
    private static final String BATCH_DELETE_LABEL = "batchDeleteLabel";
    private static final String BATCH_UPDATE_ISSUE_STATUS = "batchUpdateIssueStatus";
    private static final String BATCH_UPDATE_ISSUE_STATUS_TO_OTHER = "batchUpdateIssueStatusToOther";
    private static final String BATCH_UPDATE_ISSUE_PRIORITY = "batchUpdateIssuePriority";
    private static final String CREATE_ATTACHMENT = "createAttachment";
    private static final String DELETE_ATTACHMENT = "deleteAttachment";
    private static final String CREATE_COMMENT = "createComment";
    private static final String UPDATE_COMMENT = "updateComment";
    private static final String DELETE_COMMENT = "deleteComment";
    private static final String DELETE_COMMENT_REPLY = "deleteCommentReply";
    private static final String CREATE_WORKLOG = "createWorkLog";
    private static final String DELETE_WORKLOG = "deleteWorkLog";
    private static final String EPIC_NAME_FIELD = "epicName";
    private static final String FIELD_EPIC_NAME = "Epic Name";
    private static final String SUMMARY_FIELD = "summary";
    private static final String DESCRIPTION = "description";
    private static final String FIELD_DESCRIPTION_NULL = "[{\"insert\":\"\n\"}]";
    private static final String FIELD_PRIORITY = "priority";
    private static final String PRIORITY_CODE_FIELD = "priorityId";
    private static final String FIELD_ASSIGNEE = "assignee";
    private static final String ASSIGNEE_ID_FIELD = "assigneeId";
    private static final String REPORTER_ID_FIELD = "reporterId";
    private static final String FIELD_REPORTER = "reporter";
    private static final String FIELD_SPRINT = "Sprint";
    private static final String STORY_POINTS_FIELD = "storyPoints";
    private static final String EPIC_ID_FIELD = "epicId";
    private static final String FIELD_STORY_POINTS = "Story Points";
    private static final String ERROR_PROJECT_INFO_NOT_FOUND = "error.createIssue.projectInfoNotFound";
    private static final String ERROR_EPIC_NOT_FOUND = "error.dataLogEpic.epicNotFound";
    private static final String ERROR_METHOD_EXECUTE = "error.dataLogEpic.methodExecute";
    private static final String FIELD_EPIC_LINK = "Epic Link";
    private static final String FIELD_EPIC_CHILD = "Epic Child";
    private static final String REMAIN_TIME_FIELD = "remainingTime";
    private static final String FIELD_TIMEESTIMATE = "timeestimate";
    private static final String STATUS_ID = "statusId";
    private static final String FIELD_STATUS = "status";
    private static final String FIELD_AUTO_STATUS = "Auto Status";
    private static final String FIELD_RESOLUTION = "resolution";
    private static final String FIELD_AUTO_RESOLUTION = "Auto Resolution";
    private static final String FIELD_AUTO_TRIGGER = "Auto Trigger";
    private static final String RANK_FIELD = "rank";
    private static final String FIELD_RANK = "Rank";
    private static final String RANK_HIGHER = "评级更高";
    private static final String RANK_LOWER = "评级更低";
    private static final String FIELD_ISSUETYPE = "issuetype";
    private static final String FIELD_FIX_VERSION = "Fix Version";
    private static final String FIX_VERSION = "fix";
    private static final String FIELD_VERSION = "Version";
    private static final String FIELD_COMPONENT = "Component";
    private static final String FIELD_LABELS = "labels";
    private static final String FIELD_ATTACHMENT = "Attachment";
    private static final String FIELD_COMMENT = "Comment";
    private static final String FIELD_TIMESPENT = "timespent";
    private static final String FIELD_WORKLOGID = "WorklogId";
    private static final String FIELD_KNOWLEDGE_RELATION = "Knowledge Relation";
    private static final String ERROR_UPDATE = "error.LogDataAspect.update";
    private static final String AGILE = "Agile::";
    private static final String VERSION_CHART = AGILE + "VersionChart";
    private static final String PIECHART = AGILE + "PieChart";
    private static final String BURN_DOWN_COORDINATE_BY_TYPE = AGILE + "BurnDownCoordinateByType";
    private static final String VERSION = "Version";
    private static final String EPIC = "Epic";
    private static final String BATCH_UPDATE_STATUS_ID = "batchUpdateStatusId";
    private static final String KNOWLEDGE_RELATION_CREATE = "knowledgeRelationCreate";
    private static final String KNOWLEDGE_RELATION_DELETE = "knowledgeRelationDelete";
    private static final String ESTIMATED_START_TIME = "estimatedStartTime";
    private static final String FIELD_ESTIMATED_START_TIME = "Estimated Start Time";
    private static final String FIELD_ACTUAL_START_TIME = "Actual Start Time";
    private static final String FIELD_ACTUAL_END_TIME = "Actual End Time";
    private static final String ESTIMATED_END_TIME = "estimatedEndTime";
    private static final String ACTUAL_START_TIME = "actualStartTime";
    private static final String ACTUAL_END_TIME = "actualEndTime";
    private static final String FIELD_ESTIMATED_END_TIME = "Estimated End Time";
    private static final String PROJECT_MOVE = "projectMove";
    private static final String FIELD_PROJECT_MOVE = "Project Move";
    private static final String STATIC_FILE_CREATE = "createStaticFile";
    private static final String STATIC_FILE_DELETE = "deleteStaticFile";
    private static final String STATIC_FILE_REL_CREATE = "createStaticFileRelated";
    private static final String STATIC_FILE_REL_DELETE = "deleteStaticFileRelated";
    private static final String FIELD_STATIC_FILE = "Static File";
    private static final String FIELD_STATIC_FILE_REL = "Static File Rel";
    private static final String ISSUE_TYPE_ID = "issueTypeId";
    private static final String FIELD_ENVIRONMENT = "environment";
    private static final String FIELD_MAIN_RESPONSIBLE = "mainResponsible";
    private static final String MAIN_RESPONSIBLE_ID_FIELD = "mainResponsibleId";
    private static final String PARTICIPANT_DELETE = "deleteParticipant";
    private static final String PARTICIPANT_CREATE = "createParticipant";
    private static final String PARTICIPANT_UPDATE = "updateParticipant";
    private static final String FIELD_PARTICIPANT = "participant";


    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private DataLogService dataLogService;
    @Autowired
    private UserService userService;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired
    private ProductVersionMapper productVersionMapper;
    @Autowired
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private ComponentIssueRelMapper componentIssueRelMapper;
    @Autowired
    private IssueAttachmentMapper issueAttachmentMapper;
    @Autowired
    private DataLogMapper dataLogMapper;
    @Autowired
    private IssueCommentMapper issueCommentMapper;
    @Autowired
    private RedisUtil redisUtil;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private DataLogRedisUtil dataLogRedisUtil;
    @Autowired
    private WorkLogMapper workLogMapper;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private WikiRelationMapper wikiRelationMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired(required = false)
    private AgileTriggerService agileTriggerService;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private StaticFileHeaderMapper staticFileHeaderMapper;
    @Autowired
    private StaticFileIssueRelMapper staticFileIssueRelMapper;
    @Autowired
    private LookupValueService lookupValueService;
    @Autowired
    private IssueParticipantRelMapper issueParticipantRelMapper;

    /**
     * 定义拦截规则：拦截Spring管理的后缀为ServiceImpl的bean中带有@DataLog注解的方法。
     */
    @Pointcut("bean(*ServiceImpl) && @annotation(io.choerodon.agile.infra.annotation.DataLog)")
    public void updateMethodPointcut() {
        throw new UnsupportedOperationException();
    }

    @Around("updateMethodPointcut()")
    public Object interceptor(ProceedingJoinPoint pjp) {
        Object result = null;
        MethodSignature signature = (MethodSignature) pjp.getSignature();
        //获取被拦截的方法
        Method method = signature.getMethod();
        DataLog dataLog = method.getAnnotation(DataLog.class);
        //获取被拦截的方法名
        Object[] args = pjp.getArgs();
        if (dataLog != null && args != null) {
            if (dataLog.single()) {
                switch (dataLog.type()) {
                    case ISSUE:
                        handleIssueDataLog(args);
                        break;
                    case ISSUE_CREATE:
                        result = handleIssueCreateDataLog(pjp);
                        break;
                    case SPRINT:
                        handleSprintDataLog(args);
                        break;
                    case VERSION_CREATE:
                        handleVersionCreateDataLog(args);
                        break;
                    case VERSION_DELETE:
                        handleVersionDeleteDataLog(args);
                        break;
                    case COMPONENT_CREATE:
                        handleComponentCreateDataLog(args);
                        break;
                    case COMPONENT_DELETE:
                        handleComponentDeleteDataLog(args);
                        break;
                    case LABEL_DELETE:
                        result = handleLabelDeleteDataLog(args, pjp);
                        break;
                    case LABEL_CREATE:
                        result = handleLabelCreateDataLog(args, pjp);
                        break;
                    case CREATE_ATTACHMENT:
                        result = handleCreateAttachmentDataLog(args, pjp);
                        break;
                    case DELETE_ATTACHMENT:
                        handleDeleteAttachmentDataLog(args);
                        break;
                    case CREATE_COMMENT:
                        result = handleCreateCommentDataLog(args, pjp);
                        break;
                    case UPDATE_COMMENT:
                        handleUpdateCommentDataLog(args);
                        break;
                    case DELETE_COMMENT:
                        handleDeleteCommentDataLog(args);
                        break;
                    case DELETE_COMMENT_REPLY:
                        handleDeleteCommentReplyDataLog(args);
                        break;
                    case CREATE_WORKLOG:
                        result = handleCreateWorkLogDataLog(args, pjp);
                        break;
                    case DELETE_WORKLOG:
                        result = handleDeleteWorkLogDataLog(args);
                        break;
                    case KNOWLEDGE_RELATION_CREATE:
                        handleKnowledgeRelationCreate(args);
                        break;
                    case KNOWLEDGE_RELATION_DELETE:
                        handleKnowledgeRelationDelete(args);
                        break;
                    case PROJECT_MOVE:
                        handlerProjectMove(args);
                        break;
                    case STATIC_FILE_CREATE:
                        result = handleStaticFileCreate(args, pjp);
                        break;
                    case STATIC_FILE_DELETE:
                        handleStaticFileDelete(args);
                        break;
                    case STATIC_FILE_REL_CREATE:
                        handleStaticFileRelCreate(args, pjp);
                        break;
                    case STATIC_FILE_REL_DELETE:
                        handleStaticFileRelDelete(args);
                        break;
                    case PARTICIPANT_CREATE:
                        handlerParticipantCreate(args);
                        break;
                    case PARTICIPANT_UPDATE:
                        handlerParticipantUpdate(args);
                        break;
                    case PARTICIPANT_DELETE:
                        handlerParticipantDelete(args);
                        break;
                    default:
                        break;
                }
            } else {
                switch (dataLog.type()) {
                    case BATCH_TO_VERSION:
                        batchToVersionDataLog(args);
                        break;
                    case BATCH_REMOVE_VERSION:
                        batchRemoveVersionDataLog(args);
                        break;
                    case BATCH_TO_EPIC:
                        batchToEpicDataLog(args);
                        break;
                    case BATCH_COMPONENT_DELETE:
                        batchComponentDeleteDataLog(args);
                        break;
                    case BATCH_REMOVE_SPRINT:
                        batchRemoveSprintDataLog(args);
                        break;
                    case BATCH_REMOVE_SPRINT_TO_TARGET:
                        batchRemoveSprintToTarget(args);
                        break;
                    case BATCH_DELETE_LABEL:
                        batchDeleteLabelDataLog(args);
                        break;
                    case BATCH_DELETE_VERSION:
                        batchDeleteVersionDataLog(args);
                        break;
                    case BATCH_DELETE_VERSION_BY_VERSION:
                        batchDeleteVersionByVersion(args);
                        break;
                    case BATCH_REMOVE_SPRINT_BY_SPRINT_ID:
                        batchRemoveSprintBySprintId(args);
                        break;
                    case BATCH_UPDATE_ISSUE_STATUS:
                        batchUpdateIssueStatusDataLog(args);
                        break;
                    case BATCH_UPDATE_ISSUE_STATUS_TO_OTHER:
                        batchUpdateIssueStatusToOtherDataLog(args);
                        break;
                    case BATCH_VERSION_DELETE_BY_IN_COMPLETE_ISSUE:
                        batchVersionDeleteByInCompleteIssue(args);
                        break;
                    case BATCH_DELETE_BY_VERSIONID:
                        batchDeleteByVersionId(args);
                        break;
                    case BATCH_UPDATE_ISSUE_EPIC_ID:
                        batchUpdateIssueEpicId(args);
                        break;
                    case BATCH_UPDATE_ISSUE_PRIORITY:
                        batchUpdateIssuePriority(args);
                        break;
                    case BATCH_UPDATE_STATUS_ID:
                        batchUpdateIssueStatusId(args);
                        break;
                    default:
                        break;
                }
            }
        } else {
            throw new CommonException(ERROR_UPDATE);
        }
        try {
            // 一切正常的情况下，继续执行被拦截的方法
            if (result == null) {
                result = pjp.proceed();
            }
        } catch (Throwable e) {
            throw new CommonException(ERROR_METHOD_EXECUTE, e);
        }
        return result;
    }

    private void handlerParticipantCreate(Object[] args) {
        Long projectId = (Long) args[1];
        Long issueId = (Long) args[0];
        List<Long> participantIds = (List<Long>) args[2];
        if (!CollectionUtils.isEmpty(participantIds) && issueId != null && projectId != null) {
            List<UserDTO> userDTOS = userService.listUsersByIds(participantIds.toArray(new Long[participantIds.size()]));
            String newString = userDTOS.stream().map(UserDTO::getRealName).collect(Collectors.joining(","));
            String newValue = participantIds.stream().map(String::valueOf).collect(Collectors.joining(","));
            createDataLog(projectId, issueId,
                    FIELD_PARTICIPANT,
                    null,
                    newString,
                    null,
                    newValue);
        }
    }

    private void handlerParticipantUpdate(Object[] args) {
        Long projectId = (Long) args[1];
        Long issueId = (Long) args[0];
        List<Long> participantIds = (List<Long>) args[2];
        if (!CollectionUtils.isEmpty(participantIds) && issueId != null && projectId != null) {
            String oldString = null;
            String oldValue = null;
            List<Long> allUserIds = new ArrayList<>();
            allUserIds.addAll(participantIds);
            //查询原来的参与人
            List<Long> oldParticipants = issueParticipantRelMapper.listByIssueId(projectId, issueId);
            if (!CollectionUtils.isEmpty(oldParticipants)) {
                allUserIds.addAll(oldParticipants);
                oldValue = oldParticipants.stream().map(String::valueOf).collect(Collectors.joining(","));
            }
            List<UserDTO> userDTOS = userService.listUsersByIds(allUserIds.toArray(new Long[allUserIds.size()]));
            String newString = userDTOS.stream()
                    .filter(v -> participantIds.contains(v.getId()))
                    .map(UserDTO::getRealName).collect(Collectors.joining(","));
            String newValue = participantIds.stream().map(String::valueOf).collect(Collectors.joining(","));
            if (!CollectionUtils.isEmpty(oldParticipants)) {
                oldString = userDTOS.stream()
                        .filter(v -> oldParticipants.contains(v.getId()))
                        .map(UserDTO::getRealName).collect(Collectors.joining(","));
            }
            createDataLog(projectId, issueId,
                    FIELD_PARTICIPANT,
                    oldString,
                    newString,
                    oldValue,
                    newValue);
        }
    }

    private void handlerParticipantDelete(Object[] args) {
        Long projectId = (Long) args[1];
        Long issueId = (Long) args[0];
        List<Long> oldParticipants = issueParticipantRelMapper.listByIssueId(projectId, issueId);
        String oldString = null;
        String oldValue = null;
        if (!CollectionUtils.isEmpty(oldParticipants)) {
            List<UserDTO> userDTOS = userService.listUsersByIds(oldParticipants.toArray(new Long[oldParticipants.size()]));
            oldString = userDTOS.stream()
                    .filter(v -> oldParticipants.contains(v.getId()))
                    .map(UserDTO::getRealName).collect(Collectors.joining(","));
            oldValue = oldParticipants.stream().map(String::valueOf).collect(Collectors.joining(","));
        }
        createDataLog(projectId, issueId,
                FIELD_PARTICIPANT,
                oldString,
                null,
                oldValue,
                null);
    }

    private void handleStaticFileRelDelete(Object[] args) {
        StaticFileHeaderDTO staticFileHeaderDTO = null;
        StaticFileIssueRelDTO staticFileIssueRelDTO = null;

        for (Object arg : args) {
            if (arg instanceof StaticFileHeaderDTO) {
                staticFileHeaderDTO = (StaticFileHeaderDTO) arg;
            }else if (arg instanceof StaticFileIssueRelDTO) {
                staticFileIssueRelDTO = (StaticFileIssueRelDTO) arg;
            }
        }
        if (!ObjectUtils.isEmpty(staticFileHeaderDTO) && !ObjectUtils.isEmpty(staticFileIssueRelDTO)) {
            createDataLog(staticFileHeaderDTO.getProjectId(), staticFileIssueRelDTO.getIssueId(), FIELD_STATIC_FILE_REL,
                    staticFileHeaderDTO.getUrl(), null, staticFileHeaderDTO.getId().toString(), null);
        }
    }

    private void handleStaticFileRelCreate(Object[] args, ProceedingJoinPoint pjp) {
        StaticFileHeaderDTO staticFileHeaderDTO = null;
        StaticFileIssueRelDTO staticFileIssueRelDTO = null;
        for (Object arg : args) {
            if (arg instanceof StaticFileHeaderDTO) {
                staticFileHeaderDTO = (StaticFileHeaderDTO) arg;
            }else if (arg instanceof StaticFileIssueRelDTO) {
                staticFileIssueRelDTO = (StaticFileIssueRelDTO) arg;
            }
        }
        if (!ObjectUtils.isEmpty(staticFileHeaderDTO) && !ObjectUtils.isEmpty(staticFileIssueRelDTO)) {
            try {
                createDataLog(staticFileHeaderDTO.getProjectId(), staticFileIssueRelDTO.getIssueId(), FIELD_STATIC_FILE_REL,
                        null, staticFileHeaderDTO.getUrl(), null, staticFileHeaderDTO.getId().toString());
            } catch (Throwable e) {
                throw new CommonException(ERROR_METHOD_EXECUTE, e);
            }
        }
    }

    private void handleStaticFileDelete(Object[] args) {
        StaticFileHeaderDTO staticFileHeaderDTO = null;
        StaticFileIssueRelDTO staticFileIssueRelDTO = null;
        for (Object arg : args) {
            if (arg instanceof StaticFileHeaderDTO) {
                staticFileHeaderDTO = (StaticFileHeaderDTO) arg;
            } else if (arg instanceof StaticFileIssueRelDTO) {
                staticFileIssueRelDTO = (StaticFileIssueRelDTO) arg;
            }
        }
        if (!ObjectUtils.isEmpty(staticFileHeaderDTO) && !ObjectUtils.isEmpty(staticFileIssueRelDTO)) {
            Long fileId = staticFileHeaderDTO.getId();
            Long projectId = staticFileHeaderDTO.getProjectId();
            String url = staticFileHeaderDTO.getUrl();

            staticFileIssueRelDTO.setStaticFileId(fileId);
            List<StaticFileIssueRelDTO> staticFileIssueRelList = staticFileIssueRelMapper.select(staticFileIssueRelDTO);
            if (!CollectionUtils.isEmpty(staticFileIssueRelList)) {
                staticFileIssueRelList.forEach(staticFileIssueRel -> createDataLog(projectId, staticFileIssueRel.getIssueId(), FIELD_STATIC_FILE,
                        url, null, fileId.toString(), null));
            }
        }
    }

    private Object handleStaticFileCreate(Object[] args, ProceedingJoinPoint pjp) {
        StaticFileHeaderDTO staticFileHeaderDTO = null;
        StaticFileIssueRelDTO staticFileIssueRelDTO = null;

        Object result = null;
        for (Object arg : args) {
            if (arg instanceof StaticFileHeaderDTO) {
                staticFileHeaderDTO = (StaticFileHeaderDTO) arg;
            }else if (arg instanceof StaticFileIssueRelDTO) {
                staticFileIssueRelDTO = (StaticFileIssueRelDTO) arg;
            }
        }
        if (!ObjectUtils.isEmpty(staticFileHeaderDTO) && !ObjectUtils.isEmpty(staticFileIssueRelDTO)) {
            try {
                result = pjp.proceed();
                staticFileHeaderDTO = (StaticFileHeaderDTO) result;
                createDataLog(staticFileHeaderDTO.getProjectId(), staticFileIssueRelDTO.getIssueId(), FIELD_STATIC_FILE,
                        null, staticFileHeaderDTO.getUrl(), null, staticFileHeaderDTO.getId().toString());
            } catch (Throwable e) {
                throw new CommonException(ERROR_METHOD_EXECUTE, e);
            }
        }
        return result;
    }

    private void handleDeleteCommentReplyDataLog(Object[] args) {
        IssueCommentDTO issueCommentDTO = null;
        for (Object arg : args) {
            if (arg instanceof IssueCommentDTO) {
                issueCommentDTO = (IssueCommentDTO) arg;
            }
        }
        if (issueCommentDTO != null) {
            createDataLog(issueCommentDTO.getProjectId(), issueCommentDTO.getIssueId(), FIELD_COMMENT,
                    issueCommentDTO.getCommentText(), null, issueCommentDTO.getCommentId().toString(), null);
            IssueCommentDTO childRecord = new IssueCommentDTO();
            childRecord.setProjectId(issueCommentDTO.getProjectId());
            childRecord.setParentId(issueCommentDTO.getCommentId());
            List<IssueCommentDTO> childCommentList = issueCommentMapper.select(childRecord);
            if (!CollectionUtils.isEmpty(childCommentList)) {
                childCommentList.forEach(childComment -> createDataLog(childComment.getProjectId(), childComment.getIssueId(), FIELD_COMMENT,
                        childComment.getCommentText(), null, childComment.getCommentId().toString(), null));
            }
        }
    }

    private void handlerProjectMove(Object[] args) {
        IssueDTO issueDTO = (IssueDTO) args[0];
        if (!ObjectUtils.isEmpty(issueDTO)) {
            IssueDTO olderIssue = issueMapper.selectByPrimaryKey(issueDTO.getIssueId());
            Long projectId = olderIssue.getProjectId();
            Long targetProjectId = issueDTO.getProjectId();
            if (Objects.equals(projectId, targetProjectId)) {
                return;
            }
            String olderValue = null;
            String newValue = null;
            String olderString = null;
            String newString = null;
            Set<Long> projectIds = new HashSet<>();
            projectIds.add(projectId);
            projectIds.add(targetProjectId);
            List<ProjectVO> projectVOS = baseFeignClient.queryByIds(projectIds).getBody();
            if (CollectionUtils.isEmpty(projectVOS) || !Objects.equals(projectVOS.size(),projectIds.size())) {
                throw new CommonException("error.project.not.found");
            }
            Map<Long, ProjectVO> projectVOMap = projectVOS.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
            ProjectVO olderProject = projectVOMap.get(projectId);
            ProjectVO targetProject = projectVOMap.get(targetProjectId);
            if (!ObjectUtils.isEmpty(olderProject)) {
                olderString = olderProject.getName();
                olderValue = projectId.toString();
            }
            if (!ObjectUtils.isEmpty(targetProject)) {
                newString = targetProject.getName();
                newValue = targetProjectId.toString();
            }
            createDataLog(targetProjectId, issueDTO.getIssueId(), FIELD_PROJECT_MOVE, olderString, newString, olderValue, newValue);
        }
    }

    private Object handleDeleteWorkLogDataLog(Object[] args) {
        Long projectId = (Long) args[0];
        Long logId = (Long) args[1];
        if (logId != null && projectId != null) {
            WorkLogDTO query = new WorkLogDTO();
            query.setProjectId(projectId);
            query.setLogId(logId);
            WorkLogDTO workLogDTO = workLogMapper.selectOne(query);
            if (workLogDTO != null) {
                DataLogDTO dataLogDTO = dataLogMapper.selectLastWorkLogById(workLogDTO.getProjectId(), workLogDTO.getIssueId(), FIELD_TIMESPENT);
                if (!ObjectUtils.isEmpty(dataLogDTO)) {
                    String oldString = null;
                    String newString;
                    String oldValue = null;
                    String newValue;
                    oldValue = dataLogDTO.getNewValue();
                    oldString = dataLogDTO.getNewString();
                    BigDecimal newTime = new BigDecimal(dataLogDTO.getNewValue());
                    newValue = newTime.subtract(workLogDTO.getWorkTime()).toString();
                    newString = newTime.subtract(workLogDTO.getWorkTime()).toString();
                    createDataLog(workLogDTO.getProjectId(), workLogDTO.getIssueId(), FIELD_TIMESPENT,
                            oldString, newString, oldValue, newValue);
                }
                createDataLog(workLogDTO.getProjectId(), workLogDTO.getIssueId(), FIELD_WORKLOGID,
                        workLogDTO.getLogId().toString(), null, workLogDTO.getLogId().toString(), null);
            }
        }
        return null;
    }

    private void handleKnowledgeRelationCreate(Object[] args) {
        WikiRelationDTO wikiRelationDTO = (WikiRelationDTO) args[0];
        if (wikiRelationDTO != null) {
            createDataLog(wikiRelationDTO.getProjectId(), wikiRelationDTO.getIssueId(), FIELD_KNOWLEDGE_RELATION,
                    null, wikiRelationDTO.getWikiName(), null, wikiRelationDTO.getSpaceId().toString());
        }
    }

    private void handleKnowledgeRelationDelete(Object[] args) {
        WikiRelationDTO wikiRelationDTO = (WikiRelationDTO) args[0];
        Long projectId = wikiRelationDTO.getProjectId();
        Long id = wikiRelationDTO.getId();
        if (projectId != null && id != null) {
            WikiRelationDTO selectDTO = wikiRelationMapper.selectByPrimaryKey(id);
            createDataLog(selectDTO.getProjectId(), selectDTO.getIssueId(), FIELD_KNOWLEDGE_RELATION,
                    selectDTO.getWikiName(), null, selectDTO.getSpaceId().toString(), null);
        }
    }

    private void batchUpdateIssueStatusId(Object[] args) {
        Long programId = (Long) args[0];
        Long updateStatusId = (Long) args[1];
        List<IssueDTO> issueDTOList = (List<IssueDTO>) args[2];
        if (programId != null && updateStatusId != null && issueDTOList != null && !issueDTOList.isEmpty()) {
            Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(ConvertUtil.getOrganizationId(programId));
            StatusVO newStatus = statusMapDTOMap.get(updateStatusId);
            for (IssueDTO issueDTO : issueDTOList) {
                StatusVO oldStatus = statusMapDTOMap.get(issueDTO.getStatusId());
                createDataLog(programId, issueDTO.getIssueId(), FIELD_STATUS, oldStatus.getName(), newStatus.getName(), oldStatus.getId().toString(), newStatus.getId().toString());
            }
        }
    }

    private void batchUpdateIssueStatusToOtherDataLog(Object[] args) {
        Long projectId = (Long) args[0];
        String applyType = (String) args[1];
        Long issueTypeId = (Long) args[2];
        Long oldStatusId = (Long) args[3];
        Long newStatusId = (Long) args[4];
        Long userId = (Long) args[5];
        if (projectId != null && Objects.nonNull(applyType) && issueTypeId != null && oldStatusId != null && newStatusId != null && !oldStatusId.equals(newStatusId)) {
            StatusVO oldStatus = statusService.queryStatusById(ConvertUtil.getOrganizationId(projectId), oldStatusId);
            StatusVO newStatus = statusService.queryStatusById(ConvertUtil.getOrganizationId(projectId), newStatusId);
            IssueStatusDTO oldStatusDO = issueStatusMapper.selectByStatusId(projectId, oldStatusId);
            IssueStatusDTO newStatusDO = issueStatusMapper.selectByStatusId(projectId, newStatusId);
            List<IssueDTO> issueDTOS = issueMapper.queryIssueWithCompleteInfoByStatusId(projectId, applyType, issueTypeId, oldStatusId);
            if (issueDTOS != null && !issueDTOS.isEmpty()) {
                dataLogMapper.batchCreateChangeStatusLogByIssueDOS(projectId, issueDTOS, userId, oldStatus, newStatus);
                if (!oldStatusDO.getCompleted().equals(newStatusDO.getCompleted())) {
                    dataLogMapper.batchCreateStatusLogByIssueDOS(projectId, issueDTOS, userId, newStatus, newStatusDO.getCompleted());
                }
                dataLogRedisUtil.handleBatchDeleteRedisCacheByChangeStatusId(issueDTOS, projectId);
            }
        }
    }

    private void batchUpdateIssuePriority(Object[] args) {
        Long organizationId = (Long) args[0];
        Long priorityId = (Long) args[1];
        Long changePriorityId = (Long) args[2];
        Long userId = (Long) args[3];
        List<Long> projectIds = (List) args[4];
        if (priorityId != null && Objects.nonNull(changePriorityId) && projectIds != null && !projectIds.isEmpty()) {
            List<PriorityVO> priorityVOList = priorityService.queryByOrganizationIdList(organizationId);
            Map<Long, PriorityVO> priorityMap = priorityVOList.stream().collect(Collectors.toMap(PriorityVO::getId, Function.identity()));
            List<IssueDTO> issueDTOS = issueMapper.queryIssuesByPriorityId(priorityId, projectIds);
            if (issueDTOS != null && !issueDTOS.isEmpty()) {
                dataLogMapper.batchCreateChangePriorityLogByIssueDOs(issueDTOS, userId, priorityMap.get(priorityId).getName(), priorityMap.get(changePriorityId).getName());
            }
        }
    }

    private void batchUpdateIssueEpicId(Object[] args) {
        Long projectId = (Long) args[0];
        Long issueId = (Long) args[1];
        if (projectId != null && issueId != null) {
            IssueDTO query = new IssueDTO();
            query.setProjectId(projectId);
            query.setEpicId(issueId);
            List<IssueDTO> issueDTOList = issueMapper.select(query);
            issueDTOList.forEach(issueDO -> createIssueEpicLog(0L, issueDO));
        }
    }

    @SuppressWarnings("unchecked")
    private void batchRemoveSprintToTarget(Object[] args) {
        Long projectId = (Long) args[0];
        Long sprintId = (Long) args[1];
        List<Long> issueIds = (List<Long>) args[2];
        if (projectId != null && sprintId != null && issueIds != null && !issueIds.isEmpty()) {
            SprintDTO sprintDTO = sprintMapper.selectByPrimaryKey(sprintId);
            SprintNameDTO sprintNameDTO = new SprintNameDTO();
            sprintNameDTO.setSprintId(sprintId);
            sprintNameDTO.setSprintName(sprintDTO.getSprintName());
            for (Long issueId : issueIds) {
                StringBuilder newSprintIdStr = new StringBuilder();
                StringBuilder newSprintNameStr = new StringBuilder();
                List<SprintNameDTO> sprintNames = issueMapper.querySprintNameByIssueId(issueId);
                handleBatchCreateDataLogForSpring(sprintNames, sprintNameDTO, newSprintNameStr, newSprintIdStr, sprintDTO, projectId, issueId);
            }
            dataLogRedisUtil.deleteByBatchRemoveSprintToTarget(sprintId, projectId, null);
        }
    }

    private void batchDeleteByVersionId(Object[] args) {
        Long projectId = (Long) args[0];
        Long versionId = (Long) args[1];
        if (projectId != null && versionId != null) {
            List<VersionIssueDTO> versionIssueRelDOS = productVersionMapper.queryVersionIssueByVersionId(projectId, versionId);
            handleBatchDeleteVersion(versionIssueRelDOS, projectId, versionId);
        }
    }

    private void batchVersionDeleteByInCompleteIssue(Object[] args) {
        Long projectId = (Long) args[0];
        Long versionId = (Long) args[1];
        if (projectId != null && versionId != null) {
            List<VersionIssueDTO> versionIssues = productVersionMapper.queryInCompleteIssueByVersionId(projectId, versionId);
            handleBatchDeleteVersion(versionIssues, projectId, versionId);
        }
    }

    private void batchDeleteVersionByVersion(Object[] args) {
        ProductVersionDTO productVersion = null;
        for (Object arg : args) {
            if (arg instanceof ProductVersionDTO) {
                productVersion = (ProductVersionDTO) arg;
            }
        }
        if (productVersion != null) {
            List<VersionIssueDTO> versionIssues = productVersionMapper.queryIssueForLogByVersionIds(productVersion.getProjectId(), Collections.singletonList(productVersion.getVersionId()));
            handleBatchDeleteVersion(versionIssues, productVersion.getProjectId(), productVersion.getVersionId());
        }
    }

    private void handleBatchDeleteVersion(List<VersionIssueDTO> versionIssues, Long projectId, Long versionId) {
        if (versionIssues != null && !versionIssues.isEmpty()) {
            versionIssues.forEach(versionIssueDO -> {
                String field = FIX_VERSION.equals(versionIssueDO.getRelationType()) ? FIELD_FIX_VERSION : FIELD_VERSION;
                createDataLog(projectId, versionIssueDO.getIssueId(), field,
                        versionIssueDO.getName(), null, versionIssueDO.getVersionId().toString(), null);
            });
            dataLogRedisUtil.deleteByHandleBatchDeleteVersion(projectId, versionId);
        }
    }


    private void batchUpdateIssueStatusDataLog(Object[] args) {
        IssueStatusDTO issueStatus = null;
        for (Object arg : args) {
            if (arg instanceof IssueStatusDTO) {
                issueStatus = (IssueStatusDTO) arg;
            }
        }
        if (issueStatus != null && issueStatus.getCompleted() != null) {
            Long projectId = issueStatus.getProjectId();
            IssueDTO query = new IssueDTO();
            query.setStatusId(issueStatus.getStatusId());
            query.setProjectId(projectId);
            StatusVO statusVO = statusService.queryStatusById(ConvertUtil.getOrganizationId(projectId), issueStatus.getStatusId());
            List<IssueDTO> issueDTOS = issueMapper.select(query);
            if (issueDTOS != null && !issueDTOS.isEmpty()) {
                Long userId = DetailsHelper.getUserDetails().getUserId();
                dataLogMapper.batchCreateStatusLogByIssueDOS(projectId, issueDTOS, userId, statusVO, issueStatus.getCompleted());
                dataLogRedisUtil.handleBatchDeleteRedisCache(issueDTOS, projectId);
            }

        }
    }

    private void handleUpdateCommentDataLog(Object[] args) {
        IssueCommentDTO issueComment = null;
        for (Object arg : args) {
            if (arg instanceof IssueCommentDTO) {
                issueComment = (IssueCommentDTO) arg;
            }
        }
        if (issueComment != null) {
            IssueCommentDTO issueCommentDTO = issueCommentMapper.selectByPrimaryKey(issueComment.getCommentId());
            dataLogRedisUtil.deleteByComponentChange(issueCommentDTO.getProjectId());
            createDataLog(issueCommentDTO.getProjectId(), issueCommentDTO.getIssueId(), FIELD_COMMENT,
                    issueCommentDTO.getCommentText(), issueComment.getCommentText(), issueComment.getCommentId().toString(),
                    issueComment.getCommentId().toString());

        }
    }

    private Object handleCreateWorkLogDataLog(Object[] args, ProceedingJoinPoint pjp) {
        WorkLogDTO workLog = null;
        Object result = null;
        for (Object arg : args) {
            if (arg instanceof WorkLogDTO) {
                workLog = (WorkLogDTO) arg;
            }
        }
        if (workLog != null) {
            try {
                result = pjp.proceed();
                workLog = (WorkLogDTO) result;
                DataLogDTO dataLogDTO = dataLogMapper.selectLastWorkLogById(workLog.getProjectId(), workLog.getIssueId(), FIELD_TIMESPENT);
                String oldString = null;
                String newString;
                String oldValue = null;
                String newValue;
                if (dataLogDTO != null) {
                    oldValue = dataLogDTO.getNewValue();
                    oldString = dataLogDTO.getNewString();
                    BigDecimal newTime = new BigDecimal(dataLogDTO.getNewValue());
                    newValue = newTime.add(workLog.getWorkTime()).toString();
                    newString = newTime.add(workLog.getWorkTime()).toString();
                } else {
                    newValue = workLog.getWorkTime().toString();
                    newString = workLog.getWorkTime().toString();
                }
                createDataLog(workLog.getProjectId(), workLog.getIssueId(), FIELD_TIMESPENT,
                        oldString, newString, oldValue, newValue);
                createDataLog(workLog.getProjectId(), workLog.getIssueId(), FIELD_WORKLOGID,
                        null, workLog.getLogId().toString(), null, workLog.getLogId().toString());
            } catch (Throwable e) {
                throw new CommonException(ERROR_METHOD_EXECUTE, e);
            }
        }
        return result;
    }

    private void batchRemoveSprintBySprintId(Object[] args) {
        Long projectId = (Long) args[0];
        Long sprintId = (Long) args[1];
        if (projectId != null && sprintId != null) {
            List<Long> moveIssueIds = sprintMapper.queryIssueIds(projectId, sprintId);
            //批量将issue从sprint移除,目标冲刺为应设置为0,记录移除冲刺的日志
            handleBatchRemoveSprint(projectId, moveIssueIds, 0L);
        }
    }

    private void handleDeleteCommentDataLog(Object[] args) {
        IssueCommentDTO issueCommentDTO = null;
        for (Object arg : args) {
            if (arg instanceof IssueCommentDTO) {
                issueCommentDTO = (IssueCommentDTO) arg;
            }
        }
        if (issueCommentDTO != null) {
            createDataLog(issueCommentDTO.getProjectId(), issueCommentDTO.getIssueId(), FIELD_COMMENT,
                    issueCommentDTO.getCommentText(), null, issueCommentDTO.getCommentId().toString(), null);
        }
    }

    private Object handleCreateCommentDataLog(Object[] args, ProceedingJoinPoint pjp) {
        IssueCommentDTO issueComment = null;
        Object result = null;
        for (Object arg : args) {
            if (arg instanceof IssueCommentDTO) {
                issueComment = (IssueCommentDTO) arg;
            }
        }
        if (issueComment != null) {
            try {
                result = pjp.proceed();
                issueComment = (IssueCommentDTO) result;
                createDataLog(issueComment.getProjectId(), issueComment.getIssueId(), FIELD_COMMENT,
                        null, issueComment.getCommentText(), null, issueComment.getCommentId().toString());
            } catch (Throwable e) {
                throw new CommonException(ERROR_METHOD_EXECUTE, e);
            }
        }
        return result;
    }

    private void handleDeleteAttachmentDataLog(Object[] args) {
        Long attachmentId = null;
        for (Object arg : args) {
            if (arg instanceof Long) {
                attachmentId = (Long) arg;
            }
        }
        if (attachmentId != null) {
            IssueAttachmentDTO issueAttachmentDTO = issueAttachmentMapper.selectByPrimaryKey(attachmentId);
            createDataLog(issueAttachmentDTO.getProjectId(), issueAttachmentDTO.getIssueId(), FIELD_ATTACHMENT,
                    issueAttachmentDTO.getUrl(), null, issueAttachmentDTO.getAttachmentId().toString(), null);
        }
    }

    private Object handleCreateAttachmentDataLog(Object[] args, ProceedingJoinPoint pjp) {
        IssueAttachmentDTO issueAttachmentDTO = null;
        Object result = null;
        for (Object arg : args) {
            if (arg instanceof IssueAttachmentDTO) {
                issueAttachmentDTO = (IssueAttachmentDTO) arg;
            }
        }
        if (issueAttachmentDTO != null) {
            try {
                result = pjp.proceed();
                issueAttachmentDTO = (IssueAttachmentDTO) result;
                createDataLog(issueAttachmentDTO.getProjectId(), issueAttachmentDTO.getIssueId(), FIELD_ATTACHMENT,
                        null, issueAttachmentDTO.getUrl(), null, issueAttachmentDTO.getAttachmentId().toString());
            } catch (Throwable throwable) {
                throw new CommonException(ERROR_METHOD_EXECUTE, throwable);
            }
        }
        return result;
    }

    private void batchDeleteVersionDataLog(Object[] args) {
        VersionIssueRelDTO versionIssueRel = null;
        for (Object arg : args) {
            if (arg instanceof VersionIssueRelDTO) {
                versionIssueRel = (VersionIssueRelDTO) arg;
            }
        }
        if (versionIssueRel != null) {
            List<ProductVersionDTO> productVersionDTOS = productVersionMapper.queryVersionRelByIssueIdAndTypeArchivedExceptInfluence(
                    versionIssueRel.getProjectId(), versionIssueRel.getIssueId(), versionIssueRel.getRelationType());
            Long issueId = versionIssueRel.getIssueId();
            String field = FIX_VERSION.equals(versionIssueRel.getRelationType()) ? FIELD_FIX_VERSION : FIELD_VERSION;
            productVersionDTOS.forEach(productVersionDO -> createDataLog(productVersionDO.getProjectId(), issueId, field,
                    productVersionDO.getName(), null, productVersionDO.getVersionId().toString(), null));
            dataLogRedisUtil.deleteByBatchDeleteVersionDataLog(versionIssueRel.getProjectId(), productVersionDTOS);
        }
    }

    private void handleVersionDeleteDataLog(Object[] args) {
        VersionIssueRelDTO versionIssueRelDTO = null;
        for (Object arg : args) {
            if (arg instanceof VersionIssueRelDTO) {
                versionIssueRelDTO = (VersionIssueRelDTO) arg;
            }
        }
        if (versionIssueRelDTO != null) {
            String field;
            if (versionIssueRelDTO.getRelationType() == null) {
                field = FIELD_FIX_VERSION;
            } else {
                field = FIX_VERSION.equals(versionIssueRelDTO.getRelationType()) ? FIELD_FIX_VERSION : FIELD_VERSION;
            }
            createDataLog(versionIssueRelDTO.getProjectId(), versionIssueRelDTO.getIssueId(), field,
                    productVersionMapper.selectByPrimaryKey(versionIssueRelDTO.getVersionId()).getName(), null,
                    versionIssueRelDTO.getVersionId().toString(), null);
            dataLogRedisUtil.deleteByHandleBatchDeleteVersion(versionIssueRelDTO.getProjectId(), versionIssueRelDTO.getVersionId());
        }
    }

    private Object handleLabelCreateDataLog(Object[] args, ProceedingJoinPoint pjp) {
        LabelIssueRelDTO labelIssueRel = null;
        Object result = null;
        for (Object arg : args) {
            if (arg instanceof LabelIssueRelDTO) {
                labelIssueRel = (LabelIssueRelDTO) arg;
            }
        }
        if (labelIssueRel != null) {
            result = createLabelDataLog(labelIssueRel.getIssueId(), labelIssueRel.getProjectId(), pjp);
        }
        return result;
    }

    private Object createLabelDataLog(Long issueId, Long projectId, ProceedingJoinPoint pjp) {
        List<IssueLabelDTO> originLabels = issueMapper.selectLabelNameByIssueId(issueId);
        Object result = null;
        try {
            result = pjp.proceed();
            List<IssueLabelDTO> curLabels = issueMapper.selectLabelNameByIssueId(issueId);
            createDataLog(projectId, issueId, FIELD_LABELS, getOriginLabelNames(originLabels),
                    getOriginLabelNames(curLabels), null, null);
            dataLogRedisUtil.deleteByLabelDataLog(projectId);
        } catch (Throwable e) {
            throw new CommonException(ERROR_METHOD_EXECUTE, e);
        }
        return result;
    }


    private Object handleLabelDeleteDataLog(Object[] args, ProceedingJoinPoint pjp) {
        LabelIssueRelDTO labelIssueRelDTO = null;
        Object result = null;
        for (Object arg : args) {
            if (arg instanceof LabelIssueRelDTO) {
                labelIssueRelDTO = (LabelIssueRelDTO) arg;
            }
        }
        if (labelIssueRelDTO != null) {
            result = createLabelDataLog(labelIssueRelDTO.getIssueId(), labelIssueRelDTO.getProjectId(), pjp);
        }
        return result;
    }

    private void batchDeleteLabelDataLog(Object[] args) {
        Long issueId = null;
        for (Object arg : args) {
            if (arg instanceof Long) {
                issueId = (Long) arg;
            }
        }
        if (issueId != null) {
            IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
            List<IssueLabelDTO> originLabels = issueMapper.selectLabelNameByIssueId(issueId);
            createDataLog(issueDTO.getProjectId(), issueId, FIELD_LABELS, getOriginLabelNames(originLabels),
                    null, null, null);
            dataLogRedisUtil.deleteByLabelDataLog(issueDTO.getProjectId());
        }
    }

    private String getOriginLabelNames(List<IssueLabelDTO> originLabels) {
        StringBuilder originLabelNames = new StringBuilder();
        int originIdx = 0;
        for (IssueLabelDTO label : originLabels) {
            if (originIdx == originLabels.size() - 1) {
                originLabelNames.append(label.getLabelName());
            } else {
                originLabelNames.append(label.getLabelName()).append(" ");
            }
        }
        return originLabelNames.length() == 0 ? null : originLabelNames.toString();
    }

    private void batchRemoveSprintDataLog(Object[] args) {
        BatchRemoveSprintDTO batchRemoveSprintDTO = null;
        for (Object arg : args) {
            if (arg instanceof BatchRemoveSprintDTO) {
                batchRemoveSprintDTO = (BatchRemoveSprintDTO) arg;
            }
        }
        if (batchRemoveSprintDTO != null) {
            handleBatchRemoveSprint(batchRemoveSprintDTO.getProjectId(), batchRemoveSprintDTO.getIssueIds(), batchRemoveSprintDTO.getSprintId());
        }
    }

    private void handleBatchRemoveSprint(Long projectId, List<Long> issueIds, Long sprintId) {
        SprintDTO sprintDTO = sprintMapper.selectByPrimaryKey(sprintId);
        for (Long issueId : issueIds) {
            SprintNameDTO activeSprintName = issueMapper.queryActiveSprintNameByIssueId(issueId);
            Long originSprintId = null;
            if (activeSprintName != null) {
                if (sprintId != null && sprintId.equals(activeSprintName.getSprintId())) {
                    continue;
                }
                originSprintId = activeSprintName.getSprintId();
            }
            dataLogRedisUtil.deleteByBatchRemoveSprintToTarget(sprintId, projectId, originSprintId);
            StringBuilder newSprintIdStr = new StringBuilder();
            StringBuilder newSprintNameStr = new StringBuilder();
            List<SprintNameDTO> sprintNames = issueMapper.querySprintNameByIssueId(issueId);
            handleBatchCreateDataLogForSpring(sprintNames, activeSprintName, newSprintNameStr, newSprintIdStr, sprintDTO, projectId, issueId);
        }
    }

    private void handleBatchCreateDataLogForSpring(List<SprintNameDTO> sprintNames, SprintNameDTO activeSprintName,
                                                   StringBuilder newSprintNameStr, StringBuilder newSprintIdStr,
                                                   SprintDTO sprintDTO, Long projectId, Long issueId) {
        String oldSprintIdStr = sprintNames.stream().map(sprintName -> sprintName.getSprintId().toString()).collect(Collectors.joining(","));
        String oldSprintNameStr = sprintNames.stream().map(SprintNameDTO::getSprintName).collect(Collectors.joining(","));
        handleSprintStringBuilder(sprintNames, activeSprintName, newSprintNameStr, newSprintIdStr, sprintDTO);
        String oldString = "".equals(oldSprintNameStr) ? null : oldSprintNameStr;
        String newString = newSprintNameStr.length() == 0 ? null : newSprintNameStr.toString();
        String oldValue = "".equals(oldSprintIdStr) ? null : oldSprintIdStr;
        String newValue = newSprintIdStr.length() == 0 ? null : newSprintIdStr.toString();
        if (!Objects.equals(oldValue, newValue)) {
            createDataLog(projectId, issueId, FIELD_SPRINT, oldString,
                    newString, oldValue, newValue);
        }
    }


    private void handleSprintStringBuilder(List<SprintNameDTO> sprintNames, SprintNameDTO activeSprintName,
                                           StringBuilder newSprintNameStr, StringBuilder newSprintIdStr, SprintDTO sprintDTO) {
        int idx = 0;
        for (SprintNameDTO sprintName : sprintNames) {
            if (activeSprintName != null && activeSprintName.getSprintId().equals(sprintName.getSprintId())) {
                continue;
            }
            if (idx == 0) {
                newSprintNameStr.append(sprintName.getSprintName());
                newSprintIdStr.append(sprintName.getSprintId().toString());
                idx++;
            } else {
                newSprintNameStr.append(",").append(sprintName.getSprintName());
                newSprintIdStr.append(",").append(sprintName.getSprintId().toString());
            }
        }
        if (sprintDTO != null) {
            newSprintIdStr.append(newSprintIdStr.length() == 0 ? sprintDTO.getSprintId().toString() : "," + sprintDTO.getSprintId().toString());
            newSprintNameStr.append(newSprintNameStr.length() == 0 ? sprintDTO.getSprintName() : "," + sprintDTO.getSprintName());
        }
    }


    private void handleComponentDeleteDataLog(Object[] args) {
        ComponentIssueRelDTO componentIssueRelDTO = null;
        for (Object arg : args) {
            if (arg instanceof ComponentIssueRelDTO) {
                componentIssueRelDTO = (ComponentIssueRelDTO) arg;
            }
        }
        if (componentIssueRelDTO != null) {
            createDataLog(componentIssueRelDTO.getProjectId(), componentIssueRelDTO.getIssueId(),
                    FIELD_COMPONENT, issueComponentMapper.selectByPrimaryKey(componentIssueRelDTO.getComponentId()).getName(), null,
                    componentIssueRelDTO.getComponentId().toString(), null);
            dataLogRedisUtil.deleteByComponentChange(componentIssueRelDTO.getProjectId());
        }
    }

    private void batchComponentDeleteDataLog(Object[] args) {
        Long issueId = null;
        for (Object arg : args) {
            if (arg instanceof Long) {
                issueId = (Long) arg;
            }
        }
        if (issueId != null) {
            ComponentIssueRelDTO componentIssueRelDTO = new ComponentIssueRelDTO();
            componentIssueRelDTO.setIssueId(issueId);
            List<ComponentIssueRelDTO> componentIssueRelDTOList = componentIssueRelMapper.select(componentIssueRelDTO);
            if (componentIssueRelDTOList != null && !componentIssueRelDTOList.isEmpty()) {
                componentIssueRelDTOList.forEach(componentIssueRel -> createDataLog(componentIssueRel.getProjectId(), componentIssueRel.getIssueId(),
                        FIELD_COMPONENT, issueComponentMapper.selectByPrimaryKey(componentIssueRel.getComponentId()).getName(), null,
                        componentIssueRel.getComponentId().toString(), null));
                dataLogRedisUtil.deleteByComponentChange(componentIssueRelDTOList.get(0).getProjectId());
            }
        }
    }

    private void handleComponentCreateDataLog(Object[] args) {
        ComponentIssueRelDTO componentIssueRelDTO = null;
        for (Object arg : args) {
            if (arg instanceof ComponentIssueRelDTO) {
                componentIssueRelDTO = (ComponentIssueRelDTO) arg;
            }
        }
        if (componentIssueRelDTO != null) {
            createDataLog(componentIssueRelDTO.getProjectId(), componentIssueRelDTO.getIssueId(), FIELD_COMPONENT,
                    null, issueComponentMapper.selectByPrimaryKey(componentIssueRelDTO.getComponentId()).getName(),
                    null, componentIssueRelDTO.getComponentId().toString());
            dataLogRedisUtil.deleteByComponentChange(componentIssueRelDTO.getProjectId());
        }
    }

    @SuppressWarnings("unchecked")
    private void batchToEpicDataLog(Object[] args) {
        Long projectId = (Long) args[0];
        Long epicId = (Long) args[1];
        List<Long> issueIds = (List<Long>) args[2];
        if (projectId != null && epicId != null && issueIds != null && !issueIds.isEmpty()) {
            List<IssueDTO> issueDTOList = issueMapper.queryIssueEpicInfoByIssueIds(projectId, issueIds);
            issueDTOList.forEach(issueEpic -> createIssueEpicLog(epicId, issueEpic));
            redisUtil.deleteRedisCache(new String[]{
                    BURN_DOWN_COORDINATE_BY_TYPE + projectId + ":" + EPIC + ":" + epicId
            });
        }
    }

    private void handleVersionCreateDataLog(Object[] args) {
        VersionIssueRelDTO versionIssueRel = null;
        for (Object arg : args) {
            if (arg instanceof VersionIssueRelDTO) {
                versionIssueRel = (VersionIssueRelDTO) arg;
            }
        }
        if (versionIssueRel != null) {
            String field;
            if (versionIssueRel.getRelationType() == null) {
                field = FIELD_FIX_VERSION;
            } else {
                field = FIX_VERSION.equals(versionIssueRel.getRelationType()) ? FIELD_FIX_VERSION : FIELD_VERSION;
            }
            dataLogRedisUtil.deleteByHandleBatchDeleteVersion(versionIssueRel.getProjectId(), versionIssueRel.getVersionId());
            createDataLog(versionIssueRel.getProjectId(), versionIssueRel.getIssueId(), field,
                    null, productVersionMapper.selectByPrimaryKey(versionIssueRel.getVersionId()).getName(),
                    null, versionIssueRel.getVersionId().toString());
        }
    }

    @SuppressWarnings("unchecked")
    private void batchRemoveVersionDataLog(Object[] args) {
        Long projectId = null;
        List<Long> issueIds = null;
        for (Object arg : args) {
            if (arg instanceof Long) {
                projectId = (Long) arg;
            } else if (arg instanceof List) {
                issueIds = (List<Long>) arg;
            }
        }
        if (projectId != null && issueIds != null && !issueIds.isEmpty()) {
            handleBatchRemoveVersionDataLog(issueIds, projectId);
        }
    }

    @SuppressWarnings("unchecked")
    private void handleBatchRemoveVersionDataLog(List<Long> issueIds, Long projectId) {
        Map map = new HashMap(issueIds.size());
        for (Long issueId : issueIds) {
            map.put(issueId, productVersionMapper.selectVersionRelsByIssueId(projectId, issueId));
        }
        for (Object object : map.entrySet()) {
            Map.Entry entry = (Map.Entry<Long, List<ProductVersionDTO>>) object;
            Long issueId = Long.parseLong(entry.getKey().toString());
            List<ProductVersionDTO> versionIssueRelDOList = (List<ProductVersionDTO>) entry.getValue();
            for (ProductVersionDTO productVersionDTO : versionIssueRelDOList) {
                String field;
                if (productVersionDTO.getRelationType() == null) {
                    field = FIELD_FIX_VERSION;
                } else {
                    field = FIX_VERSION.equals(productVersionDTO.getRelationType()) ? FIELD_FIX_VERSION : FIELD_VERSION;
                }
                createDataLog(projectId, issueId, field, productVersionDTO.getName(),
                        null, productVersionDTO.getVersionId().toString(), null);
            }
            dataLogRedisUtil.deleteByBatchDeleteVersionDataLog(projectId, versionIssueRelDOList);
        }
    }


    private void batchToVersionDataLog(Object[] args) {
        VersionIssueRelDTO versionIssueRel = null;
        for (Object arg : args) {
            if (arg instanceof VersionIssueRelDTO) {
                versionIssueRel = (VersionIssueRelDTO) arg;
            }
        }
        if (versionIssueRel != null) {
            ProductVersionDTO productVersionDTO = productVersionMapper.selectByPrimaryKey(versionIssueRel.getVersionId());
            if (productVersionDTO == null) {
                throw new CommonException("error.productVersion.get");
            }
            if (versionIssueRel.getIssueIds() != null && !versionIssueRel.getIssueIds().isEmpty()) {
                Long userId = DetailsHelper.getUserDetails().getUserId();
                dataLogMapper.batchCreateVersionDataLog(versionIssueRel.getProjectId(), productVersionDTO, versionIssueRel.getIssueIds(), userId);
                redisUtil.deleteRedisCache(new String[]{VERSION_CHART + productVersionDTO.getProjectId() + ':' + productVersionDTO.getVersionId() + ":" + "*",
                        BURN_DOWN_COORDINATE_BY_TYPE + productVersionDTO.getProjectId() + ":" + VERSION + ":" + productVersionDTO.getVersionId()
                });
            }
        }
    }

    private Object handleIssueCreateDataLog(ProceedingJoinPoint pjp) {
        Object result;
        try {
            result = pjp.proceed();
            IssueConvertDTO issueConvertDTO = (IssueConvertDTO) result;
            if (issueConvertDTO != null) {
                //若创建issue的初始状态为已完成，生成日志
                IssueStatusDTO issueStatusDTO = issueStatusMapper.selectByStatusId(issueConvertDTO.getProjectId(), issueConvertDTO.getStatusId());
                Boolean condition = (issueStatusDTO.getCompleted() != null && issueStatusDTO.getCompleted());
                if (condition) {
                    StatusVO statusVO = statusService.queryStatusById(ConvertUtil.getOrganizationId(issueConvertDTO.getProjectId()), issueConvertDTO.getStatusId());
                    createDataLog(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId(), FIELD_RESOLUTION, null,
                            statusVO.getName(), null, issueStatusDTO.getStatusId().toString());
                }
                if (issueConvertDTO.getEpicId() != null && !issueConvertDTO.getEpicId().equals(0L)) {
                    //选择EPIC要生成日志
                    Long epicId = issueConvertDTO.getEpicId();
                    issueConvertDTO.setEpicId(null);
                    createIssueEpicLog(epicId, modelMapper.map(issueConvertDTO, IssueDTO.class));
                }
                Boolean storyCondition = issueConvertDTO.getStoryPoints() != null && issueConvertDTO.getStoryPoints().compareTo(BigDecimal.ZERO) != 0;
                Boolean remainingTimeCondition = issueConvertDTO.getRemainingTime() != null && issueConvertDTO.getRemainingTime().compareTo(new BigDecimal(0)) > 0;
                if (storyCondition || remainingTimeCondition) {
                    IssueDTO originIssueDTO = new IssueDTO();
                    BeanUtils.copyProperties(issueConvertDTO, originIssueDTO);
                    if (Boolean.TRUE.equals(storyCondition)) {
                        BigDecimal zero = new BigDecimal(0);
                        originIssueDTO.setStoryPoints(zero);
                        handleStoryPointsLog(originIssueDTO, issueConvertDTO);
                    }
                    if (Boolean.TRUE.equals(remainingTimeCondition)) {
                        originIssueDTO.setRemainingTime(null);
                        handleCalculateRemainData(issueConvertDTO, originIssueDTO);
                    }
                }
                dataLogRedisUtil.deleteByHandleIssueCreateDataLog(issueConvertDTO, condition);
            }
        } catch (Throwable e) {
            throw new CommonException(ERROR_METHOD_EXECUTE, e);
        }
        return result;
    }

    private void handleSprintDataLog(Object[] args) {
        IssueSprintRelDTO issueSprintRel = null;
        for (Object arg : args) {
            if (arg instanceof IssueSprintRelDTO) {
                issueSprintRel = (IssueSprintRelDTO) arg;
            }
        }
        if (issueSprintRel != null) {
            SprintDTO sprintDTO = sprintMapper.selectByPrimaryKey(issueSprintRel.getSprintId());
            createDataLog(issueSprintRel.getProjectId(), issueSprintRel.getIssueId(),
                    FIELD_SPRINT, null, sprintDTO.getSprintName(), null, issueSprintRel.getSprintId().toString());
            dataLogRedisUtil.deleteByHandleSprintDataLog(sprintDTO);
        }
    }

    private void handleIssueDataLog(Object[] args) {
        IssueConvertDTO issueConvertDTO = null;
        List<String> field = null;
        for (Object arg : args) {
            if (arg instanceof IssueConvertDTO) {
                issueConvertDTO = (IssueConvertDTO) arg;
            } else if (arg instanceof String[]) {
                field = Arrays.asList((String[]) arg);
            }
        }
        if (issueConvertDTO != null && field != null && !field.isEmpty()) {
            IssueDTO originIssueDTO = issueMapper.selectByPrimaryKey(issueConvertDTO.getIssueId());
            handleIssueEpicName(field, originIssueDTO, issueConvertDTO);
            handleIssueSummary(field, originIssueDTO, issueConvertDTO);
            handleDescription(field, originIssueDTO, issueConvertDTO);
            handlePriority(field, originIssueDTO, issueConvertDTO);
            handleAssignee(field, originIssueDTO, issueConvertDTO);
            handleReporter(field, originIssueDTO, issueConvertDTO);
            handleStoryPoints(field, originIssueDTO, issueConvertDTO);
            handleIssueEpic(field, originIssueDTO, issueConvertDTO);
            handleRemainTime(field, originIssueDTO, issueConvertDTO);
            handleStatus(field, originIssueDTO, issueConvertDTO);
            handleRank(field, originIssueDTO, issueConvertDTO);
            handleType(field, originIssueDTO, issueConvertDTO);
            handleEstimatedTime(field, originIssueDTO, issueConvertDTO);
            handleEnvironment(field, originIssueDTO, issueConvertDTO);
            handleMainResponsible(field, originIssueDTO, issueConvertDTO);
            handleActualTime(field, originIssueDTO, issueConvertDTO);
        }
    }

    private void handleActualTime(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        Long projectId = originIssueDTO.getProjectId();
        Long issueId = originIssueDTO.getIssueId();
        SimpleDateFormat smf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        if (field.contains(ACTUAL_START_TIME)
                && !Objects.equals(originIssueDTO.getActualStartTime(), issueConvertDTO.getActualStartTime())) {
            String originActualStartTime = null;
            String convertActualStartTime = null;
            if (!ObjectUtils.isEmpty(originIssueDTO.getActualStartTime())) {
                originActualStartTime = smf.format(originIssueDTO.getActualStartTime());
            }
            if (!ObjectUtils.isEmpty(issueConvertDTO.getActualStartTime())) {
                convertActualStartTime = smf.format(issueConvertDTO.getActualStartTime());
            }
            createDataLog(projectId, issueId, FIELD_ACTUAL_START_TIME, originActualStartTime, convertActualStartTime, originActualStartTime, convertActualStartTime);
        }

        if (field.contains(ACTUAL_END_TIME)
                && !Objects.equals(originIssueDTO.getActualEndTime(), issueConvertDTO.getActualEndTime())) {
            String originActualEndTime = null;
            String convertActualEndTime = null;
            if (!ObjectUtils.isEmpty(originIssueDTO.getActualEndTime())) {
                originActualEndTime = smf.format(originIssueDTO.getActualEndTime());
            }
            if (!ObjectUtils.isEmpty(issueConvertDTO.getActualEndTime())) {
                convertActualEndTime = smf.format(issueConvertDTO.getActualEndTime());
            }
            createDataLog(projectId, issueId, FIELD_ACTUAL_END_TIME, originActualEndTime, convertActualEndTime, originActualEndTime, convertActualEndTime);
        }
    }

    private void handleType(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        Long projectId = originIssueDTO.getProjectId();
        if (field.contains(ISSUE_TYPE_ID) && !Objects.equals(originIssueDTO.getIssueTypeId(), issueConvertDTO.getIssueTypeId())) {
            String originTypeName = issueTypeService.queryById(originIssueDTO.getIssueTypeId(), projectId).getName();
            String currentTypeName = issueTypeService.queryById(issueConvertDTO.getIssueTypeId(), projectId).getName();
            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(), FIELD_ISSUETYPE, originTypeName, currentTypeName,
                    originIssueDTO.getIssueTypeId().toString(), issueConvertDTO.getIssueTypeId().toString());
            dataLogRedisUtil.deleteByHandleType(issueConvertDTO, originIssueDTO);
        }
    }

    private void handleRank(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(RANK_FIELD) && originIssueDTO.getRank() != null && issueConvertDTO.getRank() != null && !Objects.equals(originIssueDTO.getRank(), issueConvertDTO.getRank())) {
            if (originIssueDTO.getRank().compareTo(issueConvertDTO.getRank()) < 0) {
                createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                        FIELD_RANK, null, RANK_HIGHER, null, null);
            } else {
                createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                        FIELD_RANK, null, RANK_LOWER, null, null);
            }
        }
    }

    private void handleStatus(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(STATUS_ID) && !Objects.equals(originIssueDTO.getStatusId(), issueConvertDTO.getStatusId())) {
            long organizationId = ConvertUtil.getOrganizationId(originIssueDTO.getProjectId());
            List<StatusVO> statusList = issueStatusMapper.listStatusByIds(
                    originIssueDTO.getProjectId(), organizationId,
                    Stream.of(originIssueDTO.getStatusId(), issueConvertDTO.getStatusId())
                            .collect(Collectors.toList()));
            Map<Long, StatusVO> statusMap = statusList.stream().collect(Collectors.toMap(StatusVO::getId, Function.identity()));
            StatusVO originStatusVO = statusMap.get(originIssueDTO.getStatusId());
            StatusVO currentStatusVO = statusMap.get(issueConvertDTO.getStatusId());

            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                    isTrue(issueConvertDTO.getAutoTranferFlag()) ? FIELD_AUTO_STATUS : FIELD_STATUS, originStatusVO.getName(),
                    currentStatusVO.getName(), originIssueDTO.getStatusId().toString(), issueConvertDTO.getStatusId().toString());
            if (isTrue(issueConvertDTO.getAutoTranferFlag())){
                // 添加自动触发日志
                createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(), FIELD_AUTO_TRIGGER, null,
                        issueConvertDTO.getAutoTriggerNum(), null, issueConvertDTO.getAutoTriggerId().toString());
            }
            Boolean condition = (originStatusVO.getCompleted() != null && originStatusVO.getCompleted()) || (currentStatusVO.getCompleted() != null && currentStatusVO.getCompleted());
            if (Boolean.TRUE.equals(condition)) {
                //生成解决问题日志
                dataLogResolution(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(), originStatusVO, currentStatusVO, issueConvertDTO.getAutoTranferFlag());
            }
            //删除缓存
            dataLogRedisUtil.deleteByHandleStatus(issueConvertDTO, originIssueDTO, condition);
        }
    }

    private boolean isTrue(Boolean flag) {
        if (flag == null) {
            return false;
        }
        return flag;
    }

    private void handleRemainTime(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(REMAIN_TIME_FIELD) && (!Objects.equals(originIssueDTO.getRemainingTime(), issueConvertDTO.getRemainingTime()))) {
            handleCalculateRemainData(issueConvertDTO, originIssueDTO);
        }
    }

    private void handleCalculateRemainData(IssueConvertDTO issueConvertDTO, IssueDTO originIssueDTO) {
        String oldData;
        String newData;
        BigDecimal zero = new BigDecimal(0);
        if (issueConvertDTO.getRemainingTime() != null && issueConvertDTO.getRemainingTime().compareTo(zero) > 0) {
            oldData = originIssueDTO.getRemainingTime() == null ? null : originIssueDTO.getRemainingTime().toString();
            newData = issueConvertDTO.getRemainingTime().toString();
        } else if (issueConvertDTO.getRemainingTime() == null) {
            oldData = originIssueDTO.getRemainingTime() == null ? null : originIssueDTO.getRemainingTime().toString();
            newData = null;
        } else {
            oldData = originIssueDTO.getRemainingTime() == null ? null : originIssueDTO.getRemainingTime().toString();
            newData = zero.toString();
        }
        createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(), FIELD_TIMEESTIMATE, oldData, newData, oldData, newData);
        dataLogRedisUtil.deleteByHandleCalculateRemainData(issueConvertDTO, originIssueDTO);
    }

    private void deleteBurnDownCoordinateByTypeEpic(Long epicId, Long projectId, Long issueId) {
        if (epicId == null && issueId != null) {
            epicId = issueMapper.selectByPrimaryKey(issueId).getEpicId();
        }
        if (epicId != null && epicId != 0) {
            redisUtil.deleteRedisCache(new String[]{
                    BURN_DOWN_COORDINATE_BY_TYPE + projectId + ":" + EPIC + ":" + epicId
            });
        }
    }

    private void handleStoryPoints(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        Boolean condition = field.contains(STORY_POINTS_FIELD) && (!Objects.equals(originIssueDTO.getStoryPoints(), issueConvertDTO.getStoryPoints()));
        if (condition) {
            handleStoryPointsLog(originIssueDTO, issueConvertDTO);
        }
    }

    private void handleStoryPointsLog(IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        String oldString = null;
        String newString = null;
        if (originIssueDTO.getStoryPoints() != null) {
            oldString = originIssueDTO.getStoryPoints().toString();
        }
        if (issueConvertDTO.getStoryPoints() != null) {
            newString = issueConvertDTO.getStoryPoints().toString();
        }
        createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                FIELD_STORY_POINTS, oldString, newString, null, null);
        dataLogRedisUtil.deleteByHandleStoryPoints(issueConvertDTO, originIssueDTO);
    }


    private void handleReporter(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(REPORTER_ID_FIELD) && !Objects.equals(originIssueDTO.getReporterId(), issueConvertDTO.getReporterId())) {
            String oldValue = null;
            String newValue = null;
            String oldString = null;
            String newString = null;
            if (originIssueDTO.getReporterId() != null && originIssueDTO.getReporterId() != 0) {
                oldValue = originIssueDTO.getReporterId().toString();
                oldString = userService.queryUserNameByOption(originIssueDTO.getReporterId(), false).getRealName();
            }
            if (issueConvertDTO.getReporterId() != null && issueConvertDTO.getReporterId() != 0) {
                newValue = issueConvertDTO.getReporterId().toString();
                newString = userService.queryUserNameByOption(issueConvertDTO.getReporterId(), false).getRealName();
            }
            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                    FIELD_REPORTER, oldString, newString, oldValue, newValue);
            dataLogRedisUtil.deleteCustomChart(originIssueDTO.getProjectId());
        }
    }

    private void handleAssignee(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(ASSIGNEE_ID_FIELD) && !Objects.equals(originIssueDTO.getAssigneeId(), issueConvertDTO.getAssigneeId())) {
            String oldValue = null;
            String newValue = null;
            String oldString = null;
            String newString = null;
            if (originIssueDTO.getAssigneeId() != null && originIssueDTO.getAssigneeId() != 0) {
                oldValue = originIssueDTO.getAssigneeId().toString();
                oldString = userService.queryUserNameByOption(originIssueDTO.getAssigneeId(), false).getRealName();
            }
            if (issueConvertDTO.getAssigneeId() != null && issueConvertDTO.getAssigneeId() != 0) {
                newValue = issueConvertDTO.getAssigneeId().toString();
                newString = userService.queryUserNameByOption(issueConvertDTO.getAssigneeId(), false).getRealName();
            }
            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                    FIELD_ASSIGNEE, oldString, newString, oldValue, newValue);
            dataLogRedisUtil.deleteByHandleAssignee(originIssueDTO.getProjectId());
        }
    }

    private void handleMainResponsible(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(MAIN_RESPONSIBLE_ID_FIELD) && !Objects.equals(originIssueDTO.getMainResponsibleId(), issueConvertDTO.getMainResponsibleId())) {
            String oldValue = null;
            String newValue = null;
            String oldString = null;
            String newString = null;
            if (originIssueDTO.getMainResponsibleId() != null && originIssueDTO.getMainResponsibleId() != 0) {
                oldValue = originIssueDTO.getMainResponsibleId().toString();
                oldString = userService.queryUserNameByOption(originIssueDTO.getMainResponsibleId(), false).getRealName();
            }
            if (issueConvertDTO.getMainResponsibleId() != null && issueConvertDTO.getMainResponsibleId() != 0) {
                newValue = issueConvertDTO.getMainResponsibleId().toString();
                newString = userService.queryUserNameByOption(issueConvertDTO.getMainResponsibleId(), false).getRealName();
            }
            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                    FIELD_MAIN_RESPONSIBLE, oldString, newString, oldValue, newValue);
            dataLogRedisUtil.deleteCustomChart(originIssueDTO.getProjectId());
        }
    }

    private void handlePriority(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(PRIORITY_CODE_FIELD) && !Objects.equals(originIssueDTO.getPriorityId(), issueConvertDTO.getPriorityId())) {
            PriorityVO originPriorityVO = priorityService.queryById(ConvertUtil.getOrganizationId(originIssueDTO.getProjectId()), originIssueDTO.getPriorityId());
            PriorityVO currentPriorityVO = priorityService.queryById(ConvertUtil.getOrganizationId(originIssueDTO.getProjectId()), issueConvertDTO.getPriorityId());
            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                    FIELD_PRIORITY, originPriorityVO.getName()
                    , currentPriorityVO.getName(), originIssueDTO.getPriorityId().toString(), issueConvertDTO.getPriorityId().toString());
            dataLogRedisUtil.deleteByHandlePriority(originIssueDTO.getProjectId());
        }
    }

    private void handleDescription(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(DESCRIPTION) && !Objects.equals(originIssueDTO.getDescription(), issueConvertDTO.getDescription())) {
            if (!FIELD_DESCRIPTION_NULL.equals(issueConvertDTO.getDescription())) {
                createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                        DESCRIPTION, originIssueDTO.getDescription(), issueConvertDTO.getDescription(), null, null);
            } else {
                createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                        DESCRIPTION, originIssueDTO.getDescription(), null, null, null);
            }
        }
    }

    private void handleEstimatedTime(List<String> field,
                                     IssueDTO originIssueDTO,
                                     IssueConvertDTO issueConvertDTO) {
        Long projectId = originIssueDTO.getProjectId();
        Long issueId = originIssueDTO.getIssueId();
        SimpleDateFormat smf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        if (field.contains(ESTIMATED_START_TIME)
                && !Objects.equals(originIssueDTO.getEstimatedStartTime(), issueConvertDTO.getEstimatedStartTime())) {
            String originEstimatedStartTime = null;
            String convertEstimatedStartTime = null;
            if (!ObjectUtils.isEmpty(originIssueDTO.getEstimatedStartTime())) {
                originEstimatedStartTime = smf.format(originIssueDTO.getEstimatedStartTime());
            }
            if (!ObjectUtils.isEmpty(issueConvertDTO.getEstimatedStartTime())) {
                convertEstimatedStartTime = smf.format(issueConvertDTO.getEstimatedStartTime());
            }
            createDataLog(projectId, issueId, FIELD_ESTIMATED_START_TIME, originEstimatedStartTime, convertEstimatedStartTime, originEstimatedStartTime, convertEstimatedStartTime);
        }

        if (field.contains(ESTIMATED_END_TIME)
                && !Objects.equals(originIssueDTO.getEstimatedEndTime(), issueConvertDTO.getEstimatedEndTime())) {
            String originEstimatedEndTime = null;
            String convertEstimatedEndTime = null;
            if (!ObjectUtils.isEmpty(originIssueDTO.getEstimatedEndTime())) {
                originEstimatedEndTime = smf.format(originIssueDTO.getEstimatedEndTime());
            }
            if (!ObjectUtils.isEmpty(issueConvertDTO.getEstimatedEndTime())) {
                convertEstimatedEndTime = smf.format(issueConvertDTO.getEstimatedEndTime());
            }
            createDataLog(projectId, issueId, FIELD_ESTIMATED_END_TIME, originEstimatedEndTime, convertEstimatedEndTime, originEstimatedEndTime, convertEstimatedEndTime);
        }
    }

    private void handleIssueSummary(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(SUMMARY_FIELD) && !Objects.equals(originIssueDTO.getSummary(), issueConvertDTO.getSummary())) {
            if (IssueTypeCode.FEATURE.value().equals(originIssueDTO.getTypeCode())) {
                dataLogRedisUtil.deleteCustomChart(originIssueDTO.getProjectId());
            }
            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                    SUMMARY_FIELD, originIssueDTO.getSummary(), issueConvertDTO.getSummary(), null, null);
        }
    }

    private void handleIssueEpicName(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(EPIC_NAME_FIELD) && !Objects.equals(originIssueDTO.getEpicName(), issueConvertDTO.getEpicName())) {
            dataLogRedisUtil.deleteCustomChart(originIssueDTO.getProjectId());
            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                    FIELD_EPIC_NAME, originIssueDTO.getEpicName(), issueConvertDTO.getEpicName(), null, null);
        }
    }

    private void handleIssueEpic(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(EPIC_ID_FIELD) && !Objects.equals(originIssueDTO.getEpicId(), issueConvertDTO.getEpicId())) {
            createIssueEpicLog(issueConvertDTO.getEpicId(), originIssueDTO);
        }
    }

    private void handleEnvironment(List<String> field, IssueDTO originIssueDTO, IssueConvertDTO issueConvertDTO) {
        if (field.contains(FIELD_ENVIRONMENT) && !Objects.equals(originIssueDTO.getEnvironment(), issueConvertDTO.getEnvironment())) {
            LookupTypeWithValuesVO environmentValuesVO = lookupValueService.queryLookupValueByCode(FIELD_ENVIRONMENT, originIssueDTO.getProjectId());

            String oldString = null;
            String newString = null;
            if (!Objects.isNull(originIssueDTO.getEnvironment()) && !Objects.equals("", originIssueDTO.getEnvironment())) {
                oldString = environmentValuesVO.getLookupValues().stream()
                        .filter(value -> Objects.equals(value.getValueCode(), originIssueDTO.getEnvironment()))
                        .findFirst().map(LookupValueVO::getName).orElse(null);
            }
            if (!Objects.isNull(issueConvertDTO.getEnvironment()) && !Objects.equals("", issueConvertDTO.getEnvironment())) {
                newString = environmentValuesVO.getLookupValues().stream()
                        .filter(value -> Objects.equals(value.getValueCode(), issueConvertDTO.getEnvironment()))
                        .findFirst().map(LookupValueVO::getName).orElse(null);
            }
            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(),
                    FIELD_ENVIRONMENT, oldString, newString, originIssueDTO.getEnvironment(), issueConvertDTO.getEnvironment());
            dataLogRedisUtil.deleteCustomChart(originIssueDTO.getProjectId());
        }
    }

    private void createIssueEpicLog(Long epicId, IssueDTO originIssueDTO) {
        ProjectInfoDTO projectInfoDTO = projectInfoMapper.queryByProjectId(originIssueDTO.getProjectId());
        if (projectInfoDTO == null) {
            throw new CommonException(ERROR_PROJECT_INFO_NOT_FOUND);
        }
        if ((originIssueDTO.getEpicId() == null || originIssueDTO.getEpicId() == 0)) {
            if (!Objects.equals(epicId, 0L)) {
                dataLogCreateEpicId(epicId, originIssueDTO, projectInfoDTO);
            }
        } else {
            dataLogChangeEpicId(epicId, originIssueDTO, projectInfoDTO);
        }
    }

    private void dataLogResolution(Long projectId, Long issueId, StatusVO originStatusVO, StatusVO currentStatusVO, Boolean autoTranferFlag) {
        Boolean condition = (originStatusVO.getCompleted() == null || !originStatusVO.getCompleted()) || (currentStatusVO.getCompleted() == null || !currentStatusVO.getCompleted());
        if (condition) {
            String oldValue = null;
            String newValue = null;
            String oldString = null;
            String newString = null;
            if (originStatusVO.getCompleted() != null && originStatusVO.getCompleted()) {
                oldValue = originStatusVO.getId().toString();
                oldString = originStatusVO.getName();
            } else if (Boolean.TRUE.equals(currentStatusVO.getCompleted())) {
                newValue = currentStatusVO.getId().toString();
                newString = currentStatusVO.getName();
            }
            createDataLog(projectId, issueId, isTrue(autoTranferFlag)? FIELD_AUTO_RESOLUTION : FIELD_RESOLUTION, oldString, newString, oldValue, newValue);
            redisUtil.deleteRedisCache(new String[]{PIECHART + projectId + ':' + FIELD_RESOLUTION + "*"});
        }
    }

    private void dataLogCreateEpicId(Long epicId, IssueDTO originIssueDTO, ProjectInfoDTO projectInfoDTO) {
        IssueDTO issueEpic = issueMapper.selectByPrimaryKey(epicId);
        if (issueEpic == null) {
            throw new CommonException(ERROR_EPIC_NOT_FOUND);
        } else {
            ProjectInfoDTO epicProject = projectInfoMapper.queryByProjectId(issueEpic.getProjectId());
            if (epicProject == null) {
                throw new CommonException(ERROR_EPIC_NOT_FOUND);
            }
            deleteBurnDownCoordinateByTypeEpic(issueEpic.getIssueId(), projectInfoDTO.getProjectId(), null);
            createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(), FIELD_EPIC_LINK,
                    null, epicProject.getProjectCode() + "-" + issueEpic.getIssueNum(),
                    null, issueEpic.getIssueId().toString());
            createDataLog(epicProject.getProjectId(), epicId, FIELD_EPIC_CHILD,
                    null, projectInfoDTO.getProjectCode() + "-" + originIssueDTO.getIssueNum(),
                    null, originIssueDTO.getIssueId().toString());
            dataLogRedisUtil.deleteByDataLogCreateEpicId(projectInfoDTO.getProjectId(), issueEpic.getIssueId());

        }
    }

    private void dataLogChangeEpicId(Long epicId, IssueDTO originIssueDTO, ProjectInfoDTO projectInfoDTO) {
        IssueDTO oldIssueEpic = issueMapper.selectByPrimaryKey(originIssueDTO.getEpicId());
        if (oldIssueEpic == null) {
            throw new CommonException(ERROR_EPIC_NOT_FOUND);
        } else {
            ProjectInfoDTO oldEpicProject = projectInfoMapper.queryByProjectId(oldIssueEpic.getProjectId());
            if (oldEpicProject == null) {
                throw new CommonException(ERROR_EPIC_NOT_FOUND);
            }
            deleteBurnDownCoordinateByTypeEpic(originIssueDTO.getEpicId(), projectInfoDTO.getProjectId(), null);
            if (epicId == null || epicId == 0) {
                createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(), FIELD_EPIC_LINK,
                        oldEpicProject.getProjectCode() + "-" + oldIssueEpic.getIssueNum(),
                        null, oldIssueEpic.getIssueId().toString(), null);
            } else {
                IssueDTO newIssueEpic = issueMapper.selectByPrimaryKey(epicId);
                if (newIssueEpic == null) {
                    throw new CommonException(ERROR_EPIC_NOT_FOUND);
                } else {
                    ProjectInfoDTO newEpicProject = projectInfoMapper.queryByProjectId(newIssueEpic.getProjectId());
                    if (newEpicProject == null) {
                        throw new CommonException(ERROR_EPIC_NOT_FOUND);
                    }
                    deleteBurnDownCoordinateByTypeEpic(epicId, projectInfoDTO.getProjectId(), null);
                    createDataLog(originIssueDTO.getProjectId(), originIssueDTO.getIssueId(), FIELD_EPIC_LINK,
                            oldEpicProject.getProjectCode() + "-" + oldIssueEpic.getIssueNum(),
                            newEpicProject.getProjectCode() + "-" + newIssueEpic.getIssueNum(),
                            oldIssueEpic.getIssueId().toString(), newIssueEpic.getIssueId().toString());
                    createDataLog(newEpicProject.getProjectId(), epicId, FIELD_EPIC_CHILD,
                            null, projectInfoDTO.getProjectCode() + "-" + originIssueDTO.getIssueNum(),
                            null, originIssueDTO.getIssueId().toString());
                    dataLogRedisUtil.deleteByDataLogCreateEpicId(projectInfoDTO.getProjectId(), newIssueEpic.getIssueId());
                }
            }
            createDataLog(oldEpicProject.getProjectId(), originIssueDTO.getEpicId(), FIELD_EPIC_CHILD,
                    projectInfoDTO.getProjectCode() + "-" + originIssueDTO.getIssueNum(), null,
                    originIssueDTO.getIssueId().toString(), null);
            dataLogRedisUtil.deleteByDataLogCreateEpicId(projectInfoDTO.getProjectId(), oldIssueEpic.getIssueId());
        }
    }

    private DataLogDTO createDataLog(Long projectId, Long issueId, String field, String oldString,
                                     String newString, String oldValue, String newValue) {
        DataLogDTO dataLogDTO = new DataLogDTO();
        dataLogDTO.setProjectId(projectId);
        dataLogDTO.setIssueId(issueId);
        dataLogDTO.setField(field);
        dataLogDTO.setOldString(oldString);
        dataLogDTO.setNewString(newString);
        dataLogDTO.setOldValue(oldValue);
        dataLogDTO.setNewValue(newValue);
        DataLogDTO result = dataLogService.create(dataLogDTO);
        if (agileTriggerService != null) {
            agileTriggerService.insertTriggerLog(result.getLogId(), issueId, projectId, ISSUE);
        }
        return result;
    }

}
