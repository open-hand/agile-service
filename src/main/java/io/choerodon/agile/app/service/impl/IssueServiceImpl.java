package io.choerodon.agile.app.service.impl;

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.collections4.SetUtils;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

import io.choerodon.agile.api.validator.IssueLinkValidator;
import io.choerodon.agile.api.validator.IssueValidator;
import io.choerodon.agile.api.validator.ProductVersionValidator;
import io.choerodon.agile.api.validator.SprintValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.*;
import io.choerodon.agile.api.vo.event.IssuePayload;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.app.assembler.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.app.service.v2.AdvancedParamParserService;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.agile.infra.aspect.DataLogRedisUtil;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.dto.business.IssueSearchDTO;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.enums.search.SearchConstant;
import io.choerodon.agile.infra.feign.operator.DevopsClientOperator;
import io.choerodon.agile.infra.feign.operator.RemoteIamOperator;
import io.choerodon.agile.infra.feign.operator.TestServiceClientOperator;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.statemachineclient.dto.ExecuteResult;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;
import io.choerodon.agile.infra.support.OpenAppIssueSyncConstant;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.asgard.saga.dto.StartInstanceDTO;
import io.choerodon.asgard.saga.feign.SagaClient;
import io.choerodon.core.domain.Page;
import io.choerodon.core.domain.PageInfo;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;

import org.hzero.core.base.AopProxy;
import org.hzero.core.base.BaseConstants;
import org.hzero.core.message.MessageAccessor;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;
import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.hzero.websocket.helper.SocketSendHelper;

/**
 * 敏捷开发Issue
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 20:30:48
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueServiceImpl implements IssueService, AopProxy<IssueService> {

    private static final Logger LOGGER = LoggerFactory.getLogger(IssueServiceImpl.class);
    @Autowired
    private IssueAccessDataService issueAccessDataService;
    @Autowired
    private ComponentIssueRelService componentIssueRelService;
    @Autowired
    private IssueLinkService issueLinkService;
    @Autowired
    private LabelIssueRelService labelIssueRelService;
    @Autowired
    private LabelIssueRelMapper labelIssueRelMapper;
    @Autowired
    private VersionIssueRelService versionIssueRelService;
    @Autowired
    private IssueAssembler issueAssembler;
    @Autowired
    private EpicDataAssembler epicDataAssembler;
    @Autowired
    protected IssueSearchAssembler issueSearchAssembler;
    @Autowired
    private ProductVersionValidator productVersionValidator;
    @Autowired
    private IssueComponentService issueComponentService;
    @Autowired
    private IssueLabelService issueLabelService;
    @Autowired
    protected SprintValidator sprintValidator;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private IssueAttachmentService issueAttachmentService;
    @Autowired
    private IssueLabelMapper issueLabelMapper;
    @Autowired
    private ProductVersionMapper productVersionMapper;
    @Autowired
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private IssueCommentService issueCommentService;
    @Autowired
    private UserService userService;
    @Autowired
    private LookupValueMapper lookupValueMapper;
    @Autowired
    private DataLogService dataLogService;
    @Autowired
    private VersionIssueRelMapper versionIssueRelMapper;
    @Autowired
    private ComponentIssueRelMapper componentIssueRelMapper;
    @Autowired
    private SprintNameAssembler sprintNameAssembler;
    @Autowired
    private IssueLinkTypeMapper issueLinkTypeMapper;
    @Autowired
    private IssueLinkMapper issueLinkMapper;
    @Autowired
    private IssueSprintRelService issueSprintRelService;
    @Autowired
    private SprintService sprintService;
    @Autowired
    private QuickFilterMapper quickFilterMapper;
    @Autowired
    private RedisUtil redisUtil;
    @Autowired
    private UserSettingService userSettingService;
    @Autowired
    private UserSettingMapper userSettingMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private StateMachineClientService stateMachineClientService;
    @Autowired
    private DataLogRedisUtil dataLogRedisUtil;
    @Autowired
    private IssueSprintRelMapper issueSprintRelMapper;
    @Autowired
    private SendMsgUtil sendMsgUtil;
    @Autowired
    private RankMapper rankMapper;
    @Autowired
    private IssueValidator issueValidator;
    @Autowired
    protected PriorityService priorityService;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private PageFieldService pageFieldService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private TestServiceClientOperator testServiceClientOperator;
    @Autowired
    protected RemoteIamOperator remoteIamOperator;
    @Autowired
    private ProjectUtil projectUtil;
    @Autowired
    protected BoardAssembler boardAssembler;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired
    private StarBeaconMapper starBeaconMapper;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired(required = false)
    private AgileTriggerService agileTriggerService;
    @Autowired
    private WikiRelationMapper wikiRelationMapper;
    @Autowired
    private FieldValueMapper fieldValueMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private LinkIssueStatusLinkageService linkIssueStatusLinkageService;
    @Autowired
    private StatusLinkageExecutionLogService statusLinkageExecutionLogService;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired
    private IssuePredecessorService issuePredecessorService;
    @Autowired
    private FilePathService filePathService;
    @Autowired
    private WorkCalendarSubscribeService workCalendarSubscribeService;
    @Autowired
    private WikiRelationService wikiRelationService;
    @Autowired
    private IWorkLogService workLogService;

    private static final String SUB_TASK = "sub_task";
    private static final String ISSUE_EPIC = "issue_epic";
    private static final String ISSUE_NUM = "issueNum";
    private static final String ISSUE_NUM_CONVERT = "issue_num_convert";
    private static final String ISSUE_ID = "issueId";
    private static final String TABLE_ISSUE_ID = "issue_id";
    private static final String ISSUE_MANAGER_TYPE = "模块负责人";
    private static final String TYPE_CODE_FIELD = "typeCode";
    private static final String EPIC_NAME_FIELD = "epicName";
    private static final String COLOR_CODE_FIELD = "colorCode";
    private static final String EPIC_ID_FIELD = "epicId";
    private static final String SPRINT_ID_FIELD = "sprintId";
    private static final String STORY_POINTS_FIELD = "storyPoints";
    private static final String ESTIMATE_TIME_FIELD = "estimateTime";
    private static final String REMAIN_TIME_FIELD = "remainingTime";
    private static final String STATUS_ID = "statusId";
    private static final String PARENT_ISSUE_ID = "parentIssueId";
    private static final String EPIC_SEQUENCE = "epicSequence";
    private static final String ISSUE_TYPE_ID = "issueTypeId";
    private static final String RELATE_ISSUE_ID = "relateIssueId";
    private static final String EPIC_COLOR_TYPE = "epic_color";
    private static final String STORY_TYPE = "story";
    private static final String ASSIGNEE = "assignee";
    private static final String ASSIGNEE_ID = "assigneeId";
    private static final String REPORTER = "reporter";
    private static final String REPORTER_ID = "reporterId";
    private static final String PRIORITY_ID = "priorityId";
    private static final String FEATURE_ID = "featureId";
    private static final String ENVIRONMENT = "environment";
    private static final String MAIN_RESPONSIBLE_ID = "mainResponsibleId";
    private static final String ESTIMATED_START_TIME = "estimatedStartTime";
    private static final String ACTUAL_START_TIME = "actualStartTime";
    private static final String ESTIMATED_END_TIME = "estimatedEndTime";
    private static final String ACTUAL_END_TIME = "actualEndTime";
    private static final String FIELD_RANK = "Rank";
    protected static final String RANK_HIGHER = "评级更高";
    protected static final String RANK_LOWER = "评级更低";
    private static final String RANK_FIELD = "rank";
    private static final String AGILE_SCHEME_CODE = "agile_issue";
    private static final String ERROR_ISSUE_NOT_FOUND = "error.Issue.queryIssue";
    private static final String ERROR_ISSUE_TYPE_NOT_ISSUE_TEST= "error.Issue.type.isNotIssueTest";
    private static final String SEARCH = "search";
    private static final String STORY_MAP = "storymap";
    private static final String AGILE = "agile";
    private static final String TRIGGER_ISSUE_ID = "triggerIssueId";
    private static final String AUTO_TRANSFER_FLAG = "autoTranferFlag";
    private static final String STAR_BEACON_TYPE_ISSUE = "issue";
    private static final String CUSTOM_FIELD = "custom_field";
    private static final String BUG_TYPE = "bug";
    private static final String TASK_TYPE = "task";
    private static final String MY_START_BEACON = "myStarBeacon";
    private static final String PARTICIPANT_IDS = "participantIds";
    private static final String PRODUCT_IDS = "productIds";
    private static final List<String> WORK_BENCH_SEARCH_TYPE = Arrays.asList("myBug", "reportedBug", MY_START_BEACON, "myReported", "myAssigned");
    private static final String[] UPDATE_TYPE_CODE_FIELD_LIST_NO_RANK = new String[]{TYPE_CODE_FIELD, REMAIN_TIME_FIELD, PARENT_ISSUE_ID, EPIC_NAME_FIELD, COLOR_CODE_FIELD, EPIC_ID_FIELD, STORY_POINTS_FIELD, EPIC_SEQUENCE, ISSUE_TYPE_ID, RELATE_ISSUE_ID};
    private static final String[] TRANSFORMED_TASK_FIELD_LIST_NO_RANK = new String[]{TYPE_CODE_FIELD, REMAIN_TIME_FIELD, PARENT_ISSUE_ID, EPIC_NAME_FIELD, COLOR_CODE_FIELD, EPIC_ID_FIELD, STORY_POINTS_FIELD, EPIC_SEQUENCE, ISSUE_TYPE_ID, STATUS_ID};
    private static final String[] COPY_PREDEFINED_FIELDS_NAME = new String[]
            {
                    ASSIGNEE_ID, EPIC_ID_FIELD, STORY_POINTS_FIELD, STATUS_ID,
                    FEATURE_ID, ENVIRONMENT, MAIN_RESPONSIBLE_ID, ESTIMATE_TIME_FIELD, REMAIN_TIME_FIELD,
                    ESTIMATED_START_TIME, ESTIMATED_END_TIME, REPORTER_ID, PRIORITY_ID,
                    ACTUAL_START_TIME, ACTUAL_END_TIME, PARTICIPANT_IDS, PRODUCT_IDS
            };
    private static final String FIX_VERSION = "fixVersion";
    private static final String INFLUENCE_VERSION = "influenceVersion";
    private static final String ORDER_STR = "orderStr";
    private static final String WEBSOCKET_COPY_ISSUE_CODE = "agile-clone-issue";

//    @Value("${services.attachment.url}")
//    private String attachmentUrl;

    private SagaClient sagaClient;

    @Autowired
    private IssueOperateService issueOperateService;

    @Autowired
    private IssueParticipantRelMapper issueParticipantRelMapper;

    @Autowired
    public IssueServiceImpl(SagaClient sagaClient) {
        this.sagaClient = sagaClient;
    }

    public void setSagaClient(SagaClient sagaClient) {
        this.sagaClient = sagaClient;
    }

    @Override
    public void setIssueMapper(IssueMapper issueMapper) {
        this.issueMapper = issueMapper;
    }

    @Autowired
    private IssueLinkAssembler issueLinkAssembler;
    @Autowired
    private IssueLinkValidator issueLinkValidator;
    @Autowired
    private ProjectInfoService projectInfoService;
    @Autowired
    private FieldValueService fieldValueService;
    @Autowired
    private StatusFieldSettingService statusFieldSettingService;
    @Autowired
    private StatusLinkageService statusLinkageService;
    @Autowired
    private StateMachineNodeService stateMachineNodeService;
    @Autowired(required = false)
    protected AgilePluginService agilePluginService;
    @Autowired
    protected WorkLogMapper workLogMapper;
    @Autowired
    private VerifyUpdateUtil verifyUpdateUtil;
    @Autowired
    private StatusMachineTransformMapper statusMachineTransformMapper;
    @Autowired
    private FieldPermissionService fieldPermissionService;
    @Autowired
    private StatusTransferSettingService transferSettingService;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Value("${services.chain.max-depth:20}")
    private int triggerMaxDepth;
    @Autowired
    private IssueParticipantRelService issueParticipantRelService;
    @Autowired
    private IssueProjectMoveService issueProjectMoveService;
    @Autowired(required = false)
    protected AgileWaterfallService agileWaterfallService;
    @Autowired
    private SocketSendHelper socketSendHelper;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private DevopsClientOperator devopsClientOperator;
    @Autowired
    private AdvancedParamParserService advancedParamParserService;

    @Override
    public void afterCreateIssue(Long issueId, IssueConvertDTO issueConvertDTO, IssueCreateVO issueCreateVO, ProjectInfoDTO projectInfoDTO) {
        handleCreateIssueRearAction(issueConvertDTO, issueId, projectInfoDTO, issueCreateVO);
    }

    private void handleCreateIssueRearAction(IssueConvertDTO issueConvertDTO,
                                             Long issueId,
                                             ProjectInfoDTO projectInfoDTO,
                                             IssueCreateVO issueCreateVO) {
        //处理冲刺
        handleCreateSprintRel(issueConvertDTO.getSprintId(), issueConvertDTO.getProjectId(), issueId);
        handleCreateLabelIssue(issueCreateVO.getLabelIssueRelVOList(), issueId);
        handleCreateComponentIssueRel(issueCreateVO.getComponentIssueRelVOList(), projectInfoDTO.getProjectId(), issueId, projectInfoDTO, issueConvertDTO.getAssigneerCondtiion());
        handleCreateVersionIssueRel(issueCreateVO.getVersionIssueRelVOList(), projectInfoDTO.getProjectId(), issueId);
        handleCreateIssueLink(issueCreateVO.getIssueLinkCreateVOList(), projectInfoDTO.getProjectId(), issueId);
        handleCreateTagIssueRel(issueCreateVO.getTags(), projectInfoDTO.getProjectId(), issueId);
        // 处理参与人
        handlerParticipantRel(issueConvertDTO, projectInfoDTO.getProjectId(), issueId);
        // 处理前置依赖
        handlerIssuePredecessors(issueConvertDTO, issueCreateVO.getIssuePredecessors(), projectInfoDTO.getProjectId(), issueId);
        // 处理产品关联
        handleCreateIssueProductRel(issueCreateVO.getProductIds(), projectInfoDTO.getProjectId(), issueId);
    }

    private void handleCreateIssueProductRel(List<Long> productIds, Long projectId, Long issueId) {
        if (agilePluginService != null) {
            agilePluginService.createIssueProductRel(productIds, projectId, issueId);
        }
    }

    private void handlerIssuePredecessors(IssueConvertDTO issueConvertDTO, List<IssuePredecessorVO> issuePredecessors, Long projectId, Long issueId) {
        // 仅创建瀑布工作项时可选
        if (agileWaterfallService == null || !Arrays.asList(IssueTypeCode.WATERFALL_ISSUE_TYPE_CODE).contains(issueConvertDTO.getTypeCode())) {
            return;
        }
        if(CollectionUtils.isNotEmpty(issuePredecessors)) {
            issuePredecessors.forEach(v -> v.setIssueId(issueId));
            issuePredecessorService.updatePredecessors(projectId, issuePredecessors, issueId);
        }
    }

    private void handlerParticipantRel(IssueConvertDTO issueConvertDTO, Long projectId, Long issueId) {
        if (CollectionUtils.isNotEmpty(issueConvertDTO.getParticipantIds())) {
            issueParticipantRelService.createParticipantRel(issueId, projectId, issueConvertDTO.getParticipantIds());
        }
    }

    private void handleCreateTagIssueRel(List<TagVO> tags, Long projectId, Long issueId) {
        if (agilePluginService != null) {
            agilePluginService.createTagIssueRel(tags, projectId, issueId);
        }
    }

    @Override
    public void afterCreateSubIssue(Long issueId, IssueConvertDTO subIssueConvertDTO, IssueSubCreateVO issueSubCreateVO, ProjectInfoDTO projectInfoDTO) {
        IssueCreateVO issueCreateVO = new IssueCreateVO();
        issueCreateVO.setLabelIssueRelVOList(issueSubCreateVO.getLabelIssueRelVOList());
        issueCreateVO.setComponentIssueRelVOList(issueSubCreateVO.getComponentIssueRelVOList());
        issueCreateVO.setVersionIssueRelVOList(issueSubCreateVO.getVersionIssueRelVOList());
        issueCreateVO.setIssueLinkCreateVOList(issueSubCreateVO.getIssueLinkCreateVOList());
        issueCreateVO.setTags(issueSubCreateVO.getTags());
        handleCreateIssueRearAction(subIssueConvertDTO, issueId, projectInfoDTO, issueCreateVO);
    }

    @Override
    public void handleInitIssue(IssueConvertDTO issueConvertDTO, Long statusId, ProjectInfoDTO projectInfoDTO) {
        List<LookupValueDTO> colorList = new ArrayList<>();
        //如果是epic，初始化颜色
        if (ISSUE_EPIC.equals(issueConvertDTO.getTypeCode())) {
            colorList = lookupValueMapper.queryLookupValueByCode(EPIC_COLOR_TYPE).getLookupValues();
        }
        if (agilePluginService != null) {
            agilePluginService.handleInitIssue(colorList, issueConvertDTO);
        }
        if (CollectionUtils.isNotEmpty(colorList)) {
            issueConvertDTO.initializationColor(colorList);
            //排序编号
            Integer sequence = issueMapper.queryMaxEpicSequenceByProject(issueConvertDTO.getProjectId());
            issueConvertDTO.setEpicSequence(sequence == null ? 0 : sequence + 1);
        }
        //初始化创建issue设置issue编号、项目默认设置
        issueConvertDTO.initializationIssue(statusId, projectInfoDTO);
        projectInfoService.updateIssueMaxNum(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueNum());
        //初始化排序
        if (Boolean.TRUE.equals(issueConvertDTO.isIssueRank())) {
            calculationRank(issueConvertDTO.getProjectId(), issueConvertDTO);
        }
        if (Boolean.TRUE.equals(issueConvertDTO.isIssueMapRank())) {
            calculationMapRank(issueConvertDTO);
        }
        issueValidator.verifyStoryPoints(issueConvertDTO);
        setRemainingTime(issueConvertDTO);
        // 处理预计、实际时间
        handleEstimateTimeAndActualTime(issueConvertDTO);
        //处理甘特图创建子缺陷可以选择已关闭冲刺的问题
        //https://choerodon.com.cn/#/agile/work-list/issue?type=project&id=243303070577803264&name=%E7%94%84%E7%9F%A5%E9%A1%B9%E7%9B%AE%E7%BE%A4&category=AGILE&organizationId=1128&paramIssueId=393079285088505856&paramName=yq-5896
        resetSubIssueSprintIfClosed(issueConvertDTO);
    }

    private void resetSubIssueSprintIfClosed(IssueConvertDTO issueConvertDTO) {
        Long sprintId = issueConvertDTO.getSprintId();
        if (sprintId == null) {
            return;
        }
        SprintDTO sprint = sprintMapper.selectByPrimaryKey(sprintId);
        if (sprint == null) {
            return;
        }
        String statusCode = sprint.getStatusCode();
        Long parentId = issueConvertDTO.getParentIssueId();
        Long relateIssueId = issueConvertDTO.getRelateIssueId();
        if ((parentId != null && !Objects.equals(0L, parentId))
                || (relateIssueId != null && !Objects.equals(0L, relateIssueId))) {
            if (SprintStatusCode.CLOSED.equals(statusCode)) {
                //冲刺已关闭，置空
                issueConvertDTO.setSprintId(null);
            }
        }
    }

    private void handleEstimateTimeAndActualTime(IssueConvertDTO issueConvertDTO) {
        issueConvertDTO.setActualStartTime(formatDate(issueConvertDTO.getActualStartTime()));
        issueConvertDTO.setActualEndTime(formatDate(issueConvertDTO.getActualEndTime()));
        issueConvertDTO.setEstimatedStartTime(formatDate(issueConvertDTO.getEstimatedStartTime()));
        issueConvertDTO.setEstimatedEndTime(formatDate(issueConvertDTO.getEstimatedEndTime()));
    }

    private Date formatDate(Date date) {
        if (ObjectUtils.isEmpty(date)) {
            return null;
        }
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.set(Calendar.SECOND, 0);
        return calendar.getTime();
    }

    protected void calculationMapRank(IssueConvertDTO issueConvertDTO) {
        String maxRank = issueMapper.selectMaxRankByProjectId(issueConvertDTO.getProjectId());
        if (maxRank == null) {
            issueConvertDTO.setMapRank(RankUtil.mid());
        } else {
            issueConvertDTO.setMapRank(RankUtil.genNext(maxRank));
        }
    }

    protected void calculationRank(Long projectId, IssueConvertDTO issueConvertDTO) {
        String rank = sprintMapper.queryMaxRank(projectId, issueConvertDTO.getSprintId());
        //处理rank为null的脏数据
        if (StringUtils.isEmpty(rank)) {
            issueConvertDTO.setRank(RankUtil.mid());
        } else {
            issueConvertDTO.setRank(RankUtil.genNext(rank));
        }
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_CREATED, instanceId = "issueId", allFieldCheck = true)
    public IssueVO queryIssueCreate(Long projectId, Long issueId) {
        return queryIssueCreateWithoutRuleNotice(projectId, issueId);
    }

    @Override
    public IssueVO queryIssueCreateWithoutRuleNotice(Long projectId, Long issueId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        final List<IssueAttachmentDTO> issueAttachmentDTOList = issue.getIssueAttachmentDTOList();
        if (CollectionUtils.isNotEmpty(issueAttachmentDTOList)) {
            for (IssueAttachmentDTO issueAttachmentDO : issueAttachmentDTOList) {
                issueAttachmentDO.setUrl(filePathService.generateFullPath(issueAttachmentDO.getUrl()));
            }
        }
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, issue.getApplyType());
        Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
        Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
        IssueVO result = issueAssembler.issueDetailDTOToVO(issue, issueTypeDTOMap, statusMapDTOMap, priorityDTOMap);
        sendMsgUtil.sendMsgByIssueCreate(projectId, result, DetailsHelper.getUserDetails().getUserId());
        setCompletedAndActualCompletedDate(result);
        return result;
    }

    private void setCompletedAndActualCompletedDate(Object result) {
        Long issueId;
        Long projectId;
        Long statusId;
        IssueVO issueVO = null;
        IssueSubVO issueSubVO = null;
        IssueSubListVO issueSubListVO = null;
        if (result instanceof IssueVO) {
            issueVO = (IssueVO) result;
            issueId = issueVO.getIssueId();
            projectId = issueVO.getProjectId();
            statusId = issueVO.getStatusId();
        } else if (result instanceof IssueSubVO) {
            issueSubVO = (IssueSubVO) result;
            issueId = issueSubVO.getIssueId();
            projectId = issueSubVO.getProjectId();
            statusId = issueSubVO.getStatusId();
        } else if (result instanceof IssueSubListVO) {
            issueSubListVO = (IssueSubListVO) result;
            issueId = issueSubListVO.getIssueId();
            projectId = issueSubListVO.getProjectId();
            statusId = issueSubListVO.getStatusId();
        } else {
            return;
        }
        IssueStatusDTO issueStatus = issueStatusMapper.selectByStatusId(projectId, statusId);
        boolean completed = Boolean.TRUE.equals(issueStatus.getCompleted());
        if (issueVO != null) {
            issueVO.setCompleted(completed);
        } else if (issueSubVO != null) {
            issueSubVO.setCompleted(completed);
        } else if (issueSubListVO != null) {
            issueSubListVO.setCompleted(completed);
        }
        Map<Long, Date> completedDateMap =
                issueMapper.selectActuatorCompletedDateByIssueIds(Arrays.asList(issueId), projectId)
                        .stream()
                        .collect(Collectors.toMap(GanttChartVO::getIssueId, GanttChartVO::getActualCompletedDate));
        Date completedDate = completedDateMap.get(issueId);
        if (completedDate != null) {
            if (issueVO != null) {
                issueVO.setActualCompletedDate(completedDate);
            } else if (issueSubVO != null) {
                issueSubVO.setActualCompletedDate(completedDate);
            } else if (issueSubListVO != null) {
                issueSubListVO.setActualCompletedDate(completedDate);
            }
        }
    }

    @Override
    public IssueVO queryIssue(Long projectId, Long issueId, Long organizationId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        IssueDTO issueById = issueMapper.selectByPrimaryKey(issueId);
        if (ObjectUtils.isEmpty(issue)) {
            if (ObjectUtils.isEmpty(issueById)) {
                throw new CommonException("error.issue.null");
            } else {
                throw new CommonException("error.issue.not.existed.in.project");
            }
        }
        issue.setSameParentIssueDTOList(Objects.nonNull(issue.getParentIssueId()) && !Objects.equals(issue.getParentIssueId(), 0L)?
                issueMapper.querySubIssueByIssueId(issue.getParentIssueId()): null);
        issue.setSameParentBugDOList(Objects.nonNull(issue.getRelateIssueId()) && !Objects.equals(issue.getRelateIssueId(), 0L)?
                issueMapper.querySubBugByIssueId(issue.getRelateIssueId()): null);
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(
                    filePathService.generateFullPath(issueAttachmentDO.getUrl())));
        }
        if (agilePluginService != null) {
            agilePluginService.setBusinessAttributes(issue);
        }
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, issue.getApplyType());
        Map<Long, StatusVO> statusMapDTOMap =
                issueStatusMapper.listWithCompleted(projectId, ConvertUtil.getOrganizationId(projectId)).stream()
                        .collect(Collectors.toMap(StatusVO::getId, Function.identity()));
        Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
        IssueVO issueVO = issueAssembler.issueDetailDTOToVO(issue, issueTypeDTOMap, statusMapDTOMap, priorityDTOMap);
        if (agilePluginService != null) {
            agilePluginService.businessIssueDetailDTOToVO(organizationId, issueVO, issue, issueTypeDTOMap, statusMapDTOMap, priorityDTOMap);
        }
        if (agileWaterfallService != null) {
            agileWaterfallService.waterfallIssueDetailDTOToVO(issueVO, issueTypeDTOMap, statusMapDTOMap, priorityDTOMap);
        }
        //设置星标
        setStarBeacon(issueVO);
        return issueVO;
    }

    private void setStarBeacon(IssueVO issue) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        StarBeaconDTO starBeaconDTO = new StarBeaconDTO();
        starBeaconDTO.setUserId(userId);
        starBeaconDTO.setType(STAR_BEACON_TYPE_ISSUE);
        starBeaconDTO.setInstanceId(issue.getIssueId());
        starBeaconDTO.setProjectId(issue.getProjectId());
        issue.setStarBeacon(false);
        if(!Objects.isNull(starBeaconMapper.selectOne(starBeaconDTO))) {
            issue.setStarBeacon(true);
        }
        String typeCode = issue.getTypeCode();
        if (Objects.equals(typeCode, STORY_TYPE) || Objects.equals(typeCode, TASK_TYPE)) {
            //子任务设置星标
            setListStarBeacon(issue.getSubIssueVOList(), starBeaconDTO);
            //子缺陷设置星标
            setListStarBeacon(issue.getSubBugVOList(), starBeaconDTO);
        }
        if (Objects.equals(typeCode, SUB_TASK)) {
            //设置父级星标
            issue.setParentStarBeacon(false);
            if (!Objects.isNull(issue.getParentIssueId())) {
                starBeaconDTO.setInstanceId(issue.getParentIssueId());
                if(!Objects.isNull(starBeaconMapper.selectOne(starBeaconDTO))) {
                    issue.setParentStarBeacon(true);
                }
            }
            //同父级子任务设置星标
            setListStarBeacon(issue.getSameParentIssueVOList(), starBeaconDTO);
        }
        if (Objects.equals(typeCode, BUG_TYPE)) {
            //设置关联星标
            issue.setRelateStarBeacon(false);
            if (!Objects.isNull(issue.getRelateIssueId())) {
                starBeaconDTO.setInstanceId(issue.getRelateIssueId());
                if(!Objects.isNull(starBeaconMapper.selectOne(starBeaconDTO))) {
                    issue.setRelateStarBeacon(true);
                }
            }
            //同父级子缺陷设置星标
            setListStarBeacon(issue.getSameParentBugVOList(), starBeaconDTO);
        }
    }

    private void setListStarBeacon(List<IssueSubListVO> issues, StarBeaconDTO starBeaconDTO) {
        if (!Objects.isNull(issues) && !issues.isEmpty()) {
            List<Long> issueIds = issues.stream().map(IssueSubListVO::getIssueId).collect(Collectors.toList());
            List<Long> starIssueIds = starBeaconMapper.selectStarIssuesByIds(issueIds, Arrays.asList(starBeaconDTO.getProjectId()), starBeaconDTO.getUserId());
            if (!Objects.isNull(starIssueIds) && !starIssueIds.isEmpty()) {
                issues.forEach(issue -> {
                    if (starIssueIds.contains(issue.getIssueId())) {
                        issue.setStarBeacon(true);
                    }
                });
            }
        }
    }

    protected IssueVO queryIssueByUpdate(Long projectId, Long issueId, List<String> fieldList) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(
                    filePathService.generateFullPath(issueAttachmentDO.getUrl())));
        }
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, issue.getApplyType());
        Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
        Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
        IssueVO result = issueAssembler.issueDetailDTOToVO(issue, issueTypeDTOMap, statusMapDTOMap, priorityDTOMap);
        Long operatorId = DetailsHelper.getUserDetails().getUserId();
        sendMsgUtil.sendMsgByIssueAssignee(projectId, fieldList, result, operatorId);
        sendMsgUtil.sendMsgByIssueComplete(projectId, fieldList, result, operatorId);
        sendMsgUtil.sendMsgByIssueParticipant(projectId, fieldList, result, operatorId);
        setCompletedAndActualCompletedDate(result);
        return result;
    }

    @Override
    @Deprecated
    public Page<IssueListFieldKVVO> listIssueWithSub(Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId) {
        if (organizationId == null) {
            organizationId = ConvertUtil.getOrganizationId(projectId);
        }
        //处理用户搜索
        Boolean condition = handleSearchUser(searchVO, projectId);
        boolean isTreeView =
                !Boolean.FALSE.equals(
                        Optional.ofNullable(searchVO.getSearchArgs())
                                .map(x -> x.get("tree"))
                                .orElse(false));
        if (condition) {
            Page<Long> issueIdPage;
            String filterSql = null;
            //处理自定义搜索
            if (CollectionUtils.isNotEmpty(searchVO.getQuickFilterIds())) {
                filterSql = getQuickFilter(searchVO.getQuickFilterIds());
            }
            //处理未匹配的筛选
            boardAssembler.handleOtherArgs(searchVO);
            final String searchSql = filterSql;
            issueIdPage = getIssueIdPage(pageRequest, projectId, searchVO, searchSql, organizationId, isTreeView);
            Page<IssueListFieldKVVO> issueListDTOPage = new Page<>();
            if (CollectionUtils.isNotEmpty(issueIdPage.getContent())) {
                List<Long> issueIds = issueIdPage.getContent();
                Set<Long> childrenIds = new HashSet<>();
                if (isTreeView) {
                    boolean withSubIssues =
                            !Boolean.FALSE.equals(
                                    Optional.ofNullable(searchVO.getSearchArgs())
                                            .map(x -> x.get("withSubIssues"))
                                            .orElse(false));
                    List<IssueDTO> childIssues;
                    if (withSubIssues) {
                        //带上所有的子级
                        childIssues = issueMapper.queryChildrenIdByParentId(issueIds, new HashSet<>(Arrays.asList(projectId)), new SearchVO(), null, null, null);
                    } else {
                        childIssues = issueMapper.queryChildrenIdByParentId(issueIds, new HashSet<>(Arrays.asList(projectId)), searchVO, searchSql, searchVO.getAssigneeFilterIds(), null);
                        if (CollectionUtils.isEmpty(childIssues)) {
                            // 如果要求不筛选出所有子级, 且待导出的子级空, 这里需要塞一个不存在的ID到子级列表里, 就能屏蔽掉子级查询了
                            childrenIds.add(0L);
                        }
                    }
                    childrenIds.addAll(childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toSet()));
                }
                List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(issueIds, childrenIds, false, isTreeView);
                Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
                Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
                Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
                List<Long> allIssueIds = issueDTOList.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
                Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, Arrays.asList(projectId), allIssueIds, false);
                Map<Long, List<WorkLogVO>> workLogVOMap = workLogMapper.queryByIssueIds(Collections.singletonList(projectId), allIssueIds).stream().collect(Collectors.groupingBy(WorkLogVO::getIssueId));
                List<IssueListFieldKVVO> issueListFieldKVVOS = issueAssembler.issueDoToIssueListFieldKVDTO(issueDTOList, priorityMap, statusMapDTOMap, issueTypeDTOMap, foundationCodeValue, workLogVOMap);
                if (!ObjectUtils.isEmpty(agilePluginService) && CollectionUtils.isNotEmpty(issueListFieldKVVOS)) {
                    boolean countSubIssue = Boolean.TRUE.equals(Optional.ofNullable(searchVO.getSearchArgs())
                            .map(x -> x.get("countSubIssue"))
                            .orElse(false));
                    agilePluginService.doToIssueListFieldKVDTO(Arrays.asList(projectId), issueListFieldKVVOS, countSubIssue);
                }
                issueListDTOPage = PageUtil.buildPageInfoWithPageInfoList(issueIdPage,issueListFieldKVVOS);
            }
            return issueListDTOPage;
        } else {
            return new Page<>();
        }
    }

    @Override
    public Page<IssueListFieldKVVO> pagedQueryWorkList(Long projectId,
                                                       SearchParamVO searchParamVO,
                                                       PageRequest pageRequest,
                                                       Long organizationId) {
        if (organizationId == null) {
            organizationId = ConvertUtil.getOrganizationId(projectId);
        }
        //默认为true
        boolean isTreeView = Boolean.TRUE.equals(Optional.ofNullable(searchParamVO.getTreeFlag()).orElse(true));
        String quickFilterSql = getQuickFilter(searchParamVO.getQuickFilterIds());
        Set<Long> projectIds = SetUtils.unmodifiableSet(projectId);
        Map<String, FieldTableVO> predefinedFieldMap = new HashMap<>(SearchConstant.PREDEFINED_FIELD_TABLE_MAP);
        if (agilePluginService != null) {
            predefinedFieldMap.putAll(agilePluginService.queryAdvanceParamFieldTableMap());
        }
        if (backlogExpandService != null) {
            predefinedFieldMap.putAll(backlogExpandService.queryAdvanceParamFieldTableMap());
        }
        String advancedSql = advancedParamParserService.parse(InstanceType.ISSUE, searchParamVO, projectIds, predefinedFieldMap);
        Map<String, Object> sortMap = processSortMap(pageRequest, projectId, organizationId, TableAliasConstant.DEFAULT_ALIAS);
        Page<Long> issueIdPage = pagedQueryRoot(pageRequest, projectId, quickFilterSql, advancedSql, sortMap, isTreeView, false, null);
        Page<IssueListFieldKVVO> issueListDTOPage = new Page<>();
        if (!CollectionUtils.isEmpty(issueIdPage.getContent())) {
            List<Long> issueIds = issueIdPage.getContent();
            Set<Long> childrenIds = new HashSet<>();
            if (isTreeView) {
                boolean withSubIssues = Boolean.TRUE.equals(Optional.ofNullable(searchParamVO.getWithSubIssues()).orElse(false));
                List<IssueDTO> childIssues;
                if (withSubIssues) {
                    //带上所有的子级
                    childIssues = issueMapper.queryChildrenList(issueIds, projectIds, null, null, null, false, null);
                } else {
                    childIssues = issueMapper.queryChildrenList(issueIds, projectIds, quickFilterSql, advancedSql, null, false, null);
                    if (CollectionUtils.isEmpty(childIssues)) {
                        // 如果要求不筛选出所有子级, 且待导出的子级空, 这里需要塞一个不存在的ID到子级列表里, 就能屏蔽掉子级查询了
                        childrenIds.add(0L);
                    }
                }
                childrenIds.addAll(childIssues.stream().map(IssueDTO::getIssueId).collect(Collectors.toSet()));
            }
            List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(issueIds, childrenIds, false, isTreeView);
            Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
            Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
            Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
            List<Long> allIssueIds = issueDTOList.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
            Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, Arrays.asList(projectId), allIssueIds, false);
            Map<Long, List<WorkLogVO>> workLogVOMap = workLogMapper.queryByIssueIds(Collections.singletonList(projectId), allIssueIds).stream().collect(Collectors.groupingBy(WorkLogVO::getIssueId));
            List<IssueListFieldKVVO> issueListFieldKVVOS = issueAssembler.issueDoToIssueListFieldKVDTO(issueDTOList, priorityMap, statusMapDTOMap, issueTypeDTOMap, foundationCodeValue, workLogVOMap);
            if (!ObjectUtils.isEmpty(agilePluginService) && !CollectionUtils.isEmpty(issueListFieldKVVOS)) {
                boolean countSubIssue = Boolean.TRUE.equals(searchParamVO.getCountSubIssue());
                agilePluginService.doToIssueListFieldKVDTO(Arrays.asList(projectId), issueListFieldKVVOS, countSubIssue);
            }
            if(backlogExpandService != null){
                backlogExpandService.addIssueBacklogInfo(organizationId, projectId, issueListFieldKVVOS);
            }
            issueListDTOPage = PageUtil.buildPageInfoWithPageInfoList(issueIdPage,issueListFieldKVVOS);
        }
        return issueListDTOPage;
    }

    @Override
    public Page<Long> pagedQueryRoot(PageRequest pageRequest,
                                     Long projectId,
                                     String quickFilterSql,
                                     String advancedSql,
                                     Map<String, Object> sortMap,
                                     boolean isTreeView,
                                     boolean ganttDefaultOrder,
                                     String dimension) {
        Page<IssueDTO> issuePage;
        if (isTreeView) {
            issuePage =
                    PageHelper.doPage(pageRequest, () -> issueMapper.queryRootList(SetUtils.unmodifiableSet(projectId), quickFilterSql, advancedSql, sortMap, ganttDefaultOrder, dimension));
        } else {
            issuePage =
                    PageHelper.doPage(pageRequest, () -> issueMapper.queryIssueList(SetUtils.unmodifiableSet(projectId), quickFilterSql, advancedSql, sortMap, ganttDefaultOrder, dimension));
        }
        List<Long> issueIds = issuePage.getContent().stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        return PageUtil.buildPageInfoWithPageInfoList(issuePage, issueIds);
    }


    private Page<Long> getIssueIdPage(PageRequest pageRequest, Long projectId, SearchVO searchVO, String searchSql, Long organizationId, Boolean isTreeView) {
        Map<String, Object> sortMap = processSortMap(pageRequest, projectId, organizationId, TableAliasConstant.DEFAULT_ALIAS);
        return pagedQueryByTreeView(pageRequest, new HashSet<>(Arrays.asList(projectId)), searchVO, searchSql, sortMap, isTreeView);
    }

    @Override
    public Map<String, Object> processSortMap(PageRequest pageRequest,
                                              Long projectId,
                                              Long organizationId,
                                              String alias) {
        Map<String, Object> sortMap = new HashMap<>();
        if (ObjectUtils.isEmpty(pageRequest.getSort())) {
            return sortMap;
        }
        if (!handleSortField(pageRequest).equals(StringUtils.EMPTY)) {
            setSortMap(organizationId, projectId, pageRequest, sortMap, alias);
        } else {
            String orderStr = getOrderStrOfQueryingIssuesWithSub(pageRequest.getSort());
            sortMap.put(ORDER_STR, orderStr);
        }
        return sortMap;
    }

    @Override
    @Deprecated
    public Page<Long> pagedQueryByTreeView(PageRequest pageRequest,
                                           Set<Long> projectIds,
                                           SearchVO searchVO,
                                           String searchSql,
                                           Map<String, Object> sortMap,
                                           boolean isTreeView) {
        splitIssueNumProjectCodePrefix(searchVO, projectIds);
        Page<IssueDTO> issuePage;
        if (isTreeView) {
            issuePage =
                    PageHelper.doPage(pageRequest, () -> issueMapper.queryParentIssueIdsList(projectIds, searchVO, searchSql, searchVO.getAssigneeFilterIds(), sortMap));
        } else {
            issuePage =
                    PageHelper.doPage(pageRequest, () -> issueMapper.queryIssueIdsList(projectIds, searchVO, searchSql, searchVO.getAssigneeFilterIds(), sortMap));
        }
        List<Long> issueIds = issuePage.getContent().stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        return PageUtil.buildPageInfoWithPageInfoList(issuePage, issueIds);
    }

    @Override
    public List<Long> listByTreeView(Set<Long> projectIds,
                                     SearchVO searchVO,
                                     String searchSql,
                                     Map<String, Object> sortMap,
                                     boolean isTreeView) {
        splitIssueNumProjectCodePrefix(searchVO, projectIds);
        if (isTreeView) {
            return issueMapper.queryParentIssueIdsList(projectIds, searchVO, searchSql, searchVO.getAssigneeFilterIds(), sortMap)
                    .stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        } else {
            return issueMapper.queryIssueIdsList(projectIds, searchVO, searchSql, searchVO.getAssigneeFilterIds(), sortMap)
                    .stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        }
    }

    @Override
    public List<Long> listByTreeViewV2(Set<Long> projectIds,
                                       String quickFilterSql,
                                       String advancedSql,
                                       Map<String, Object> sortMap,
                                       boolean isTreeView,
                                       boolean ganttDefaultOrder,
                                       String dimension) {
        if (isTreeView) {
            return issueMapper.queryRootList(projectIds, quickFilterSql, advancedSql, sortMap, ganttDefaultOrder, dimension)
                    .stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        } else {
            return issueMapper.queryIssueList(projectIds, quickFilterSql, advancedSql, sortMap, ganttDefaultOrder, dimension)
                    .stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        }
    }

    @Override
    public void splitIssueNumProjectCodePrefix(SearchVO searchVO, Set<Long> projectIds) {
        //去除searchVO.searchArgs.issueNum或searchVO.contents的项目code前缀
        List<ProjectInfoDTO> projectInfos = projectInfoMapper.selectByProjectIds(projectIds);
        if (projectInfos.isEmpty()) {
            return;
        }
        List<String> projectCodes = projectInfos.stream().map(ProjectInfoDTO::getProjectCode).collect(Collectors.toList());
        List<String> contents = searchVO.getContents();
        if (!ObjectUtils.isEmpty(contents)) {
            List<String> replaceContents = new ArrayList<>();
            contents.forEach(content -> {
                projectCodes.forEach(projectCode -> {
                    String prefix = projectCode + BaseConstants.Symbol.MIDDLE_LINE;
                    if (content.startsWith(prefix)) {
                        replaceContents.add(content.substring(prefix.length()));
                    } else {
                        replaceContents.add(content);
                    }
                });
            });
            searchVO.setContents(replaceContents);
        }
        Map<String, Object> searchArgs = searchVO.getSearchArgs();
        if (!ObjectUtils.isEmpty(searchArgs)) {
            String issueNum = (String) searchArgs.get("issueNum");
            if (!StringUtils.isEmpty(issueNum)) {
                projectCodes.forEach(projectCode -> {
                    String prefix = projectCode + BaseConstants.Symbol.MIDDLE_LINE;
                    if (issueNum.startsWith(prefix)) {
                        searchArgs.put("issueNum", issueNum.substring(prefix.length()));
                        return;
                    }
                });
            }
        }
    }

    @Override
    public void setSortMap(Long organizationId,
                           Long projectId,
                           PageRequest pageRequest,
                           Map<String, Object> sortMap,
                           String mainTableAlias) {
        Sort.Order issueIdOrder = new Sort.Order(Sort.Direction.DESC, ISSUE_ID);
        Sort sort = PageUtil.sortResetOrder(new Sort(issueIdOrder), mainTableAlias, new HashMap<>());
        String orderStr = PageableHelper.getSortSql(sort);
        sortMap.put(ORDER_STR,  orderStr);

        String sortCode = handleSortField(pageRequest);
        ObjectSchemeFieldDTO field = generateSortField(sortCode, projectId);
        String fieldCode = field.getCode();
        projectId = field.getProjectId();
        ObjectSchemeFieldDTO objectSchemeField = objectSchemeFieldService.queryByFieldCode(organizationId, projectId, fieldCode);
        if (Objects.isNull(objectSchemeField)) {
            return;
        }
        String fieldType = objectSchemeField.getFieldType();
        FieldValueUtil.handleAgileSortPageRequest(sortCode, fieldType, pageRequest);
        PageUtil.sortResetOrder(pageRequest.getSort(), "fv", new HashMap<>());
        pageRequest.setSort(pageRequest.getSort().and(sort));
        orderStr = PageableHelper.getSortSql(pageRequest.getSort());

        List<ObjectSchemeFieldExtendDTO> fieldExtendDTOList = objectSchemeFieldExtendMapper.selectExtendFields(organizationId, objectSchemeField.getId(), projectId, null);
        List<Long> fieldExtendIssueTypeIds = fieldExtendDTOList.stream().map(ObjectSchemeFieldExtendDTO::getIssueTypeId).collect(Collectors.toList());
        sortMap.put("sortFieldId", objectSchemeField.getId());
        sortMap.put("fieldExtendIssueTypeIds", fieldExtendIssueTypeIds);
        sortMap.put(ORDER_STR, orderStr);
    }

    private ObjectSchemeFieldDTO generateSortField(String sortCode,
                                                   Long projectId) {
        //foundation.fieldCode.projectId
        String[] sortArray = sortCode.split("\\.");
        int len = sortArray.length;
        String fieldCode;
        if (len == 2) {
            fieldCode = sortArray[1];
        } else if (len == 3) {
            fieldCode = sortArray[1];
            String projectIdStr = sortArray[2];
            if (!org.apache.commons.lang3.StringUtils.isNumeric(projectIdStr)) {
                throw new CommonException("error.illegal.sort.custom.field.projectId");
            }
            projectId = Long.parseLong(projectIdStr);
        } else {
            throw new CommonException("error.illegal.sort.custom.field");
        }
        ObjectSchemeFieldDTO dto = new ObjectSchemeFieldDTO();
        dto.setCode(fieldCode);
        dto.setProjectId(projectId);
        return dto;
    }

    protected String getOrderStrOfQueryingIssuesWithSub(Sort sort) {
        Map<String, String> order = new HashMap<>();
        order.put(ISSUE_ID, TABLE_ISSUE_ID);
        order.put(ISSUE_NUM, ISSUE_NUM_CONVERT);
        if (Objects.isNull(sort.getOrderFor(ISSUE_ID))) {
            Sort.Order issueIdOrder = new Sort.Order(Sort.Direction.DESC, ISSUE_ID);
            sort = sort.and(new Sort(issueIdOrder));
        }
        return PageableHelper.getSortSql(PageUtil.sortResetOrder(sort, null, order));
    }

    @Override
    public String handleSortField(PageRequest pageRequest) {
        if (!ObjectUtils.isEmpty(pageRequest.getSort())) {
            Iterator<Sort.Order> iterator = pageRequest.getSort().iterator();
            String fieldCode = StringUtils.EMPTY;
            while (iterator.hasNext()) {
                Sort.Order order = iterator.next();
                fieldCode = order.getProperty();
            }
            if (fieldCode.contains("foundation.")) {
                return fieldCode;
            } else return StringUtils.EMPTY;
        } else return StringUtils.EMPTY;
    }

    /**
     * @see BoardAssembler#handleOtherArgs(io.choerodon.agile.api.vo.SearchVO)
     * @deprecated {@link io.choerodon.agile.app.assembler.BoardAssembler#handleOtherArgs(io.choerodon.agile.api.vo.SearchVO)}
     * @param searchVO searchVO
     */
    @Deprecated
    protected void handleOtherArgs(SearchVO searchVO) {
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (otherArgs != null) {
            List<String> list = (List<String>) otherArgs.get("sprint");
            handlerNullValue(list, "sprintNull", otherArgs);

            list = (List<String>) otherArgs.get("version");
            handlerNullValue(list, "versionNull", otherArgs);

            list = (List<String>) otherArgs.get("component");
            handlerNullValue(list, "componentNull", otherArgs);

            list = (List<String>) otherArgs.get("epic");
            handlerNullValue(list, "epicNull", otherArgs);

            list = (List<String>) otherArgs.get("label");
            handlerNullValue(list, "labelNull", otherArgs);

            list = (List<String>) otherArgs.get(ASSIGNEE_ID);
            handlerNullValue(list, "assigneeIdNull", otherArgs);
        }
    }

    private void handlerNullValue(List<String> list, String key, Map<String, Object> otherArgs){
        if (list != null && list.contains("0")) {
            otherArgs.put(key, true);
        }
    }

    @Override
    public boolean handleSearchUser(SearchVO searchVO, Long projectId) {
        if (searchVO.getSearchArgs() != null && searchVO.getSearchArgs().get(ASSIGNEE) != null) {
            String userName = (String) searchVO.getSearchArgs().get(ASSIGNEE);
            if (!handlerUserSearch(projectId, userName, searchVO, "assigneeIds")) {
                return false;
            }
        }
        if (searchVO.getSearchArgs() != null && searchVO.getSearchArgs().get(REPORTER) != null) {
            String userName = (String) searchVO.getSearchArgs().get(REPORTER);
            if (!handlerUserSearch(projectId, userName, searchVO, "reporterIds")) {
                return false;
            }
        }
        return true;
    }

    private boolean handlerUserSearch(Long projectId, String userName, SearchVO searchVO, String key) {
        if (StringUtils.isNotBlank(userName)) {
            List<UserVO> userVOS = userService.queryUsersByNameAndProjectId(projectId, userName);
            if (CollectionUtils.isNotEmpty(userVOS)) {
                searchVO.getAdvancedSearchArgs().put(key, userVOS.stream().map(UserVO::getId).collect(Collectors.toList()));
            } else {
                return false;
            }
        }
        return true;
    }

    @Override
    public IssueVO updateIssue(Long projectId, IssueUpdateVO issueUpdateVO, List<String> fieldList) {
        validateBeforeUpdate(projectId, issueUpdateVO, fieldList);
        if (agilePluginService != null) {
            agilePluginService.buildFieldList(fieldList, issueUpdateVO);
        }
        if (agileWaterfallService != null) {
            agileWaterfallService.buildWaterfallFieldList(fieldList, issueUpdateVO);
        }
        //更新issue表字段，fieldList包含issueId，objectVersionNumber和一个field
        boolean updateRelationField =
                fieldList.size() == 2
                        && fieldList.contains(IssueDTO.FIELD_ISSUE_ID)
                        && fieldList.contains(IssueDTO.FIELD_OBJECT_VERSION_NUMBER);
        if (CollectionUtils.isNotEmpty(fieldList)) {
            if (updateRelationField) {
                handleUpdateIssue(issueUpdateVO, fieldList, projectId, issueUpdateVO.getIssueId());
            } else {
                this.self().handleUpdateIssue(issueUpdateVO, fieldList, projectId, issueUpdateVO.getIssueId());
            }
        }
        Long issueId = postUpdateIssue(projectId, issueUpdateVO);
        if (issueUpdateVO.getParticipantIds() != null) {
           fieldList.add("participantIds");
        }
        return queryIssueByUpdate(projectId, issueId, fieldList);
    }

    @Override
    public IssueVO updateIssueWithoutRuleNotice(Long projectId,
                                                IssueUpdateVO issueUpdateVO,
                                                List<String> fieldList) {
        validateBeforeUpdate(projectId, issueUpdateVO, fieldList);
        if (agilePluginService != null) {
            agilePluginService.buildFieldList(fieldList, issueUpdateVO);
        }
        if (agileWaterfallService != null) {
            agileWaterfallService.buildWaterfallFieldList(fieldList, issueUpdateVO);
        }
        if (!fieldList.isEmpty()) {
            //处理issue自己字段
            handleUpdateIssueWithoutRuleNotice(issueUpdateVO, fieldList, projectId);
        }
        Long issueId = postUpdateIssueWithoutRuleNotice(projectId, issueUpdateVO);
        if (issueUpdateVO.getParticipantIds() != null) {
            fieldList.add("participantIds");
        }
        return queryIssueByUpdate(projectId, issueId, fieldList);
    }


    private Long postUpdateIssue(Long projectId, IssueUpdateVO issueUpdateVO) {
        Long issueId = issueUpdateVO.getIssueId();
        if (issueUpdateVO.getLabelIssueRelVOList() != null) {
            this.self().handleUpdateLabelIssue(issueUpdateVO.getLabelIssueRelVOList(), issueId, projectId);
        }
        if (issueUpdateVO.getComponentIssueRelVOList() != null) {
            this.self().handleUpdateComponentIssueRel(issueUpdateVO.getComponentIssueRelVOList(), projectId, issueId);
        }
        if (issueUpdateVO.getVersionIssueRelVOList() != null && issueUpdateVO.getVersionType() != null) {
            this.self().handleUpdateVersionIssueRel(issueUpdateVO.getVersionIssueRelVOList(), projectId, issueId, issueUpdateVO.getVersionType());
        }
        if (issueUpdateVO.getTags() != null) {
            this.self().handleUpdateTagIssueRel(issueUpdateVO.getTags(), projectId, issueId);
        }
        if (issueUpdateVO.getParticipantIds() != null) {
            this.self().handleUpdateParticipant(issueUpdateVO.getParticipantIds(), projectId, issueId);
        }
        if (agileWaterfallService != null) {
            agileWaterfallService.handleUpdateWaterfallField(projectId, issueUpdateVO);
        }
        if (issueUpdateVO.getProductIds() != null) {
            this.self().handleUpdateIssueProductRel(issueUpdateVO.getProductIds(), projectId, issueId);
        }
        return issueId;
    }

    private Long postUpdateIssueWithoutRuleNotice(Long projectId, IssueUpdateVO issueUpdateVO) {
        Long issueId = issueUpdateVO.getIssueId();
        if (issueUpdateVO.getLabelIssueRelVOList() != null) {
            this.self().handleUpdateLabelIssueWithoutRuleNotice(issueUpdateVO.getLabelIssueRelVOList(), issueId, projectId);
        }
        if (issueUpdateVO.getComponentIssueRelVOList() != null) {
            this.self().handleUpdateComponentIssueRelWithoutRuleNotice(issueUpdateVO.getComponentIssueRelVOList(), projectId, issueId);
        }
        if (issueUpdateVO.getVersionIssueRelVOList() != null && issueUpdateVO.getVersionType() != null) {
            this.self().handleUpdateVersionIssueRelWithoutRuleNotice(issueUpdateVO.getVersionIssueRelVOList(), projectId, issueId, issueUpdateVO.getVersionType());
        }
        if (issueUpdateVO.getTags() != null) {
            this.self().handleUpdateTagIssueRel(issueUpdateVO.getTags(), projectId, issueId);
        }
        if(issueUpdateVO.getParticipantIds() != null){
            this.self().handleUpdateParticipantWithoutRuleNotice(issueUpdateVO.getParticipantIds(), projectId, issueId);
        }
        if (agileWaterfallService != null) {
            agileWaterfallService.handleUpdateWaterfallFieldWithoutRuleNotice(projectId, issueUpdateVO);
        }
        if (issueUpdateVO.getProductIds() != null) {
            this.self().handleUpdateIssueProductRel(issueUpdateVO.getProductIds(), projectId, issueId);
        }
        return issueId;
    }

    private void validateBeforeUpdate(Long projectId, IssueUpdateVO issueUpdateVO, List<String> fieldList) {
        if (agilePluginService != null) {
            agilePluginService.checkFeatureBeforeUpdateIssue(issueUpdateVO,projectId);
        }
        if (agileWaterfallService != null) {
            agileWaterfallService.checkBeforeUpdateIssue(issueUpdateVO, projectId, fieldList);
        }
        if (fieldList.contains(EPIC_NAME_FIELD)
                && issueUpdateVO.getEpicName() != null
                && checkEpicName(projectId, issueUpdateVO.getEpicName(), issueUpdateVO.getIssueId())) {
            throw new CommonException("error.epicName.exist");
        }
    }

    @Override
    public void handleUpdateTagIssueRel(List<TagVO> tags, Long projectId, Long issueId) {
        if (agilePluginService != null) {
            agilePluginService.updateTagIssueRel(tags, projectId, issueId);
        }
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldList = {RuleNotice.PARTICIPANT_ID}, instanceId = "issueId", idPosition = "arg")
    public void handleUpdateParticipant(List<Long> participantIds, Long projectId, Long issueId) {
        handleUpdateParticipantWithoutRuleNotice(participantIds, projectId, issueId);
    }

    @Override
    public void handleUpdateParticipantWithoutRuleNotice(List<Long> participantIds, Long projectId, Long issueId) {
        if (!participantIds.isEmpty()) {
            issueParticipantRelService.updateParticipantRel(issueId, projectId, participantIds);
        } else {
            issueParticipantRelService.deleteParticipantRel(issueId, projectId);
        }
    }

    @Override
    @Transactional(propagation = Propagation.NESTED)
    public void handleData(Map<String, Object> result,
                           ProjectVO projectVO,
                           IssueDTO issueDTO,
                           ProjectVO targetProjectVO,
                           Long projectId,
                           BatchUpdateFieldStatusVO batchUpdateFieldStatusVO) {
        boolean isMove = Boolean.TRUE.equals(result.get("isMove"));
        JSONObject issueJSONObject = JSON.parseObject(JSON.toJSONString(result.get("jsonObj")));
        try {
            if (isMove) {
                issueProjectMoveService.handlerIssueValue(projectVO, issueDTO.getIssueId(), targetProjectVO, issueJSONObject);
            } else {
                IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
                List<String> fieldList = verifyUpdateUtil.verifyUpdateData(issueJSONObject, issueUpdateVO);
                this.self().handleUpdateIssueWithoutRuleNotice(issueUpdateVO, fieldList, projectId);
            }
            batchUpdateFieldStatusVO.setSuccessCount(batchUpdateFieldStatusVO.getSuccessCount() + 1);
        } catch (Exception e) {
            LOGGER.error("error.batch.transfer.issue", e);
            batchUpdateFieldStatusVO.setFailedCount(batchUpdateFieldStatusVO.getFailedCount() + 1);
        }
    }

    @Override
    public List<PageFieldViewVO> listRequiredFieldByIssueType(Long projectId,
                                                              Long organizationId,
                                                              Long issueId,
                                                              Long issueTypeId) {
        List<PageFieldViewVO> requiredSystemFields = listRequiredFieldByIssueTypeNoFilter(projectId, organizationId, issueId, issueTypeId);
        return fieldPermissionService.filterPageFieldViewVO(projectId, organizationId, issueTypeId, requiredSystemFields);
    }


    @Override
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void deleteIssueOnRequiresNew(Long projectId, Long issueId, BatchUpdateFieldStatusVO batchUpdateFieldStatusVO) {
        try {
            deleteIssue(projectId, issueId);
        } catch (Exception e) {
            LOGGER.error(e.getMessage());
            batchUpdateFieldStatusVO.setFailedCount(batchUpdateFieldStatusVO.getFailedCount() + 1);
        }
    }

    @Override
    public void handleUpdateIssueProductRel(List<Long> productIds, Long projectId, Long issueId) {
        if (agilePluginService != null) {
            agilePluginService.updateIssueProductRel(productIds, projectId, issueId);
        }
    }

    @Override
    public List<PageFieldViewVO> listRequiredFieldByIssueTypeNoFilter(Long projectId,
                                                                      Long organizationId,
                                                                      Long issueId,
                                                                      Long issueTypeId) {
        String schemeCode = AGILE_SCHEME_CODE;
        IssueVO issue = queryIssue(projectId, issueId, organizationId);
        Map<String, Object> customFieldMap =
                pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, Arrays.asList(projectId), Arrays.asList(issueId), false)
                        .getOrDefault(issueId, new HashMap<>());
        AssertUtilsForCommonException.notNull(issue, "error.issue.null");
        PageFieldViewParamVO param = new PageFieldViewParamVO();
        param.setIssueTypeId(issueTypeId);
        param.setSchemeCode(schemeCode);
        param.setPageCode(PageCode.AGILE_ISSUE_CREATE);
        List<PageFieldViewVO> createPageFields =
                pageFieldService.queryPageFieldViewsNoPermissionFilter(organizationId, projectId, param);
        Set<Long> fieldIds =
                createPageFields
                        .stream()
                        .map(PageFieldViewVO::getFieldId)
                        .collect(Collectors.toSet());
        param.setPageCode(PageCode.AGILE_ISSUE_EDIT);
        pageFieldService.queryPageFieldViewsNoPermissionFilter(organizationId, projectId, param)
                .forEach(x -> {
                    Long fieldId = x.getFieldId();
                    if (!fieldIds.contains(fieldId)) {
                        createPageFields.add(x);
                        fieldIds.add(fieldId);
                    }
                });
        boolean belongToProgram = belongToProgram(organizationId, projectId);
        List<PageFieldViewVO> requiredSystemFields = new ArrayList<>();
        List<PageFieldViewVO> requiredCustomFields = new ArrayList<>();
        createPageFields.forEach(x -> {
            if (Boolean.TRUE.equals(x.getRequired())) {
                handlerSystemAndCustomRequiredField(customFieldMap, belongToProgram, x, requiredSystemFields, requiredCustomFields, issue);
            }
        });
        requiredSystemFields.addAll(requiredCustomFields);
        return requiredSystemFields;
    }

    private boolean belongToProgram(Long organizationId, Long projectId) {
        if (agilePluginService == null) {
            return false;
        }
        return !ObjectUtils.isEmpty(agilePluginService.getProgram(organizationId, projectId));
    }

    private void handlerSystemAndCustomRequiredField(Map<String, Object> customFieldMap, boolean belongToProgram, PageFieldViewVO pageFieldView, List<PageFieldViewVO> requiredSystemFields, List<PageFieldViewVO> requiredCustomFields, IssueVO issue) {
        if (Boolean.TRUE.equals(pageFieldView.getSystem())) {
            //系统字段
            String code = pageFieldView.getFieldCode();
            if (FieldCode.EPIC.equals(code) && belongToProgram) {
                return;
            }
            if (isSystemFieldEmpty(pageFieldView.getFieldCode(), issue)) {
                requiredSystemFields.add(pageFieldView);
            }
        } else {
            if (ObjectUtils.isEmpty(customFieldMap.get(pageFieldView.getFieldCode()))) {
                requiredCustomFields.add(pageFieldView);
            }
        }
    }

    @Override
    public void executionUpdateStatus(Long projectId, Long issueId, ExecutionUpdateIssueVO executionUpdateIssueVO) {
        Long sprintId = executionUpdateIssueVO.getSprintId();
        Map<Long, Long> map = executionUpdateIssueVO.getIssueTypeStatusMap();
        IssueDetailDTO issueDetailDTO = issueMapper.queryIssueDetail(projectId, issueId);
        String issueTypeCode = issueDetailDTO.getIssueTypeCode();
        List<String> waterfallIssueTypes = Arrays.asList(IssueTypeCode.WATERFALL_ISSUE_TYPE_CODE);
        if (!waterfallIssueTypes.contains(issueTypeCode)) {
            //敏捷问题需要测试计划关联冲刺才可执行
            if (ObjectUtils.isEmpty(sprintId)) {
                return;
            }
            IssueSprintRelDTO issueSprintRelDTO = new IssueSprintRelDTO();
            issueSprintRelDTO.setProjectId(projectId);
            issueSprintRelDTO.setSprintId(sprintId);
            issueSprintRelDTO.setIssueId(issueId);
            List<IssueSprintRelDTO> sprintRelDTOS = issueSprintRelMapper.select(issueSprintRelDTO);
            if (CollectionUtils.isEmpty(sprintRelDTOS)) {
                return;
            }
        }
        Long issueTypeId = issueDetailDTO.getIssueTypeId();
        Long currentStatusId = issueDetailDTO.getStatusId();
        Long targetStatusId = map.get(issueDetailDTO.getIssueTypeId());
        if (ObjectUtils.isEmpty(targetStatusId) || Objects.equals(currentStatusId, targetStatusId)) {
            return;
        }
        String appleType = projectConfigService.getApplyType(projectId, issueTypeId);
        List<TransformVO> transformVOS = projectConfigService.queryTransformsByProjectId(projectId, currentStatusId, issueId, issueTypeId, appleType);
        if (CollectionUtils.isNotEmpty(transformVOS)) {
            Map<Long, TransformVO> transformVOMap = transformVOS.stream().collect(Collectors.toMap(TransformVO::getEndStatusId, Function.identity()));
            TransformVO transformVO = transformVOMap.get(targetStatusId);
            if (!ObjectUtils.isEmpty(transformVO)) {
                updateIssueStatus(projectId, issueId, transformVO.getId(), transformVO.getStatusVO().getObjectVersionNumber(), appleType);
            }
        }
    }

    private boolean isSystemFieldEmpty(String fieldCode, IssueVO issue) {
        Object value;
        switch (fieldCode) {
            case FieldCode.DESCRIPTION:
                value = issue.getDescription();
                break;
            case FieldCode.COMPONENT:
                value = issue.getComponentIssueRelVOList();
                break;
            case FieldCode.LABEL:
                value = issue.getLabelIssueRelVOList();
                break;
            case FieldCode.FIX_VERSION:
                value = issue.getVersionIssueRelVOList() == null ?
                        null :
                        issue.getVersionIssueRelVOList().stream()
                                .filter(versionIssueRel -> Objects.equals(versionIssueRel.getRelationType(), ProductVersionService.VERSION_RELATION_TYPE_FIX))
                                .filter(versionIssueRel -> Objects.equals(versionIssueRel.getStatusCode(), ProductVersionService.VERSION_STATUS_CODE_PLANNING))
                                .collect(Collectors.toList());
                break;
            case FieldCode.INFLUENCE_VERSION:
                value = issue.getVersionIssueRelVOList() == null ?
                        null :
                        issue.getVersionIssueRelVOList().stream()
                                .filter(versionIssueRel -> Objects.equals(versionIssueRel.getRelationType(), ProductVersionService.VERSION_RELATION_TYPE_INFLUENCE))
                                .collect(Collectors.toList());
                break;
            case FieldCode.SPRINT:
                List<SprintNameVO> sprints = issue.getCloseSprint();
                if (sprints == null) {
                    sprints = new ArrayList<>();
                }
                if (issue.getActiveSprint() != null) {
                    sprints.add(issue.getActiveSprint());
                }
                value = sprints;
                break;
            case FieldCode.EPIC_NAME:
                value = issue.getEpicName();
                break;
            case FieldCode.ASSIGNEE:
                value = issue.getAssigneeId();
                break;
            case FieldCode.ESTIMATED_START_TIME:
                value = issue.getEstimatedStartTime();
                break;
            case FieldCode.ESTIMATED_END_TIME:
                value = issue.getEstimatedEndTime();
                break;
            case FieldCode.REMAINING_TIME:
                value = issue.getRemainingTime();
                break;
            case FieldCode.STORY_POINTS:
                value = issue.getStoryPoints();
                break;
            case FieldCode.MAIN_RESPONSIBLE:
                value = issue.getMainResponsible();
                break;
            case FieldCode.TAG:
                value = issue.getTags();
                break;
            case FieldCode.ENVIRONMENT:
                value = issue.getEnvironment();
                break;
            case FieldCode.ACTUAL_START_TIME:
                value = issue.getActualStartTime();
                break;
            case FieldCode.ACTUAL_END_TIME:
                value = issue.getActualEndTime();
                break;
            case FieldCode.PARTICIPANT:
                value = issue.getParticipants();
                break;
            case FieldCode.ESTIMATE_TIME:
                value = issue.getEstimateTime();
                break;
            case FieldCode.PRODUCT:
                value = issue.getProductVOList();
                break;
            case FieldCode.EPIC:
                Long epicId = issue.getEpicId();
                if (Objects.equals(0L, epicId)) {
                    value = null;
                } else {
                    value = epicId;
                }
                break;
            default:
                value = new Object();
        }
        return ObjectUtils.isEmpty(value);
    }

    @Override
    public IssueVO updateIssueStatus(Long projectId, Long issueId, Long transformId, Long objectVersionNumber, String applyType) {
        return this.self().updateIssueStatus(projectId, issueId, transformId, objectVersionNumber, applyType, null, false);
    }

    @Override
    public IssueVO updateIssueStatus(Long projectId, Long issueId, Long transformId, Long objectVersionNumber,
                                     String applyType, IssueDTO triggerIssue, boolean autoTransferFlag) {
        Set<Long> influenceIssueIds = new HashSet<>();
        IssueVO result = this.self().doStateMachineCustomFlowAndRuleNotice(projectId, issueId, applyType, influenceIssueIds, false, transformId, new InputDTO(issueId, "updateStatus", updateTrigger(autoTransferFlag, triggerIssue)));
        if (backlogExpandService != null) {
            backlogExpandService.changeDetection(issueId, projectId, ConvertUtil.getOrganizationId(projectId));
        }
        IssueVO issueVO = queryIssueByUpdate(projectId, issueId, Collections.singletonList(STATUS_ID));
        issueVO.setInfluenceIssueIds(new ArrayList<>(influenceIssueIds));
        if (result != null) {
            issueVO.setErrorMsg(result.getErrorMsg());
        }
        return issueVO;
    }

    @Override
    public IssueVO updateIssueStatusWithoutRuleNotice(Long projectId, Long issueId, Long transformId, Long objectVersionNumber,
                                     String applyType, IssueDTO triggerIssue, boolean autoTranferFlag) {
        Set<Long> influenceIssueIds = new HashSet<>();
        IssueVO result = doStateMachineTransformAndCustomFlow(projectId, issueId, applyType, influenceIssueIds, new TriggerCarrierVO(), false, transformId, new InputDTO(issueId, "updateStatus", updateTrigger(autoTranferFlag, triggerIssue)));
        if (backlogExpandService != null) {
            backlogExpandService.changeDetection(issueId, projectId, ConvertUtil.getOrganizationId(projectId));
        }
        IssueVO issueVO = queryIssueByUpdate(projectId, issueId, Collections.singletonList(STATUS_ID));
        issueVO.setInfluenceIssueIds(new ArrayList<>(influenceIssueIds));
        if (result != null) {
            issueVO.setErrorMsg(result.getErrorMsg());
        }
        return issueVO;
    }

    @Override
    public IssueVO doStateMachineCustomFlow(Long projectId,
                                            Long issueId,
                                            String applyType,
                                            Set<Long> influenceIssueIds,
                                            TriggerCarrierVO triggerCarrierVO) {

        /**
         * 修改属性报错，导致数据回滚但是状态机实例已经完成状态变更，导致issue无论变更什么状态都无效
         * 抛异常并清空当前实例的状态机的状态信息
         */
        try {
            triggerCarrierVO.setInstanceId(issueId);
            triggerCarrierVO.setFieldList(Collections.singletonList("statusId"));
            triggerCarrierVO.setExecutedRules(new ArrayList<>());
            triggerCarrierVO.setNoticeInstanceId(issueId);
            triggerCarrierVO.setMemberFieldIds(new HashSet<>());
            statusFieldSettingService.handlerSettingToUpdateIssue(projectId, issueId, triggerCarrierVO);
            boolean transformFlag = statusLinkageService.updateParentStatus(projectId, issueId, applyType, influenceIssueIds);
            IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
            String encryptType = EncryptContext.encryptType().name();
            RequestAttributes requestAttributes = null;
            if (EncryptContext.isEncrypt()) {
                requestAttributes = RequestContextHolder.currentRequestAttributes();
            }
            issueOperateService.updateIssueStatusLinkage(projectId, issueId, issueDTO, applyType, encryptType, requestAttributes);
            triggerCarrierVO.setAuditDomain(issueDTO);
            triggerCarrierVO.setProjectId(projectId);
            triggerCarrierVO.setIssueTypeId(issueDTO.getIssueTypeId());
            if (transformFlag) {
                return null;
            } else {
                IssueVO error = new IssueVO();
                error.setErrorMsg(MessageAccessor.getMessage("error.update.status.transform.parent_status_update_failed").getDesc());
                return error;
            }
        } catch (Exception e) {
            stateMachineClientService.cleanInstanceCache(projectId, issueId, applyType);
            throw new CommonException("error.update.status.transform.setting", e);
        }
    }

    @Override
    public void updateLinkIssueStatus(Long projectId, Long issueId, IssueDTO issueDTO, String applyType, Set<Long> influenceIssueIds) {
        Map<Long, List<Long>> allInfluenceMap = new HashMap<>();
        InfluenceIssueVO influenceIssueVO = new InfluenceIssueVO();
        influenceIssueVO.setIssueId(issueId);
        influenceIssueVO.setStatusId(issueDTO.getStatusId());
        influenceIssueVO.setLoop(false);
        influenceIssueVO.setLevel(1);
        influenceIssueVO.setMaxDepth(false);
        List<IssueLinkChangeVO> issueLinkChangeVOS = issueLinkMapper.issueLinkChangeByProjectId(projectId);
        if (CollectionUtils.isEmpty(issueLinkChangeVOS)) {
            issueLinkChangeVOS = new ArrayList<>();
        }
        Map<Long, List<IssueLinkChangeVO>> issueLinkChangeGroup = issueLinkChangeVOS.stream().collect(Collectors.groupingBy(IssueLinkChangeVO::getIssueId));
        handlerInfluenceMap(allInfluenceMap, issueId, issueDTO.getStatusId(), issueLinkChangeGroup, null, influenceIssueVO, false);
        if (MapUtils.isEmpty(allInfluenceMap) && allInfluenceMap.size() <= 1) {
            return;
        }
        Set<Long> linkIssueIds = allInfluenceMap.keySet();
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, new ArrayList<>(linkIssueIds), null);
        if (CollectionUtils.isEmpty(issueDTOS)) {
            return;
        }
        Map<Long, IssueDTO> issueDTOMap = new HashMap<>(issueDTOS.stream().collect(Collectors.toMap(IssueDTO::getIssueId, Function.identity())));
        List<InfluenceIssueVO> childrenVO = influenceIssueVO.getChildrenVO();
        Map<Long, IssueStatusLinkageVO> issueStatusLinkageMap = linkIssueStatusLinkageService.queryMapByProject(projectId, ConvertUtil.getOrganizationId(projectId));
        if (CollectionUtils.isNotEmpty(childrenVO)) {
            // 处理需要联动的issue
            for (InfluenceIssueVO influenceVO : childrenVO) {
                try {
                    this.self().handlerInfluenceIssue(projectId, applyType, influenceVO, issueId, issueStatusLinkageMap, influenceIssueIds);
                } catch (Exception e) {
                    // 返回受影响的issue
                    influenceIssueIds.add(influenceVO.getIssueId());
                    statusLinkageExecutionLog(influenceVO, influenceVO.getIssueId(), issueDTOMap.get(issueId), false, issueStatusLinkageMap, TriggerExecutionStatus.ERROR.getValue(), null);
                    LOGGER.info("error update link issue", e);
                }
            }
        }
    }

    @Override
    public void handlerInfluenceIssue(Long projectId, String applyType, InfluenceIssueVO influenceIssueVO, Long linkIssueId, Map<Long, IssueStatusLinkageVO> issueStatusLinkageMap, Set<Long> influenceIssueIds) {
        Long issueId = influenceIssueVO.getIssueId();
        Long statusId = influenceIssueVO.getStatusId();
        IssueDTO influenceIssue = issueMapper.selectByPrimaryKey(linkIssueId);
        IssueDTO issue = issueMapper.selectByPrimaryKey(issueId);
        Boolean isSub = Objects.equals("sub_task",influenceIssue.getTypeCode()) || (Objects.equals("bug",influenceIssue.getTypeCode()) && !ObjectUtils.isEmpty(influenceIssue.getRelateIssueId()) && !Objects.equals(influenceIssue.getRelateIssueId(), 0L));
        // 变更issue的状态和更新属性
        TriggerCarrierVO triggerCarrierVO = new TriggerCarrierVO();
        this.self().executionUpdateInfluenceIssue(issueId, statusId, influenceIssue, projectId, applyType, influenceIssueVO, isSub, issueStatusLinkageMap, triggerCarrierVO);
        // 处理当前issue会影响的issue
        List<InfluenceIssueVO> childrenVO = influenceIssueVO.getChildrenVO();
        if (CollectionUtils.isNotEmpty(childrenVO)) {
            for (InfluenceIssueVO issueVO : childrenVO) {
                try {
                    this.self().handlerInfluenceIssue(projectId, applyType, issueVO, issueId, issueStatusLinkageMap, influenceIssueIds);
                } catch (Exception e) {
                    influenceIssueIds.add(issueVO.getIssueId());
                    statusLinkageExecutionLog(issueVO, issueVO.getIssueId(), issue, false, issueStatusLinkageMap, TriggerExecutionStatus.ERROR.getValue(), null);
                    LOGGER.info("error.update.link.issue",e);
                }
            }
        }
    }

    @Override
    public void statusLinkageExecutionLog(InfluenceIssueVO influenceIssueVO, Long issueId, IssueDTO influenceIssue, Boolean isSub, Map<Long, IssueStatusLinkageVO> issueStatusLinkageMap, String statusCode, String remark) {
        // 记录联动的执行日志
        Long projectId = influenceIssue.getProjectId();
        if (!ObjectUtils.isEmpty(influenceIssueVO.getLinkageSettingId())) {
            IssueStatusLinkageVO linkIssueStatusLinkageVO = isSub && Boolean.TRUE.equals(influenceIssueVO.getChildrenTriggered()) ? statusLinkageService.queryById(projectId, influenceIssueVO.getLinkageSettingId()) : issueStatusLinkageMap.getOrDefault(influenceIssueVO.getLinkageSettingId(), null);
            if (ObjectUtils.isEmpty(linkIssueStatusLinkageVO)) {
                throw new CommonException("error.link.issue.status.linkage.empty");
            }
            String content = buildStatusLinkageContent(linkIssueStatusLinkageVO);
            StatusLinkageExecutionLogDTO statusLinkageExecutionLogDTO = new StatusLinkageExecutionLogDTO();
            statusLinkageExecutionLogDTO.setPreIssueId(influenceIssue.getIssueId());
            statusLinkageExecutionLogDTO.setCurIssueId(issueId);
            statusLinkageExecutionLogDTO.setContent(content);
            if (ObjectUtils.isEmpty(statusCode)) {
                if (Boolean.TRUE.equals(influenceIssueVO.getMaxDepth())) {
                    statusCode = TriggerExecutionStatus.MAX_DEPTH.getValue();
                    remark = TriggerExecutionStatus.MAX_DEPTH.getValue();
                } else if (Boolean.TRUE.equals(influenceIssueVO.getLoop())) {
                    statusCode = TriggerExecutionStatus.LOOP.getValue();
                } else {
                    statusCode = TriggerExecutionStatus.SUCCESS.getValue();
                }
            }
            statusLinkageExecutionLogDTO.setRemark(remark);
            statusLinkageExecutionLogDTO.setStatusCode(statusCode);
            statusLinkageExecutionLogService.create(projectId, ConvertUtil.getOrganizationId(projectId), statusLinkageExecutionLogDTO);
        }
    }

    @Override
    public String buildStatusLinkageContent(IssueStatusLinkageVO issueStatusLinkageVO) {
        StringBuilder stringBuilder = new StringBuilder();
        IssueTypeVO issueTypeVO = issueStatusLinkageVO.getIssueTypeVO();
        StatusVO statusVO = issueStatusLinkageVO.getStatusVO();
        if (!ObjectUtils.isEmpty(statusVO) && !ObjectUtils.isEmpty(issueTypeVO)) {
            stringBuilder.append(issueTypeVO.getName())
                    .append(": 当前" + IssueConstant.ISSUE_CN + "状态为")
                    .append(statusVO.getName())
                    .append("时");
        }
        IssueLinkTypeVO linkTypeVO = issueStatusLinkageVO.getLinkTypeVO();
        IssueTypeVO linkageIssueType = issueStatusLinkageVO.getLinkageIssueType();
        StatusVO linkageIssueStatus = issueStatusLinkageVO.getLinkageIssueStatus();
        if (!ObjectUtils.isEmpty(linkageIssueType) && Arrays.asList(IssueTypeCode.WATERFALL_ISSUE_TYPE_CODE).contains(linkageIssueType.getTypeCode())) {
            String predecessorType = issueStatusLinkageVO.getPredecessorType();
            if (ObjectUtils.isEmpty(predecessorType)) {
                stringBuilder
                        .append(" 【")
                        .append(ObjectUtils.isEmpty(linkTypeVO) ? "关联" : linkTypeVO.getLinkName())
                        .append("】的");
            } else {
                stringBuilder
                        .append("，存在【")
                        .append(getPredecessorType(issueStatusLinkageVO.getPredecessorType()))
                        .append("】依赖关系的");
            }
        } else {
            stringBuilder.append(" 【")
                    .append(ObjectUtils.isEmpty(linkTypeVO) ? "关联" : linkTypeVO.getLinkName())
                    .append("】的");
        }
        if (!ObjectUtils.isEmpty(linkageIssueType) && !ObjectUtils.isEmpty(linkageIssueStatus)) {
            stringBuilder.append("【" + linkageIssueType.getName() + "】")
                    .append("流转状态到")
                    .append("【" + linkageIssueStatus.getName() + "】");
        }
        return stringBuilder.toString();
    }

    private String getPredecessorType(String predecessorType) {
        String result = StringUtils.EMPTY;
        switch(predecessorType) {
            case "predecessor_fs":
                result = "完成-开始（FS）";
                break;
            case "predecessor_ff":
                result = "完成-完成（FF）";
                break;
            case "predecessor_ss":
                result = "开始-开始（SS）";
                break;
            case "predecessor_sf":
                result = "开始-完成（SF）";
                break;
            default:
                break;
        }
        return result;
    }


    @Override
    @Transactional(rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
    public Boolean executionUpdateInfluenceIssue(Long issueId, Long executionStatusId, IssueDTO influenceIssue, Long projectId, String applyType, InfluenceIssueVO influenceIssueVO, Boolean isSub,  Map<Long, IssueStatusLinkageVO> issueStatusLinkageMap, TriggerCarrierVO triggerCarrierVO) {
        IssueDTO issue = issueMapper.selectByPrimaryKey(issueId);
        triggerCarrierVO.setInstanceId(issue.getIssueId());
        triggerCarrierVO.setProjectId(projectId);
        triggerCarrierVO.setIssueTypeId(issue.getIssueTypeId());
        triggerCarrierVO.setExecutedRules(new ArrayList<>());
        triggerCarrierVO.setNoticeInstanceId(issue.getIssueId());
        triggerCarrierVO.setFieldList(Collections.singletonList("statusId"));
        triggerCarrierVO.setMemberFieldIds(new HashSet<>());
        if (Boolean.TRUE.equals(influenceIssueVO.getLoop()) || Boolean.TRUE.equals(influenceIssueVO.getMaxDepth())) {
            statusLinkageExecutionLog(influenceIssueVO, issue.getIssueId(), influenceIssue, isSub, issueStatusLinkageMap, null, null);
            return Boolean.TRUE;
        }
        if (Objects.equals("bug", issue.getTypeCode()) && !ObjectUtils.isEmpty(issue.getRelateIssueId()) && !Objects.equals(issue.getRelateIssueId(), 0L)) {
            return Boolean.TRUE;
        }
        if (Objects.equals(issue.getStatusId(), executionStatusId)) {
            statusLinkageExecutionLog(influenceIssueVO, issue.getIssueId(), influenceIssue, isSub, issueStatusLinkageMap, TriggerExecutionStatus.STOP.getValue(), "same_status");
            return Boolean.TRUE;
        }
        Boolean verifyStatusTransferSetting = transferSettingService.verifyStatusTransferSetting(projectId, issue, executionStatusId);
        if (Boolean.TRUE.equals(verifyStatusTransferSetting)) {
            statusLinkageExecutionLog(influenceIssueVO, issue.getIssueId(), influenceIssue, isSub, issueStatusLinkageMap, TriggerExecutionStatus.STOP.getValue(), "condition_limit");
            return Boolean.TRUE;
        }
        // 获取当前状态对应的transformId
        Long stateMachineId = projectConfigService.queryStateMachineId(projectId, applyType, issue.getIssueTypeId());
        // 获取开始状态和结束状态查询转换Id
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<StatusMachineTransformDTO> statusMachineTransformDTOS = statusMachineTransformMapper
                .selectTransformByStatusId(organizationId, stateMachineId, issue.getStatusId(), executionStatusId, false);
        if (CollectionUtils.isEmpty(statusMachineTransformDTOS)){
            statusMachineTransformDTOS = statusMachineTransformMapper
                    .selectTransformByStatusId(organizationId, stateMachineId, issue.getStatusId(),  executionStatusId, true);
        }
        StatusMachineTransformDTO statusTransform = statusMachineTransformDTOS.get(0);
        stateMachineClientService.executeTransform(projectId, issueId, statusTransform.getId(), issue.getObjectVersionNumber(), applyType, new InputDTO(issueId, "updateStatus", updateTrigger(true, influenceIssue)));
        if (SchemeApplyType.AGILE.equals(applyType)) {
            IssueConvertDTO issueConvertDTO = new IssueConvertDTO();
            issueConvertDTO.setIssueId(issueId);
            issueConvertDTO.setStayDate(new Date());
            issueConvertDTO.setObjectVersionNumber(issueMapper.selectByPrimaryKey(issueId).getObjectVersionNumber());
            issueAccessDataService.updateSelective(issueConvertDTO);
        }
        // 更新属性
        statusFieldSettingService.handlerSettingToUpdateIssue(projectId, issueId, triggerCarrierVO);
        AgilePluginService agilePluginService = SpringBeanUtil.getExpandBean(AgilePluginService.class);
        if (agilePluginService != null) {
            agilePluginService.storyLinkageFeature(projectId, issue,applyType);
        }
        // 记录联动的执行日志
        statusLinkageExecutionLog(influenceIssueVO, issueId, influenceIssue, isSub, issueStatusLinkageMap, null, null);
        LOGGER.info("项目{}下状态联动触发{}编号为{}的issue状态变更", issue.getProjectId(), IssueConstant.ISSUE_CN, issue.getIssueNum());
        triggerCarrierVO.setAuditDomain(issueMapper.selectByPrimaryKey(issueId));
        this.self().batchUpdateInvokeTrigger(Collections.singletonList(triggerCarrierVO));
        return Boolean.FALSE;
    }

    @Override
    public void handlerInfluenceMap(Map<Long, List<Long>> influenceMap, Long issueId, Long statusId, Map<Long, List<IssueLinkChangeVO>> issueLinkChangeGroup, Long influenceId, InfluenceIssueVO influenceIssueVO, Boolean linkTriggered) {
        if (ObjectUtils.isEmpty(issueId)) {
            return;
        }
        List<Long> statusIds = influenceMap.getOrDefault(issueId, new ArrayList<>());
        if (!statusIds.contains(statusId)) {
            statusIds.add(statusId);
            influenceMap.put(issueId, statusIds);
        } else {
            influenceIssueVO.setLoop(true);
            return;
        }
        List<IssueLinkChangeVO> issueLinkChangeVOS = issueLinkChangeGroup.get(issueId);
        if (CollectionUtils.isEmpty(issueLinkChangeVOS)) {
            return;
        }
        Map<Long, List<IssueLinkChangeVO>> issueLinkChangeGroupByStatus = issueLinkChangeVOS.stream().collect(Collectors.groupingBy(IssueLinkChangeVO::getStatusId));
        List<IssueLinkChangeVO> linkChangeVOS = issueLinkChangeGroupByStatus.get(statusId);
        if (CollectionUtils.isNotEmpty(linkChangeVOS)) {
            List<InfluenceIssueVO> influenceIssueVOS = new ArrayList<>();
            Integer level = influenceIssueVO.getLevel() + 1;
            for (IssueLinkChangeVO linkChangeVO : linkChangeVOS) {
                // 当前是联动触发，并且配置的是联动时不触发状态变更就停止
                if (Boolean.TRUE.equals(linkTriggered) && Boolean.FALSE.equals(linkChangeVO.getTriggered()) ) {
                   continue;
                }
                List<Long> linkIssueStatusIds = influenceMap.getOrDefault(linkChangeVO.getLinkedIssueId(), new ArrayList<>());
                InfluenceIssueVO influenceIssue = new InfluenceIssueVO();
                influenceIssue.setIssueId(linkChangeVO.getLinkedIssueId());
                influenceIssue.setStatusId(linkChangeVO.getLinkIssueStatusId());
                influenceIssue.setMaxDepth(level > triggerMaxDepth);
                influenceIssue.setLoop(false);
                influenceIssue.setLevel(level);
                influenceIssue.setLinkageSettingId(linkChangeVO.getLinkSettingId());
                if (linkIssueStatusIds.contains(linkChangeVO.getLinkIssueStatusId())) {
                    influenceIssue.setLoop(true);
                }
                if (Boolean.FALSE.equals(influenceIssue.getMaxDepth()) && Boolean.FALSE.equals(influenceIssue.getLoop())){
                    handlerInfluenceMap(influenceMap, linkChangeVO.getLinkedIssueId(), linkChangeVO.getLinkIssueStatusId(), issueLinkChangeGroup, issueId, influenceIssue, true);
                }
                influenceIssueVOS.add(influenceIssue);
            }
            influenceIssueVO.setChildrenVO(influenceIssueVOS);
        }
    }

    private String updateTrigger(boolean autoTransferFlag, IssueDTO triggerIssue) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put(AUTO_TRANSFER_FLAG, autoTransferFlag);
        if(Objects.nonNull(triggerIssue)){
            jsonObject.put(TRIGGER_ISSUE_ID, triggerIssue.getIssueId());
        }
        return JSON.toJSONString(jsonObject);
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldListName = "fieldList", instanceId = "issueId", idPosition = "arg")
    public void handleUpdateIssue(IssueUpdateVO issueUpdateVO, List<String> fieldList, Long projectId, Long issueId) {
        handleUpdateIssueWithoutRuleNotice(issueUpdateVO, fieldList, projectId);
    }

    @Override
    public void handleUpdateIssueWithoutRuleNotice(IssueUpdateVO issueUpdateVO,
                                                   List<String> fieldList,
                                                   Long projectId) {
        fieldList = new ArrayList<>(fieldList);
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        IssueDTO originIssue = issueMapper.queryIssueWithNoCloseSprint(issueUpdateVO.getIssueId());
        IssueConvertDTO issueConvertDTO = issueAssembler.toTarget(issueUpdateVO, IssueConvertDTO.class);
        String issueType = originIssue.getTypeCode();
        //处理用户，前端可能会传0，处理为null
        issueConvertDTO.initializationIssueUser();
        if (fieldList.contains(SPRINT_ID_FIELD)) {
            final IssueConvertDTO oldIssue = modelMapper.map(originIssue, IssueConvertDTO.class);
            final Long sprintId = issueConvertDTO.getSprintId();
            // sprintId传入空或0, 说明是执行清空冲刺操作
            final boolean clearSprint = sprintId == null || sprintId == 0;

            // 查询关联的子任务和子缺陷，一并处理
            final List<Long> subTaskIds = issueMapper.querySubIssueIdsByIssueId(projectId, issueConvertDTO.getIssueId());
            final List<Long> subBugIds = issueMapper.querySubBugIdsByIssueId(projectId, issueConvertDTO.getIssueId());
            final List<Long> issueIds = ListUtils.union(subTaskIds, subBugIds);
            issueIds.add(issueConvertDTO.getIssueId());
            // 检查冲刺是否有变化
            boolean sprintChanged = (!Objects.equals(oldIssue.getSprintId(), issueUpdateVO.getSprintId()));
            if (sprintChanged) {
                // 如果冲刺有变化
                // 批量删除父子工作项, 所有未关闭冲刺的关联关系
                BatchRemoveSprintDTO batchRemoveSprintDTO = new BatchRemoveSprintDTO(projectId, issueUpdateVO.getSprintId(), issueIds);
                issueAccessDataService.removeIssueFromSprintByIssueIds(batchRemoveSprintDTO);
                // 批量插入父子工作项, 目标冲刺的关联关系
                if(!clearSprint) {
                    issueAccessDataService.issueToDestinationByIds(projectId, sprintId, issueIds, new Date(), customUserDetails.getUserId());
                }
                // 触发商业版插件逻辑
                if (agilePluginService != null) {
                    agilePluginService.updateIssueSprintChanged(oldIssue, projectId, sprintId, issueType);
                }
            }
            if (oldIssue.isIssueRank()) {
                calculationRank(projectId, issueConvertDTO);
                fieldList.add(RANK_FIELD);
                issueConvertDTO.setOriginSprintId(originIssue.getSprintId());
            }
        }
        if (fieldList.contains("estimateTime")
                && !ObjectUtils.isEmpty(issueConvertDTO.getEstimateTime())
                && ObjectUtils.isEmpty(originIssue.getRemainingTime())) {
            fieldList.add("remainingTime");
            issueConvertDTO.setRemainingTime(issueConvertDTO.getEstimateTime());
        }
        if (agilePluginService != null) {
            agilePluginService.handlerBusinessUpdateIssue(issueType, fieldList, projectId, issueUpdateVO, originIssue);
            agilePluginService.issueSyncByIssueId(ConvertUtil.getOrganizationId(projectId), issueConvertDTO.getIssueId(), OpenAppIssueSyncConstant.AppType.DIND.getValue(), OpenAppIssueSyncConstant.OperationType.UPDATE);
        }
        if (agileWaterfallService != null) {
            agileWaterfallService.handlerWaterfallUpdateIssue(issueType, fieldList, projectId, issueUpdateVO, originIssue);
        }
        // 处理issue改变状态时，满足条件改变 is_pre_sprint_done 的值
        handleHistorySprintCompleted(projectId, issueConvertDTO, fieldList, originIssue);
        // 处理预计、实际时间
        handleEstimateTimeAndActualTime(issueConvertDTO);
        issueAccessDataService.update(issueConvertDTO, fieldList.toArray(new String[0]));
    }

    private void handleHistorySprintCompleted(Long projectId, IssueConvertDTO issueConvertDTO, List<String> fieldList, IssueDTO originIssue) {
        // issue的 is_pre_sprint_done 值为 True, 并且更新了状态
        if (Boolean.TRUE.equals(originIssue.getPreSprintDone()) && fieldList.contains("statusId")) {
            // 检查这个issue的当前状态是不是已完成，不是就需要设置 is_pre_sprint_done 为false
            Long statusId = issueConvertDTO.getStatusId();
            IssueStatusDTO issueStatusDTO = issueStatusMapper.selectByStatusId(projectId, statusId);
            if (Boolean.FALSE.equals(issueStatusDTO.getCompleted())) {
                issueConvertDTO.setPreSprintDone(false);
                fieldList.add("isPreSprintDone");
            }
        }
    }


    @Override
    public List<EpicDataVO> listEpic(Long projectId) {
        List<EpicDataVO> epicDataList = epicDataAssembler.toTargetList(issueMapper.queryEpicList(projectId), EpicDataVO.class);
        if (!epicDataList.isEmpty()) {
            List<Long> epicIds = epicDataList.stream().map(EpicDataVO::getIssueId).collect(Collectors.toList());
            Map<Long, Integer> issueCountMap = issueMapper.queryIssueCountByEpicIds(projectId, epicIds).stream().collect(Collectors.toMap(IssueCountDTO::getId, IssueCountDTO::getIssueCount));
            Map<Long, Integer> doneIssueCountMap = issueMapper.queryDoneIssueCountByEpicIds(projectId, epicIds).stream().collect(Collectors.toMap(IssueCountDTO::getId, IssueCountDTO::getIssueCount));
            Map<Long, Integer> notEstimateIssueCountMap = issueMapper.queryNotEstimateIssueCountByEpicIds(projectId, epicIds).stream().collect(Collectors.toMap(IssueCountDTO::getId, IssueCountDTO::getIssueCount));
            Map<Long, BigDecimal> totalEstimateMap = issueMapper.queryTotalEstimateByEpicIds(projectId, epicIds).stream().collect(Collectors.toMap(IssueCountDTO::getId, IssueCountDTO::getStoryPointCount));
            epicDataList.forEach(epicData -> {
                epicData.setIssueCount(issueCountMap.get(epicData.getIssueId()));
                epicData.setDoneIssueCount(doneIssueCountMap.get(epicData.getIssueId()));
                epicData.setNotEstimate(notEstimateIssueCountMap.get(epicData.getIssueId()));
                epicData.setTotalEstimate(totalEstimateMap.get(epicData.getIssueId()));
            });
        }
        return epicDataList;
    }

    protected void dataLogDeleteByIssueId(Long projectId, Long issueId) {
        DataLogDTO dataLogDTO = new DataLogDTO();
        dataLogDTO.setProjectId(projectId);
        dataLogDTO.setIssueId(issueId);
        dataLogService.delete(dataLogDTO);
    }


    //    @Saga(code = "agile-delete-issue", description = "删除issue", inputSchemaClass = IssuePayload.class)
    @Override
    public void deleteIssue(Long projectId, Long issueId) {
        IssueConvertDTO issueConvertDTO = queryIssueByProjectIdAndIssueId(projectId, issueId);
        if (issueConvertDTO == null) {
            throw new CommonException(ERROR_ISSUE_NOT_FOUND);
        }
        // 更新订阅
        workCalendarSubscribeService.handleWorkCalendarSubscribeChanged(projectId, issueId, false, new ArrayList<>());
        //删除issueLink
        issueLinkService.deleteByIssueId(issueConvertDTO.getIssueId());
        //删除标签关联
        labelIssueRelService.deleteByIssueId(issueConvertDTO.getIssueId());
        //没有issue使用的标签进行垃圾回收
        issueLabelService.labelGarbageCollection(projectId);
        //删除模块关联
        componentIssueRelService.deleteByIssueId(issueConvertDTO.getIssueId());
        //删除版本关联
        versionIssueRelService.deleteByIssueId(issueConvertDTO.getIssueId());
        //删除冲刺关联
        issueAccessDataService.deleteIssueFromSprintByIssueId(projectId, issueId);
        //删除评论信息
        issueCommentService.deleteByIssueId(issueConvertDTO.getIssueId());
        //删除附件
        issueAttachmentService.deleteByIssueId(issueConvertDTO.getIssueId());
        // 删除issue关联的知识
        if (!Objects.equals(issueConvertDTO.getTypeCode(), "feature")) {
            wikiRelationMapper.deleteByIssueId(projectId, issueId);
        }
        // 删除自定义字段的值
        FieldValueDTO fieldValueDTO = new FieldValueDTO();
        fieldValueDTO.setProjectId(projectId);
        fieldValueDTO.setSchemeCode(AGILE_SCHEME_CODE);
        fieldValueDTO.setInstanceId(issueId);
        Set<Long> fieldIds = fieldValueMapper.select(fieldValueDTO).stream().map(FieldValueDTO::getFieldId).collect(Collectors.toSet());
        fieldValueMapper.deleteList(projectId, issueId, AGILE_SCHEME_CODE, null);
        //不是子任务的issue删除子任务
        if (!(SUB_TASK).equals(issueConvertDTO.getTypeCode())) {
            if ((ISSUE_EPIC).equals(issueConvertDTO.getTypeCode())) {
                //如果是epic，会把该epic下的issue的epicId置为0
                issueAccessDataService.batchUpdateIssueEpicId(projectId, issueConvertDTO.getIssueId());
            } else {
                redisUtil.deleteRedisCache(new String[]{"Agile:EpicChart" + projectId + BaseConstants.Symbol.COLON + issueConvertDTO.getEpicId() + BaseConstants.Symbol.COLON + "*"});
            }
            List<IssueDTO> issueDTOList = issueMapper.queryIssueSubList(projectId, issueConvertDTO.getIssueId());
            if (issueDTOList != null && !issueDTOList.isEmpty()) {
                issueDTOList.forEach(subIssue -> deleteIssue(subIssue.getProjectId(), subIssue.getIssueId()));
            }
        }
        // 清除故事或者任务与子bug的关联
        if ("task".equals(issueConvertDTO.getTypeCode()) || STORY_TYPE.equals(issueConvertDTO.getTypeCode())) {
            issueMapper.updateSubBugRelateIssueId(projectId, issueId);
        }
        if (agilePluginService != null) {
            agilePluginService.deleteIssueForBusiness(issueConvertDTO);
            agilePluginService.issueSyncByIssueId(ConvertUtil.getOrganizationId(projectId), issueId, OpenAppIssueSyncConstant.AppType.DIND.getValue(), OpenAppIssueSyncConstant.OperationType.DELETE);
        }
        if (backlogExpandService != null) {
            backlogExpandService.deleteIssueBacklogRel(issueId);
        }
        if (agileWaterfallService != null) {
            agileWaterfallService.deleteIssueForWaterfall(projectId, issueId, issueConvertDTO);
        }
        // 删除工时
        this.workLogService.deleteByProjectIdWithoutDataLog(projectId, issueId );
        //删除日志信息
        dataLogDeleteByIssueId(projectId, issueId);
        deleteRuleLogRel(projectId, issueId, fieldIds);
        issueAccessDataService.delete(projectId, issueConvertDTO.getIssueId());
        //删除rank数据
        rankMapper.deleteRankByIssueId(issueId);
        //delete cache
        dataLogRedisUtil.handleDeleteRedisByDeleteIssue(projectId);
        testServiceClientOperator.deleteTestRel(projectId, issueId);
        if (backlogExpandService != null) {
            backlogExpandService.changeDetection(issueId, projectId, ConvertUtil.getOrganizationId(projectId));
        }
        // 删除issue的参与人数据
        issueParticipantRelMapper.deleteByIssueIdAndParticipantIds(projectId, issueId, null);
        // 删除当前issue相关的状态联动执行记录
        statusLinkageExecutionLogService.deleteByIssueId(projectId, ConvertUtil.getOrganizationId(projectId), issueId);
        issuePredecessorService.deleteNode(projectId, issueId);
    }

    private void deleteRuleLogRel(Long projectId, Long issueId, Set<Long> fieldIds) {
        if (agileTriggerService != null) {
            RuleLogRelVO ruleLogRel = new RuleLogRelVO();
            ruleLogRel.setBusinessType(STAR_BEACON_TYPE_ISSUE);
            ruleLogRel.setInstanceId(issueId);
            ruleLogRel.setProjectId(projectId);
            agileTriggerService.delete(ruleLogRel);
            if (!ObjectUtils.isEmpty(fieldIds)) {
                agileTriggerService.deleteRuleLog(projectId, CUSTOM_FIELD, fieldIds);
            }
        }
    }

    @Override
    public void batchDeleteIssuesAgile(Long projectId, List<Long> issueIds) {
        if (CollectionUtils.isEmpty(issueIds)) {
            return;
        }
        if (issueMapper.queryIssueIdsIsNotTest(projectId, issueIds) != issueIds.size()) {
            throw new CommonException(ERROR_ISSUE_TYPE_NOT_ISSUE_TEST);
        }
        List<IssueDTO> issueDTOList = null;
        if(agileWaterfallService != null) {
            issueDTOList = this.issueMapper.selectByIds(
                    issueIds.stream().map(String::valueOf).collect(Collectors.joining(BaseConstants.Symbol.COMMA))
            );
        }
        issueMapper.batchDeleteIssues(projectId, issueIds);
        if (agilePluginService != null) {
            agilePluginService.deleteIssueProductRel(projectId, issueIds);
        }
        if (agileWaterfallService != null) {
            final Map<Long, IssueDTO> idToIssueMap = issueDTOList.stream().collect(Collectors.toMap(IssueDTO::getIssueId, Function.identity()));
            for (Long issueId : issueIds) {
                IssueDTO issue = idToIssueMap.getOrDefault(issueId, new IssueDTO());
                agileWaterfallService.deleteIssueForWaterfall(projectId, issueId, this.modelMapper.map(issue, IssueConvertDTO.class));
            }
        }
        dataLogRedisUtil.deleteByDeleteIssueInfo(projectId);
    }

    @Override
    public void batchDeleteIssues(Long projectId, List<Long> issueIds) {
        if (issueMapper.queryIssueIdsIsTest(projectId, issueIds) != issueIds.size()) {
            throw new CommonException(ERROR_ISSUE_TYPE_NOT_ISSUE_TEST);
        }
        List<Long> issueIdList = issueMapper.queryIssueSubListByIssueIds(projectId, issueIds);
        issueIds.addAll(issueIdList);
        issueMapper.batchDeleteIssues(projectId, issueIds);
        issueIds.forEach(issueId -> deleteIssueInfo(issueId, projectId));
        //delete cache
        dataLogRedisUtil.deleteByDeleteIssueInfo(projectId);
    }


    public void handleCreateSprintRel(Long sprintId, Long projectId, Long issueId) {
        if (sprintId != null && !Objects.equals(sprintId, 0L)) {
            IssueSprintRelDTO issueSprintRelDTO = new IssueSprintRelDTO();
            issueSprintRelDTO.setIssueId(issueId);
            issueSprintRelDTO.setSprintId(sprintId);
            if (issueSprintRelMapper.selectOne(issueSprintRelDTO) == null) {
                issueSprintRelDTO.setProjectId(projectId);
                issueSprintRelService.createIssueSprintRel(issueSprintRelDTO);
            }
        }
    }

    @Override
    public void handleInitSubIssue(IssueConvertDTO subIssueConvertDTO, Long statusId, ProjectInfoDTO projectInfoDTO) {
        IssueConvertDTO parentIssueConvertDTO = modelMapper.map(issueMapper.queryIssueSprintNotClosed(subIssueConvertDTO.getProjectId(), subIssueConvertDTO.getParentIssueId()), IssueConvertDTO.class);
        //设置初始状态,跟随父类状态
        subIssueConvertDTO = parentIssueConvertDTO.initializationSubIssue(subIssueConvertDTO, statusId, projectInfoDTO);
        projectInfoService.updateIssueMaxNum(subIssueConvertDTO.getProjectId(), subIssueConvertDTO.getIssueNum());
        //初始化排序
        if (subIssueConvertDTO.isIssueRank()) {
            calculationRank(subIssueConvertDTO.getProjectId(), subIssueConvertDTO);
        }
        setRemainingTime(subIssueConvertDTO);
        // 处理预计、实际时间
        handleEstimateTimeAndActualTime(subIssueConvertDTO);
    }

    private void setRemainingTime(IssueConvertDTO issueConvertDTO) {
        if (!ISSUE_EPIC.equals(issueConvertDTO.getTypeCode())) {
            if (!ObjectUtils.isEmpty(issueConvertDTO.getEstimateTime()) && ObjectUtils.isEmpty(issueConvertDTO.getRemainingTime())) {
                issueConvertDTO.setRemainingTime(issueConvertDTO.getEstimateTime());
            }
        }
    }

    @Override
    public List<IssueSearchVO> batchIssueToVersion(Long projectId, Long versionId, List<Long> issueIds) {
        if (versionId != null && !Objects.equals(versionId, 0L)) {
            productVersionValidator.judgeExist(projectId, versionId);
            VersionIssueRelDTO versionIssueRelDTO = new VersionIssueRelDTO();
            versionIssueRelDTO.createBatchIssueToVersionDTO(projectId, versionId, issueIds);
            issueAccessDataService.batchIssueToVersion(versionIssueRelDTO);
        } else {
            issueAccessDataService.batchRemoveVersion(projectId, issueIds);
        }
        return issueSearchAssembler.dtoListToVO(issueMapper.queryIssueByIssueIds(projectId, issueIds),
                new HashMap<>(), new HashMap<>(), new HashMap<>(), new HashMap<>());
    }

    @Override
    public void batchIssueToVersionTest(Long projectId, Long versionId, List<Long> issueIds) {
        if (versionId != null && !Objects.equals(versionId, 0L)) {
            productVersionValidator.judgeExist(projectId, versionId);
            if (issueMapper.queryIssueIdsIsTest(projectId, issueIds) != issueIds.size()) {
                throw new CommonException(ERROR_ISSUE_TYPE_NOT_ISSUE_TEST);
            }
            issueAccessDataService.batchRemoveVersionTest(projectId, issueIds);
            VersionIssueRelDTO versionIssueRelDTO = new VersionIssueRelDTO();
            versionIssueRelDTO.createBatchIssueToVersionDTO(projectId, versionId, issueIds);
            issueAccessDataService.batchIssueToVersion(versionIssueRelDTO);
        }
    }

    @Override
    public List<IssueSearchVO> batchIssueToEpic(Long projectId, Long epicId, List<Long> issueIds) {
        issueValidator.judgeEpicCanUpdateAndExist(projectId, epicId);
        issueAccessDataService.batchIssueToEpic(projectId, epicId, issueIds);
        return issueSearchAssembler.dtoListToVO(issueMapper.queryIssueByIssueIds(projectId, issueIds), new HashMap<>(), new HashMap<>(), new HashMap<>(), new HashMap<>());
    }


    protected void dataLogRank(Long projectId, MoveIssueVO moveIssueVO, String rankStr, Long sprintId) {
        for (Long issueId : moveIssueVO.getIssueIds()) {
            SprintNameVO activeSprintName = sprintNameAssembler.toTarget(issueMapper.queryActiveSprintNameByIssueId(issueId), SprintNameVO.class);
            Boolean condition = (sprintId == 0 && activeSprintName == null) || (activeSprintName != null
                    && sprintId.equals(activeSprintName.getSprintId()));
            if (condition) {
                DataLogDTO dataLogDTO = new DataLogDTO();
                dataLogDTO.setProjectId(projectId);
                dataLogDTO.setField(FIELD_RANK);
                dataLogDTO.setIssueId(issueId);
                dataLogDTO.setNewString(rankStr);
                dataLogService.create(dataLogDTO);
            }
        }
    }

    @Override
    public List<IssueSearchVO> batchIssueToSprint(Long projectId, Long sprintId, MoveIssueVO moveIssueVO) {
        sprintValidator.judgeExist(projectId, sprintId);
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        List<MoveIssueDTO> moveIssueDTOS = new ArrayList<>();
        if (moveIssueVO.getBefore()) {
            beforeRank(projectId, sprintId, moveIssueVO, moveIssueDTOS);
        } else {
            afterRank(projectId, sprintId, moveIssueVO, moveIssueDTOS);
        }
        //处理评级日志
        handlerDataLogRank(moveIssueVO, projectId, sprintId);
        issueAccessDataService.batchUpdateIssueRank(projectId, moveIssueDTOS);
        List<Long> moveIssueIds = moveIssueVO.getIssueIds();
        List<Long> frontIncomingIssues = deepCopy(moveIssueIds);
        //处理子任务与子缺陷
        List<Long> subTaskIds = issueMapper.querySubIssueIds(projectId, moveIssueIds);
        List<Long> subBugIds = issueMapper.querySubBugIds(projectId, moveIssueIds);
        if (CollectionUtils.isNotEmpty(subTaskIds)) {
            moveIssueIds.addAll(subTaskIds);
        }
        if (CollectionUtils.isNotEmpty(subBugIds)) {
            moveIssueIds.addAll(subBugIds);
        }
        //把与现在冲刺与要移动的冲刺相同的issue排除掉
        List<IssueSearchDTO> issueSearchDTOList = issueMapper.queryIssueByIssueIds(projectId, moveIssueVO.getIssueIds()).stream()
                .filter(issueDO -> issueDO.getSprintId() == null ? sprintId != 0 : !issueDO.getSprintId().equals(sprintId)).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(issueSearchDTOList)) {
            List<Long> moveIssueIdsFilter = issueSearchDTOList.stream().map(IssueSearchDTO::getIssueId).collect(Collectors.toList());
            BatchRemoveSprintDTO batchRemoveSprintDTO = new BatchRemoveSprintDTO(projectId, sprintId, moveIssueIdsFilter);
            issueAccessDataService.removeIssueFromSprintByIssueIds(batchRemoveSprintDTO);
            if (sprintId != null && !Objects.equals(sprintId, 0L)) {
                issueAccessDataService.issueToDestinationByIds(projectId, sprintId, moveIssueIdsFilter, new Date(), customUserDetails.getUserId());
                invokeUpdateSprintAspect(projectId, moveIssueIdsFilter);
                if (agilePluginService != null) {
                    agilePluginService.handlerAssociateSprintsWithFeature(projectId, sprintId, frontIncomingIssues, issueSearchDTOList);
                }
            }
            List<Long> assigneeIds = issueSearchDTOList.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueSearchDTO::getAssigneeId).distinct().collect(Collectors.toList());
            Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
            return issueSearchAssembler.dtoListToVO(issueSearchDTOList, usersMap, new HashMap<>(), new HashMap<>(), new HashMap<>());
        } else {
            return new ArrayList<>();
        }

    }

    private void invokeUpdateSprintAspect(Long projectId, List<Long> issueIds) {
        if (ObjectUtils.isEmpty(issueIds)) {
            return;
        }
        Map<Long, IssueDTO> issueMap =
                issueMapper.selectByIds(org.apache.commons.lang3.StringUtils.join(issueIds, ","))
                        .stream()
                        .collect(Collectors.toMap(IssueDTO::getIssueId, Function.identity()));
        List<String> fieldList = Arrays.asList("rank", "sprintId");
        List<TriggerCarrierVO> triggerCarriers = new ArrayList<>();
        issueIds.forEach(issueId -> {
            IssueDTO dto = issueMap.get(issueId);
            if (ObjectUtils.isEmpty(dto)) {
                return;
            }
            TriggerCarrierVO triggerCarrier = new TriggerCarrierVO();
            triggerCarrier.setProjectId(projectId);
            triggerCarrier.setInstanceId(issueId);
            triggerCarrier.setMemberFieldIds(new HashSet<>());
            triggerCarrier.setFieldList(fieldList);
            triggerCarrier.setExecutedRules(new ArrayList<>());
            triggerCarrier.setIssueTypeId(dto.getIssueTypeId());
            triggerCarrier.setCheckMode(false);
            triggerCarrier.setAuditDomain(dto);
            triggerCarrier.setNoticeInstanceId(issueId);
            triggerCarriers.add(triggerCarrier);
        });
        this.self().batchUpdateInvokeTrigger(triggerCarriers);
    }

    private void handlerDataLogRank(MoveIssueVO moveIssueVO, Long projectId, Long sprintId) {
        if (moveIssueVO.getRankIndex() != null) {
            if (!moveIssueVO.getRankIndex()) {
                dataLogRank(projectId, moveIssueVO, RANK_LOWER, sprintId);
            } else if (moveIssueVO.getRankIndex()) {
                dataLogRank(projectId, moveIssueVO, RANK_HIGHER, sprintId);
            }
        }
    }

    private List<Long> deepCopy(List<Long> src) {
        List<Long> dest = new ArrayList<>(src.size());
        src.forEach(dest::add);
        return dest;
    }

    @Override
    public void batchHandleIssueStatus(Long projectId, List<Long> moveIssueIds, Long sprintId) {
        SprintSearchDTO sprintSearchDTO = sprintMapper.queryActiveSprintNoIssueIds(projectId);
        if (sprintSearchDTO == null || !Objects.equals(sprintId, sprintSearchDTO.getSprintId())) {
            List<IssueConvertDTO> issueConvertDTOList = issueAssembler.toTargetList(issueMapper.queryIssueByIssueIdsAndSubIssueIds(moveIssueIds), IssueConvertDTO.class);
            Map<Long, IssueTypeWithStateMachineIdVO> issueTypeWithStateMachineIdDTOMap = ConvertUtil.queryIssueTypesWithStateMachineIdByProjectId(projectId, AGILE);
            issueConvertDTOList.forEach(issueE -> {
                Long initStatusId = issueTypeWithStateMachineIdDTOMap.get(issueE.getIssueTypeId()).getInitStatusId();
                if (!issueE.getStatusId().equals(initStatusId)) {
                    issueE.setStatusId(initStatusId);
                    issueAccessDataService.update(issueE, new String[]{STATUS_ID});
                }
            });
        }
    }


    protected void beforeRank(Long projectId, Long sprintId, MoveIssueVO moveIssueVO, List<MoveIssueDTO> moveIssueDTOS) {
        moveIssueVO.setIssueIds(issueMapper.queryIssueIdOrderByRankDesc(projectId, moveIssueVO.getIssueIds()));
        if (moveIssueVO.getOutsetIssueId() == null || Objects.equals(moveIssueVO.getOutsetIssueId(), 0L)) {
            noOutsetBeforeRank(projectId, sprintId, moveIssueVO, moveIssueDTOS);
        } else {
            outsetBeforeRank(projectId, sprintId, moveIssueVO, moveIssueDTOS);
        }
    }

    private void outsetBeforeRank(Long projectId, Long sprintId, MoveIssueVO moveIssueVO, List<MoveIssueDTO> moveIssueDTOS) {
        String rightRank = issueMapper.queryRank(projectId, moveIssueVO.getOutsetIssueId());
        String leftRank = issueMapper.queryLeftRank(projectId, sprintId, rightRank);
        if (leftRank == null) {
            for (Long issueId : moveIssueVO.getIssueIds()) {
                rightRank = RankUtil.genPre(rightRank);
                moveIssueDTOS.add(new MoveIssueDTO(issueId, rightRank));
            }
        } else {
            for (Long issueId : moveIssueVO.getIssueIds()) {
                rightRank = RankUtil.between(leftRank, rightRank);
                moveIssueDTOS.add(new MoveIssueDTO(issueId, rightRank));
            }
        }
    }

    private void noOutsetBeforeRank(Long projectId, Long sprintId, MoveIssueVO moveIssueVO, List<MoveIssueDTO> moveIssueDTOS) {
        String minRank = sprintMapper.queryMinRank(projectId, sprintId);
        if (minRank == null) {
            minRank = RankUtil.mid();
            for (Long issueId : moveIssueVO.getIssueIds()) {
                moveIssueDTOS.add(new MoveIssueDTO(issueId, minRank));
                minRank = RankUtil.genPre(minRank);
            }
        } else {
            for (Long issueId : moveIssueVO.getIssueIds()) {
                minRank = RankUtil.genPre(minRank);
                moveIssueDTOS.add(new MoveIssueDTO(issueId, minRank));
            }
        }
    }

    protected void afterRank(Long projectId, Long sprintId, MoveIssueVO moveIssueVO, List<MoveIssueDTO> moveIssueDTOS) {
        moveIssueVO.setIssueIds(issueMapper.queryIssueIdOrderByRankAsc(projectId, moveIssueVO.getIssueIds()));
        String leftRank = issueMapper.queryRank(projectId, moveIssueVO.getOutsetIssueId());
        String rightRank = issueMapper.queryRightRank(projectId, sprintId, leftRank);
        if (rightRank == null) {
            for (Long issueId : moveIssueVO.getIssueIds()) {
                leftRank = RankUtil.genNext(leftRank);
                moveIssueDTOS.add(new MoveIssueDTO(issueId, leftRank));
            }
        } else {
            for (Long issueId : moveIssueVO.getIssueIds()) {
                leftRank = RankUtil.between(leftRank, rightRank);
                moveIssueDTOS.add(new MoveIssueDTO(issueId, leftRank));
            }
        }
    }

    @Override
    public Page<IssueEpicVO> listEpicSelectData(Long projectId, PageRequest pageRequest, Boolean onlyUnCompleted, String param, List<Long> epicIds) {
        Page<IssueEpicVO> page;
        boolean append = !ObjectUtils.isEmpty(epicIds) && pageRequest.getPage() == 0;
        if (agilePluginService != null && agilePluginService.isSubProjectAndArtDoing(projectId)) {
            page = agilePluginService.selectEpicBySubProjectFeature(projectId, pageRequest, onlyUnCompleted, param, epicIds, append);
        } else {
            page = PageHelper.doPage(pageRequest, () -> issueMapper.queryIssueEpicSelectList(projectId, onlyUnCompleted, param, epicIds));
            if (append) {
                List<IssueEpicVO> list = issueMapper.queryIssueEpicByIds(projectId, epicIds);
                list.addAll(page.getContent());
                page.setContent(list);
            }
        }
        return page;
    }


    @Override
    public IssueSubVO queryIssueSub(Long projectId, Long organizationId, Long issueId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        issue.setPriorityVO(priorityService.queryById(organizationId, issue.getPriorityId()));
        issue.setIssueTypeVO(issueTypeService.queryById(issue.getIssueTypeId(), projectId));
        issue.setStatusVO(statusService.queryStatusById(organizationId, issue.getStatusId()));
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(filePathService.generateFullPath(issueAttachmentDO.getUrl())));
        }
        return issueAssembler.issueDetailDoToIssueSubDto(issue);
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_CREATED, instanceId = "issueId", allFieldCheck = true)
    public IssueSubVO queryIssueSubByCreate(Long projectId, Long issueId) {
        return queryIssueSubByCreateWithoutRuleNotice(projectId, issueId);
    }

    @Override
    public IssueSubVO queryIssueSubByCreateWithoutRuleNotice(Long projectId, Long issueId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(filePathService.generateFullPath(issueAttachmentDO.getUrl())));
        }
        IssueSubVO result = issueAssembler.issueDetailDoToIssueSubDto(issue);
        sendMsgUtil.sendMsgBySubIssueCreate(projectId, result, DetailsHelper.getUserDetails().getUserId());
        setCompletedAndActualCompletedDate(result);
        return result;
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_CREATED, isBatch = true, allFieldCheck = true)
    public void batchCreateIssueInvokeTrigger(List<TriggerCarrierVO> triggerCarriers) {
        // TODO document why this method is empty
    }

    @Override
    public synchronized IssueVO updateIssueTypeCode(IssueConvertDTO issueConvertDTO,
                                                    IssueUpdateTypeVO issueUpdateTypeVO,
                                                    Long organizationId,
                                                    Long projectId) {
        List<String> fieldList = new ArrayList<>(Arrays.asList(UPDATE_TYPE_CODE_FIELD_LIST_NO_RANK));
        String originType = issueConvertDTO.getTypeCode();
        if (originType.equals(SUB_TASK) && !Objects.equals(issueUpdateTypeVO.getTypeCode(), SUB_TASK)) {
            issueConvertDTO.setParentIssueId(null);
        }
        if (STORY_TYPE.equals(issueConvertDTO.getTypeCode()) && issueConvertDTO.getStoryPoints() != null) {
            issueConvertDTO.setStoryPoints(null);
        }
        if (originType.equals("bug") && (issueConvertDTO.getRelateIssueId() != null && !Objects.equals(issueConvertDTO.getRelateIssueId(), 0L))) {
            issueConvertDTO.setRelateIssueId(null);
        }
        if ((originType.equals(STORY_TYPE) || originType.equals("task"))
                && (!Objects.equals(issueUpdateTypeVO.getTypeCode(), STORY_TYPE) && !Objects.equals(issueUpdateTypeVO.getTypeCode(), "task"))) {
            issueMapper.updateSubBugRelateIssueId(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId());
        }
        if (agileWaterfallService != null && Objects.equals(SchemeApplyType.WATERFALL, issueUpdateTypeVO.getApplyType())) {
            agileWaterfallService.checkUpdateIssueTypeCode(projectId, issueConvertDTO, issueUpdateTypeVO, fieldList);
        }
        if (issueUpdateTypeVO.getTypeCode().equals(ISSUE_EPIC)) {
            issueConvertDTO.setRank(null);
            fieldList.add(RANK_FIELD);
            issueConvertDTO.setTypeCode(issueUpdateTypeVO.getTypeCode());
            issueConvertDTO.setEpicName(issueUpdateTypeVO.getEpicName());
            List<LookupValueDTO> colorList = lookupValueMapper.queryLookupValueByCode(EPIC_COLOR_TYPE).getLookupValues();
            issueConvertDTO.initializationColor(colorList);
            issueConvertDTO.setRemainingTime(null);
            issueConvertDTO.setEpicId(0L);
            //排序编号
            Integer sequence = issueMapper.queryMaxEpicSequenceByProject(issueConvertDTO.getProjectId());
            issueConvertDTO.setEpicSequence(sequence == null ? 0 : sequence + 1);
        } else if (issueConvertDTO.getTypeCode().equals(ISSUE_EPIC)) {
            // 如果之前类型是epic，会把该epic下的issue的epicId置为0
            issueAccessDataService.batchUpdateIssueEpicId(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId());
            issueConvertDTO.setTypeCode(issueUpdateTypeVO.getTypeCode());
            issueConvertDTO.setColorCode(null);
            issueConvertDTO.setEpicName(null);
            issueConvertDTO.setEpicSequence(null);
            //rank值重置
            calculationRank(issueConvertDTO.getProjectId(), issueConvertDTO);
            fieldList.add(RANK_FIELD);
        } else {
            issueConvertDTO.setTypeCode(issueUpdateTypeVO.getTypeCode());
        }
        issueConvertDTO.setIssueTypeId(issueUpdateTypeVO.getIssueTypeId());
        issueAccessDataService.update(issueConvertDTO, fieldList.toArray(new String[fieldList.size()]));
        // 查看目标问题类型的状态机是否含有当前状态，没有就是用默认状态
        handlerStatus(issueConvertDTO.getProjectId(),issueUpdateTypeVO);
        //更新字段值
        updateIssueFieldValue(issueUpdateTypeVO, issueUpdateTypeVO.getIssueId(), projectId, organizationId);
        if (agileWaterfallService != null) {
            agileWaterfallService.handlerUpdateIssueTypeCode(projectId, originType, issueUpdateTypeVO);
        }
        IssueVO result = queryIssue(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId(), organizationId);
        setCompletedAndActualCompletedDate(result);
        return result;
    }

    private void updateIssueFieldValue(IssueUpdateTypeVO issueUpdateTypeVO,
                                       Long issueId,
                                       Long projectId,
                                       Long organizationId) {
        BatchUpdateFieldsValueVo batchUpdateFieldsValueVo = issueUpdateTypeVO.getBatchUpdateFieldsValueVo();
        if (batchUpdateFieldsValueVo == null) {
            return;
        }
        validateRequiredFields(issueUpdateTypeVO, issueId, projectId, organizationId);
        List<Long> issueIds = Arrays.asList(issueId);
        JSONObject predefinedFields = batchUpdateFieldsValueVo.getPredefinedFields();
        BatchUpdateFieldStatusVO batchUpdateFieldStatusVO = new BatchUpdateFieldStatusVO();
        Map<Long, TriggerCarrierVO> triggerCarrierMap = new HashMap<>();
        if (!ObjectUtils.isEmpty(predefinedFields)) {
            fieldValueService.handlerPredefinedFields(projectId, issueIds, predefinedFields, batchUpdateFieldStatusVO, SchemeApplyType.AGILE, false, triggerCarrierMap);
        }
        List<PageFieldViewUpdateVO> customFields = batchUpdateFieldsValueVo.getCustomFields();
        if (!ObjectUtils.isEmpty(customFields)) {
            fieldValueService.handlerCustomFields(projectId, customFields, AGILE_SCHEME_CODE, issueIds, batchUpdateFieldStatusVO, false, triggerCarrierMap);
        }
        if(!ObjectUtils.isEmpty(batchUpdateFieldStatusVO.getErrorMsgMap())) {
            throw new CommonException("error.update.field.value");
        }
        this.self().batchUpdateInvokeTrigger(triggerCarrierMap.values().stream().collect(Collectors.toList()));
    }

    private void validateRequiredFields(IssueUpdateTypeVO issueUpdateTypeVO,
                                        Long issueId,
                                        Long projectId,
                                        Long organizationId) {
        List<PageFieldViewVO> pageFieldViewList =
                listRequiredFieldByIssueType(projectId, organizationId, issueId, issueUpdateTypeVO.getIssueTypeId());
        if (pageFieldViewList.isEmpty()) {
            return;
        }
        Set<String> systemFieldCodes = new HashSet<>();
        Set<Long> customFieldIds = new HashSet<>();
        pageFieldViewList.forEach(x -> {
            if (x.getSystem()) {
                systemFieldCodes.add(x.getFieldCode());
            } else {
                customFieldIds.add(x.getFieldId());
            }
        });
        BatchUpdateFieldsValueVo batchUpdateFieldsValueVo = issueUpdateTypeVO.getBatchUpdateFieldsValueVo();
        if (batchUpdateFieldsValueVo == null) {
            throw new CommonException("error.required.field.empty");
        }
        JSONObject predefinedFields = batchUpdateFieldsValueVo.getPredefinedFields();
        boolean allFieldEmpty = true;
        if (!ObjectUtils.isEmpty(predefinedFields)) {
            Set<String> systemFields = predefinedFields.keySet();
            Set<String> fieldCodes = convertToFieldCodes(systemFields);
            systemFieldCodes.forEach(x -> {
                if (!fieldCodes.contains(x)) {
                    throw new CommonException("error.required.field.empty." + x);
                }
            });
            allFieldEmpty = false;
        }
        List<PageFieldViewUpdateVO> customFields = batchUpdateFieldsValueVo.getCustomFields();
        if (!ObjectUtils.isEmpty(customFields)) {
            Set<Long> inputCustomFields = new HashSet<>();
            customFields.forEach(x -> inputCustomFields.add(x.getFieldId()));
            customFieldIds.forEach(x -> {
                if (!inputCustomFields.contains(x)) {
                    throw new CommonException("error.required.field.empty." + x);
                }
            });
            allFieldEmpty=false;
        }
        if (allFieldEmpty) {
            throw new CommonException("error.required.field.empty");
        }
    }

    private Set<String> convertToFieldCodes(Set<String> systemFields) {
        Map<String, String> map = new HashMap<>();
        map.put(ASSIGNEE_ID, FieldCode.ASSIGNEE);
        map.put("componentIssueRelVOList", FieldCode.COMPONENT);
        map.put(ENVIRONMENT, FieldCode.ENVIRONMENT);
        map.put(ESTIMATED_END_TIME, FieldCode.ESTIMATED_END_TIME);
        map.put(ESTIMATED_START_TIME, FieldCode.ESTIMATED_START_TIME);
        map.put("actualStartTime", FieldCode.ACTUAL_START_TIME);
        map.put("actualEndTime", FieldCode.ACTUAL_END_TIME);
        map.put("fixVersion", FieldCode.FIX_VERSION);
        map.put("influenceVersion", FieldCode.INFLUENCE_VERSION);
        map.put("labelIssueRelVOList", FieldCode.LABEL);
        map.put(MAIN_RESPONSIBLE_ID, FieldCode.MAIN_RESPONSIBLE);
        map.put(PRIORITY_ID, FieldCode.PRIORITY);
        map.put(REMAIN_TIME_FIELD, FieldCode.REMAINING_TIME);
        map.put(REPORTER_ID, FieldCode.REPORTER);
        map.put(SPRINT_ID_FIELD, FieldCode.SPRINT);
        map.put(STATUS_ID, FieldCode.STATUS);
        map.put(STORY_POINTS_FIELD, FieldCode.STORY_POINTS);
        map.put("description", FieldCode.DESCRIPTION);
        map.put(EPIC_NAME_FIELD, FieldCode.EPIC_NAME);
        map.put(EPIC_ID_FIELD, FieldCode.EPIC);
        map.put("tags", FieldCode.TAG);
        map.put("participantIds", FieldCode.PARTICIPANT);
        map.put("estimateTime", FieldCode.ESTIMATE_TIME);
        map.put("productIds", FieldCode.PRODUCT);
        Set<String> result = new HashSet<>();
        systemFields.forEach(x -> {
            if (map.get(x) != null) {
                result.add(map.get(x));
            }
        });
        return result;
    }

    private void handlerStatus(Long projectId, IssueUpdateTypeVO issueUpdateTypeVO) {
        IssueConvertDTO issueConvertDTO = queryIssueByProjectIdAndIssueId(projectId, issueUpdateTypeVO.getIssueId());
        Long currentStateMachineId = projectConfigService.queryStateMachineId(projectId, issueUpdateTypeVO.getApplyType(), issueUpdateTypeVO.getIssueTypeId());
        // 查询状态机里面的状态
        List<StatusMachineNodeVO> statusMachineNodeVOS = stateMachineNodeService.queryByStateMachineId(ConvertUtil.getOrganizationId(projectId), currentStateMachineId, false);
        if (CollectionUtils.isEmpty(statusMachineNodeVOS)) {
            throw new CommonException("error.current.state.machine.node.null");
        }
        List<Long> list = statusMachineNodeVOS.stream().map(StatusMachineNodeVO::getStatusId).collect(Collectors.toList());
        if (!list.contains(issueConvertDTO.getStatusId())) {
            StatusMachineNodeVO statusMachineNodeVO = statusMachineNodeVOS.stream().filter(v -> NodeType.INIT.equals(v.getType())).findAny().orElse(null);
            if (ObjectUtils.isEmpty(statusMachineNodeVO)) {
                throw new CommonException("error.init.node.not.found");
            }
            issueConvertDTO.setStatusId(statusMachineNodeVO.getStatusId());
            issueAccessDataService.update(issueConvertDTO, new String[]{STATUS_ID});
        }
    }

    @Override
    public IssueConvertDTO queryIssueByProjectIdAndIssueId(Long projectId, Long issueId) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setProjectId(projectId);
        issueDTO.setIssueId(issueId);
        return modelMapper.map(issueMapper.selectOne(issueDTO), IssueConvertDTO.class);
    }

    private void handleCreateLabelIssue(List<LabelIssueRelVO> labelIssueRelVOList, Long issueId) {
        if (labelIssueRelVOList != null && !labelIssueRelVOList.isEmpty()) {
            List<LabelIssueRelDTO> labelIssueDTOList = modelMapper.map(labelIssueRelVOList, new TypeToken<List<LabelIssueRelDTO>>() {
            }.getType());
            labelIssueDTOList.forEach(labelIssueRelDTO -> {
                labelIssueRelDTO.setIssueId(issueId);
                handleLabelIssue(labelIssueRelDTO);
            });
        }
    }

    private void handleCreateVersionIssueRel(List<VersionIssueRelVO> versionIssueRelVOList, Long projectId, Long issueId) {
        if (versionIssueRelVOList != null && !versionIssueRelVOList.isEmpty()) {
            handleVersionIssueRel(modelMapper.map(versionIssueRelVOList, new TypeToken<List<VersionIssueRelDTO>>() {
            }.getType()), projectId, issueId);
        }
    }

    private void handleCreateIssueLink(List<IssueLinkCreateVO> issueLinkCreateVOList, Long projectId, Long issueId) {
        if (issueLinkCreateVOList != null && !issueLinkCreateVOList.isEmpty()) {
            List<IssueLinkDTO> issueLinkDTOList = issueLinkAssembler.toTargetList(issueLinkCreateVOList, IssueLinkDTO.class);
            issueLinkDTOList.forEach(issueLinkDTO -> {
                Long linkIssueId = issueLinkDTO.getLinkedIssueId();
                issueLinkDTO.setIssueId(Boolean.TRUE.equals(issueLinkDTO.getIn()) ? issueId : linkIssueId);
                issueLinkDTO.setLinkedIssueId(Boolean.TRUE.equals(issueLinkDTO.getIn()) ? linkIssueId : issueId);
                issueLinkDTO.setProjectId(projectId);
                issueLinkValidator.verifyCreateData(issueLinkDTO);
                if (issueLinkValidator.checkUniqueLink(issueLinkDTO)) {
                    issueLinkService.create(issueLinkDTO);
                    BaseFieldUtil.updateIssueLastUpdateInfo(issueLinkDTO.getLinkedIssueId(), issueLinkDTO.getProjectId());
                }
            });
        }
    }

    private void handleVersionIssueRel(List<VersionIssueRelDTO> versionIssueRelDTOList, Long projectId, Long issueId) {
        for (VersionIssueRelDTO versionIssueRel : versionIssueRelDTOList) {
            versionIssueRel.setIssueId(issueId);
            versionIssueRel.setProjectId(projectId);
            versionIssueRel.setRelationType(versionIssueRel.getRelationType() == null ? ProductVersionService.VERSION_RELATION_TYPE_FIX : versionIssueRel.getRelationType());
            issueValidator.verifyVersionIssueRelData(versionIssueRel);
            handleVersionIssueRelCreate(versionIssueRel);
        }
    }

    private void handleVersionIssueRelCreate(VersionIssueRelDTO versionIssueRelDTO) {
        if (issueValidator.notExistVersionIssueRel(versionIssueRelDTO)) {
            versionIssueRelService.create(versionIssueRelDTO);
        }
    }

    private void handleCreateComponentIssueRel(List<ComponentIssueRelVO> componentIssueRelVOList, Long projectId, Long issueId, ProjectInfoDTO projectInfoDTO, Boolean assigneeCondition) {
        if (componentIssueRelVOList != null && !componentIssueRelVOList.isEmpty()) {
            handleComponentIssueRelWithHandleAssignee(modelMapper.map(componentIssueRelVOList, new TypeToken<List<ComponentIssueRelDTO>>() {
            }.getType()), projectId, issueId, projectInfoDTO, assigneeCondition);
        }
    }

    private void handleComponentIssueRelWithHandleAssignee(List<ComponentIssueRelDTO> componentIssueRelDTOList, Long projectId, Long issueId, ProjectInfoDTO projectInfoDTO, Boolean assigneeCondition) {
        componentIssueRelDTOList.forEach(componentIssueRelDTO -> {
            handleComponentIssueRel(componentIssueRelDTO, projectId, issueId);
            //issue经办人可以根据模块策略进行区分
            if (assigneeCondition) {
                handleComponentIssue(componentIssueRelDTO, issueId, projectInfoDTO);
            }
        });
    }


    private void handleComponentIssueRel(ComponentIssueRelDTO componentIssueRelDTO, Long projectId, Long issueId) {
        componentIssueRelDTO.setIssueId(issueId);
        componentIssueRelDTO.setProjectId(projectId);
        issueValidator.verifyComponentIssueRelData(componentIssueRelDTO);
        //重名校验
        if (componentIssueRelDTO.getName() != null && componentIssueRelDTO.getComponentId() == null) {
            if (issueComponentMapper.checkNameExist(componentIssueRelDTO.getName(), componentIssueRelDTO.getProjectId())) {
                componentIssueRelDTO.setComponentId(issueComponentMapper.queryComponentIdByNameAndProjectId(
                        componentIssueRelDTO.getName(), componentIssueRelDTO.getProjectId()));
            } else {
                IssueComponentDTO issueComponentDTO = new IssueComponentDTO(componentIssueRelDTO.getName(), componentIssueRelDTO.getProjectId());
                issueComponentDTO = issueComponentService.createBase(issueComponentDTO);
                componentIssueRelDTO.setComponentId(issueComponentDTO.getComponentId());
            }
        }
        if (issueValidator.notExistComponentIssueRel(componentIssueRelDTO)) {
            componentIssueRelService.create(componentIssueRelDTO);
        }
    }

    private void handleComponentIssue(ComponentIssueRelDTO componentIssueRelDTO, Long issueId, ProjectInfoDTO projectInfoDTO) {
        IssueComponentDTO issueComponentDTO = modelMapper.map(issueComponentMapper.selectByPrimaryKey(
                componentIssueRelDTO.getComponentId()), IssueComponentDTO.class);
        if (ISSUE_MANAGER_TYPE.equals(issueComponentDTO.getDefaultAssigneeRole()) && issueComponentDTO.getManagerId() !=
                null && issueComponentDTO.getManagerId() != 0) {
            //如果模块有选择模块负责人或者经办人的话，对应的issue的负责人要修改
            IssueConvertDTO issueConvertDTO = modelMapper.map(issueMapper.selectByPrimaryKey(issueId), IssueConvertDTO.class);
            Boolean condition = (issueConvertDTO.getAssigneeId() == null || issueConvertDTO.getAssigneeId() == 0) ||
                    (projectInfoDTO.getDefaultAssigneeType() != null);
            if (condition) {
                issueConvertDTO.setAssigneeId(issueComponentDTO.getManagerId());
                issueAccessDataService.update(issueConvertDTO, new String[]{ASSIGNEE_ID});
            }
        }
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldList = {RuleNotice.LABEL_ID}, instanceId = "issueId", idPosition = "arg")
    public void handleUpdateLabelIssue(List<LabelIssueRelVO> labelIssueRelVOList, Long issueId, Long projectId) {
        handleUpdateLabelIssueWithoutRuleNotice(labelIssueRelVOList, issueId, projectId);
    }

    @Override
    public void handleUpdateLabelIssueWithoutRuleNotice(List<LabelIssueRelVO> labelIssueRelVOList, Long issueId, Long projectId) {
        if (labelIssueRelVOList != null) {
            if (!labelIssueRelVOList.isEmpty()) {
                LabelIssueRelDTO labelIssueRelDTO = new LabelIssueRelDTO();
                labelIssueRelDTO.setIssueId(issueId);
                List<LabelIssueRelDTO> originLabels = modelMapper.map(labelIssueRelMapper.select(labelIssueRelDTO), new TypeToken<List<LabelIssueRelDTO>>() {
                }.getType());
                List<LabelIssueRelDTO> labelIssueDTOList = modelMapper.map(labelIssueRelVOList, new TypeToken<List<LabelIssueRelDTO>>() {
                }.getType());
                List<LabelIssueRelDTO> labelIssueCreateList = labelIssueDTOList.stream().filter(labelIssueRel ->
                        labelIssueRel.getLabelId() != null).collect(Collectors.toList());
                List<Long> curLabelIds = originLabels.stream().
                        map(LabelIssueRelDTO::getLabelId).collect(Collectors.toList());
                List<Long> createLabelIds = labelIssueCreateList.stream().
                        map(LabelIssueRelDTO::getLabelId).collect(Collectors.toList());
                curLabelIds.forEach(id -> {
                    if (!createLabelIds.contains(id)) {
                        LabelIssueRelDTO delete = new LabelIssueRelDTO();
                        delete.setIssueId(issueId);
                        delete.setLabelId(id);
                        delete.setProjectId(projectId);
                        labelIssueRelService.delete(delete);
                    }
                });
                labelIssueDTOList.forEach(labelIssueRel -> {
                    labelIssueRel.setIssueId(issueId);
                    handleLabelIssue(labelIssueRel);
                });
            } else {
                labelIssueRelService.batchDeleteByIssueId(issueId);
            }
            //没有issue使用的标签进行垃圾回收
            issueLabelService.labelGarbageCollection(projectId);
        }
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldList = {RuleNotice.VERSION_ID}, instanceId = "issueId", idPosition = "arg")
    public void handleUpdateVersionIssueRel(List<VersionIssueRelVO> versionIssueRelVOList, Long projectId, Long issueId, String versionType) {
        handleUpdateVersionIssueRelWithoutRuleNotice(versionIssueRelVOList, projectId, issueId, versionType);
    }

    private List<ComponentIssueRelDTO> getComponentIssueRel(Long projectId, Long issueId) {
        return componentIssueRelMapper.selectByProjectIdAndIssueId(projectId, issueId);
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldList = {RuleNotice.COMPONENT_ID}, instanceId = "issueId", idPosition = "arg")
    public void handleUpdateComponentIssueRel(List<ComponentIssueRelVO> componentIssueRelVOList, Long projectId, Long issueId) {
        handleUpdateComponentIssueRelWithoutRuleNotice(componentIssueRelVOList, projectId, issueId);
    }

    private void handleLabelIssue(LabelIssueRelDTO labelIssueRelDTO) {
        issueValidator.verifyLabelIssueData(labelIssueRelDTO);
        if (labelIssueRelDTO.getLabelName() != null && labelIssueRelDTO.getLabelId() == null) {
            //重名校验
            if (issueLabelMapper.checkNameExist(labelIssueRelDTO.getLabelName(), labelIssueRelDTO.getProjectId())) {
                labelIssueRelDTO.setLabelId(issueLabelMapper.queryLabelIdByLabelNameAndProjectId(labelIssueRelDTO.getLabelName(), labelIssueRelDTO.getProjectId()));
            } else {
                IssueLabelDTO issueLabelDTO = new IssueLabelDTO(labelIssueRelDTO.getLabelName(), labelIssueRelDTO.getProjectId());
                issueLabelDTO = issueLabelService.createBase(issueLabelDTO);
                labelIssueRelDTO.setLabelId(issueLabelDTO.getLabelId());
            }
        }
        if (issueValidator.notExistLabelIssue(labelIssueRelDTO)) {
            labelIssueRelService.create(labelIssueRelDTO);
        }
    }

    private Long getActiveSprintId(Long projectId) {
        SprintDTO sprintDTO = sprintService.getActiveSprint(projectId);
        if (sprintDTO != null) {
            return sprintDTO.getSprintId();
        }
        return null;
    }

    @Override
    public Page<IssueNumVO> queryIssueByOption(Long projectId, IssueFilterParamVO issueFilterParamVO, PageRequest pageRequest) {
        List<String> typeCodes = queryTypeCodesFromProject(projectId);
        //连表查询需要设置主表别名
        Map<String,String> orders = new HashMap<>();
        orders.put(ISSUE_NUM,ISSUE_NUM_CONVERT);
        Sort sort = PageUtil.sortResetOrder( pageRequest.getSort(), "ai", orders);
        pageRequest.setSort(sort);
        IssueNumDTO issueNumDTO = null;
        if (Boolean.TRUE.equals(issueFilterParamVO.getSelf())) {
            issueNumDTO = issueMapper.queryIssueByIssueNumOrIssueId(projectId, issueFilterParamVO.getIssueId(), issueFilterParamVO.getIssueNum());
            if (issueNumDTO != null) {
                pageRequest = new PageRequest(pageRequest.getPage(), pageRequest.getSize() - 1);
            }
        }
        Long activeSprintId = issueFilterParamVO.getOnlyActiveSprint() ? getActiveSprintId(projectId) : null;
        Page<IssueNumDTO> issueDOPage = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.queryIssueByOption(projectId, activeSprintId, issueFilterParamVO, typeCodes));
        if (issueFilterParamVO.getSelf() && issueNumDTO != null) {
            issueDOPage.getContent().add(0, issueNumDTO);
            issueDOPage.setSize(issueDOPage.getSize() + 1);
        }

        return PageUtil.buildPageInfoWithPageInfoList(issueDOPage, issueAssembler.issueNumDoToDto(issueDOPage.getContent(), projectId));
    }

    private List<String> queryTypeCodesFromProject(Long projectId) {
        List<String> typeCodes = new ArrayList<>();
        typeCodes.addAll(Arrays.asList(
                IssueTypeCode.STORY.value(),
                IssueTypeCode.TASK.value(),
                IssueTypeCode.BUG.value(),
                IssueTypeCode.SUB_TASK.value()));
        boolean isWaterfallProject = ProjectCategory.isWaterfallProject(projectId);
        if (isWaterfallProject) {
            typeCodes.addAll(Arrays.asList(IssueTypeCode.WATERFALL_ISSUE_TYPE_CODE));
        }
        return typeCodes;
    }

    @Override
    public void cloneIssueByIssueId(Long projectId, Long issueId, CopyConditionVO copyConditionVO, Long organizationId, String applyType, String asyncTraceId) {
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
            throw new CommonException("error.applyType.illegal");
        }
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String websocketKey = WEBSOCKET_COPY_ISSUE_CODE + BaseConstants.Symbol.MIDDLE_LINE + asyncTraceId;
        Map<String, Object> paramsMap = new HashMap<>();
        paramsMap.put("projectId", projectId);
        paramsMap.put("userId", EncryptionUtils.encrypt(userId));
        sendCloneProcess(userId, websocketKey, paramsMap, DOING_STATUS, 0);
        redisUtil.set(CLONE_ISSUE_KEY + issueId + BaseConstants.Symbol.COLON + asyncTraceId , DOING_STATUS, 24L, TimeUnit.HOURS);
        IssueDetailDTO issueDetailDTO = issueMapper.queryIssueDetail(projectId, issueId);
        if (issueDetailDTO == null) {
            sendCloneProcess(userId, websocketKey, paramsMap, FAILED_STATUS, 100);
            redisUtil.set(CLONE_ISSUE_KEY + issueId +BaseConstants.Symbol.COLON + asyncTraceId , FAILED_STATUS, 24L, TimeUnit.HOURS);
            throw new CommonException("error.issue.copyIssueByIssueId");
        }
        //处理需要复制的预定义字段
        List<String> predefinedFieldNames = copyConditionVO.getPredefinedFieldNames();
        if (predefinedFieldNames == null) {
            predefinedFieldNames = new ArrayList<>();
        }
        handleCopyPredefinedFields(organizationId, issueDetailDTO, predefinedFieldNames);
        List<CopyIssueRequiredFieldVO> copyIssueRequiredFieldVOS = copyConditionVO.getCopyIssueRequiredFieldVOS();
        Map<Long, CopyIssueRequiredFieldVO> copyIssueRequiredFieldVOMap = new HashMap<>();
        if(CollectionUtils.isNotEmpty(copyIssueRequiredFieldVOS)){
            copyIssueRequiredFieldVOMap.putAll(copyIssueRequiredFieldVOS.stream().collect(Collectors.toMap(CopyIssueRequiredFieldVO::getIssueId, Function.identity())));
        }
        CopyIssueRequiredFieldVO copyIssueRequiredFieldVO = copyIssueRequiredFieldVOMap.getOrDefault(issueId, new CopyIssueRequiredFieldVO());
        sendCloneProcess(userId, websocketKey, paramsMap, DOING_STATUS, 10);
        final Long newIssueId;
        Long objectVersionNumber;
        final String newIssueNum;
        issueDetailDTO.setSummary(copyConditionVO.getSummary());
        IssueTypeVO issueTypeVO = issueTypeService.queryById(issueDetailDTO.getIssueTypeId(), projectId);
        List<String> handlerRequireFiled = new ArrayList<>();
        if (issueTypeVO.getTypeCode().equals(SUB_TASK)) {
            IssueSubCreateVO issueSubCreateVO = issueAssembler.issueDtoToIssueSubCreateDto(issueDetailDTO, predefinedFieldNames);
            handlerRequireFiled = handlerCopyRequirePredefinedField(issueSubCreateVO, copyIssueRequiredFieldVO.getPredefinedFields());
            IssueSubVO newIssue = stateMachineClientService.createSubIssueWithoutRuleNotice(issueSubCreateVO);
            newIssueId = newIssue.getIssueId();
            newIssueNum = newIssue.getIssueNum();
            objectVersionNumber = newIssue.getObjectVersionNumber();
        } else {
            IssueCreateVO issueCreateVO = issueAssembler.issueDtoToIssueCreateDto(issueDetailDTO, predefinedFieldNames);
            if (ISSUE_EPIC.equals(issueCreateVO.getTypeCode())) {
                setEpicName(projectId, copyConditionVO, issueCreateVO);
            }
            handlerRequireFiled = handlerCopyRequirePredefinedField(issueCreateVO, copyIssueRequiredFieldVO.getPredefinedFields());
            IssueVO newIssue = stateMachineClientService.createIssueWithoutRuleNotice(issueCreateVO, applyType);
            newIssueId = newIssue.getIssueId();
            newIssueNum = newIssue.getIssueNum();
            objectVersionNumber = newIssue.getObjectVersionNumber();
        }
        sendCloneProcess(userId, websocketKey, paramsMap, DOING_STATUS, 30);
        // 复制关联内容
        objectVersionNumber = copyIssueLinkContents(copyConditionVO.getLinkContents(), issueId, newIssueId, projectId).getObjectVersionNumber();
        sendCloneProcess(userId, websocketKey, paramsMap, DOING_STATUS, 50);
        // 复制项目群的特性和史诗都不会去创建关联关系
        if (!(applyType.equals("program") && (issueDetailDTO.getTypeCode().equals(ISSUE_EPIC) || issueDetailDTO.getTypeCode().equals("feature")))) {
            //生成一条复制的关联
            objectVersionNumber = createCopyIssueLink(issueDetailDTO.getIssueId(), newIssueId, projectId).getObjectVersionNumber();
        }
        // 如果故事点和剩余工作量必填,就不需要再复制原issue的了
        setCopyRequireField(issueDetailDTO, handlerRequireFiled);
        //复制故事点和剩余工作量并记录日志
        objectVersionNumber = copyStoryPointAndRemainingTimeData(issueDetailDTO, projectId, newIssueId, objectVersionNumber).getObjectVersionNumber();
        // 处理冲刺、子任务、自定义字段的值
        List<TriggerCarrierVO> triggerCarrierVOS = new ArrayList<>();
        sendCloneProcess(userId, websocketKey, paramsMap, DOING_STATUS, 80);
        objectVersionNumber = handlerOtherFields(projectId, predefinedFieldNames, issueDetailDTO, newIssueId, copyConditionVO, copyIssueRequiredFieldVOMap, triggerCarrierVOS).getObjectVersionNumber();
        this.self().batchCreateIssueInvokeTrigger(triggerCarrierVOS);
        if (agileWaterfallService != null) {
            agileWaterfallService.handlerCopyIssue(issueDetailDTO, newIssueId, projectId);
        }
        paramsMap.put("newIssueId", EncryptionUtils.encrypt(newIssueId));
        paramsMap.put("newIssueNum", newIssueNum);
        sendCloneProcess(userId, websocketKey, paramsMap, SUCCEED_STATUS, 100);
        redisUtil.set(CLONE_ISSUE_KEY + issueId +BaseConstants.Symbol.COLON + asyncTraceId , SUCCEED_STATUS, 24L, TimeUnit.HOURS);
    }

    private void sendCloneProcess(Long userId,
                                  String websocketKey,
                                  Map<String, Object> paramsMap,
                                  String status,
                                  int process) {
        paramsMap.put("status", status);
        paramsMap.put("process", process);
        String message = null;
        try {
            message = objectMapper.writeValueAsString(paramsMap);
        } catch (JsonProcessingException e) {
            LOGGER.error("object to json error: {0}", e);
        }
        socketSendHelper.sendByUserId(userId, websocketKey, message);
    }

    @Override
    public IssueDTO copyIssueLinkContents(List<String> linkContents, Long oldIssueId, Long newIssueId, Long projectId) {
        IssueDTO issueDTO = this.issueMapper.selectByPrimaryKey(newIssueId);
        if (CollectionUtils.isNotEmpty(linkContents)) {
            for (String linkContent : linkContents) {
                issueDTO = copyIssueLinkContent(linkContent, oldIssueId, newIssueId, projectId);
            }
        }
        return issueDTO;
    }

    @Override
    public String queryAsyncCloneStatus(Long projectId, Long issueId, String asyncTraceId) {
        String key = CLONE_ISSUE_KEY + issueId + BaseConstants.Symbol.COLON + asyncTraceId;
        Object object = redisUtil.get(key);
        String status = StringUtils.EMPTY;
        if (!Objects.isNull(object)) {
            status = object.toString();
        }
        return ObjectUtils.isEmpty(status) ? "failed" : status;
    }

    private IssueDTO copyIssueLinkContent(String linkContent, Long issueId, Long newIssueId, Long projectId) {
        switch(linkContent) {
            case IssueCopyLinkContents.ISSUE_LINKS:
                // 关联工作项
                batchCreateCopyIssueLink(true, issueId, newIssueId, projectId);
                break;
            case IssueCopyLinkContents.ATTACHMENTS:
                // 附件
                issueAttachmentService.copyIssueAttachments(projectId, issueId, newIssueId);
                break;
            case IssueCopyLinkContents.KNOWLEDGE_RELATIONS:
                // 关联知识
                wikiRelationService.copyIssueKnowledgeRelations(projectId, issueId, newIssueId);
                break;
            case IssueCopyLinkContents.PREDECESSORS:
                // 前置依赖项
                issuePredecessorService.copyIssuePredecessors(projectId, issueId, newIssueId);
                break;
            case IssueCopyLinkContents.RELATED_BACKLOGS:
                // 关联需求
                if (backlogExpandService != null) {
                    backlogExpandService.copyIssueBacklogRel(projectId, issueId, newIssueId);
                }
                break;
            case IssueCopyLinkContents.COMMENTS:
                // 评论
                issueCommentService.copyIssueComments(projectId, issueId, newIssueId);
                break;
            case IssueCopyLinkContents.RELATED_TEST_CASES:
                // 关联测试用例
                testServiceClientOperator.copyIssueRelatedTestCases(projectId, issueId, newIssueId);
                break;
            case IssueCopyLinkContents.RELATED_BRANCHES:
                // 关联分支
                devopsClientOperator.copyIssueRelatedBranches(projectId, issueId, newIssueId);
                break;
            default:
                break;
        }
        return this.issueMapper.selectByPrimaryKey(newIssueId);
    }

    @Override
    public void buildTriggerCarrierVO(Long projectId, Long issueId, List<TriggerCarrierVO> list, List<Long> customFieldIds) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (ObjectUtils.isEmpty(issueDTO)) {
            return;
        }
        TriggerCarrierVO triggerCarrierVO = new TriggerCarrierVO();
        triggerCarrierVO.setInstanceId(issueId);
        triggerCarrierVO.setProjectId(projectId);
        triggerCarrierVO.setIssueTypeId(issueDTO.getIssueTypeId());
        triggerCarrierVO.setNoticeInstanceId(issueId);
        triggerCarrierVO.setFieldList(new ArrayList<>());
        triggerCarrierVO.setExecutedRules(new ArrayList<>());
        triggerCarrierVO.setMemberFieldIds(new HashSet<>(customFieldIds));
        triggerCarrierVO.setAuditDomain(issueDTO);
        list.add(triggerCarrierVO);
    }

    @Override
    public void addCollectionFieldIfNotNull(IssueUpdateVO issueUpdateVO,
                                            List<String> fieldList) {
        if (issueUpdateVO.getComponentIssueRelVOList() != null
                && !fieldList.contains(RuleNotice.COMPONENT_ID)) {
            fieldList.add(RuleNotice.COMPONENT_ID);
        }
        if (issueUpdateVO.getLabelIssueRelVOList() != null
                && !fieldList.contains(RuleNotice.LABEL_ID)) {
            fieldList.add(RuleNotice.LABEL_ID);
        }
        if (issueUpdateVO.getVersionIssueRelVOList() != null
                && !fieldList.contains(RuleNotice.VERSION_ID)) {
            fieldList.add(RuleNotice.VERSION_ID);
        }
    }

    private void setCopyRequireField(IssueDetailDTO issueDetailDTO, List<String> handlerRequireFiled) {
        if (handlerRequireFiled.contains(STORY_POINTS_FIELD)) {
            issueDetailDTO.setStoryPoints(null);
        }

        if (handlerRequireFiled.contains(REMAIN_TIME_FIELD)) {
            issueDetailDTO.setRemainingTime(null);
        }
    }

    @Override
    public List<String> handlerCopyRequirePredefinedField(Object object, JSONObject predefinedFields) {
        if (ObjectUtils.isEmpty(predefinedFields)) {
            return new ArrayList<>();
        }
        List<VersionIssueRelVO> fixVersion = buildVersionData(predefinedFields.get(FIX_VERSION));
        List<VersionIssueRelVO> influenceVersion = buildVersionData(predefinedFields.get(INFLUENCE_VERSION));
        predefinedFields.remove(FIX_VERSION);
        predefinedFields.remove(INFLUENCE_VERSION);
        IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
        List<String> fieldList = verifyUpdateUtil.verifyUpdateData(predefinedFields, issueUpdateVO);
        Set<String> fields = predefinedFields.keySet();
        for (String field : fields) {
            setValue(field, object, getValue(field, issueUpdateVO));
        }
        if (CollectionUtils.isNotEmpty(fixVersion)) {
            handlerIssueVersionVO(fixVersion, ProductVersionService.VERSION_RELATION_TYPE_FIX);
            setValue("versionIssueRelVOList", object, fixVersion);
        }
        if (CollectionUtils.isNotEmpty(influenceVersion)) {
            handlerIssueVersionVO(influenceVersion, ProductVersionService.VERSION_RELATION_TYPE_INFLUENCE);
            setValue("versionIssueRelVOList", object, influenceVersion);
        }
        return fieldList;
    }

    private void handlerIssueVersionVO(List<VersionIssueRelVO> influenceVersion, String relationType) {
        for (VersionIssueRelVO versionIssueRelVO : influenceVersion) {
            versionIssueRelVO.setRelationType(relationType);
        }
    }

    private Object getValue(String fieldName, IssueUpdateVO issueUpdateVO) {
        Method method;
        try {
            PropertyDescriptor pd = new PropertyDescriptor(fieldName, issueUpdateVO.getClass());
            method = pd.getReadMethod();//获得读方法
        } catch (IntrospectionException e) {
            throw new CommonException("error.copy.issue.getFiledValueEmpty");
        }
        Object result = null;
        if (!ObjectUtils.isEmpty(method)) {
            try {
                result = method.invoke(issueUpdateVO);
            } catch (InvocationTargetException | IllegalAccessException e) {
                throw new CommonException("error.copy.issue.getFiledValueEmpty");
            }
        }
        return result;
    }

    private void setValue(String fieldName, Object object, Object value){
        Method method;
        try {
            PropertyDescriptor pd = new PropertyDescriptor(fieldName, object.getClass());
            method = pd.getWriteMethod();//获得写方法
        } catch (IntrospectionException e) {
            throw new CommonException("error.copy.issue.setFiledValueEmpty");
        }
        if (!ObjectUtils.isEmpty(method)) {
            try {
               method.invoke(object, value);
            } catch (InvocationTargetException | IllegalAccessException e) {
                throw new CommonException("error.copy.issue.setFiledValueEmpty");
            }
        }
    }

    private List<VersionIssueRelVO> buildVersionData(Object object) {
        return ObjectUtils.isEmpty(object) ? null : EncryptionUtils.jsonToList(object,VersionIssueRelVO.class);
    }

    private IssueDTO handlerOtherFields(Long projectId, List<String> predefinedFieldNames, IssueDetailDTO issueDetailDTO, Long newIssueId, CopyConditionVO copyConditionVO, Map<Long, CopyIssueRequiredFieldVO> copyIssueRequiredFieldVOMap, List<TriggerCarrierVO> triggerCarrierVOS) {
        //复制冲刺
        if (predefinedFieldNames.contains(SPRINT_ID_FIELD)) {
            handleCreateCopyIssueSprintRel(issueDetailDTO, newIssueId);
        }
        if (copyConditionVO.getSubTask() && CollectionUtils.isNotEmpty(issueDetailDTO.getSubIssueDTOList())) {
            List<IssueDTO> subIssueDTOList = issueDetailDTO.getSubIssueDTOList();
            subIssueDTOList.forEach(issueDO -> {
                copySubIssue(issueDO, newIssueId, projectId,copyConditionVO, copyIssueRequiredFieldVOMap);
                CopyIssueRequiredFieldVO copyIssueRequiredFieldVO = copyIssueRequiredFieldVOMap.getOrDefault(issueDO.getIssueId(), new CopyIssueRequiredFieldVO());
                List<Long> customFieldIds = new ArrayList<>();
                if (CollectionUtils.isNotEmpty(copyIssueRequiredFieldVO.getCustomFields())) {
                    customFieldIds.addAll(copyIssueRequiredFieldVO.getCustomFields().stream().map(PageFieldViewCreateVO::getFieldId).collect(Collectors.toList()));
                }
                buildTriggerCarrierVO(projectId, newIssueId, triggerCarrierVOS, customFieldIds);
            });
        }
        CopyIssueRequiredFieldVO copyIssueRequiredFieldVO = copyIssueRequiredFieldVOMap.getOrDefault(issueDetailDTO.getIssueId(), new CopyIssueRequiredFieldVO());
        // 复制自定义字段的值
        fieldValueService.copyCustomFieldValue(projectId, issueDetailDTO, newIssueId, copyConditionVO.getCustomFieldIds(), copyIssueRequiredFieldVO.getCustomFields());
        List<Long> customFieldIds = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(copyIssueRequiredFieldVO.getCustomFields())) {
            customFieldIds.addAll(copyIssueRequiredFieldVO.getCustomFields().stream().map(PageFieldViewCreateVO::getFieldId).collect(Collectors.toList()));
        }
        buildTriggerCarrierVO(projectId, newIssueId, triggerCarrierVOS, customFieldIds);
        return issueMapper.selectByPrimaryKey(newIssueId);
    }

    @Override
    public void handleCopyPredefinedFields(Long organizationId, IssueDetailDTO issueDetail, List<String> predefinedFieldNames) {
        // 对比前端传入的需要复制的预定义字段和支持复制的预定义字段, 找出不需要复制的字段名
        // 将不需要复制的预定义字段置空
        for (String fieldName : COPY_PREDEFINED_FIELDS_NAME) {
            if (!predefinedFieldNames.contains(fieldName)) {
                setFieldValueEmpty(issueDetail, fieldName);
            }
        }
        // 不复制报告人, 则将报告人设置为复制人
        if (!predefinedFieldNames.contains(REPORTER_ID)) {
            issueDetail.setReporterId(Optional.ofNullable(DetailsHelper.getUserDetails()).map(CustomUserDetails::getUserId).orElse(null));
        }
        // 不复制优先级, 则将优先级设置为默认优先级
        if (!predefinedFieldNames.contains(PRIORITY_ID)) {
            PriorityVO priorityVO = priorityService.queryDefaultByOrganizationId(organizationId);
            issueDetail.setPriorityCode("priority-" + priorityVO.getId());
            issueDetail.setPriorityId(priorityVO.getId());
        }
        // 过滤掉已归档/已发版的"修复的版本"
        if (predefinedFieldNames.contains(FIX_VERSION)) {
            List<VersionIssueRelDTO> versionIssueRelations = issueDetail.getVersionIssueRelDTOList();
            if(CollectionUtils.isNotEmpty(versionIssueRelations)) {
                versionIssueRelations = versionIssueRelations.stream().filter(relation -> !(
                        Objects.equals(relation.getRelationType(), ProductVersionService.VERSION_RELATION_TYPE_FIX)
                        && !Objects.equals(relation.getStatusCode(), ProductVersionService.VERSION_STATUS_CODE_PLANNING)
                )).collect(Collectors.toList());
                issueDetail.setVersionIssueRelDTOList(versionIssueRelations);
            }
        }
    }

    private void setFieldValueEmpty(IssueDetailDTO issueDetailDTO, String fieldName) {
        // 获得写方法
        Method method;
        PropertyDescriptor pd = BeanUtils.getPropertyDescriptor(issueDetailDTO.getClass(), fieldName);
        if(pd == null) {
            throw new CommonException("error.copy.issue.setFiledValueEmpty");
        }
        method = pd.getWriteMethod();
        if (!ObjectUtils.isEmpty(method)) {
            try {
                if (Objects.equals(EPIC_ID_FIELD, fieldName)) {
                    method.invoke(issueDetailDTO, 0L);
                } else {
                    method.invoke(issueDetailDTO, (Object) null);
                }
            } catch (InvocationTargetException | IllegalAccessException e) {
                throw new CommonException("error.copy.issue.setFiledValueEmpty");
            }
        }
    }

    protected void setEpicName(Long projectId, CopyConditionVO copyConditionVO, IssueCreateVO issueCreateVO) {
        String epicName = copyConditionVO.getEpicName();
        IssueDTO epicExample = new IssueDTO();
        epicExample.setProjectId(projectId);
        epicExample.setEpicName(epicName);
        if (StringUtils.isEmpty(epicName)) {
            throw new CommonException("error.issue.epic.name.empty");
        } else if (epicName.length() > 20) {
            throw new CommonException("error.epic.name.more.than.20character");
        } else if (!issueMapper.select(epicExample).isEmpty()) {
            throw new CommonException("error.epic.name.duplicate");
        }
        issueCreateVO.setEpicName(epicName);
    }

    protected IssueDTO copyStoryPointAndRemainingTimeData(IssueDetailDTO issueDetailDTO, Long projectId, Long issueId, Long objectVersionNumber) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (issueDetailDTO.getStoryPoints() == null && issueDetailDTO.getRemainingTime() == null) {
            return issueDTO;
        }
        IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
        issueUpdateVO.setStoryPoints(issueDetailDTO.getStoryPoints());
        issueUpdateVO.setRemainingTime(issueDetailDTO.getRemainingTime());
        issueUpdateVO.setIssueId(issueId);
        issueUpdateVO.setObjectVersionNumber(objectVersionNumber);
        List<String> fieldList = new ArrayList<>();
        if (issueDetailDTO.getStoryPoints() != null) {
            fieldList.add(STORY_POINTS_FIELD);
        }
        if (issueDetailDTO.getRemainingTime() != null) {
            fieldList.add(REMAIN_TIME_FIELD);
        }
        updateIssueWithoutRuleNotice(projectId, issueUpdateVO, fieldList);
        return issueMapper.selectByPrimaryKey(issueId);
    }

    protected void copySubIssue(IssueDTO issueDTO, Long newIssueId, Long projectId, CopyConditionVO copyConditionVO, Map<Long, CopyIssueRequiredFieldVO> copyIssueRequiredFieldVOMap) {
        IssueDetailDTO subIssueDetailDTO = issueMapper.queryIssueDetail(issueDTO.getProjectId(), issueDTO.getIssueId());
        IssueSubCreateVO issueSubCreateVO = issueAssembler.issueDtoToSubIssueCreateDto(subIssueDetailDTO, newIssueId);
        CopyIssueRequiredFieldVO copyIssueRequiredFieldVO = copyIssueRequiredFieldVOMap.getOrDefault(issueDTO.getIssueId(), new CopyIssueRequiredFieldVO());
        List<String> requireFieldList = handlerCopyRequirePredefinedField(issueSubCreateVO, copyIssueRequiredFieldVO.getPredefinedFields());
        IssueSubVO newSubIssue = stateMachineClientService.createSubIssueWithoutRuleNotice(issueSubCreateVO);
        setCopyRequireField(subIssueDetailDTO, requireFieldList);
        //复制剩余工作量并记录日志
        if (issueDTO.getRemainingTime() != null) {
            IssueUpdateVO subIssueUpdateVO = new IssueUpdateVO();
            subIssueUpdateVO.setRemainingTime(issueDTO.getRemainingTime());
            subIssueUpdateVO.setIssueId(newSubIssue.getIssueId());
            subIssueUpdateVO.setObjectVersionNumber(newSubIssue.getObjectVersionNumber());
            updateIssueWithoutRuleNotice(projectId, subIssueUpdateVO, Lists.newArrayList(REMAIN_TIME_FIELD));
        }
        fieldValueService.copyCustomFieldValue(projectId, subIssueDetailDTO, newSubIssue.getIssueId(), null, copyIssueRequiredFieldVO.getCustomFields());
    }

    protected void handleCreateCopyIssueSprintRel(IssueDetailDTO issueDetailDTO, Long newIssueId) {
        if (issueDetailDTO.getActiveSprint() != null) {
            handleCreateSprintRel(issueDetailDTO.getActiveSprint().getSprintId(), issueDetailDTO.getProjectId(), newIssueId);
        }
    }

    protected void batchCreateCopyIssueLink(Boolean condition, Long issueId, Long newIssueId, Long projectId) {
        if (condition) {
            List<IssueLinkDTO> issueLinkDTOList = modelMapper.map(issueLinkMapper.queryIssueLinkByIssueId(new HashSet<>(Arrays.asList(issueId)), new HashSet<>(Arrays.asList(projectId)), false), new TypeToken<List<IssueLinkDTO>>() {
            }.getType());
            issueLinkDTOList.forEach(issueLinkDTO -> {
                IssueLinkDTO copy = new IssueLinkDTO();
                if (issueLinkDTO.getIssueId().equals(issueId)) {
                    copy.setIssueId(newIssueId);
                    copy.setLinkedIssueId(issueLinkDTO.getLinkedIssueId());
                }
                if (issueLinkDTO.getLinkedIssueId().equals(issueId)) {
                    copy.setIssueId(issueLinkDTO.getIssueId());
                    copy.setLinkedIssueId(newIssueId);
                }
                copy.setLinkTypeId(issueLinkDTO.getLinkTypeId());
                copy.setProjectId(projectId);
                if (issueLinkValidator.checkUniqueLink(copy)) {
                    issueLinkService.create(copy);
                }
            });
        }
    }

    protected IssueDTO createCopyIssueLink(Long issueId, Long newIssueId, Long projectId) {
        IssueLinkTypeDTO query = new IssueLinkTypeDTO();
        query.setProjectId(projectId);
        query.setOutWard("复制");
        IssueLinkTypeDTO issueLinkTypeDTO = issueLinkTypeMapper.selectOne(query);
        if (issueLinkTypeDTO != null) {
            IssueLinkDTO issueLink = new IssueLinkDTO();
            issueLink.setLinkedIssueId(issueId);
            issueLink.setLinkTypeId(issueLinkTypeDTO.getLinkTypeId());
            issueLink.setIssueId(newIssueId);
            issueLink.setProjectId(projectId);
            if (issueLinkValidator.checkUniqueLink(issueLink)) {
                issueLinkService.create(issueLink);
            }
        }
        return this.issueMapper.selectByPrimaryKey(newIssueId);
    }

    private void insertSprintWhenTransform(Long issueId, Long sprintId, Long projectId, List<Long> issueIds) {
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        IssueSprintRelDTO issueSprintRelDTO = new IssueSprintRelDTO();
        issueSprintRelDTO.setIssueId(issueId);
        issueSprintRelDTO.setSprintId(sprintId);
        issueSprintRelDTO.setProjectId(projectId);
        if (issueSprintRelMapper.selectOne(issueSprintRelDTO) == null) {
            if (issueMapper.selectUnCloseSprintId(projectId, issueId) != null) {
                BatchRemoveSprintDTO batchRemoveSprintDTO = new BatchRemoveSprintDTO(projectId, sprintId, issueIds);
                issueAccessDataService.removeIssueFromSprintByIssueIds(batchRemoveSprintDTO);
                issueAccessDataService.issueToDestinationByIds(projectId, sprintId, issueIds, new Date(), customUserDetails.getUserId());
            } else {
                issueSprintRelService.createIssueSprintRel(modelMapper.map(issueSprintRelDTO, IssueSprintRelDTO.class));
            }
        }
    }

    @Override
    public IssueSubVO transformedSubTask(Long projectId, Long organizationId, IssueTransformSubTask issueTransformSubTask) {
        IssueConvertDTO issueConvertDTO = modelMapper.map(queryIssueByIssueIdAndProjectId(projectId, issueTransformSubTask.getIssueId()), IssueConvertDTO.class);
        if (issueConvertDTO != null) {
            if (!issueConvertDTO.getTypeCode().equals(SUB_TASK)) {
                issueConvertDTO.setObjectVersionNumber(issueTransformSubTask.getObjectVersionNumber());
                List<Long> subIssueIds = issueMapper.querySubIssueIdsByIssueId(projectId, issueConvertDTO.getIssueId());
                if (CollectionUtils.isNotEmpty(subIssueIds)) {
                    throw new CommonException("error.transformedSubTask.issueHaveSubIssue");
                }
                issueConvertDTO.setEpicSequence(null);
                issueConvertDTO.setStoryPoints(null);
                issueConvertDTO.setStatusId(issueTransformSubTask.getStatusId());
                issueConvertDTO.setTypeCode(SUB_TASK);
                issueConvertDTO.setIssueTypeId(issueTransformSubTask.getIssueTypeId());
                issueConvertDTO.setParentIssueId(issueTransformSubTask.getParentIssueId());
                List<String> fieldList = new ArrayList<>();
                List<String> list = Arrays.asList(TYPE_CODE_FIELD, ISSUE_TYPE_ID, STATUS_ID, PARENT_ISSUE_ID, EPIC_SEQUENCE, STORY_POINTS_FIELD);
                fieldList.addAll(list);
                //如果为Bug类型,就删除与原来问题的关联
                if (!Objects.isNull(issueConvertDTO.getRelateIssueId())) {
                    issueConvertDTO.setRelateIssueId(null);
                    fieldList.add(RELATE_ISSUE_ID);
                }
                issueValidator.verifySubTask(issueTransformSubTask.getParentIssueId());
                //删除链接
                List<IssueLinkDTO> issueLinkDTOS = issueLinkMapper.listIssueLinkByBatch(projectId, Arrays.asList(issueConvertDTO.getIssueId()));
                if (CollectionUtils.isNotEmpty(issueLinkDTOS)) {
                    issueLinkService.deleteIssueLinkByIssueId(issueConvertDTO,issueLinkDTOS);
                }
                if (agilePluginService != null) {
                    agilePluginService.handlerProgramValueWhenTransferSubTask(issueConvertDTO, projectId, fieldList);
                }
                if (agileWaterfallService != null) {
                    agileWaterfallService.handlerTransferSubTask(issueConvertDTO, projectId, fieldList);
                }
                issueAccessDataService.update(issueConvertDTO, fieldList.toArray(new String[fieldList.size()]));
                Long sprintId = issueMapper.selectUnCloseSprintId(projectId, issueTransformSubTask.getParentIssueId());
                List<Long> issueIds = new ArrayList<>();
                issueIds.add(issueConvertDTO.getIssueId());
                if (sprintId != null) {
                    insertSprintWhenTransform(issueConvertDTO.getIssueId(), sprintId, projectId, issueIds);
                } else {
                    if (issueMapper.selectUnCloseSprintId(projectId, issueConvertDTO.getIssueId()) != null) {
                        BatchRemoveSprintDTO batchRemoveSprintDTO = new BatchRemoveSprintDTO(projectId, sprintId, issueIds);
                        issueAccessDataService.removeIssueFromSprintByIssueIds(batchRemoveSprintDTO);
                    }
                }
                IssueSubVO result = queryIssueSub(projectId, organizationId, issueConvertDTO.getIssueId());
                setCompletedAndActualCompletedDate(result);
                return result;
            } else {
                throw new CommonException("error.issueValidator.subTaskError");
            }
        } else {
            throw new CommonException("error.issueValidator.issueNoFound");
        }
    }

    @Override
    public synchronized IssueVO transformedTask(IssueConvertDTO issueConvertDTO, IssueTransformTask issueTransformTask, Long organizationId) {
        String originType = issueConvertDTO.getTypeCode();
        List<String> fieldList = new ArrayList<>(Arrays.asList(TRANSFORMED_TASK_FIELD_LIST_NO_RANK));
        if (originType.equals(SUB_TASK) && !Objects.equals(issueTransformTask.getTypeCode(), SUB_TASK)) {
            issueConvertDTO.setParentIssueId(null);
        }
        if (STORY_TYPE.equals(issueConvertDTO.getTypeCode()) && issueConvertDTO.getStoryPoints() != null) {
            issueConvertDTO.setStoryPoints(null);
        }
        if (issueTransformTask.getTypeCode().equals(ISSUE_EPIC)) {
            issueConvertDTO.setRank(null);
            fieldList.add(RANK_FIELD);
            issueConvertDTO.setTypeCode(issueTransformTask.getTypeCode());
            issueConvertDTO.setEpicName(issueTransformTask.getEpicName());
            List<LookupValueDTO> colorList = lookupValueMapper.queryLookupValueByCode(EPIC_COLOR_TYPE).getLookupValues();
            issueConvertDTO.initializationColor(colorList);
            issueConvertDTO.setRemainingTime(null);
            issueConvertDTO.setEpicId(0L);
            //排序编号
            Integer sequence = issueMapper.queryMaxEpicSequenceByProject(issueConvertDTO.getProjectId());
            issueConvertDTO.setEpicSequence(sequence == null ? 0 : sequence + 1);
        } else if (issueConvertDTO.getTypeCode().equals(ISSUE_EPIC)) {
            // 如果之前类型是epic，会把该epic下的issue的epicId置为0
            issueAccessDataService.batchUpdateIssueEpicId(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId());
            issueConvertDTO.setTypeCode(issueTransformTask.getTypeCode());
            issueConvertDTO.setColorCode(null);
            issueConvertDTO.setEpicName(null);
            issueConvertDTO.setEpicSequence(null);
            //rank值重置
            fieldList.add(RANK_FIELD);
            calculationRank(issueConvertDTO.getProjectId(), issueConvertDTO);
        } else {
            issueConvertDTO.setTypeCode(issueTransformTask.getTypeCode());
        }
        if (issueTransformTask.getStatusId() != null) {
            issueConvertDTO.setStatusId(issueTransformTask.getStatusId());
        }
        issueConvertDTO.setIssueTypeId(issueTransformTask.getIssueTypeId());
        issueAccessDataService.update(issueConvertDTO, fieldList.toArray(new String[fieldList.size()]));
        return queryIssue(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId(), organizationId);
    }

    private IssueDTO queryIssueByIssueIdAndProjectId(Long projectId, Long issueId) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setIssueId(issueId);
        issueDTO.setProjectId(projectId);
        return issueMapper.selectOne(issueDTO);
    }

    @Override
    public List<IssueInfoVO> listByIssueIds(Long projectId, List<Long> issueIds) {
        return modelMapper.map(issueMapper.listByIssueIds(projectId, issueIds), new TypeToken<List<IssueInfoVO>>() {
        }.getType());
    }

    @Override
    public Page<IssueListTestVO> listIssueWithoutSubToTestComponent(Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId) {
        //连表查询需要设置主表别名
        HashMap<String, String> orders = new HashMap<>();
        orders.put(ISSUE_NUM,ISSUE_NUM_CONVERT);
        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), SEARCH, orders);
        pageRequest.setSort(sort);
        Page<IssueDTO> issueDOPage = PageHelper.doPageAndSort(pageRequest,
                () -> issueMapper.listIssueWithoutSubToTestComponent(projectId, searchVO.getSearchArgs(),
                searchVO.getAdvancedSearchArgs(), searchVO.getOtherArgs(), searchVO.getContents()));
        return handleIssueListTestDoToDto(issueDOPage, organizationId, projectId);
    }

    private Page<IssueListTestVO> handleIssueListTestDoToDto(Page<IssueDTO> issueDOPage, Long organizationId, Long projectId) {
        Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
        Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
        Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(ConvertUtil.getOrganizationId(projectId), projectId);
        return PageUtil.buildPageInfoWithPageInfoList(issueDOPage, issueAssembler.issueDoToIssueTestListDto(issueDOPage.getContent(), priorityMap, statusMapDTOMap, issueTypeDTOMap));
    }

    @Override
    public Page<IssueListTestWithSprintVersionVO> listIssueWithLinkedIssues(Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId) {
        Map<String, String> orders = new HashMap<>();
        orders.put(ISSUE_NUM, ISSUE_NUM_CONVERT);
        Page<IssueDTO> issueDOPage = PageHelper.doPageAndSort(pageRequest, () ->
                issueMapper.listIssueWithLinkedIssues(projectId, searchVO.getSearchArgs(),
                        searchVO.getAdvancedSearchArgs(), searchVO.getOtherArgs(), searchVO.getContents()));
        return handleILTDTOToILTWSVDTO(projectId, handleIssueListTestDoToDto(issueDOPage, organizationId, projectId));
    }

    private Page<IssueListTestWithSprintVersionVO> handleILTDTOToILTWSVDTO(Long projectId, Page<IssueListTestVO> issueListTestDTOSPage) {
        Map<Long, SprintDTO> sprintDoMap = sprintMapper.getSprintByProjectId(projectId).stream().collect(
                Collectors.toMap(SprintDTO::getSprintId, x -> x));

        List<IssueListTestWithSprintVersionVO> issueListTestWithSprintVersionVOS = new ArrayList<>();

        for (IssueListTestVO issueListTestVO :issueListTestDTOSPage.getContent()) {
            IssueListTestWithSprintVersionVO issueListTestWithSprintVersionVO = new IssueListTestWithSprintVersionVO(issueListTestVO);

            List<VersionIssueRelVO> versionList = new ArrayList<>();
            List<IssueSprintVO> sprintList = new ArrayList<>();

            issueMapper.queryVersionIssueRelByIssueId(issueListTestWithSprintVersionVO.getIssueId()).forEach(v -> {
                VersionIssueRelVO versionIssueRelVO = new VersionIssueRelVO();
                versionIssueRelVO.setVersionId(v.getVersionId());
                versionIssueRelVO.setName(v.getName());

                versionList.add(versionIssueRelVO);
            });

            issueMapper.querySprintNameByIssueId(issueListTestWithSprintVersionVO.getIssueId()).forEach(v -> {
                SprintDTO sprintDTO = sprintDoMap.get(v.getSprintId());

                IssueSprintVO issueSprintVO = new IssueSprintVO();
                issueSprintVO.setSprintId(sprintDTO.getSprintId());
                issueSprintVO.setSprintName(sprintDTO.getSprintName());
                issueSprintVO.setStatusCode(sprintDTO.getStatusCode());

                sprintList.add(issueSprintVO);
            });

            issueListTestWithSprintVersionVO.setVersionDTOList(versionList);
            issueListTestWithSprintVersionVO.setSprintDTOList(sprintList);

            issueListTestWithSprintVersionVOS.add(issueListTestWithSprintVersionVO);
        }
        return PageUtil.buildPageInfoWithPageInfoList(issueListTestDTOSPage, issueListTestWithSprintVersionVOS);
    }

    @Override
    public List<IssueCreationNumVO> queryIssueNumByTimeSlot(Long projectId, String typeCode, Integer timeSlot) {
        //h2 不支持dateSub函数，这个函数不能自定义
        Date date = MybatisFunctionTestUtil.dataSubFunction(new Date(), timeSlot);
        return modelMapper.map(issueMapper.queryIssueNumByTimeSlot(projectId, typeCode, date), new TypeToken<List<IssueCreationNumVO>>() {
        }.getType());
    }

    @Override
    public Page<IssueNumVO> queryIssueByOptionForAgile(Long projectId, Long issueId, String issueNum, Boolean self, String content, PageRequest pageRequest, List<Long> excludeIssueIds) {
        Map<String, String> orders = new HashMap<>();
        orders.put(ISSUE_NUM, ISSUE_NUM_CONVERT);
        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), SEARCH, orders);
        pageRequest.setSort(sort);
        IssueNumDTO issueNumDTO = null;
        if (Boolean.TRUE.equals(self)) {
            issueNumDTO = issueMapper.queryIssueByIssueNumOrIssueId(projectId, issueId, issueNum);
            if (issueNumDTO != null) {
                pageRequest = new PageRequest(pageRequest.getPage(), pageRequest.getSize() - 1);
            }
        }
        Page<IssueNumDTO> issueDOPage = PageHelper.doPageAndSort(pageRequest,
                () ->
                issueMapper.queryIssueByOptionForAgile(projectId, issueId, issueNum, self, content, excludeIssueIds));
        if (self && issueNumDTO != null) {
            issueDOPage.getContent().add(0, issueNumDTO);
            issueDOPage.setSize(issueDOPage.getSize() + 1);
        }
        return PageUtil.buildPageInfoWithPageInfoList(issueDOPage, issueAssembler.issueNumDoToDto(issueDOPage.getContent(), projectId));
    }

    @Override
    public synchronized EpicDataVO dragEpic(Long projectId, EpicSequenceVO epicSequenceVO) {
        if (epicSequenceVO.getAfterSequence() == null && epicSequenceVO.getBeforeSequence() == null) {
            throw new CommonException("error.dragEpic.noSequence");
        }
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setIssueId(epicSequenceVO.getEpicId());
        issueDTO.setProjectId(projectId);
        IssueConvertDTO issueConvertDTO = modelMapper.map(issueMapper.selectOne(issueDTO), IssueConvertDTO.class);
        if (issueConvertDTO == null) {
            throw new CommonException("error.issue.notFound");
        } else {
            if (epicSequenceVO.getAfterSequence() == null) {
                Integer maxSequence = productVersionMapper.queryMaxAfterSequence(epicSequenceVO.getBeforeSequence(), projectId);
                epicSequenceVO.setAfterSequence(maxSequence);
            } else if (epicSequenceVO.getBeforeSequence() == null) {
                Integer minSequence = productVersionMapper.queryMinBeforeSequence(epicSequenceVO.getAfterSequence(), projectId);
                epicSequenceVO.setBeforeSequence(minSequence);
            }
            handleSequence(epicSequenceVO, projectId, issueConvertDTO);
        }
        return epicDataAssembler.toTarget(issueMapper.queryEpicListByEpic(epicSequenceVO.getEpicId(), projectId), EpicDataVO.class);
    }


    private void handleSequence(EpicSequenceVO epicSequenceVO, Long projectId, IssueConvertDTO issueConvertDTO) {
        if (epicSequenceVO.getBeforeSequence() == null) {
            issueConvertDTO.setEpicSequence(epicSequenceVO.getAfterSequence() + 1);
            issueAccessDataService.update(issueConvertDTO, new String[]{EPIC_SEQUENCE});
        } else if (epicSequenceVO.getAfterSequence() == null) {
            if (issueConvertDTO.getEpicSequence() > epicSequenceVO.getBeforeSequence()) {
                Integer add = issueConvertDTO.getEpicSequence() - epicSequenceVO.getBeforeSequence();
                if (add > 0) {
                    issueConvertDTO.setEpicSequence(epicSequenceVO.getBeforeSequence() - 1);
                    issueAccessDataService.update(issueConvertDTO, new String[]{EPIC_SEQUENCE});
                } else {
                    issueAccessDataService.batchUpdateSequence(epicSequenceVO.getBeforeSequence(), projectId,
                            issueConvertDTO.getEpicSequence() - epicSequenceVO.getBeforeSequence() + 1, issueConvertDTO.getIssueId());
                }
            }
        } else {
            Integer sequence = epicSequenceVO.getAfterSequence() + 1;
            issueConvertDTO.setEpicSequence(sequence);
            issueAccessDataService.update(issueConvertDTO, new String[]{EPIC_SEQUENCE});
            Integer update = sequence - epicSequenceVO.getBeforeSequence();
            if (update >= 0) {
                issueAccessDataService.batchUpdateSequence(epicSequenceVO.getBeforeSequence(), projectId, update + 1, issueConvertDTO.getIssueId());
            }
        }
    }

    @Override
    public String getQuickFilter(List<Long> quickFilterIds) {
        if (ObjectUtils.isEmpty(quickFilterIds)) {
            return null;
        }
        List<String> sqlQuerys = quickFilterMapper.selectSqlQueryByIds(quickFilterIds);
        if (sqlQuerys.isEmpty()) {
            return null;
        }
        int idx = 0;
        StringBuilder sql = new StringBuilder("select issue_id from agile_issue where ");
        for (String filter : sqlQuerys) {
            if (idx == 0) {
                sql.append(" ( " + filter + " ) ");
                idx += 1;
            } else {
                sql.append(" and " + " ( " + filter + " ) ");
            }
        }
        return sql.toString();
    }

    @Override
    public IssueVO issueParentIdUpdate(Long projectId, IssueUpdateParentIdVO issueUpdateParentIdVO) {
        Long issueId = issueUpdateParentIdVO.getIssueId();
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        Long parentIssueId = issueUpdateParentIdVO.getParentIssueId();
        IssueDTO parentIssueDTO = issueMapper.selectByPrimaryKey(parentIssueId);
        IssueValidator.checkParentIdUpdate(issueDTO, parentIssueDTO);
        IssueConvertDTO updateIssue = new IssueConvertDTO();
        updateIssue.setIssueId(issueId);
        updateIssue.setObjectVersionNumber(issueUpdateParentIdVO.getObjectVersionNumber());
        updateIssue.setParentIssueId(issueUpdateParentIdVO.getParentIssueId());
        // update sprint
        updateSubTaskSprint(projectId, issueUpdateParentIdVO);
        issueAccessDataService.updateSelective(updateIssue);
        if (agileWaterfallService != null) {
            agileWaterfallService.handlerSubIssueUpdateParent(projectId, issueId, parentIssueId);
        }
        return queryIssueCreateWithoutRuleNotice(projectId, issueId);
    }

    private void updateSubTaskSprint(Long projectId, IssueUpdateParentIdVO issueUpdateParentIdVO) {
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        IssueSprintRelDTO parentIssueRel = issueSprintRelMapper.selectNoClosed(projectId, issueUpdateParentIdVO.getParentIssueId());
        IssueSprintRelDTO subTaskIssueRel = issueSprintRelMapper.selectNoClosed(projectId, issueUpdateParentIdVO.getIssueId());
        List<Long> subIssueIds = new ArrayList<>();
        subIssueIds.add(issueUpdateParentIdVO.getIssueId());
        if (parentIssueRel != null) {
            if (subTaskIssueRel != null && !Objects.equals(subTaskIssueRel.getSprintId(), parentIssueRel.getSprintId())) {
                BatchRemoveSprintDTO batchRemoveSprintDTO = new BatchRemoveSprintDTO(subTaskIssueRel.getProjectId(), subTaskIssueRel.getSprintId(), subIssueIds);
                issueAccessDataService.removeIssueFromSprintByIssueIds(batchRemoveSprintDTO);
                issueAccessDataService.issueToDestinationByIds(projectId, parentIssueRel.getSprintId(), subIssueIds, new Date(), customUserDetails.getUserId());
            } else if (subTaskIssueRel == null) {
                issueAccessDataService.issueToDestinationByIds(projectId, parentIssueRel.getSprintId(), subIssueIds, new Date(), customUserDetails.getUserId());
            }
        } else {
            if (subTaskIssueRel != null) {
                BatchRemoveSprintDTO batchRemoveSprintDTO = new BatchRemoveSprintDTO(subTaskIssueRel.getProjectId(), subTaskIssueRel.getSprintId(), subIssueIds);
                issueAccessDataService.removeIssueFromSprintByIssueIds(batchRemoveSprintDTO);
            }
        }
    }

    @Override
    public JSONObject countUnResolveByProjectId(Long projectId) {
        JSONObject result = new JSONObject();
        result.put("all", issueMapper.countIssueByProjectId(projectId));
        result.put("unresolved", issueMapper.countUnResolveByProjectId(projectId));
        return result;
    }

    @Override
    public List<Long> queryIssueIdsByOptions(Long projectId, SearchVO searchVO) {
        return issueMapper.queryIssueIdsByOptions(projectId, searchVO.getAdvancedSearchArgs(), searchVO.getOtherArgs(), searchVO.getContents());
    }

    @Override
    public Page<UndistributedIssueVO> queryUnDistributedIssues(Long projectId, PageRequest pageRequest) {
        Page<UndistributedIssueDTO> undistributedIssueDOPage = PageHelper.doPageAndSort(pageRequest, () ->
                issueMapper.queryUnDistributedIssues(projectId)
        );
        return PageUtil.buildPageInfoWithPageInfoList(undistributedIssueDOPage, issueAssembler.undistributedIssueDOToDto(undistributedIssueDOPage.getContent(), projectId));
    }

    @Override
    public List<UnfinishedIssueVO> queryUnfinishedIssues(Long projectId, Long assigneeId) {
        return issueAssembler.unfinishedIssueDoToDto(issueMapper.queryUnfinishedIssues(projectId, assigneeId), projectId);
    }

    @Override
    public String querySwimLaneCode(Long projectId) {
        UserSettingDTO userSettingDTO = new UserSettingDTO();
        userSettingDTO.initUserSetting(projectId);
        userSettingDTO.setTypeCode(STORY_MAP);
        UserSettingDTO query = userSettingMapper.selectOne(userSettingDTO);
        String result;
        if (query == null) {
            userSettingDTO.setStorymapSwimlaneCode("none");
            result = userSettingService.create(userSettingDTO).getStorymapSwimlaneCode();
        } else {
            result = query.getStorymapSwimlaneCode();
        }
        return result;
    }

    public void deleteIssueInfo(Long issueId, Long projectId) {
        //删除issue发送消息
        IssuePayload issuePayload = new IssuePayload();
        issuePayload.setIssueId(issueId);
        issuePayload.setProjectId(projectId);
        sagaClient.startSaga("agile-delete-issue", new StartInstanceDTO(JSON.toJSONString(issuePayload), StringUtils.EMPTY, StringUtils.EMPTY, ResourceLevel.PROJECT.value(), projectId));
    }

    @Override
    public Boolean checkEpicName(Long projectId, String epicName, Long epicId) {
        boolean isUpdate = !ObjectUtils.isEmpty(epicId);
        IssueDTO example = new IssueDTO();
        example.setProjectId(projectId);
        example.setEpicName(epicName);
        List<IssueDTO> result = issueMapper.select(example);
        if (isUpdate) {
            boolean existed = false;
            for (IssueDTO issue : result) {
                if (!epicId.equals(issue.getIssueId())) {
                    existed = true;
                    break;
                }
            }
            return existed;
        } else {
            return !result.isEmpty();
        }
    }


    @Override
    public IssueNumDTO queryIssueByIssueNum(Long projectId, String issueNum, boolean includeEpic) {
        return issueMapper.queryIssueByIssueNum(projectId, issueNum, includeEpic);
    }

    @Override
    public List<TestCaseDTO> migrateTestCaseByProjectId(Long projectId) {
        List<TestCaseDTO> testCaseDTOS = issueMapper.migrateTestCase(projectId);
        if (CollectionUtils.isEmpty(testCaseDTOS)) {
            return new ArrayList<>();
        }
        return testCaseDTOS;
    }

    @Override
    public List<Long> queryProjectIds() {
        List<Long> list = issueMapper.queryProjectIds();
        if (CollectionUtils.isEmpty(list)) {
            return new ArrayList<>();
        }
        return list;
    }

    @Override
    public List<IssueLinkVO> queryIssueByIssueIds(Long projectId, List<Long> issueIds) {
        if (ObjectUtils.isEmpty(issueIds)) {
            return new ArrayList<>();
        }
        return issueAssembler.issueDTOTOVO(projectId, issueMapper.listIssueInfoByIssueIds(projectId, issueIds, null));
    }

    @Override
    public Page<IssueLinkVO> pagedQueryByOptions(Long projectId, PageRequest pageRequest, IssueQueryVO issueQueryVO) {
        List<Long> issueIds = issueQueryVO.getIssueIds();
        Page emptyPage = PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        if (ObjectUtils.isEmpty(issueIds)) {
            return emptyPage;
        }
        Page<IssueDTO> result =
                PageHelper.doPage(pageRequest, () -> issueMapper.listIssueInfoByIssueIds(projectId, issueIds, issueQueryVO));
        List<IssueDTO> content = result.getContent();
        if (ObjectUtils.isEmpty(content)) {
            return emptyPage;
        }
        List<IssueLinkVO> list = issueAssembler.issueDTOTOVO(projectId, content);
        return PageUtils.copyPropertiesAndResetContent(result, list);
    }

    @Override
    public Page<IssueListFieldKVVO> queryStoryAndTask(Long projectId, PageRequest pageRequest, SearchVO searchVO) {
        //连表查询需要设置主表别名
        Map<String, String> orders = new HashMap<>();
        orders.put(ISSUE_NUM,ISSUE_NUM_CONVERT);
        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), null, orders);
        pageRequest.setSort(sort);
        Page<IssueDTO> pageInfo = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.queryStoryAndTaskByProjectId(projectId, searchVO));
        List<IssueDTO> list = pageInfo.getContent();
        if (CollectionUtils.isNotEmpty(list)) {
            Long organizationId = projectUtil.getOrganizationId(projectId);
            Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
            Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
            Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
            List<IssueListFieldKVVO> listFieldKVVOS = new ArrayList<>();
            list.forEach(v -> {
                IssueListFieldKVVO map = modelMapper.map(v, IssueListFieldKVVO.class);
                map.setPriorityVO(priorityMap.get(v.getPriorityId()) == null ? null : priorityMap.get(v.getPriorityId()));
                map.setStatusVO(statusMapDTOMap.get(v.getStatusId()) == null ? null : statusMapDTOMap.get(v.getStatusId()));
                map.setIssueTypeVO(issueTypeDTOMap.get(v.getIssueTypeId()) == null ? null : issueTypeDTOMap.get(v.getIssueTypeId()));
                listFieldKVVOS.add(map);
            });
            return PageUtil.buildPageInfoWithPageInfoList(pageInfo, listFieldKVVOS);
        } else {
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
    }

    @Override
    public Page<UserDTO> pagingQueryUsers(PageRequest pageRequest, Long projectId, String param, Set<Long> ignoredUserIds) {
        Set<Long> userIds = issueMapper.selectUserIdsByProjectIds(Arrays.asList(projectId));
        AgileUserVO agileUserVO = new AgileUserVO(userIds, null,  param, null, ignoredUserIds);
        return remoteIamOperator.agileUsers(projectId, pageRequest.getPage(), pageRequest.getSize(), agileUserVO);
    }

    @Override
    public Page<UserDTO> pagingQueryReporters(PageRequest pageRequest, Long projectId, String param, Set<Long> ignoredUserIds) {
        Set<Long> userIds = issueMapper.selectReporterIdsByProjectId(projectId);
        AgileUserVO agileUserVO = new AgileUserVO(userIds, null,  param, null, ignoredUserIds);
        return remoteIamOperator.agileUsers(projectId, pageRequest.getPage(), pageRequest.getSize(), agileUserVO);
    }

    @Override
    public void deleteSelfIssue(Long projectId, Long issueId) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        // 判断要删除的issue是否是自己创建的
        if (!userId.equals(issueDTO.getCreatedBy())) {
            throw new CommonException("error.created.user.illegal");
        }
        deleteIssue(projectId, issueId);
    }

    @Override
    public Page<IssueListFieldKVVO> pagedQueryMyReported(Long organizationId,
                                                         Long projectId,
                                                         PageRequest pageRequest,
                                                         WorkBenchIssueSearchVO workBenchIssueSearchVO) {
        workBenchIssueSearchVO.setType("myReported");
        return queryBackLogIssuesByPersonal(organizationId, projectId, pageRequest, workBenchIssueSearchVO);
    }

    @Override
    public Page<IssueListFieldKVVO> queryBackLogIssuesByPersonal(Long organizationId,
                                                                 Long projectId,
                                                                 PageRequest pageRequest,
                                                                 WorkBenchIssueSearchVO workBenchIssueSearchVO) {
        EncryptionUtils.decryptSearchVO(workBenchIssueSearchVO.getSearchVO());
        if (ObjectUtils.isEmpty(organizationId)) {
            throw new CommonException("error.organizationId.iss.null");
        }
        checkSearchType(workBenchIssueSearchVO.getType());
        List<Long> projectIds = new ArrayList<>();
        List<ProjectVO> projects = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String searchType = workBenchIssueSearchVO.getType();
        queryUserProjects(organizationId, projectId, projectIds, projects, userId, searchType);

        if (CollectionUtils.isEmpty(projectIds)) {
            return new Page<>();
        }
        Page<IssueDTO> parentPage =
                queryIssuesByTypeAndUserId(projectIds, userId, searchType, workBenchIssueSearchVO.getSearchVO(), pageRequest);
        List<IssueDTO> allIssue = parentPage.getContent();
        if (CollectionUtils.isEmpty(allIssue)) {
            return new Page<>();
        }
        Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
        Map<Long, List<IssueTypeVO>> issueTypeDTOMap = issueTypeService.listIssueTypeMapByProjectIds(organizationId, projectIds);
        Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
        Map<Long, ProjectVO> projectVOMap = projects.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        List<IssueListFieldKVVO> list = new ArrayList<>();
        Set<Long> userIds = new HashSet<>();
        List<IssueListFieldKVVO> waterfallIssues = new ArrayList<>();
        List<IssueListFieldKVVO> agileIssues = new ArrayList<>();
        List<String> waterfallIssueTypeCodes = Arrays.asList(IssueTypeCode.WATERFALL_ISSUE_TYPE_CODE);
        allIssue.forEach(v -> {
            IssueListFieldKVVO issueListFieldKVVO = new IssueListFieldKVVO();
            modelMapper.map(v,issueListFieldKVVO);
            String typeCode = issueListFieldKVVO.getTypeCode();
            if (waterfallIssueTypeCodes.contains(typeCode)) {
                waterfallIssues.add(issueListFieldKVVO);
            } else {
                agileIssues.add(issueListFieldKVVO);
            }
            setIssueTypeVO(issueListFieldKVVO, issueTypeDTOMap.get(v.getIssueTypeId()));
            issueListFieldKVVO.setStatusVO(statusMapDTOMap.get(v.getStatusId()));
            issueListFieldKVVO.setPriorityVO(priorityMap.get(v.getPriorityId()));
            issueListFieldKVVO.setProjectVO(projectVOMap.get(v.getProjectId()));
            // 设置父级issueId
//            Long parentId = null;
//            Long parentIssueId = v.getParentIssueId();
//            Long relateIssueId = v.getRelateIssueId();
//            parentId = setParentId(parentIssueId);
//            if (parentId == null) {
//                parentId = setParentId(relateIssueId);
//            }
//            issueListFieldKVVO.setParentId(parentId);
            list.add(issueListFieldKVVO);
            Long assigneeId = v.getAssigneeId();
            if (!ObjectUtils.isEmpty(assigneeId)) {
                userIds.add(assigneeId);
            }
        });
        setProgress(waterfallIssues, agileIssues, new HashSet<>(projectIds));
        if (agilePluginService != null) {
            agilePluginService.setFeatureTypeAndFeatureTeams(list, organizationId);
        }
        if (Objects.equals(searchType, MY_START_BEACON)) {
            setListStarBeacon(list, userId, projectIds);
        }
        if (!userIds.isEmpty()) {
            setAssignee(list, userIds);
        }
        PageInfo pageInfo = new PageInfo(pageRequest.getPage(), pageRequest.getSize());
        return new Page<>(list, pageInfo, parentPage.getTotalElements());
    }

    @Override
    public void setProgress(List<IssueListFieldKVVO> waterfallIssues,
                            List<IssueListFieldKVVO> agileIssues,
                            Set<Long> projectIds) {
        if (!ObjectUtils.isEmpty(agileWaterfallService) && !ObjectUtils.isEmpty(waterfallIssues)) {
            List<Long> waterfallIssueIds =
                    waterfallIssues.stream().map(IssueListFieldKVVO::getIssueId).collect(Collectors.toList());
            Map<Long, Integer> issueProgressMap = agileWaterfallService.getIssueProgressMap(new ArrayList<>(projectIds), waterfallIssueIds);
            waterfallIssues.forEach(issue -> issue.setProgress(issueProgressMap.get(issue.getIssueId())));
        }
        if (!ObjectUtils.isEmpty(agileIssues)) {
            Set<Long> issueIds =
                    agileIssues.stream().map(IssueListFieldKVVO::getIssueId).collect(Collectors.toSet());
            List<IssueDTO> issues = issueMapper.queryChildrenWithCompleted(projectIds, issueIds);
            Map<Long, List<IssueDTO>> issueMap =
                    issues.stream().collect(Collectors.groupingBy(IssueDTO::getParentIssueId));
            agileIssues.forEach(issue -> {
                Long issueId = issue.getIssueId();
                List<IssueDTO> subIssues = issueMap.get(issueId);
                int total = 0;
                int completed = 0;
                if (!ObjectUtils.isEmpty(subIssues)) {
                    total = subIssues.size();
                    completed = subIssues.stream().filter(x -> x.getCompleted()).collect(Collectors.toList()).size();
                }
                issue.setTotalSubIssues(total);
                issue.setCompletedSubIssues(completed);
            });
        }
    }


    private Page<IssueDTO> queryIssuesByTypeAndUserId(List<Long> projectIds,
                                                      Long userId,
                                                      String searchType,
                                                      SearchVO searchVO,
                                                      PageRequest pageRequest) {

        if (ObjectUtils.isEmpty(searchType)) {
            searchType = WorkBenchSearchType.MY_TODO;
        }
        Set<Long> projectIdSet = new HashSet<>(projectIds);
        //查活跃冲刺
        Set<Long> activeSprintIds =
                sprintMapper.selectActiveSprintsByProjectIds(projectIdSet)
                        .stream()
                        .map(SprintDTO::getSprintId)
                        .collect(Collectors.toSet());
        addProjectOrderIfNotExisted(pageRequest);
        Page<IssueDTO> parentPage;
        switch (searchType) {
            case WorkBenchSearchType.MY_TODO:
                parentPage = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.selectMyTodoIssues(projectIds, userId, searchVO, activeSprintIds));
                break;
            case WorkBenchSearchType.MY_BUG:
                if (activeSprintIds.isEmpty()) {
                    parentPage = PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
                } else {
                    parentPage = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.selectMyBugs(projectIds, userId, searchVO, activeSprintIds));
                }
                break;
            case WorkBenchSearchType.MY_REPORTED_BUG:
                if(activeSprintIds.isEmpty()) {
                    parentPage = PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
                } else {
                    parentPage = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.selectReportedBug(projectIds, userId, searchVO, activeSprintIds));
                }
                break;
            case WorkBenchSearchType.MY_STAR_BEACON:
                parentPage = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.selectMyStarBeacon(projectIds, userId, searchVO));
                break;
            case WorkBenchSearchType.MY_REPORTED:
                parentPage = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.selectMyReported(projectIds, userId, searchVO, activeSprintIds));
                break;
            case WorkBenchSearchType.MY_ASSIGNED:
                parentPage = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.selectMyAssigned(projectIds, userId, searchVO, activeSprintIds));
                break;
            default:
                parentPage = PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
                break;
        }
        return parentPage;
    }

    private void addProjectOrderIfNotExisted(PageRequest pageRequest) {
        List<Sort.Order> orders = new ArrayList<>();
        Sort sort = pageRequest.getSort();
        String projectIdKey = "projectId";
        Sort.Order projectOrder = sort.getOrderFor(projectIdKey);
        if (ObjectUtils.isEmpty(projectOrder)) {
            Sort.Order order = new Sort.Order(Sort.Direction.ASC, projectIdKey);
            orders.add(order);
        } else {
            orders.add(projectOrder);
        }
        if (!ObjectUtils.isEmpty(sort)) {
            Iterator<Sort.Order> iterator = sort.iterator();
            while (iterator.hasNext()) {
                Sort.Order thisOrder = iterator.next();
                String property = thisOrder.getProperty();
                if (!Objects.equals(projectIdKey, property)) {
                    orders.add(thisOrder);
                }
            }
        }
        Sort newSort = new Sort(orders);
        pageRequest.setSort(newSort);
        pageRequest.resetOrder("ai", Collections.emptyMap());
    }

    private Long setParentId(Long issueId){
        if (!ObjectUtils.isEmpty(issueId) && issueId != 0) {
            return issueId;
        }
        return null;
    }

    private void setIssueTypeVO(IssueListFieldKVVO issueListFieldKVVO, List<IssueTypeVO> issueTypeVOList) {
        if (CollectionUtils.isNotEmpty(issueTypeVOList)) {
            Map<Long, IssueTypeVO> issueTypeVOMap = issueTypeVOList.stream().collect(Collectors.toMap(IssueTypeVO::getProjectId, Function.identity()));
            if (issueTypeVOMap.containsKey(issueListFieldKVVO.getProjectId())) {
                issueListFieldKVVO.setIssueTypeVO(issueTypeVOMap.get(issueListFieldKVVO.getProjectId()));
            } else {
                //未项目自定义的系统问题类型
                issueListFieldKVVO.setIssueTypeVO(issueTypeVOMap.get(0L));
            }
        }
    }

    private void checkSearchType(String searchType) {
        if (!Objects.isNull(searchType) && !WORK_BENCH_SEARCH_TYPE.contains(searchType)) {
            throw new CommonException("error.search.type.is.illegal");
        }
    }

    private void setListStarBeacon(List<IssueListFieldKVVO> issues, Long userId, List<Long> projectIds) {
        if (CollectionUtils.isNotEmpty(issues)) {
            List<Long> issueIds = issues.stream().map(IssueListFieldKVVO::getIssueId).collect(Collectors.toList());
            List<Long> starIssueIds = starBeaconMapper.selectStarIssuesByIds(issueIds, projectIds, userId);
            if (CollectionUtils.isNotEmpty(starIssueIds)) {
                issues.forEach(issue -> {
                    if (starIssueIds.contains(issue.getIssueId())) {
                        issue.setStarBeacon(true);
                    }
                });
            }
        }
    }

    private void setAssignee(List<IssueListFieldKVVO> list, Set<Long> userIds) {
        Map<Long, UserMessageDTO> userMap = userService.queryUsersMap(new ArrayList<>(userIds), true);
        list.forEach(l -> {
            Long assigneeId = l.getAssigneeId();
            if (!ObjectUtils.isEmpty(assigneeId)) {
                UserMessageDTO user = userMap.get(assigneeId);
                if (!ObjectUtils.isEmpty(user)) {
                    l.setAssigneeName(user.getName());
                    l.setAssigneeImageUrl(user.getImageUrl());
                    l.setAssigneeLoginName(user.getLoginName());
                    l.setAssigneeRealName(user.getRealName());
                }
            }
        });
    }

    @Override
    public void queryUserProjects(Long organizationId, Long projectId, List<Long> projectIds, List<ProjectVO> projects, Long userId, String type) {
        if (ObjectUtils.isEmpty(projectId)) {
            List<ProjectVO> projectVOS = remoteIamOperator.listProjectsByUserIdForSimple(organizationId,userId, null, true);
            if (CollectionUtils.isNotEmpty(projectVOS)) {
                projectIds.addAll(projectVOS.stream().map(ProjectVO::getId).collect(Collectors.toList()));
                projects.addAll(projectVOS);
            }
        } else {
            ProjectVO projectVO = ConvertUtil.queryProject(projectId);
            if (!organizationId.equals(projectVO.getOrganizationId())) {
                throw new CommonException("error.organization.illegal");
            }
            projects.add(projectVO);
            projectIds.add(projectId);
        }
    }

    @Override
    public Page<UserDTO> pagingUserProjectUsers(PageRequest pageRequest, Long organizationId, AgileUserVO agileUserVO) {
        List<Long> projectIds = new ArrayList<>();
        List<ProjectVO> projects = new ArrayList<>();
        Long userId = DetailsHelper.getUserDetails().getUserId();
        queryUserProjects(organizationId, null, projectIds, projects, userId, null);
        if (CollectionUtils.isEmpty(projectIds)) {
            return new Page<>();
        }
        agileUserVO.setProjectIds(new HashSet<>(projectIds));
        agileUserVO.setOrganizationId(organizationId);
        Page<UserDTO> page = remoteIamOperator.agileUsersByProjectIds(0L, pageRequest.getPage(), pageRequest.getSize(), agileUserVO);
        Set<Long> ignoredUserIds = agileUserVO.getIgnoredUserIds();
        List<UserDTO> result = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(ignoredUserIds)) {
            result.addAll(remoteIamOperator.listUsersByIds(ignoredUserIds.toArray(new Long[ignoredUserIds.size()]), false));
        }
        if (CollectionUtils.isNotEmpty(page.getContent())) {
            result.addAll(page.getContent());
        }
        return PageUtil.buildPageInfoWithPageInfoList(page, result);
    }

    @Override
    public Page<IssueVO> pagingQueryAvailableParents(PageRequest pageRequest,
                                                     Long projectId,
                                                     String issueType,
                                                     String param) {
        if (IssueTypeCode.isBug(issueType) || IssueTypeCode.isSubTask(issueType)) {
            /*
             * 选择子任务：可关联问题：故事、缺陷（不是其他的子缺陷）、任务
             * 选择缺陷：故事、任务
             */
            Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId,SchemeApplyType.AGILE);
            Page<IssueVO> result = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.listAvailableParents(projectId, issueType, param));
            result.getContent().forEach(r -> r.setIssueTypeVO(issueTypeDTOMap.get(r.getIssueTypeId())));
            return result;
        } else {
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
    }

    @Override
    public Page<IssueListFieldKVVO> pagedQueryMyAssigned(Long organizationId, Long projectId, PageRequest pageRequest, WorkBenchIssueSearchVO workBenchIssueSearchVO) {
        workBenchIssueSearchVO.setType("myAssigned");
        return queryBackLogIssuesByPersonal(organizationId, projectId, pageRequest, workBenchIssueSearchVO);
    }

    @Override
    public void handleUpdateComponentIssueRelWithoutRuleNotice(List<ComponentIssueRelVO> componentIssueRelVOList, Long projectId, Long issueId) {
        if (componentIssueRelVOList != null) {
            if (!componentIssueRelVOList.isEmpty()) {
                List<ComponentIssueRelDTO> componentIssueRelDTOList = modelMapper.map(componentIssueRelVOList, new TypeToken<List<ComponentIssueRelDTO>>() {
                }.getType());
                List<ComponentIssueRelDTO> componentIssueRelCreate = componentIssueRelDTOList.stream().filter(componentIssueRel ->
                        componentIssueRel.getComponentId() != null).collect(Collectors.toList());
                List<Long> curComponentIds = getComponentIssueRel(projectId, issueId).stream().
                        map(ComponentIssueRelDTO::getComponentId).collect(Collectors.toList());
                List<Long> createComponentIds = componentIssueRelCreate.stream().
                        map(ComponentIssueRelDTO::getComponentId).collect(Collectors.toList());
                curComponentIds.forEach(id -> {
                    if (!createComponentIds.contains(id)) {
                        ComponentIssueRelDTO componentIssueRelDTO = new ComponentIssueRelDTO();
                        componentIssueRelDTO.setIssueId(issueId);
                        componentIssueRelDTO.setComponentId(id);
                        componentIssueRelDTO.setProjectId(projectId);
                        componentIssueRelService.delete(componentIssueRelDTO);
                    }
                });
                componentIssueRelDTOList.forEach(componentIssueRel -> handleComponentIssueRel(componentIssueRel, projectId, issueId));
            } else {
                componentIssueRelService.batchComponentDelete(issueId);
            }
        }
    }

    @Override
    public void handleUpdateVersionIssueRelWithoutRuleNotice(List<VersionIssueRelVO> targetVOList, Long projectId, Long issueId, String versionType) {
        if(Objects.equals(versionType, ProductVersionService.VERSION_RELATION_TYPE_FIX)) {
            if (targetVOList == null || versionType == null) {
                return;
            }
            if (CollectionUtils.isNotEmpty(targetVOList)) {
                // 1. targetList中移除已发布和已归档的，记为realTargetList
                // 2. 用inDbVersions减去realTarget，记为preDelete
                // 3. preDelete移除已归档的，记为realDelete
                // 4. 使用realTarget减去DB，记为realCreate
                // 5. 删除realDelete, 添加realCreate，得到final
                List<VersionIssueRelDTO> targetList = modelMapper.map(targetVOList, new TypeToken<List<VersionIssueRelDTO>>() {}.getType());
                final List<Long> targetVersionIds = targetList.stream()
                        .map(VersionIssueRelDTO::getVersionId)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList());
                List<Long> inDbVersionIds = versionIssueRelMapper.queryByIssueIdAndProjectIdNoArchivedExceptInfluence(projectId, issueId, versionType);
                final List<ProductVersionDTO> versions = this.productVersionMapper.selectByCondition(Condition.builder(ProductVersionDTO.class)
                        .select(ProductVersionDTO.FIELD_VERSION_ID, ProductVersionDTO.FIELD_STATUS_CODE)
                        .andWhere(Sqls.custom()
                                .andIn(ProductVersionDTO.FIELD_VERSION_ID, ListUtils.union(targetVersionIds, inDbVersionIds))
                        ).build());
                final Map<Long, String> versionIdToStatusMap = versions.stream().collect(Collectors.toMap(ProductVersionDTO::getVersionId, ProductVersionDTO::getStatusCode));
                final List<Long> realTargetVersionIds = targetVersionIds.stream()
                        .filter(id -> Objects.equals(versionIdToStatusMap.get(id), ProductVersionService.VERSION_STATUS_CODE_PLANNING))
                        .collect(Collectors.toList());
                final List<Long> preDeleteVersionIds = ListUtils.subtract(inDbVersionIds, realTargetVersionIds);
                final List<Long> realDeleteVersionIds = preDeleteVersionIds.stream()
                        .filter(id -> !Objects.equals(versionIdToStatusMap.get(id), ProductVersionService.VERSION_STATUS_CODE_ARCHIVED))
                        .collect(Collectors.toList());
                final List<Long> realCreateVersionIds = ListUtils.subtract(realTargetVersionIds, inDbVersionIds);

                for (Long realDeleteVersionId : realDeleteVersionIds) {
                    versionIssueRelService.delete(
                            new VersionIssueRelDTO()
                                    .setIssueId(issueId)
                                    .setVersionId(realDeleteVersionId)
                                    .setRelationType(versionType)
                                    .setProjectId(projectId)
                    );
                }
                targetList = targetList.stream()
                        .filter(rel -> realCreateVersionIds.contains(rel.getVersionId()))
                        .peek(rel -> rel.setRelationType(versionType))
                        .collect(Collectors.toList());
                handleVersionIssueRel(targetList, projectId, issueId);
            } else {
                // 归档状态的版本之间的关联不删除
                VersionIssueRelDTO versionIssueRel = new VersionIssueRelDTO();
                versionIssueRel.createBatchDeleteVersionIssueRel(projectId, issueId, versionType);
                versionIssueRelService.batchDeleteByIssueIdAndTypeArchivedExceptInfluence(versionIssueRel);
            }
        } else if(Objects.equals(versionType, ProductVersionService.VERSION_RELATION_TYPE_INFLUENCE)) {
            List<VersionIssueRelDTO> targetList = modelMapper.map(targetVOList, new TypeToken<List<VersionIssueRelDTO>>() {}.getType());
            final List<Long> targetVersionIds = targetList.stream()
                    .map(VersionIssueRelDTO::getVersionId)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
            List<Long> inDbVersionIds = versionIssueRelMapper.queryByIssueIdAndProjectIdNoArchivedExceptInfluence(projectId, issueId, versionType);
            final List<Long> realCreateVersionIds = ListUtils.subtract(targetVersionIds, inDbVersionIds);
            final List<Long> realDeleteVersionIds = ListUtils.subtract(inDbVersionIds, targetVersionIds);
            for (Long realDeleteVersionId : realDeleteVersionIds) {
                versionIssueRelService.delete(
                        new VersionIssueRelDTO()
                                .setIssueId(issueId)
                                .setVersionId(realDeleteVersionId)
                                .setRelationType(versionType)
                                .setProjectId(projectId)
                );
            }
            targetList = targetList.stream()
                    .filter(rel -> realCreateVersionIds.contains(rel.getVersionId()))
                    .peek(rel -> rel.setRelationType(versionType))
                    .collect(Collectors.toList());
            handleVersionIssueRel(targetList, projectId, issueId);
        } else {
            throw new CommonException(BaseConstants.ErrorCode.DATA_INVALID);
        }
    }

    @Override
    public IssueVO doStateMachineCustomFlowAndRuleNotice(Long projectId, Long issueId,
                                                         String applyType, Set<Long> influenceIssueIds,
                                                         Boolean isDemo, Long transformId, InputDTO inputDTO) {
        TriggerCarrierVO triggerCarrierVO = new TriggerCarrierVO();
        IssueVO issueVO = this.self().doStateMachineTransformAndCustomFlow(projectId, issueId, applyType, influenceIssueIds, triggerCarrierVO, isDemo, transformId, inputDTO);
        this.self().batchUpdateInvokeTrigger(Collections.singletonList(triggerCarrierVO));
        return issueVO;
    }

    @Override
    public IssueVO doStateMachineTransformAndCustomFlow(Long projectId, Long issueId,
                                                        String applyType, Set<Long> influenceIssueIds,
                                                        TriggerCarrierVO triggerCarrierVO, Boolean isDemo,
                                                        Long transformId, InputDTO inputDTO){
        IssueDTO issue = issueMapper.selectByPrimaryKey(issueId);
        ExecuteResult executeResult;
        if (Boolean.TRUE.equals(isDemo)) {
            executeResult = stateMachineClientService.executeTransformForDemo(projectId, issueId, transformId, issue.getObjectVersionNumber(),
                    applyType, inputDTO);
        } else {
            executeResult = stateMachineClientService.executeTransform(projectId, issueId, transformId, issue.getObjectVersionNumber(),
                    applyType, inputDTO);
        }
        if (SchemeApplyType.AGILE.equals(applyType)) {
            IssueConvertDTO issueConvertDTO = new IssueConvertDTO();
            issueConvertDTO.setIssueId(issueId);
            issueConvertDTO.setStayDate(new Date());
            issueConvertDTO.setObjectVersionNumber(issueMapper.selectByPrimaryKey(issueId).getObjectVersionNumber());
            issueAccessDataService.updateSelective(issueConvertDTO);
        }
        Boolean onlyUpdateRank = executeResult.getOnlyUpdateRank();
        if (Boolean.TRUE.equals(onlyUpdateRank)) {
            return modelMapper.map(issueMapper.selectByPrimaryKey(issueId), IssueVO.class);
        } else {
            return doStateMachineCustomFlow(projectId, issueId, applyType, influenceIssueIds, triggerCarrierVO);
        }
    }

    @Override
    public IssueVO executionStateMachineCustomFlow(Long projectId, Long issueId,
                                                        String applyType, Set<Long> influenceIssueIds) {
        TriggerCarrierVO triggerCarrierVO = new TriggerCarrierVO();
        IssueVO issueVO = doStateMachineCustomFlow(projectId, issueId, applyType, influenceIssueIds, triggerCarrierVO);
        triggerCarrierVO.setAuditDomain(issueMapper.selectByPrimaryKey(issueId));
        this.self().batchUpdateInvokeTrigger(Collections.singletonList(triggerCarrierVO));
        return issueVO;
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, isBatch = true)
    public void batchUpdateInvokeTrigger(List<TriggerCarrierVO> triggerCarriers) {
        // 通过切面触发, 无需方法体
    }

    @Override
    public List<String> listLinkContents(Long projectId, Long issueId) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (Objects.isNull(issueDTO)) {
            return new ArrayList<>();
        }
        // 敏捷查询：知识、关联、评论、附件、前置依赖
        IssueCopyLinkContents linkContents = issueMapper.queryIssueLinkContents(projectId, issueId);
        // 关联测试用例
        linkContents.setRelatedTestCases(testServiceClientOperator.checkExistTestCaseLink(projectId, issueId));
        // 关联需求
        if (backlogExpandService != null) {
            linkContents.setRelatedBacklogs(backlogExpandService.checkExistBacklogRel(projectId, issueId));
        }
        // 关联分支
        linkContents.setRelatedBranches(devopsClientOperator.checkExistIssueBranchRel(projectId, issueId));
        return getLinkContents(linkContents);
    }

    private List<String> getLinkContents(IssueCopyLinkContents linkContents) {
        Class<?> clazz = linkContents.getClass();
        List<String> result = new ArrayList<>();
        IssueCopyLinkContents.ISSUE_COPY_LINK_CONTENTS.forEach(content -> {
            try {
                Field field = clazz.getDeclaredField(content);
                field.setAccessible(true);
                if (Boolean.TRUE.equals(field.getBoolean(linkContents) )) {
                    result.add(content);
                }
            } catch (Exception e) {
                throw new CommonException("error.get.link.contents");
            }
        });
        return result;
    }

    @Override
    public List<IssueRequiredFields> listAllRequiredField(Long projectId, Long organizationId, Long issueId, Boolean subTask) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (Objects.isNull(issueDTO)) {
            throw new CommonException("error.issue.not.exist");
        }
        Set<Long> issueIds = new HashSet<>();
        issueIds.add(issueId);
        if (Boolean.TRUE.equals(subTask)) {
            if (Objects.equals(SchemeApplyType.AGILE, issueDTO.getApplyType())) {
                // 敏捷工作项：查询子任务和自身
                issueIds.addAll(issueMapper.selectSubTaskIds(projectId, issueId));
            } else if (Objects.equals(SchemeApplyType.WATERFALL, issueDTO.getApplyType()) && agileWaterfallService != null) {
                // 瀑布工作项：查询所有子级
                issueIds.addAll(agileWaterfallService.selectDescendants(projectId, issueId));
            }
        }
        return listRequiredFieldByIssueTypeIds(projectId, organizationId, new ArrayList<>(issueIds));
    }

    private List<IssueRequiredFields> listRequiredFieldByIssueTypeIds(Long projectId,
                                                                      Long organizationId,
                                                                      List<Long> issueIds) {
        List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(issueIds, null, true, false);
        if (ObjectUtils.isEmpty(issueDTOList)) {
            return new ArrayList<>();
        }
        Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, Collections.singletonList(projectId), issueIds, false);
        List<Long> issueTypeIds = issueDTOList.stream().map(IssueDTO::getIssueTypeId).collect(Collectors.toList());
        List<IssueVO> issueVOList = issueAssembler.issueDOToCopyIssueVOList(issueDTOList, organizationId, projectId, issueIds);
        Map<Long, List<PageFieldViewVO>> issueTypeFieldMap = new HashMap<>();
        boolean belongToProgram = belongToProgram(organizationId, projectId);
        for (Long issueTypeId : issueTypeIds) {
            PageFieldViewParamVO param = new PageFieldViewParamVO();
            param.setIssueTypeId(issueTypeId);
            param.setSchemeCode(AGILE_SCHEME_CODE);
            param.setPageCode(PageCode.AGILE_ISSUE_CREATE);
            List<PageFieldViewVO> createPageFields =
                    pageFieldService.queryPageFieldViewsNoPermissionFilter(organizationId, projectId, param);
            Set<Long> fieldIds =
                    createPageFields
                            .stream()
                            .map(PageFieldViewVO::getFieldId)
                            .collect(Collectors.toSet());
            param.setPageCode(PageCode.AGILE_ISSUE_EDIT);
            for (PageFieldViewVO pageFieldView : pageFieldService.queryPageFieldViewsNoPermissionFilter(organizationId, projectId, param)) {
                Long fieldId = pageFieldView.getFieldId();
                if (!fieldIds.contains(fieldId)) {
                    createPageFields.add(pageFieldView);
                    fieldIds.add(fieldId);
                }
            }
            issueTypeFieldMap.put(issueTypeId, createPageFields);
        }
        Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
        List<IssueRequiredFields> result = new ArrayList<>();
        for (IssueVO issue : issueVOList) {
            List<PageFieldViewVO> createPageFields = issueTypeFieldMap.get(issue.getIssueTypeId());
            Map<String, Object> customFieldMap = Optional.ofNullable(foundationCodeValue.get(issue.getIssueId())).orElse(new HashMap<>());
            if (ObjectUtils.isEmpty(createPageFields)) {
                continue;
            }
            List<PageFieldViewVO> requiredSystemFields = new ArrayList<>();
            List<PageFieldViewVO> requiredCustomFields = new ArrayList<>();
            for (PageFieldViewVO createPageField : createPageFields) {
                if (Boolean.TRUE.equals(createPageField.getRequired())) {
                    handlerSystemAndCustomRequiredField(customFieldMap, belongToProgram, createPageField, requiredSystemFields, requiredCustomFields, issue);
                }
            }
            requiredSystemFields.addAll(requiredCustomFields);
            fieldPermissionService.filterPageFieldViewVO(projectId, organizationId, issue.getIssueTypeId(), requiredSystemFields);
            IssueRequiredFields issueRequiredFields = new IssueRequiredFields()
                    .setIssueId(issue.getIssueId())
                    .setSummary(issue.getSummary())
                    .setIssueNum(issue.getIssueNum())
                    .setRequiredFields(requiredSystemFields);
            Long issueTypeId = issue.getIssueTypeId();
            issueRequiredFields.setIssueTypeVO(issueTypeDTOMap.get(issueTypeId));
            result.add(issueRequiredFields);
        }
        return result;
    }

}
