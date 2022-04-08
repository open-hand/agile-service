package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.business.*;
import io.choerodon.agile.infra.annotation.RuleNotice;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueSearchDTO;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.feign.operator.TestServiceClientOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.core.domain.Page;
import com.google.common.collect.Lists;
import io.choerodon.agile.api.validator.IssueLinkValidator;
import io.choerodon.agile.api.validator.IssueValidator;
import io.choerodon.agile.api.validator.ProductVersionValidator;
import io.choerodon.agile.api.validator.SprintValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.event.IssuePayload;
import io.choerodon.agile.app.assembler.*;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.aspect.DataLogRedisUtil;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.asgard.saga.dto.StartInstanceDTO;
import io.choerodon.asgard.saga.feign.SagaClient;
import io.choerodon.core.domain.PageInfo;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.domain.AuditDomain;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.hzero.core.base.AopProxy;
import org.hzero.core.message.MessageAccessor;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 敏捷开发Issue
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 20:30:48
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueServiceImpl implements IssueService, AopProxy<IssueService> {
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
    private PriorityService priorityService;
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
    private BaseFeignClient baseFeignClient;
    @Autowired
    private ProjectUtil projectUtil;
    @Autowired
    private BoardAssembler boardAssembler;
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

    private static final String SUB_TASK = "sub_task";
    private static final String ISSUE_EPIC = "issue_epic";
    private static final String ISSUE_NUM = "issueNum";
    private static final String ISSUE_NUM_CONVERT = "issue_num_convert";
    private static final String ISSUE_MANAGER_TYPE = "模块负责人";
    private static final String TYPE_CODE_FIELD = "typeCode";
    private static final String EPIC_NAME_FIELD = "epicName";
    private static final String COLOR_CODE_FIELD = "colorCode";
    private static final String EPIC_ID_FIELD = "epicId";
    private static final String SPRINT_ID_FIELD = "sprintId";
    private static final String STORY_POINTS_FIELD = "storyPoints";
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
    private static final String ESTIMATED_END_TIME = "estimatedEndTime";
    private static final String FIELD_RANK = "Rank";
    protected static final String RANK_HIGHER = "评级更高";
    protected static final String RANK_LOWER = "评级更低";
    private static final String RANK_FIELD = "rank";
    private static final String AGILE_SCHEME_CODE = "agile_issue";
    private static final String ERROR_ISSUE_NOT_FOUND = "error.Issue.queryIssue";
    private static final String ERROR_ISSUE_TYPE_NOT_ISSUE_TEST= "error.Issue.type.isNotIssueTest";
    private static final String SEARCH = "search";
    private static final String STORYMAP = "storymap";
    private static final String AGILE = "agile";
    private static final String BACKETNAME = "agile-service";
    private static final String TRIGGER_ISSUE_ID = "triggerIssueId";
    private static final String AUTO_TRANFER_FLAG = "autoTranferFlag";
    private static final String STAR_BEACON_TYPE_ISSUE = "issue";
    private static final String BUG_TYPE = "bug";
    private static final String TASK_TYPE = "task";
    private static final String MY_START_BEACON = "myStarBeacon";
    private static final List<String> WORK_BENCH_SEARCH_TYPE = Arrays.asList("myBug", "reportedBug", MY_START_BEACON, "myReported", "myAssigned");
    private static final String[] UPDATE_TYPE_CODE_FIELD_LIST_NO_RANK = new String[]{TYPE_CODE_FIELD, REMAIN_TIME_FIELD, PARENT_ISSUE_ID, EPIC_NAME_FIELD, COLOR_CODE_FIELD, EPIC_ID_FIELD, STORY_POINTS_FIELD, EPIC_SEQUENCE, ISSUE_TYPE_ID, RELATE_ISSUE_ID};
    private static final String[] TRANSFORMED_TASK_FIELD_LIST_NO_RANK = new String[]{TYPE_CODE_FIELD, REMAIN_TIME_FIELD, PARENT_ISSUE_ID, EPIC_NAME_FIELD, COLOR_CODE_FIELD, EPIC_ID_FIELD, STORY_POINTS_FIELD, EPIC_SEQUENCE, ISSUE_TYPE_ID, STATUS_ID};
    private static final String[] COPY_PREDEFINED_FIELDS_NAME = new String[]
            {
                    ASSIGNEE_ID, EPIC_ID_FIELD, STORY_POINTS_FIELD, STATUS_ID,
                    FEATURE_ID, ENVIRONMENT, MAIN_RESPONSIBLE_ID, REMAIN_TIME_FIELD,
                    ESTIMATED_START_TIME, ESTIMATED_END_TIME, REPORTER_ID, PRIORITY_ID
            };
    @Value("${services.attachment.url}")
    private String attachmentUrl;

    private SagaClient sagaClient;

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
    private AgilePluginService agilePluginService;
    @Autowired
    private WorkLogMapper workLogMapper;

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
        if (!CollectionUtils.isEmpty(colorList)) {
            issueConvertDTO.initializationColor(colorList);
            //排序编号
            Integer sequence = issueMapper.queryMaxEpicSequenceByProject(issueConvertDTO.getProjectId());
            issueConvertDTO.setEpicSequence(sequence == null ? 0 : sequence + 1);
        }
        //初始化创建issue设置issue编号、项目默认设置
        issueConvertDTO.initializationIssue(statusId, projectInfoDTO);
        projectInfoService.updateIssueMaxNum(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueNum());
        //初始化排序
        if (issueConvertDTO.isIssueRank()) {
            calculationRank(issueConvertDTO.getProjectId(), issueConvertDTO);
        }
        if (issueConvertDTO.isIssueMapRank()) {
            calculationMapRank(issueConvertDTO);
        }
        issueValidator.verifyStoryPoints(issueConvertDTO);
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
        if (sprintValidator.hasIssue(projectId, issueConvertDTO.getSprintId())) {
            String rank = sprintMapper.queryMaxRank(projectId, issueConvertDTO.getSprintId());
            //处理rank为null的脏数据
            if (StringUtils.isEmpty(rank)) {
                issueConvertDTO.setRank(RankUtil.mid());
            } else {
                issueConvertDTO.setRank(RankUtil.genNext(rank));
            }
        } else {
            issueConvertDTO.setRank(RankUtil.mid());
        }
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_CREATED, instanceId = "issueId", allFieldCheck = true)
    public IssueVO queryIssueCreate(Long projectId, Long issueId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(attachmentUrl + "/" + BACKETNAME + "/" + issueAttachmentDO.getUrl()));
        }
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, issue.getApplyType());
        Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
        Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
        IssueVO result = issueAssembler.issueDetailDTOToVO(issue, issueTypeDTOMap, statusMapDTOMap, priorityDTOMap);
        sendMsgUtil.sendMsgByIssueCreate(projectId, result, DetailsHelper.getUserDetails().getUserId());
        return result;
    }

    @Override
    public IssueVO queryIssue(Long projectId, Long issueId, Long organizationId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        if (Objects.isNull(issue)) {
            throw new CommonException("error.issue.null");
        }
        issue.setSameParentIssueDTOList(Objects.nonNull(issue.getParentIssueId()) && !Objects.equals(issue.getParentIssueId(), 0L)?
                issueMapper.querySubIssueByIssueId(issue.getParentIssueId()): null);
        issue.setSameParentBugDOList(Objects.nonNull(issue.getRelateIssueId()) && !Objects.equals(issue.getRelateIssueId(), 0L)?
                issueMapper.querySubBugByIssueId(issue.getRelateIssueId()): null);
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(attachmentUrl + "/" + BACKETNAME + "/" + issueAttachmentDO.getUrl()));
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
            agilePluginService.programIssueDetailDTOToVO(issueVO,issue);
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
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(attachmentUrl + issueAttachmentDO.getUrl()));
        }
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, issue.getApplyType());
        Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
        Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
        IssueVO result = issueAssembler.issueDetailDTOToVO(issue, issueTypeDTOMap, statusMapDTOMap, priorityDTOMap);
        Long operatorId = DetailsHelper.getUserDetails().getUserId();
        sendMsgUtil.sendMsgByIssueAssignee(projectId, fieldList, result, operatorId);
        sendMsgUtil.sendMsgByIssueComplete(projectId, fieldList, result, operatorId);
        return result;
    }

    @Override
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
            if (!CollectionUtils.isEmpty(searchVO.getQuickFilterIds())) {
                filterSql = getQuickFilter(searchVO.getQuickFilterIds());
            }
            //处理未匹配的筛选
            boardAssembler.handleOtherArgs(searchVO);
            final String searchSql = filterSql;
            issueIdPage = getIssueIdPage(pageRequest, projectId, searchVO, searchSql, organizationId, isTreeView);
            Page<IssueListFieldKVVO> issueListDTOPage = new Page<>();
            if (!CollectionUtils.isEmpty(issueIdPage.getContent())) {
                List<Long> issueIds = issueIdPage.getContent();
                Set<Long> childrenIds = new HashSet<>();
                if (isTreeView) {
                    childrenIds = issueMapper.queryChildrenIdByParentId(issueIds, projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds());
                    boolean withSubIssues =
                            !Boolean.FALSE.equals(
                                    Optional.ofNullable(searchVO.getSearchArgs())
                                            .map(x -> x.get("withSubIssues"))
                                            .orElse(false));
                    if (!withSubIssues && CollectionUtils.isEmpty(childrenIds)){
                        childrenIds.add(0L);
                    }
                }
                List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(issueIds, childrenIds, false, isTreeView);
                Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
                Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
                Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
                List<Long> allIssueIds = issueDTOList.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
                Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, projectId, allIssueIds, false, AGILE_SCHEME_CODE);
                Map<Long, List<WorkLogVO>> workLogVOMap = workLogMapper.queryByIssueIds(Collections.singletonList(projectId), allIssueIds).stream().collect(Collectors.groupingBy(WorkLogVO::getIssueId));
                List<IssueListFieldKVVO> issueListFieldKVVOS = issueAssembler.issueDoToIssueListFieldKVDTO(issueDTOList, priorityMap, statusMapDTOMap, issueTypeDTOMap, foundationCodeValue, workLogVOMap);
                if (!ObjectUtils.isEmpty(agilePluginService) && !CollectionUtils.isEmpty(issueListFieldKVVOS)) {
                    agilePluginService.doToIssueListFieldKVDTO(projectId,issueListFieldKVVOS);
                }
                issueListDTOPage = PageUtil.buildPageInfoWithPageInfoList(issueIdPage,issueListFieldKVVOS);
            }
            return issueListDTOPage;
        } else {
            return new Page<>();
        }
    }

    private Page<Long> getIssueIdPage(PageRequest pageRequest, Long projectId, SearchVO searchVO, String searchSql, Long organizationId, Boolean isTreeView) {
        Page<Long>issueIdPage = new Page<>();
        if (!handleSortField(pageRequest).equals("")) {
            String fieldCode = handleSortField(pageRequest);
            Map<String, String> order = new HashMap<>(1);
            String sortCode = fieldCode.split("\\.")[1];
            order.put(fieldCode, sortCode);
            order.put(ISSUE_NUM, ISSUE_NUM_CONVERT);
            PageUtil.sortResetOrder(pageRequest.getSort(), null, order);
            List<Long> issueIdsWithSub =
                    issueMapper.queryIssueIdsListWithSub(projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds(), null, isTreeView)
                            .stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
            List<Long> foundationIssueIds = fieldValueService.sortIssueIdsByFieldValue(organizationId, projectId, pageRequest);

            List<Long> foundationIssueIdsWithSub = foundationIssueIds.stream().filter(issueIdsWithSub::contains).collect(Collectors.toList());
            List<Long> issueIdsWithSubWithoutFoundation = issueIdsWithSub.stream().filter(t -> !foundationIssueIdsWithSub.contains(t)).collect(Collectors.toList());


            issueIdPage.setNumber(pageRequest.getPage());
            issueIdPage.setSize(pageRequest.getSize());
            issueIdPage.setTotalElements(issueIdsWithSub.size());
            issueIdPage.addAll(handleIssueLists(foundationIssueIdsWithSub, issueIdsWithSubWithoutFoundation, pageRequest)
                    .subList((pageRequest.getPage() - 1) * pageRequest.getSize(), pageRequest.getPage() * pageRequest.getSize()));
        } else {
            String orderStr = getOrderStrOfQueryingIssuesWithSub(pageRequest.getSort());
            Page<IssueDTO> issues = PageHelper.doPage(pageRequest, () -> issueMapper.queryIssueIdsListWithSub(projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds(), orderStr, isTreeView));
            List<Long> issueIds = issues.getContent().stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
            issueIdPage = PageUtil.buildPageInfoWithPageInfoList(issues, issueIds);
        }
        return issueIdPage;
    }

    protected String getOrderStrOfQueryingIssuesWithSub(Sort sort) {
        Map<String, String> order = new HashMap<>(1);
        order.put("issueId", "issue_issue_id");
        order.put(ISSUE_NUM, ISSUE_NUM_CONVERT);
        return PageableHelper.getSortSql(PageUtil.sortResetOrder(sort, null, order));
    }

    protected List<Long> handleIssueLists(List<Long> foundationList, List<Long> agileList, PageRequest pageRequest) {
        if (!ObjectUtils.isEmpty(pageRequest.getSort())) {
            Iterator<Sort.Order> iterator = pageRequest.getSort().iterator();
            Sort.Direction direction = Sort.Direction.ASC;
            while (iterator.hasNext()) {
                Sort.Order order = iterator.next();
                direction = order.getDirection();
            }
            if (direction.isAscending()) {
                agileList.addAll(foundationList);
                return agileList;
            } else {
                foundationList.addAll(agileList);
                return foundationList;
            }
        } else return new ArrayList<>();
    }

    protected String handleSortField(PageRequest pageRequest) {
        if (!ObjectUtils.isEmpty(pageRequest.getSort())) {
            Iterator<Sort.Order> iterator = pageRequest.getSort().iterator();
            String fieldCode = "";
            while (iterator.hasNext()) {
                Sort.Order order = iterator.next();
                fieldCode = order.getProperty();
            }
            if (fieldCode.contains("foundation.")) {
                return fieldCode;
            } else return "";
        } else return "";
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
    public Boolean handleSearchUser(SearchVO searchVO, Long projectId) {
        if (searchVO.getSearchArgs() != null && searchVO.getSearchArgs().get(ASSIGNEE) != null) {
            String userName = (String) searchVO.getSearchArgs().get(ASSIGNEE);
            Boolean result = handlerUserSearch(projectId, userName, searchVO, "assigneeIds");
            if (!result) {
                return false;
            }
        }
        if (searchVO.getSearchArgs() != null && searchVO.getSearchArgs().get(REPORTER) != null) {
            String userName = (String) searchVO.getSearchArgs().get(REPORTER);
            Boolean result = handlerUserSearch(projectId, userName, searchVO, "reporterIds");
            if (!result) {
                return false;
            }
        }
        return true;
    }

    private Boolean handlerUserSearch(Long projectId, String userName, SearchVO searchVO, String key) {
        if (userName != null && !"".equals(userName)) {
            List<UserVO> userVOS = userService.queryUsersByNameAndProjectId(projectId, userName);
            if (!CollectionUtils.isEmpty(userVOS)) {
                searchVO.getAdvancedSearchArgs().put(key, userVOS.stream().map(UserVO::getId).collect(Collectors.toList()));
            } else {
                return false;
            }
        }
        return true;
    }

    @Override
    public IssueVO updateIssue(Long projectId, IssueUpdateVO issueUpdateVO, List<String> fieldList) {
        if (agilePluginService != null) {
            agilePluginService.checkFeatureBeforeUpdateIssue(issueUpdateVO,projectId);
        }
        if (fieldList.contains(EPIC_NAME_FIELD)
                && issueUpdateVO.getEpicName() != null
                && checkEpicName(projectId, issueUpdateVO.getEpicName(), issueUpdateVO.getIssueId())) {
            throw new CommonException("error.epicName.exist");
        }
        if (!fieldList.isEmpty()) {
            //处理issue自己字段
            this.self().handleUpdateIssue(issueUpdateVO, fieldList, projectId, issueUpdateVO.getIssueId());
        }
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
        return queryIssueByUpdate(projectId, issueId, fieldList);
    }

    @Override
    public void handleUpdateTagIssueRel(List<TagVO> tags, Long projectId, Long issueId) {
        if (agilePluginService != null) {
            agilePluginService.updateTagIssueRel(tags, projectId, issueId);
        }
    }

    @Override
    public List<PageFieldViewVO> listRequiredFieldByIssueType(Long projectId,
                                                              Long organizationId,
                                                              Long issueId,
                                                              Long issueTypeId) {
        String schemeCode = AGILE_SCHEME_CODE;
        IssueVO issue = queryIssue(projectId, issueId, organizationId);
        Map<String, Object> customFieldMap =
                pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, projectId, Arrays.asList(issueId), false, AGILE_SCHEME_CODE)
                        .get(issueId);
        AssertUtilsForCommonException.notNull(issue, "error.issue.null");
        PageFieldViewParamVO param = new PageFieldViewParamVO();
        param.setIssueTypeId(issueTypeId);
        param.setSchemeCode(schemeCode);
        param.setPageCode(PageCode.AGILE_ISSUE_CREATE);
        List<PageFieldViewVO> createPageFields =
                pageFieldService.queryPageFieldViewList(organizationId, projectId, param);
        Set<Long> fieldIds =
                createPageFields
                        .stream()
                        .map(PageFieldViewVO::getFieldId)
                        .collect(Collectors.toSet());
        param.setPageCode(PageCode.AGILE_ISSUE_EDIT);
        pageFieldService.queryPageFieldViewList(organizationId, projectId, param)
                .forEach(x -> {
                    Long fieldId = x.getFieldId();
                    if (!fieldIds.contains(fieldId)) {
                        createPageFields.add(x);
                        fieldIds.add(fieldId);
                    }
                });
        boolean belongToProgram;
        if (agilePluginService == null) {
            belongToProgram = false;
        } else {
            belongToProgram = baseFeignClient.getGroupInfoByEnableProject(organizationId, projectId).getBody() != null;
        }
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

    private void handlerSystemAndCustomRequiredField(Map<String, Object> customFieldMap, boolean belongToProgram, PageFieldViewVO x, List<PageFieldViewVO> requiredSystemFields, List<PageFieldViewVO> requiredCustomFields, IssueVO issue) {
        if (Boolean.TRUE.equals(x.getSystem())) {
            //系统字段
            String code = x.getFieldCode();
            if (FieldCode.EPIC.equals(code) && belongToProgram) {
                return;
            }
            if (isSystemFieldEmpty(x.getFieldCode(), issue)) {
                requiredSystemFields.add(x);
            }
        } else {
            if (ObjectUtils.isEmpty(customFieldMap.get(x.getFieldCode()))) {
                requiredCustomFields.add(x);
            }
        }
    }

    @Override
    public void executionUpdateStatus(Long projectId, Long issueId, ExecutionUpdateIssueVO executionUpdateIssueVO) {
        Long sprintId = executionUpdateIssueVO.getSprintId();
        String appleType = getApplyType(projectId);
        if (StringUtils.isEmpty(appleType)) {
            return;
        }
        Map<Long, Long> map = executionUpdateIssueVO.getIssueTypeStatusMap();
        IssueDetailDTO issueDetailDTO = issueMapper.queryIssueDetail(projectId, issueId);
        Long issueTypeId = issueDetailDTO.getIssueTypeId();
        Long currentStatusId = issueDetailDTO.getStatusId();
        Long targetStatusId = map.get(issueDetailDTO.getIssueTypeId());
        IssueSprintRelDTO issueSprintRelDTO = new IssueSprintRelDTO();
        issueSprintRelDTO.setProjectId(projectId);
        issueSprintRelDTO.setSprintId(sprintId);
        issueSprintRelDTO.setIssueId(issueId);
        List<IssueSprintRelDTO> sprintRelDTOS = issueSprintRelMapper.select(issueSprintRelDTO);
        if (CollectionUtils.isEmpty(sprintRelDTOS)) {
            return;
        }
        if (ObjectUtils.isEmpty(targetStatusId) || Objects.equals(currentStatusId, targetStatusId)) {
            return;
        }
        List<TransformVO> transformVOS = projectConfigService.queryTransformsByProjectId(projectId, currentStatusId, issueId, issueTypeId, appleType);
        if (!CollectionUtils.isEmpty(transformVOS)) {
            Map<Long, TransformVO> transformVOMap = transformVOS.stream().collect(Collectors.toMap(TransformVO::getEndStatusId, Function.identity()));
            TransformVO transformVO = transformVOMap.get(targetStatusId);
            if (!ObjectUtils.isEmpty(transformVO)) {
                updateIssueStatus(projectId, issueId, transformVO.getId(), transformVO.getStatusVO().getObjectVersionNumber(), appleType);
            }
        }
    }

    private String getApplyType(Long projectId) {
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        List<String> projectCodes = projectVO.getCategories().stream().map(ProjectCategoryDTO::getCode).collect(Collectors.toList());
        if (projectCodes.contains(ProjectCategory.MODULE_PROGRAM)) {
            return SchemeApplyType.PROGRAM;
        } else if (projectCodes.contains(ProjectCategory.MODULE_AGILE)) {
            return SchemeApplyType.AGILE;
        } else {
            return null;
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
            case FieldCode.INFLUENCE_VERSION:
                value = issue.getVersionIssueRelVOList();
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
                                     String applyType, IssueDTO triggerIssue, boolean autoTranferFlag) {
        stateMachineClientService.executeTransform(projectId, issueId, transformId, objectVersionNumber, applyType, new InputDTO(issueId, "updateStatus", updateTrigger(autoTranferFlag, triggerIssue)));
        if (SchemeApplyType.AGILE.equals(applyType)) {
            IssueConvertDTO issueConvertDTO = new IssueConvertDTO();
            issueConvertDTO.setIssueId(issueId);
            issueConvertDTO.setStayDate(new Date());
            issueConvertDTO.setObjectVersionNumber(issueMapper.selectByPrimaryKey(issueId).getObjectVersionNumber());
            issueAccessDataService.updateSelective(issueConvertDTO);
        }
        IssueVO result = doStateMachineCustomFlow(projectId, issueId, applyType);
        if (result != null) {
            return result;
        }
        if (backlogExpandService != null) {
            backlogExpandService.changeDetection(issueId, projectId, ConvertUtil.getOrganizationId(projectId));
        }
        return queryIssueByUpdate(projectId, issueId, Collections.singletonList(STATUS_ID));
    }

    @Override
    public IssueVO doStateMachineCustomFlow(Long projectId, Long issueId, String applyType) {
        /**
         * 修改属性报错，导致数据回滚但是状态机实例已经完成状态变更，导致issue无论变更什么状态都无效
         * 抛异常并清空当前实例的状态机的状态信息
         */
        try {
            statusFieldSettingService.handlerSettingToUpdateIssue(projectId, issueId);
            boolean transformFlag = statusLinkageService.updateParentStatus(projectId, issueId, applyType);
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

    private String updateTrigger(boolean autoTranferFlag, IssueDTO triggerIssue) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put(AUTO_TRANFER_FLAG, autoTranferFlag);
        if(Objects.nonNull(triggerIssue)){
            jsonObject.put(TRIGGER_ISSUE_ID, triggerIssue.getIssueId());
        }
        return JSON.toJSONString(jsonObject);
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldListName = "fieldList", instanceId = "issueId", idPosition = "arg")
    public void handleUpdateIssue(IssueUpdateVO issueUpdateVO, List<String> fieldList, Long projectId, Long issueId) {
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        IssueDTO originIssue = issueMapper.queryIssueWithNoCloseSprint(issueUpdateVO.getIssueId());
        IssueConvertDTO issueConvertDTO = issueAssembler.toTarget(issueUpdateVO, IssueConvertDTO.class);
        String issueType = originIssue.getTypeCode();
        //处理用户，前端可能会传0，处理为null
        issueConvertDTO.initializationIssueUser();
        if (fieldList.contains(SPRINT_ID_FIELD)) {
            IssueConvertDTO oldIssue = modelMapper.map(originIssue, IssueConvertDTO.class);
            Long sprintId = issueConvertDTO.getSprintId();
            //处理子任务的冲刺
            List<Long> issueIds = issueMapper.querySubIssueIdsByIssueId(projectId, issueConvertDTO.getIssueId());
            List<Long> subBugIds = issueMapper.querySubBugIdsByIssueId(projectId, issueConvertDTO.getIssueId());
            if (subBugIds != null && !subBugIds.isEmpty()) {
                issueIds.addAll(subBugIds);
            }
            Boolean exitSprint = issueConvertDTO.getSprintId() != null && !Objects.equals(issueConvertDTO.getSprintId(), 0L);
            Boolean condition = (!Objects.equals(oldIssue.getSprintId(), issueUpdateVO.getSprintId()));
            issueIds.add(issueConvertDTO.getIssueId());
            if (condition) {
                BatchRemoveSprintDTO batchRemoveSprintDTO = new BatchRemoveSprintDTO(projectId, issueConvertDTO.getSprintId(), issueIds);
                issueAccessDataService.removeIssueFromSprintByIssueIds(batchRemoveSprintDTO);
                if (agilePluginService != null) {
                    agilePluginService.updateIssueSprintChanged(oldIssue, projectId, sprintId, issueType);
                }
            }
            if (exitSprint) {
                issueAccessDataService.issueToDestinationByIds(projectId, issueConvertDTO.getSprintId(), issueIds, new Date(), customUserDetails.getUserId());
            }
            if (oldIssue.isIssueRank()) {
                calculationRank(projectId, issueConvertDTO);
                fieldList.add(RANK_FIELD);
                issueConvertDTO.setOriginSprintId(originIssue.getSprintId());
            }
        }
        if (agilePluginService != null) {
            agilePluginService.handlerProgramUpdateIssue(issueType,fieldList,projectId,issueUpdateVO,originIssue);
        }
        issueAccessDataService.update(issueConvertDTO, fieldList.toArray(new String[fieldList.size()]));
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
        //删除issueLink
        issueLinkService.deleteByIssueId(issueConvertDTO.getIssueId());
        //删除标签关联
        labelIssueRelService.deleteByIssueId(issueConvertDTO.getIssueId());
        //没有issue使用且没有设为默认值的标签进行垃圾回收
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
        fieldValueMapper.deleteList(projectId, issueId, AGILE_SCHEME_CODE, null);
        //不是子任务的issue删除子任务
        if (!(SUB_TASK).equals(issueConvertDTO.getTypeCode())) {
            if ((ISSUE_EPIC).equals(issueConvertDTO.getTypeCode())) {
                //如果是epic，会把该epic下的issue的epicId置为0
                issueAccessDataService.batchUpdateIssueEpicId(projectId, issueConvertDTO.getIssueId());
            } else {
                redisUtil.deleteRedisCache(new String[]{"Agile:EpicChart" + projectId + ":" + issueConvertDTO.getEpicId() + ":" + "*"});
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
        }
        if (backlogExpandService != null) {
            backlogExpandService.deleteIssueBacklogRel(issueId);
        }
        //删除日志信息
        dataLogDeleteByIssueId(projectId, issueId);
        deleteRuleLogRel(projectId, issueId);
        issueAccessDataService.delete(projectId, issueConvertDTO.getIssueId());
        //删除rank数据
        rankMapper.deleteRankByIssueId(issueId);
        //delete cache
        dataLogRedisUtil.handleDeleteRedisByDeleteIssue(projectId);
        testServiceClientOperator.deleteTestRel(projectId, issueId);
        if (backlogExpandService != null) {
            backlogExpandService.changeDetection(issueId, projectId, ConvertUtil.getOrganizationId(projectId));
        }
    }

    private void deleteRuleLogRel(Long projectId, Long issueId) {
        if (agileTriggerService != null) {
            RuleLogRelVO ruleLogRel = new RuleLogRelVO();
            ruleLogRel.setBusinessType(STAR_BEACON_TYPE_ISSUE);
            ruleLogRel.setInstanceId(issueId);
            ruleLogRel.setProjectId(projectId);
            agileTriggerService.delete(ruleLogRel);
        }
    }

    @Override
    public void batchDeleteIssuesAgile(Long projectId, List<Long> issueIds) {
        if (issueMapper.queryIssueIdsIsNotTest(projectId, issueIds) != issueIds.size()) {
            throw new CommonException(ERROR_ISSUE_TYPE_NOT_ISSUE_TEST);
        }
        issueMapper.batchDeleteIssues(projectId, issueIds);
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
        issueValidator.judgeExist(projectId, epicId);
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
        if (!CollectionUtils.isEmpty(subTaskIds)) {
            moveIssueIds.addAll(subTaskIds);
        }
        if (!CollectionUtils.isEmpty(subBugIds)) {
            moveIssueIds.addAll(subBugIds);
        }
        //把与现在冲刺与要移动的冲刺相同的issue排除掉
        List<IssueSearchDTO> issueSearchDTOList = issueMapper.queryIssueByIssueIds(projectId, moveIssueVO.getIssueIds()).stream()
                .filter(issueDO -> issueDO.getSprintId() == null ? sprintId != 0 : !issueDO.getSprintId().equals(sprintId)).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(issueSearchDTOList)) {
            List<Long> moveIssueIdsFilter = issueSearchDTOList.stream().map(IssueSearchDTO::getIssueId).collect(Collectors.toList());
            BatchRemoveSprintDTO batchRemoveSprintDTO = new BatchRemoveSprintDTO(projectId, sprintId, moveIssueIdsFilter);
            issueAccessDataService.removeIssueFromSprintByIssueIds(batchRemoveSprintDTO);
            if (sprintId != null && !Objects.equals(sprintId, 0L)) {
                issueAccessDataService.issueToDestinationByIds(projectId, sprintId, moveIssueIdsFilter, new Date(), customUserDetails.getUserId());
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
    public List<IssueEpicVO> listEpicSelectData(Long projectId) {
        return issueAssembler.toTargetList(Stream.of(issueMapper.queryIssueEpicSelectList(projectId),
                Optional.ofNullable(agilePluginService).map(service -> service
                        .selectEpicBySubProjectFeature(projectId)).orElse(Collections.emptyList()))
                .flatMap(Collection::stream).sorted(Comparator.comparing(AuditDomain::getCreationDate).reversed())
                .collect(Collectors.toList()), IssueEpicVO.class);
    }


    @Override
    public IssueSubVO queryIssueSub(Long projectId, Long organizationId, Long issueId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        issue.setPriorityVO(priorityService.queryById(organizationId, issue.getPriorityId()));
        issue.setIssueTypeVO(issueTypeService.queryById(issue.getIssueTypeId(), projectId));
        issue.setStatusVO(statusService.queryStatusById(organizationId, issue.getStatusId()));
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(attachmentUrl + issueAttachmentDO.getUrl()));
        }
        return issueAssembler.issueDetailDoToIssueSubDto(issue);
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_CREATED, instanceId = "issueId", allFieldCheck = true)
    public IssueSubVO queryIssueSubByCreate(Long projectId, Long issueId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(attachmentUrl + issueAttachmentDO.getUrl()));
        }
        IssueSubVO result = issueAssembler.issueDetailDoToIssueSubDto(issue);
        sendMsgUtil.sendMsgBySubIssueCreate(projectId, result, DetailsHelper.getUserDetails().getUserId());
        return result;
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
        return queryIssue(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId(), organizationId);
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
        if (!ObjectUtils.isEmpty(predefinedFields)) {
            fieldValueService.handlerPredefinedFields(projectId, issueIds, predefinedFields, batchUpdateFieldStatusVO, SchemeApplyType.AGILE, false);
        }
        List<PageFieldViewUpdateVO> customFields = batchUpdateFieldsValueVo.getCustomFields();
        if (!ObjectUtils.isEmpty(customFields)) {
            fieldValueService.handlerCustomFields(projectId, customFields, AGILE_SCHEME_CODE, issueIds, batchUpdateFieldStatusVO, false);
        }
        if(!ObjectUtils.isEmpty(batchUpdateFieldStatusVO.getErrorMsgMap())) {
            throw new CommonException("error.update.field.value");
        }
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
        Long currentStateMachineId = projectConfigService.queryStateMachineId(projectId, AGILE, issueUpdateTypeVO.getIssueTypeId());
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
        versionIssueRelDTOList.forEach(versionIssueRel -> {
            versionIssueRel.setIssueId(issueId);
            versionIssueRel.setProjectId(projectId);
            versionIssueRel.setRelationType(versionIssueRel.getRelationType() == null ? "fix" : versionIssueRel.getRelationType());
            issueValidator.verifyVersionIssueRelData(versionIssueRel);
            handleVersionIssueRelCreate(versionIssueRel);
        });
    }

    private void handleVersionIssueRelCreate(VersionIssueRelDTO versionIssueRelDTO) {
        if (issueValidator.existVersionIssueRel(versionIssueRelDTO)) {
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
        if (issueValidator.existComponentIssueRel(componentIssueRelDTO)) {
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
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldList = {"labelId"}, instanceId = "issueId", idPosition = "arg")
    public void handleUpdateLabelIssue(List<LabelIssueRelVO> labelIssueRelVOList, Long issueId, Long projectId) {
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
            //没有issue使用且没有设为默认值的标签进行垃圾回收
            issueLabelService.labelGarbageCollection(projectId);
        }

    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldList = {"versionId"}, instanceId = "issueId", idPosition = "arg")
    public void handleUpdateVersionIssueRel(List<VersionIssueRelVO> versionIssueRelVOList, Long projectId, Long issueId, String versionType) {
        if (versionIssueRelVOList != null && versionType != null) {
            if (!versionIssueRelVOList.isEmpty()) {
                //归档状态的版本之间的关联不删除
                List<VersionIssueRelDTO> versionIssueRelDTOS = modelMapper.map(versionIssueRelVOList, new TypeToken<List<VersionIssueRelDTO>>() {
                }.getType());
                List<VersionIssueRelDTO> versionIssueRelCreate = versionIssueRelDTOS.stream().filter(versionIssueRel ->
                        versionIssueRel.getVersionId() != null).collect(Collectors.toList());
                List<Long> curVersionIds = versionIssueRelMapper.queryByIssueIdAndProjectIdNoArchivedExceptInfluence(projectId, issueId, versionType);
                List<Long> createVersionIds = versionIssueRelCreate.stream().map(VersionIssueRelDTO::getVersionId).collect(Collectors.toList());
                curVersionIds.forEach(id -> {
                    if (!createVersionIds.contains(id)) {
                        VersionIssueRelDTO versionIssueRelDTO = new VersionIssueRelDTO();
                        versionIssueRelDTO.setIssueId(issueId);
                        versionIssueRelDTO.setVersionId(id);
                        versionIssueRelDTO.setRelationType(versionType);
                        versionIssueRelDTO.setProjectId(projectId);
                        versionIssueRelService.delete(versionIssueRelDTO);
                    }
                });
                versionIssueRelDTOS.forEach(rel -> rel.setRelationType(versionType));
                handleVersionIssueRel(versionIssueRelDTOS, projectId, issueId);
            } else {
                VersionIssueRelDTO versionIssueRel = new VersionIssueRelDTO();
                versionIssueRel.createBatchDeleteVersionIssueRel(projectId, issueId, versionType);
                versionIssueRelService.batchDeleteByIssueIdAndTypeArchivedExceptInfluence(versionIssueRel);
            }
        }

    }

    private List<ComponentIssueRelDTO> getComponentIssueRel(Long projectId, Long issueId) {
        return componentIssueRelMapper.selectByProjectIdAndIssueId(projectId, issueId);
    }

    @Override
    @RuleNotice(event = RuleNoticeEvent.ISSUE_UPDATE, fieldList = {"componentId"}, instanceId = "issueId", idPosition = "arg")
    public void handleUpdateComponentIssueRel(List<ComponentIssueRelVO> componentIssueRelVOList, Long projectId, Long issueId) {
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
        if (issueValidator.existLabelIssue(labelIssueRelDTO)) {
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
    public Page<IssueNumVO> queryIssueByOption(Long projectId, Long issueId, String issueNum, Boolean onlyActiveSprint, Boolean self, String content, PageRequest pageRequest) {
        //连表查询需要设置主表别名
        Map<String,String> orders = new HashMap<>();
        orders.put(ISSUE_NUM,ISSUE_NUM_CONVERT);
        Sort sort = PageUtil.sortResetOrder( pageRequest.getSort(), "ai", orders);
        pageRequest.setSort(sort);
        IssueNumDTO issueNumDTO = null;
        if (self) {
            issueNumDTO = issueMapper.queryIssueByIssueNumOrIssueId(projectId, issueId, issueNum);
            if (issueNumDTO != null) {
                pageRequest = new PageRequest(pageRequest.getPage(), pageRequest.getSize() - 1);
            }
        }
        Long activeSprintId = onlyActiveSprint ? getActiveSprintId(projectId) : null;
        Page<IssueNumDTO> issueDOPage = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.queryIssueByOption(projectId, issueId, issueNum, activeSprintId, self, content));
        if (self && issueNumDTO != null) {
            issueDOPage.getContent().add(0, issueNumDTO);
            issueDOPage.setSize(issueDOPage.getSize() + 1);
        }

        return PageUtil.buildPageInfoWithPageInfoList(issueDOPage, issueAssembler.issueNumDoToDto(issueDOPage.getContent(), projectId));
    }

    @Override
    public IssueVO cloneIssueByIssueId(Long projectId, Long issueId, CopyConditionVO copyConditionVO, Long organizationId, String applyType) {
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
            throw new CommonException("error.applyType.illegal");
        }
        IssueDetailDTO issueDetailDTO = issueMapper.queryIssueDetail(projectId, issueId);
        //处理需要复制的预定义字段
        List<String> predefinedFieldNames = copyConditionVO.getPredefinedFieldNames();
        if (CollectionUtils.isEmpty(predefinedFieldNames)) {
            predefinedFieldNames = new ArrayList<>();
        }
        handleCopyPredefinedFields(organizationId, issueDetailDTO, predefinedFieldNames);
        if (issueDetailDTO != null) {
            Long newIssueId;
            Long objectVersionNumber;
            issueDetailDTO.setSummary(copyConditionVO.getSummary());
            IssueTypeVO issueTypeVO = issueTypeService.queryById(issueDetailDTO.getIssueTypeId(), projectId);
            if (issueTypeVO.getTypeCode().equals(SUB_TASK)) {
                IssueSubCreateVO issueSubCreateVO = issueAssembler.issueDtoToIssueSubCreateDto(issueDetailDTO, predefinedFieldNames);
                IssueSubVO newIssue = stateMachineClientService.createSubIssue(issueSubCreateVO);
                newIssueId = newIssue.getIssueId();
                objectVersionNumber = newIssue.getObjectVersionNumber();
            } else {
                IssueCreateVO issueCreateVO = issueAssembler.issueDtoToIssueCreateDto(issueDetailDTO, predefinedFieldNames);
                if (ISSUE_EPIC.equals(issueCreateVO.getTypeCode())) {
                    setEpicName(projectId, copyConditionVO, issueCreateVO);
                }
                IssueVO newIssue = stateMachineClientService.createIssue(issueCreateVO, applyType);
                newIssueId = newIssue.getIssueId();
                objectVersionNumber = newIssue.getObjectVersionNumber();
            }
            //复制链接
            batchCreateCopyIssueLink(copyConditionVO.getIssueLink(), issueId, newIssueId, projectId);
            // 复制项目群的特性和史诗都不会去创建关联关系
            if (!(applyType.equals("program") && (issueDetailDTO.getTypeCode().equals(ISSUE_EPIC) || issueDetailDTO.getTypeCode().equals("feature")))) {
                //生成一条复制的关联
                createCopyIssueLink(issueDetailDTO.getIssueId(), newIssueId, projectId);
            }
            //复制故事点和剩余工作量并记录日志
            copyStoryPointAndRemainingTimeData(issueDetailDTO, projectId, newIssueId, objectVersionNumber);
            // 处理冲刺、子任务、自定义字段的值
            handlerOtherFields(projectId, predefinedFieldNames, issueDetailDTO, newIssueId, copyConditionVO);
            return queryIssue(projectId, newIssueId, organizationId);
        } else {
            throw new CommonException("error.issue.copyIssueByIssueId");
        }
    }

    private void handlerOtherFields(Long projectId, List<String> predefinedFieldNames, IssueDetailDTO issueDetailDTO, Long newIssueId, CopyConditionVO copyConditionVO) {
        //复制冲刺
        if (predefinedFieldNames.contains(SPRINT_ID_FIELD)) {
            handleCreateCopyIssueSprintRel(issueDetailDTO, newIssueId);
        }
        if (copyConditionVO.getSubTask() && !CollectionUtils.isEmpty(issueDetailDTO.getSubIssueDTOList())) {
            List<IssueDTO> subIssueDTOList = issueDetailDTO.getSubIssueDTOList();
            subIssueDTOList.forEach(issueDO -> copySubIssue(issueDO, newIssueId, projectId,copyConditionVO));
        }
        if (!CollectionUtils.isEmpty(copyConditionVO.getCustomFieldIds())) {
            // 复制自定义字段的值
            fieldValueService.copyCustomFieldValue(projectId, issueDetailDTO, newIssueId, copyConditionVO.getCustomFieldIds());
        }
    }

    private void handleCopyPredefinedFields(Long origanizationId, IssueDetailDTO issueDetailDTO, List<String> predefinedFieldNames) {
        //将不需要复制的预定义字段置空
        for (String fieldName : COPY_PREDEFINED_FIELDS_NAME) {
            if (!predefinedFieldNames.contains(fieldName)) {
                setFieldValueEmpty(issueDetailDTO, fieldName);
            }
        }

        //设置报告人为复制人
        if (!predefinedFieldNames.contains(REPORTER_ID)) {
            issueDetailDTO.setReporterId(DetailsHelper.getUserDetails().getUserId());
        }
        //设置优先级为默认优先级
        if (!predefinedFieldNames.contains(PRIORITY_ID)) {
            PriorityVO priorityVO = priorityService.queryDefaultByOrganizationId(origanizationId);
            issueDetailDTO.setPriorityCode("priority-" + priorityVO.getId());
            issueDetailDTO.setPriorityId(priorityVO.getId());
        }
    }

    private void setFieldValueEmpty(IssueDetailDTO issueDetailDTO, String fieldName) {
        Method method;
        try {
            PropertyDescriptor pd = new PropertyDescriptor(fieldName, issueDetailDTO.getClass());
            method = pd.getWriteMethod();//获得写方法
        } catch (IntrospectionException e) {
            throw new CommonException("error.copy.issue.setFiledValueEmpty");
        }
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

    protected void copyStoryPointAndRemainingTimeData(IssueDetailDTO issueDetailDTO, Long projectId, Long issueId, Long objectVersionNumber) {
        if (issueDetailDTO.getStoryPoints() == null && issueDetailDTO.getRemainingTime() == null) {
            return;
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
        updateIssue(projectId, issueUpdateVO, fieldList);
    }

    protected void copySubIssue(IssueDTO issueDTO, Long newIssueId, Long projectId, CopyConditionVO copyConditionVO) {
        IssueDetailDTO subIssueDetailDTO = issueMapper.queryIssueDetail(issueDTO.getProjectId(), issueDTO.getIssueId());
        IssueSubCreateVO issueSubCreateVO = issueAssembler.issueDtoToSubIssueCreateDto(subIssueDetailDTO, newIssueId);
        IssueSubVO newSubIssue = stateMachineClientService.createSubIssue(issueSubCreateVO);
        //复制剩余工作量并记录日志
        if (issueDTO.getRemainingTime() != null) {
            IssueUpdateVO subIssueUpdateVO = new IssueUpdateVO();
            subIssueUpdateVO.setRemainingTime(issueDTO.getRemainingTime());
            subIssueUpdateVO.setIssueId(newSubIssue.getIssueId());
            subIssueUpdateVO.setObjectVersionNumber(newSubIssue.getObjectVersionNumber());
            updateIssue(projectId, subIssueUpdateVO, Lists.newArrayList(REMAIN_TIME_FIELD));
        }
        fieldValueService.copyCustomFieldValue(projectId, subIssueDetailDTO, newSubIssue.getIssueId(), null);
    }

    protected void handleCreateCopyIssueSprintRel(IssueDetailDTO issueDetailDTO, Long newIssueId) {
        if (issueDetailDTO.getActiveSprint() != null) {
            handleCreateSprintRel(issueDetailDTO.getActiveSprint().getSprintId(), issueDetailDTO.getProjectId(), newIssueId);
        }
    }

    protected void batchCreateCopyIssueLink(Boolean condition, Long issueId, Long newIssueId, Long projectId) {
        if (condition) {
            List<IssueLinkDTO> issueLinkDTOList = modelMapper.map(issueLinkMapper.queryIssueLinkByIssueId(issueId, projectId, false), new TypeToken<List<IssueLinkDTO>>() {
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

    protected void createCopyIssueLink(Long issueId, Long newIssueId, Long projectId) {
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
                if (!CollectionUtils.isEmpty(subIssueIds)) {
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
                if (!CollectionUtils.isEmpty(issueLinkDTOS)) {
                    issueLinkService.deleteIssueLinkByIssueId(issueConvertDTO,issueLinkDTOS);
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
                return queryIssueSub(projectId, organizationId, issueConvertDTO.getIssueId());
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
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.TEST);
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
        return modelMapper.map(issueAccessDataService.updateSelective(updateIssue), IssueVO.class);
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
        userSettingDTO.setTypeCode(STORYMAP);
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
        sagaClient.startSaga("agile-delete-issue", new StartInstanceDTO(JSON.toJSONString(issuePayload), "", "", ResourceLevel.PROJECT.value(), projectId));
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
    public IssueNumDTO queryIssueByIssueNum(Long projectId, String issueNum) {
        return issueMapper.queryIssueByIssueNum(projectId, issueNum);
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
        Page emptyPage = PageUtil.emptyPageInfo(pageRequest.getPage(), pageRequest.getSize());
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
        if (!CollectionUtils.isEmpty(list)) {
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
            return PageUtil.emptyPageInfo(pageRequest.getPage(), pageRequest.getSize());
        }
    }

    @Override
    public Page<UserDTO> pagingQueryUsers(PageRequest pageRequest, Long projectId, String param) {
        Set<Long> userIds = issueMapper.selectUserIdsByProjectIds(Arrays.asList(projectId));
        return baseFeignClient.agileUsers(projectId, pageRequest.getPage(), pageRequest.getSize(), param, userIds).getBody();
    }

    @Override
    public Page<UserDTO> pagingQueryReporters(PageRequest pageRequest, Long projectId, String param) {
        Set<Long> userIds = issueMapper.selectReporterIdsByProjectId(projectId);
        return baseFeignClient.agileUsers(projectId, pageRequest.getPage(), pageRequest.getSize(), param, userIds).getBody();
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
                                                         PageRequest pageRequest) {
        WorkBenchIssueSearchVO workBenchIssueSearchVO = new WorkBenchIssueSearchVO();
        workBenchIssueSearchVO.setType("myReported");
        return queryBackLogIssuesByPersonal(organizationId, projectId, pageRequest, workBenchIssueSearchVO);
    }

    @Override
    public Page<IssueListFieldKVVO> queryBackLogIssuesByPersonal(Long organizationId,
                                                                 Long projectId,
                                                                 PageRequest pageRequest,
                                                                 WorkBenchIssueSearchVO workBenchIssueSearchVO) {
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
        Page<IssueDTO> parentPage = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.queryParentIssueByProjectIdsAndUserId(projectIds, userId, searchType));
        List<IssueDTO> parentIssuesDTOS = parentPage.getContent();
        if (CollectionUtils.isEmpty(parentIssuesDTOS)) {
            return new Page<>();
        }
        List<Long> parentIssues = parentIssuesDTOS.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
        List<IssueDTO> allIssue;
        if (Objects.equals(searchType, MY_START_BEACON)) {
            allIssue = issueMapper.listMyStarIssuesByProjectIdsAndUserId(projectIds, parentIssues, userId);
        } else {
            allIssue = issueMapper.listIssuesByParentIssueIdsAndUserId(projectIds,parentIssues, userId, searchType);
        }
        Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
        Map<Long, List<IssueTypeVO>> issueTypeDTOMap = issueTypeService.listIssueTypeMapByProjectIds(organizationId, projectIds);
        Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
        Map<Long, ProjectVO> projectVOMap = projects.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        List<IssueListFieldKVVO> list = new ArrayList<>();
        Set<Long> userIds = new HashSet<>();
        allIssue.forEach(v -> {
            IssueListFieldKVVO issueListFieldKVVO = new IssueListFieldKVVO();
            modelMapper.map(v,issueListFieldKVVO);
            setIssueTypeVO(issueListFieldKVVO, issueTypeDTOMap.get(v.getIssueTypeId()));
            issueListFieldKVVO.setStatusVO(statusMapDTOMap.get(v.getStatusId()));
            issueListFieldKVVO.setPriorityVO(priorityMap.get(v.getPriorityId()));
            issueListFieldKVVO.setProjectVO(projectVOMap.get(v.getProjectId()));
            // 设置父级issueId
            Long parentId = null;
            Long parentIssueId = v.getParentIssueId();
            Long relateIssueId = v.getRelateIssueId();
            parentId = setParentId(parentIssueId);
            if (parentId == null) {
                parentId = setParentId(relateIssueId);
            }
            issueListFieldKVVO.setParentId(parentId);
            list.add(issueListFieldKVVO);
            Long assigneeId = v.getAssigneeId();
            if (!ObjectUtils.isEmpty(assigneeId)) {
                userIds.add(assigneeId);
            }
        });
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

    private Long setParentId(Long issueId){
        if (!ObjectUtils.isEmpty(issueId) && issueId != 0) {
            return issueId;
        }
        return null;
    }

    private void setIssueTypeVO(IssueListFieldKVVO issueListFieldKVVO, List<IssueTypeVO> issueTypeVOList) {
        if (!CollectionUtils.isEmpty(issueTypeVOList)) {
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
        if (!CollectionUtils.isEmpty(issues)) {
            List<Long> issueIds = issues.stream().map(IssueListFieldKVVO::getIssueId).collect(Collectors.toList());
            List<Long> starIssueIds = starBeaconMapper.selectStarIssuesByIds(issueIds, projectIds, userId);
            if (!CollectionUtils.isEmpty(starIssueIds)) {
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

    private void queryUserProjects(Long organizationId, Long projectId, List<Long> projectIds, List<ProjectVO> projects, Long userId, String type) {
        if (ObjectUtils.isEmpty(projectId)) {
            List<ProjectVO> projectVOS = baseFeignClient.queryOrgProjects(organizationId,userId).getBody();
            if (!CollectionUtils.isEmpty(projectVOS)) {
                projectVOS
                        .stream()
                        .filter(v -> ((!Objects.isNull(type) && Objects.equals(type, MY_START_BEACON))
                                ? (Boolean.TRUE.equals(v.getEnabled()))
                                : (!ProjectCategory.checkContainProjectCategory(v.getCategories(),ProjectCategory.MODULE_PROGRAM) && Boolean.TRUE.equals(v.getEnabled()))))
                        .forEach(obj -> {
                            projectIds.add(obj.getId());
                            projects.add(obj);
                        });
            }
        } else {
            ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
            if (!organizationId.equals(projectVO.getOrganizationId())) {
                throw new CommonException("error.organization.illegal");
            }
            projects.add(projectVO);
            projectIds.add(projectId);
        }
    }

    @Override
    public Page<IssueVO> pagingQueryAvailableParents(PageRequest pageRequest,
                                                     Long projectId,
                                                     String issueType,
                                                     String param) {
        if (IssueTypeCode.isBug(issueType) || IssueTypeCode.isSubTask(issueType)) {
            /**
             * 选择子任务：可关联问题：故事、缺陷（不是其他的子缺陷）、任务
             * 选择缺陷：故事、任务
             */
            Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId,SchemeApplyType.AGILE);
            Page<IssueVO> result = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.listAvailableParents(projectId, issueType, param));
            result.getContent().forEach(r -> r.setIssueTypeVO(issueTypeDTOMap.get(r.getIssueTypeId())));
            return result;
        } else {
            return PageUtil.emptyPageInfo(pageRequest.getPage(), pageRequest.getSize());
        }
    }

    @Override
    public Page<IssueListFieldKVVO> pagedQueryMyAssigned(Long organizationId, Long projectId, PageRequest pageRequest) {
        WorkBenchIssueSearchVO workBenchIssueSearchVO = new WorkBenchIssueSearchVO();
        workBenchIssueSearchVO.setType("myAssigned");
        return queryBackLogIssuesByPersonal(organizationId, projectId, pageRequest, workBenchIssueSearchVO);
    }
}
