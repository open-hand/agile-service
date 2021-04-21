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
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

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
    private ReportAssembler reportAssembler;
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
    private ProjectInfoMapper projectInfoMapper;
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
    private InstanceService instanceService;
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
    private TagIssueRelMapper tagIssueRelMapper;

    private static final String SUB_TASK = "sub_task";
    private static final String ISSUE_EPIC = "issue_epic";
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
    private static final String REPORTER = "reporter";
    private static final String FIELD_RANK = "Rank";
    protected static final String RANK_HIGHER = "评级更高";
    protected static final String RANK_LOWER = "评级更低";
    private static final String RANK_FIELD = "rank";
//    private static final String FIX_RELATION_TYPE = "fix";
//    private static final String INFLUENCE_RELATION_TYPE = "influence";
//    private static final String PROJECT_ERROR = "error.project.notFound";
    private static final String ERROR_ISSUE_NOT_FOUND = "error.Issue.queryIssue";
    private static final String ERROR_PROJECT_INFO_NOT_FOUND = "error.createIssue.projectInfoNotFound";
    private static final String ERROR_ISSUE_STATE_MACHINE_NOT_FOUND = "error.createIssue.stateMachineNotFound";
    private static final String SEARCH = "search";
    private static final String STORYMAP = "storymap";
    private static final String AGILE = "agile";
    private static final String FIELD_CODES = "fieldCodes";
    private static final String FIELD_NAMES = "fieldNames";
    private static final String BACKETNAME = "agile-service";
    private static final String TRIGGER_ISSUE_ID = "triggerIssueId";
    private static final String AUTO_TRANFER_FLAG = "autoTranferFlag";
    private static final String STAR_BEACON_TYPE_ISSUE = "issue";
    private static final String BUG_TYPE = "bug";
    private static final String TASK_TYPE = "task";
    private static final List<String> WORK_BENCH_SEARCH_TYPE = Arrays.asList("myBug", "reportedBug", "myStarBeacon", "myReported", "myAssigned");

    @Autowired
    private ModelMapper modelMapper;
//
//    private static final String[] FIELDS_NAME;
//
//    private static final String[] FIELDS;
//
//    protected static Map<String, String> FIELD_MAP = new LinkedHashMap<>();
//
//    protected static String[] AUTO_SIZE_WIDTH = {"summary", "epicName", "feature",
//            "creationDate", "lastUpdateDate", "sprintName"};
//
//    static {
//        FIELD_MAP.put("typeName", "问题类型");
//        FIELD_MAP.put("issueNum", "问题编号");
//        FIELD_MAP.put("summary", "概要");
//        FIELD_MAP.put("description", "描述");
//        FIELD_MAP.put("priorityName", "优先级");
//        FIELD_MAP.put("statusName", "状态");
//        FIELD_MAP.put("resolution", "解决状态");
//        FIELD_MAP.put("sprintName", "冲刺");
//        FIELD_MAP.put("assigneeName", "经办人");
//        FIELD_MAP.put("reporterName", "报告人");
//        FIELD_MAP.put("storyPoints", "故事点");
//        FIELD_MAP.put("remainingTime", "剩余预估时间");
//        FIELD_MAP.put("versionName", "版本");
//        FIELD_MAP.put("epicName", "所属史诗");
//        FIELD_MAP.put("labelName", "标签");
//        FIELD_MAP.put("componentName", "模块");
//        FIELD_MAP.put("creationDate", "创建时间");
//        FIELD_MAP.put("lastUpdateDate", "最后更新时间");
//        FIELDS = new ArrayList<>(FIELD_MAP.keySet()).toArray(new String[FIELD_MAP.keySet().size()]);
//        FIELDS_NAME = new ArrayList<>(FIELD_MAP.values()).toArray(new String[FIELD_MAP.values().size()]);
//    }

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
        if (!ObjectUtils.isEmpty(tags)) {
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            tags.forEach(x -> {
                String tagName = x.getTagName();
                String appServiceCode = x.getAppServiceCode();
                if (!StringUtils.hasText(tagName) || !StringUtils.hasText(appServiceCode)) {
                    throw new CommonException("error.issue.tag.null");
                }
                TagIssueRelDTO dto = new TagIssueRelDTO();
                dto.setIssueId(issueId);
                dto.setOrganizationId(organizationId);
                dto.setTagName(tagName);
                dto.setAppServiceCode(appServiceCode);
                dto.setProjectId(projectId);
                if (tagIssueRelMapper.select(dto).isEmpty()) {
                    tagIssueRelMapper.insertSelective(dto);
                }
            });
        }
    }

    @Override
    public void afterCreateSubIssue(Long issueId, IssueConvertDTO subIssueConvertDTO, IssueSubCreateVO issueSubCreateVO, ProjectInfoDTO projectInfoDTO) {
        IssueCreateVO issueCreateVO = new IssueCreateVO();
        issueCreateVO.setLabelIssueRelVOList(issueCreateVO.getLabelIssueRelVOList());
        issueCreateVO.setComponentIssueRelVOList(issueCreateVO.getComponentIssueRelVOList());
        issueCreateVO.setVersionIssueRelVOList(issueCreateVO.getVersionIssueRelVOList());
        issueCreateVO.setIssueLinkCreateVOList(issueCreateVO.getIssueLinkCreateVOList());
        issueCreateVO.setTags(issueCreateVO.getTags());
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
            issueConvertDTO.setRank(RankUtil.genNext(rank));
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
            if (searchVO.getQuickFilterIds() != null && !searchVO.getQuickFilterIds().isEmpty()) {
                filterSql = getQuickFilter(searchVO.getQuickFilterIds());
            }
            //处理未匹配的筛选
            boardAssembler.handleOtherArgs(searchVO);
            final String searchSql = filterSql;
            if (!handleSortField(pageRequest).equals("")) {
                String fieldCode = handleSortField(pageRequest);
                Map<String, String> order = new HashMap<>(1);
                String sortCode = fieldCode.split("\\.")[1];
                order.put(fieldCode, sortCode);
                order.put("issueNum", "issue_num_convert");
                PageUtil.sortResetOrder(pageRequest.getSort(), null, order);
                List<Long> issueIdsWithSub =
                        issueMapper.queryIssueIdsListWithSub(projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds(), null, isTreeView)
                                .stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
                List<Long> foundationIssueIds = fieldValueService.sortIssueIdsByFieldValue(organizationId, projectId, pageRequest);

                List<Long> foundationIssueIdsWithSub = foundationIssueIds.stream().filter(issueIdsWithSub::contains).collect(Collectors.toList());
                List<Long> issueIdsWithSubWithoutFoundation = issueIdsWithSub.stream().filter(t -> !foundationIssueIdsWithSub.contains(t)).collect(Collectors.toList());

                issueIdPage = new Page<>();
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

            Page<IssueListFieldKVVO> issueListDTOPage;
            if (issueIdPage.getContent() != null && !issueIdPage.getContent().isEmpty()) {
                List<Long> issueIds = issueIdPage.getContent();
                Set<Long> childrenIds = new HashSet<>();
                if (isTreeView) {
                    childrenIds = issueMapper.queryChildrenIdByParentId(issueIds, projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds());
                }
                List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(issueIds, childrenIds, false, isTreeView);
                Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
                Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId, projectId);
                Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
                List<Long> allIssueIds = issueDTOList.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
                Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, projectId, allIssueIds, false, "agile_issue");
                Map<Long, List<WorkLogVO>> workLogVOMap = workLogMapper.queryByIssueIds(Collections.singletonList(projectId), allIssueIds).stream().collect(Collectors.groupingBy(WorkLogVO::getIssueId));
                List<IssueListFieldKVVO> issueListFieldKVVOS = issueAssembler.issueDoToIssueListFieldKVDTO(issueDTOList, priorityMap, statusMapDTOMap, issueTypeDTOMap, foundationCodeValue, workLogVOMap);
                AgilePluginService expandBean = SpringBeanUtil.getExpandBean(AgilePluginService.class);
                if (!ObjectUtils.isEmpty(expandBean) && !CollectionUtils.isEmpty(issueListFieldKVVOS)) {
                    expandBean.doToIssueListFieldKVDTO(projectId,issueListFieldKVVOS);
                }
                issueListDTOPage = PageUtil.buildPageInfoWithPageInfoList(issueIdPage,issueListFieldKVVOS);
            } else {
                issueListDTOPage = new Page<>();
            }
            return issueListDTOPage;
        } else {
            return new Page<>();
        }
    }

    protected String getOrderStrOfQueryingIssuesWithSub(Sort sort) {
        Map<String, String> order = new HashMap<>(1);
        order.put("issueId", "issue_issue_id");
        order.put("issueNum", "issue_num_convert");
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
            if (list != null && list.contains("0")) {
                otherArgs.put("sprintNull", true);
            }
            list = (List<String>) otherArgs.get("version");
            if (list != null && list.contains("0")) {
                otherArgs.put("versionNull", true);
            }
            list = (List<String>) otherArgs.get("component");
            if (list != null && list.contains("0")) {
                otherArgs.put("componentNull", true);
            }
            list = (List<String>) otherArgs.get("epic");
            if (list != null && list.contains("0")) {
                otherArgs.put("epicNull", true);
            }
            list = (List<String>) otherArgs.get("label");
            if (list != null && list.contains("0")) {
                otherArgs.put("labelNull", true);
            }
            list = (List<String>) otherArgs.get("assigneeId");
            if (list != null && list.contains("0")) {
                otherArgs.put("assigneeIdNull", true);
            }
        }
    }

    @Override
    public Boolean handleSearchUser(SearchVO searchVO, Long projectId) {
        if (searchVO.getSearchArgs() != null && searchVO.getSearchArgs().get(ASSIGNEE) != null) {
            String userName = (String) searchVO.getSearchArgs().get(ASSIGNEE);
            if (userName != null && !"".equals(userName)) {
                List<UserVO> userVOS = userService.queryUsersByNameAndProjectId(projectId, userName);
                if (userVOS != null && !userVOS.isEmpty()) {
                    searchVO.getAdvancedSearchArgs().put("assigneeIds", userVOS.stream().map(UserVO::getId).collect(Collectors.toList()));
                } else {
                    return false;
                }
            }
        }
        if (searchVO.getSearchArgs() != null && searchVO.getSearchArgs().get(REPORTER) != null) {
            String userName = (String) searchVO.getSearchArgs().get(REPORTER);
            if (userName != null && !"".equals(userName)) {
                List<UserVO> userVOS = userService.queryUsersByNameAndProjectId(projectId, userName);
                if (userVOS != null && !userVOS.isEmpty()) {
                    searchVO.getAdvancedSearchArgs().put("reporterIds", userVOS.stream().map(UserVO::getId).collect(Collectors.toList()));
                } else {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public IssueVO updateIssue(Long projectId, IssueUpdateVO issueUpdateVO, List<String> fieldList) {
        if (agilePluginService != null) {
            agilePluginService.checkFeatureBeforeUpdateIssue(issueUpdateVO,projectId);
        }
        if (fieldList.contains("epicName")
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
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        TagIssueRelDTO dto = new TagIssueRelDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setIssueId(issueId);
        tagIssueRelMapper.delete(dto);
        handleCreateTagIssueRel(tags, projectId, issueId);
    }

    @Override
    public IssueVO updateIssueStatus(Long projectId, Long issueId, Long transformId, Long objectVersionNumber, String applyType) {
        return this.self().updateIssueStatus(projectId, issueId, transformId, objectVersionNumber, applyType, null, false);
    }

    @Override
    public IssueVO updateIssueStatus(Long projectId, Long issueId, Long transformId, Long objectVersionNumber,
                                     String applyType, IssueDTO triggerIssue, boolean autoTranferFlag) {
        stateMachineClientService.executeTransform(projectId, issueId, transformId, objectVersionNumber, applyType, new InputDTO(issueId, "updateStatus", updateTrigger(autoTranferFlag, triggerIssue)));
        if ("agile".equals(applyType)) {
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
        return queryIssueByUpdate(projectId, issueId, Collections.singletonList("statusId"));
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
//        //史诗删除默认值校验
//        if(Objects.equals(issueConvertDTO.getTypeCode(), IssueTypeCode.ISSUE_EPIC.value())) {
//            objectSchemeFieldService.checkObjectSchemeFieldDefaultValueOfSingle(projectId, issueId, FieldCode.EPIC);
//        }
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
        fieldValueMapper.deleteList(projectId, issueId, "agile_issue", null);
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
        if ("task".equals(issueConvertDTO.getTypeCode()) || "story".equals(issueConvertDTO.getTypeCode())) {
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
//        //删除issue发送消息
//        IssuePayload issuePayload = new IssuePayload();
//        issuePayload.setIssueId(issueId);
//        issuePayload.setProjectId(projectId);
//        sagaClient.startSaga("agile-delete-issue", new StartInstanceDTO(JSON.toJSONString(issuePayload), "", "", ResourceLevel.PROJECT.value(), projectId));
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
            ruleLogRel.setBusinessType("issue");
            ruleLogRel.setInstanceId(issueId);
            ruleLogRel.setProjectId(projectId);
            agileTriggerService.delete(ruleLogRel);
        }
    }

    @Override
    public void batchDeleteIssuesAgile(Long projectId, List<Long> issueIds) {
        if (issueMapper.queryIssueIdsIsNotTest(projectId, issueIds) != issueIds.size()) {
            throw new CommonException("error.Issue.type.isNotIssueTest");
        }
        issueMapper.batchDeleteIssues(projectId, issueIds);
        dataLogRedisUtil.deleteByDeleteIssueInfo(projectId);
    }

    @Override
    public void batchDeleteIssues(Long projectId, List<Long> issueIds) {
        if (issueMapper.queryIssueIdsIsTest(projectId, issueIds) != issueIds.size()) {
            throw new CommonException("error.Issue.type.isNotIssueTest");
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
                throw new CommonException("error.Issue.type.isNotIssueTest");
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
        if (moveIssueVO.getRankIndex() != null && !moveIssueVO.getRankIndex()) {
            dataLogRank(projectId, moveIssueVO, RANK_LOWER, sprintId);
        } else if (moveIssueVO.getRankIndex() != null && moveIssueVO.getRankIndex()) {
            dataLogRank(projectId, moveIssueVO, RANK_HIGHER, sprintId);
        }
        issueAccessDataService.batchUpdateIssueRank(projectId, moveIssueDTOS);
        List<Long> moveIssueIds = moveIssueVO.getIssueIds();
        List<Long> frontIncomingIssues = deepCopy(moveIssueIds);
        //处理子任务与子缺陷
        List<Long> subTaskIds = issueMapper.querySubIssueIds(projectId, moveIssueIds);
        List<Long> subBugIds = issueMapper.querySubBugIds(projectId, moveIssueIds);
        if (subTaskIds != null && !subTaskIds.isEmpty()) {
            moveIssueIds.addAll(subTaskIds);
        }
        if (subBugIds != null && !subBugIds.isEmpty()) {
            moveIssueIds.addAll(subBugIds);
        }
        //把与现在冲刺与要移动的冲刺相同的issue排除掉
        List<IssueSearchDTO> issueSearchDTOList = issueMapper.queryIssueByIssueIds(projectId, moveIssueVO.getIssueIds()).stream()
                .filter(issueDO -> issueDO.getSprintId() == null ? sprintId != 0 : !issueDO.getSprintId().equals(sprintId)).collect(Collectors.toList());
        if (issueSearchDTOList != null && !issueSearchDTOList.isEmpty()) {
            List<Long> moveIssueIdsFilter = issueSearchDTOList.stream().map(IssueSearchDTO::getIssueId).collect(Collectors.toList());
            BatchRemoveSprintDTO batchRemoveSprintDTO = new BatchRemoveSprintDTO(projectId, sprintId, moveIssueIdsFilter);
            issueAccessDataService.removeIssueFromSprintByIssueIds(batchRemoveSprintDTO);
            if (sprintId != null && !Objects.equals(sprintId, 0L)) {
                issueAccessDataService.issueToDestinationByIds(projectId, sprintId, moveIssueIdsFilter, new Date(), customUserDetails.getUserId());
                if (agilePluginService != null) {
                    agilePluginService.handlerAssociateSprintsWithFeature(projectId, sprintId, frontIncomingIssues, issueSearchDTOList);
                }
            }
//            //如果移动冲刺不是活跃冲刺，则状态回到默认状态
//            batchHandleIssueStatus(projectId, moveIssueIdsFilter, sprintId);
            List<Long> assigneeIds = issueSearchDTOList.stream().filter(issue -> issue.getAssigneeId() != null && !Objects.equals(issue.getAssigneeId(), 0L)).map(IssueSearchDTO::getAssigneeId).distinct().collect(Collectors.toList());
            Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(assigneeIds, true);
            return issueSearchAssembler.dtoListToVO(issueSearchDTOList, usersMap, new HashMap<>(), new HashMap<>(), new HashMap<>());
        } else {
            return new ArrayList<>();
        }

    }

    private List<Long> deepCopy(List<Long> src) {
        List<Long> dest = new ArrayList<>(src.size());
        src.forEach(i -> dest.add(i));
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
    public synchronized IssueVO updateIssueTypeCode(IssueConvertDTO issueConvertDTO, IssueUpdateTypeVO issueUpdateTypeVO, Long organizationId) {
        String originType = issueConvertDTO.getTypeCode();
        if (originType.equals(SUB_TASK)) {
            issueConvertDTO.setParentIssueId(null);
        }
        if (STORY_TYPE.equals(issueConvertDTO.getTypeCode()) && issueConvertDTO.getStoryPoints() != null) {
            issueConvertDTO.setStoryPoints(null);
        }
        if (originType.equals("bug") && (issueConvertDTO.getRelateIssueId() != null && !Objects.equals(issueConvertDTO.getRelateIssueId(), 0L))) {
            issueConvertDTO.setRelateIssueId(null);
        }
        if ((originType.equals("story") || originType.equals("task"))
                && (!Objects.equals(issueUpdateTypeVO.getTypeCode(), "story") && !Objects.equals(issueUpdateTypeVO.getTypeCode(), "task"))) {
            issueMapper.updateSubBugRelateIssueId(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId());
        }
        if (issueUpdateTypeVO.getTypeCode().equals(ISSUE_EPIC)) {
            issueConvertDTO.setRank(null);
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
        } else {
            issueConvertDTO.setTypeCode(issueUpdateTypeVO.getTypeCode());
        }
        issueConvertDTO.setIssueTypeId(issueUpdateTypeVO.getIssueTypeId());
        issueAccessDataService.update(issueConvertDTO, new String[]{TYPE_CODE_FIELD, REMAIN_TIME_FIELD, PARENT_ISSUE_ID, EPIC_NAME_FIELD, COLOR_CODE_FIELD, EPIC_ID_FIELD, STORY_POINTS_FIELD, RANK_FIELD, EPIC_SEQUENCE, ISSUE_TYPE_ID, RELATE_ISSUE_ID});
        // 查看目标问题类型的状态机是否含有当前状态，没有就是用默认状态
        handlerStatus(issueConvertDTO.getProjectId(),issueUpdateTypeVO);
        return queryIssue(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId(), organizationId);
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
            issueAccessDataService.update(issueConvertDTO, new String[]{"statusId"});
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
                issueLinkDTO.setIssueId(issueLinkDTO.getIn() ? issueId : linkIssueId);
                issueLinkDTO.setLinkedIssueId(issueLinkDTO.getIn() ? linkIssueId : issueId);
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
//            if (versionIssueRel.getName() != null && versionIssueRel.getVersionId() == null) {
//                //重名校验
//                ProductVersionDTO productVersionDTO = versionIssueRel.createProductVersionDTO();
//                if (productVersionMapper.isRepeatName(productVersionDTO.getProjectId(), productVersionDTO.getName())) {
//                    //已归档的版本id是null,不进行任何操作
//                    Long versionId = productVersionMapper.queryVersionIdByNameAndProjectId(productVersionDTO.getName(), productVersionDTO.getProjectId());
//                    if (versionId != null) {
//                        productVersionDTO.setVersionId(versionId);
//                    } else {
//                        return;
//                    }
//                } else {
//                    ProductVersionCreateVO productVersionCreateVO = issueAssembler.toTarget(productVersionDTO, ProductVersionCreateVO.class);
//                    ProductVersionDetailVO productVersionDetailVO = productVersionService.createVersion(projectId, productVersionCreateVO);
//                    productVersionDTO.setVersionId(productVersionDetailVO.getVersionId());
//                }
//            }
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
                issueAccessDataService.update(issueConvertDTO, new String[]{"assigneeId"});
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
        orders.put("issueNum","issue_num_convert");
        Sort sort = PageUtil.sortResetOrder( pageRequest.getSort(), "ai", orders);
        pageRequest.setSort(sort);
        //pageable.resetOrder("ai", new HashMap<>());
        IssueNumDTO issueNumDTO = null;
//        PageRequest pageRequest = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize());
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

//    @Override
//    public void exportIssues(Long projectId, SearchVO searchVO, HttpServletRequest request,
//                             HttpServletResponse response, Long organizationId, Sort sort) {
//        //处理根据界面筛选结果导出的字段
//        Map<String, String[]> fieldMap =
//                handleExportFields(searchVO.getExportFieldCodes(), projectId, organizationId, FIELDS_NAME, FIELDS);
//        String[] fieldCodes = sortFieldCodes(fieldMap.get(FIELD_CODES));
//        String[] fieldNames = sortFieldNames(fieldMap.get(FIELD_NAMES));
//        ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
//        projectInfoDTO.setProjectId(projectId);
//        projectInfoDTO = projectInfoMapper.selectOne(projectInfoDTO);
//        ProjectVO project = userService.queryProject(projectId);
//        if (project == null) {
//            throw new CommonException(PROJECT_ERROR);
//        }
//        project.setCode(projectInfoDTO.getProjectCode());
//        Boolean condition = handleSearchUser(searchVO, projectId);
//
//        String sheetName = project.getName();
//        Workbook workbook = ExcelUtil.initIssueExportWorkbook(sheetName, fieldNames);
//        ExcelCursorDTO cursor = new ExcelCursorDTO(1, 0, 1000);
//        if (condition) {
//            String filterSql = null;
//            if (searchVO.getQuickFilterIds() != null && !searchVO.getQuickFilterIds().isEmpty()) {
//                filterSql = getQuickFilter(searchVO.getQuickFilterIds());
//            }
//            final String searchSql = filterSql;
//            String orderStr = getOrderStrOfQueryingIssuesWithSub(sort);
//            while (true) {
//                //查询所有父节点问题
//                Page<IssueDTO> page =
//                        PageHelper.doPage(cursor.getPage(), cursor.getSize(), () -> issueMapper.queryIssueIdsListWithSub(projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds(), orderStr));
//                if (page.getTotalElements() < 1) {
//                    break;
//                }
//                List<Long> parentIds = page.getContent().stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
//                List<Long> issueIds = new ArrayList<>();
//                Map<Long, Set<Long>> parentSonMap = new HashMap<>();
//                List<IssueDTO> issues = new ArrayList<>();
//                if (!parentIds.isEmpty()) {
//                    Set<Long> childrenIds = issueMapper.queryChildrenIdByParentId(parentIds, projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds());
//                    cursor.addCollections(childrenIds);
//                    issues = issueMapper.queryIssueListWithSubByIssueIds(parentIds, childrenIds, true);
//                }
//                Map<Long, ExportIssuesVO> issueMap = new LinkedHashMap<>();
//                cursor
//                        .addCollections(page.getContent())
//                        .addCollections(parentIds)
//                        .addCollections(issueIds)
//                        .addCollections(parentSonMap)
//                        .addCollections(issueMap)
//                        .addCollections(issues);
//                if (!ObjectUtils.isEmpty(issues)) {
//                    Set<Long> userIds = new HashSet<>();
//                    issues.forEach(i -> {
//                        issueIds.add(i.getIssueId());
//                        Long assigneeId = i.getAssigneeId();
//                        Long reporterId = i.getReporterId();
//                        if (!ObjectUtils.isEmpty(assigneeId) && !Objects.equals(assigneeId, 0L)) {
//                            userIds.add(assigneeId);
//                        }
//                        if (!ObjectUtils.isEmpty(reporterId) && !Objects.equals(reporterId, 0L)) {
//                            userIds.add(reporterId);
//                        }
//                    });
//                    Map<Long, UserMessageDTO> usersMap = userService.queryUsersMap(new ArrayList<>(userIds), true);
//                    Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.AGILE);
//                    Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
//                    Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
//                    Map<Long, List<SprintNameDTO>> closeSprintNames = issueMapper.querySprintNameByIssueIds(projectId, issueIds).stream().collect(Collectors.groupingBy(SprintNameDTO::getIssueId));
//                    Map<Long, List<VersionIssueRelDTO>> fixVersionNames = issueMapper.queryVersionNameByIssueIds(projectId, issueIds, FIX_RELATION_TYPE).stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
//                    Map<Long, List<VersionIssueRelDTO>> influenceVersionNames = issueMapper.queryVersionNameByIssueIds(projectId, issueIds, INFLUENCE_RELATION_TYPE).stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
//                    Map<Long, List<LabelIssueRelDTO>> labelNames = issueMapper.queryLabelIssueByIssueIds(projectId, issueIds).stream().collect(Collectors.groupingBy(LabelIssueRelDTO::getIssueId));
//                    Map<Long, List<ComponentIssueRelDTO>> componentMap = issueMapper.queryComponentIssueByIssueIds(projectId, issueIds).stream().collect(Collectors.groupingBy(ComponentIssueRelDTO::getIssueId));
//                    Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, projectId, issueIds, true);
//                    cursor
//                            .addCollections(userIds)
//                            .addCollections(usersMap)
//                            .addCollections(issueTypeDTOMap)
//                            .addCollections(statusMapDTOMap)
//                            .addCollections(priorityDTOMap)
//                            .addCollections(closeSprintNames)
//                            .addCollections(fixVersionNames)
//                            .addCollections(influenceVersionNames)
//                            .addCollections(labelNames)
//                            .addCollections(componentMap)
//                            .addCollections(foundationCodeValue);
//                    issues.forEach(issue -> {
//                        Long issueId = issue.getIssueId();
//                        ExportIssuesVO exportIssuesVO = new ExportIssuesVO();
//                        BeanUtils.copyProperties(issue, exportIssuesVO);
//
//                        exportIssuesVO.setProjectName(project.getName());
//                        exportIssuesVO.setSprintName(getActiveSprintName(issue));
//                        setAssignee(usersMap, issue, exportIssuesVO);
//                        serReporter(usersMap, issue, exportIssuesVO);
//                        setPriorityName(priorityDTOMap, issue, exportIssuesVO);
//                        setStatusName(statusMapDTOMap, issue, exportIssuesVO);
//                        setTypeName(issueTypeDTOMap, issue, exportIssuesVO);
//                        setCloseSprintName(closeSprintNames, issueId, exportIssuesVO);
//                        setFixVersionName(fixVersionNames, issueId, exportIssuesVO);
//                        exportIssuesVO.setSprintName(exportIssuesSprintName(exportIssuesVO));
//                        setInfluenceVersionName(influenceVersionNames, issueId, exportIssuesVO);
//                        setLabelName(labelNames, issueId, exportIssuesVO);
//                        setComponentName(componentMap, issueId, exportIssuesVO);
//                        exportIssuesVO.setVersionName(exportIssuesVersionName(exportIssuesVO));
//                        exportIssuesVO.setDescription(getDes(exportIssuesVO.getDescription()));
//                        setFoundationFieldValue(foundationCodeValue, issueId, exportIssuesVO);
//                        issueMap.put(issueId, exportIssuesVO);
//                        processParentSonRelation(parentSonMap, issue);
//                    });
//                }
//                ExcelUtil.writeIssue(issueMap, parentSonMap, ExportIssuesVO.class, fieldNames, fieldCodes, sheetName, Arrays.asList(AUTO_SIZE_WIDTH), workbook, cursor);
//
//                boolean hasNextPage = cursor.getPage() < page.getTotalPages();
//                cursor.clean();
//                if (!hasNextPage) {
//                    break;
//                }
//                //查询后页数增1
//                cursor.increasePage();
//            }
//        }
//        ExcelUtil.writeToResponse(response, workbook);
//    }
//
//    protected void setLabelName(Map<Long, List<LabelIssueRelDTO>> labelNames, Long issueId, ExportIssuesVO exportIssuesVO) {
//        String labelName = "";
//        List<LabelIssueRelDTO> labelIssueRel = labelNames.get(issueId);
//        if (!ObjectUtils.isEmpty(labelIssueRel)) {
//            labelName = labelIssueRel.stream().map(LabelIssueRelDTO::getLabelName).collect(Collectors.joining(","));
//        }
//        exportIssuesVO.setLabelName(labelName);
//    }
//
//    protected void setComponentName(Map<Long, List<ComponentIssueRelDTO>> componentMap, Long issueId, ExportIssuesVO exportIssuesVO) {
//        String componentName = "";
//        List<ComponentIssueRelDTO> componentIssueRel = componentMap.get(issueId);
//        if (!ObjectUtils.isEmpty(componentIssueRel)) {
//            componentName = componentIssueRel.stream().map(ComponentIssueRelDTO::getName).collect(Collectors.joining(","));
//        }
//        exportIssuesVO.setComponentName(componentName);
//    }
//
//    protected void setFoundationFieldValue(Map<Long, Map<String, Object>> foundationCodeValue, Long issueId, ExportIssuesVO exportIssuesVO) {
//        Map<String, Object> fieldValue = foundationCodeValue.get(issueId);
//        if (fieldValue == null) {
//            fieldValue = new HashMap<>();
//        }
//        exportIssuesVO.setFoundationFieldValue(fieldValue);
//    }
//
//    protected void setInfluenceVersionName(Map<Long, List<VersionIssueRelDTO>> influenceVersionNames, Long issueId, ExportIssuesVO exportIssuesVO) {
//        String influenceVersionName = "";
//        List<VersionIssueRelDTO> versionIssueRel = influenceVersionNames.get(issueId);
//        if (!ObjectUtils.isEmpty(versionIssueRel)) {
//            influenceVersionName = versionIssueRel.stream().map(VersionIssueRelDTO::getName).collect(Collectors.joining(","));
//        }
//        exportIssuesVO.setInfluenceVersionName(influenceVersionName);
//    }
//
//    protected void setCloseSprintName(Map<Long, List<SprintNameDTO>> closeSprintNames, Long issueId, ExportIssuesVO exportIssuesVO) {
//        String closeSprintName = "";
//        List<SprintNameDTO> sprintNames = closeSprintNames.get(issueId);
//        if (!ObjectUtils.isEmpty(sprintNames)) {
//            closeSprintName =
//                    sprintNames
//                            .stream()
//                            .map(SprintNameDTO::getSprintName)
//                            .collect(Collectors.joining(","));
//        }
//        exportIssuesVO.setCloseSprintName(closeSprintName);
//    }
//
//    protected void setFixVersionName(Map<Long, List<VersionIssueRelDTO>> fixVersionNames, Long issueId, ExportIssuesVO exportIssuesVO) {
//        String fixVersionName = "";
//        List<VersionIssueRelDTO> versionIssueRel = fixVersionNames.get(issueId);
//        if (!ObjectUtils.isEmpty(versionIssueRel)) {
//            fixVersionName =
//                    versionIssueRel
//                            .stream()
//                            .map(VersionIssueRelDTO::getName)
//                            .collect(Collectors.joining(","));
//        }
//        exportIssuesVO.setFixVersionName(fixVersionName);
//    }
//
//    protected void setTypeName(Map<Long, IssueTypeVO> issueTypeDTOMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
//        IssueTypeVO issueTypeVO = issueTypeDTOMap.get(issue.getIssueTypeId());
//        if (!ObjectUtils.isEmpty(issueTypeVO)) {
//            exportIssuesVO.setTypeName(issueTypeVO.getName());
//        }
//    }
//
//    protected void setStatusName(Map<Long, StatusVO> statusMapDTOMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
//        StatusVO statusVO = statusMapDTOMap.get(issue.getStatusId());
//        if (!ObjectUtils.isEmpty(statusVO)) {
//            exportIssuesVO.setStatusName(statusVO.getName());
//        }
//    }
//
//    protected void setPriorityName(Map<Long, PriorityVO> priorityDTOMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
//        Long priorityId = issue.getPriorityId();
//        PriorityVO priorityVO = priorityDTOMap.get(priorityId);
//        if (!ObjectUtils.isEmpty(priorityVO)) {
//            exportIssuesVO.setPriorityName(priorityVO.getName());
//        }
//    }
//
//    protected void serReporter(Map<Long, UserMessageDTO> usersMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
//        Long reporterId = issue.getReporterId();
//        UserMessageDTO userMessage = usersMap.get(reporterId);
//        if (!ObjectUtils.isEmpty(userMessage)) {
//            exportIssuesVO.setReporterName(userMessage.getName());
//            exportIssuesVO.setReporterRealName(userMessage.getRealName());
//        }
//    }
//
//    protected void setAssignee(Map<Long, UserMessageDTO> usersMap, IssueDTO issue, ExportIssuesVO exportIssuesVO) {
//        Long assigneeId = issue.getAssigneeId();
//        UserMessageDTO userMessage = usersMap.get(assigneeId);
//        if (!ObjectUtils.isEmpty(userMessage)) {
//            exportIssuesVO.setAssigneeName(userMessage.getName());
//            exportIssuesVO.setAssigneeRealName(userMessage.getRealName());
//        }
//    }
//
//    protected String getActiveSprintName(IssueDTO issue) {
//        List<IssueSprintDTO>  issueSprintList = issue.getIssueSprintDTOS();
//        if (!ObjectUtils.isEmpty(issueSprintList)) {
//            for(IssueSprintDTO sprint : issueSprintList) {
//                if (!"closed".equals(sprint.getStatusCode())) {
//                    return sprint.getSprintName();
//                }
//            }
//        }
//        return null;
//    }
//
//    protected String[] sortFieldNames(String[] fieldNames) {
//        List<String> result = new ArrayList<>();
//        result.add("问题类型");
//        result.add("问题编号");
//        result.add("概要");
//        for (String str : fieldNames) {
//            if (result.get(0).equals(str)
//                    || result.get(1).equals(str)
//                    || result.get(2).equals(str)) {
//                continue;
//            }
//            result.add(str);
//        }
//        return result.toArray(new String[result.size()]);
//    }
//
//    protected String[] sortFieldCodes(String[] fieldCodes) {
//        List<String> result = new ArrayList<>();
//        result.add("typeName");
//        result.add("issueNum");
//        result.add("summary");
//        for (String str : fieldCodes) {
//            if (result.get(0).equals(str)
//                    || result.get(1).equals(str)
//                    || result.get(2).equals(str)) {
//                continue;
//            }
//            result.add(str);
//        }
//        return result.toArray(new String[result.size()]);
//    }
//
//    protected void processParentSonRelation(Map<Long, Set<Long>> parentSonMap, IssueDTO issue) {
//        String typeCode = issue.getTypeCode();
//        Long issueId = issue.getIssueId();
//        if (IssueTypeCode.isBug(typeCode)) {
//            Long relateIssueId = issue.getRelateIssueId();
//            if (!ObjectUtils.isEmpty(relateIssueId) && !Objects.equals(relateIssueId, 0L)) {
//                appendToParentSonMap(relateIssueId, issueId, parentSonMap);
//            }
//        }
//        if (IssueTypeCode.isSubTask(typeCode)) {
//            Long parentIssueId = issue.getParentIssueId();
//            if (!ObjectUtils.isEmpty(parentIssueId) && !Objects.equals(parentIssueId, 0L)) {
//                appendToParentSonMap(parentIssueId, issueId, parentSonMap);
//            }
//        }
//    }
//
//    private void appendToParentSonMap(Long parentId, Long issueId, Map<Long, Set<Long>> parentSonMap) {
//        Set<Long> childrenSet =  parentSonMap.get(parentId);
//        if (childrenSet == null) {
//            childrenSet = new HashSet<>();
//            parentSonMap.put(parentId, childrenSet);
//        }
//        childrenSet.add(issueId);
//    }
//
//
//    /**
//     * 处理根据界面筛选结果导出的字段
//     *
//     * @param exportFieldCodes
//     * @return
//     */
//    protected Map<String, String[]> handleExportFields(List<String> exportFieldCodes,
//                                                     Long projectId,
//                                                     Long organizationId,
//                                                     String[] fieldsName,
//                                                     String[] fields) {
//        Map<String, String[]> fieldMap = new HashMap<>(2);
//        ObjectMapper m = new ObjectMapper();
//
//        Object content = Optional.ofNullable(objectSchemeFieldService
//                .listQuery(organizationId, projectId, ObjectSchemeCode.AGILE_ISSUE))
//                .orElseThrow(() -> new CommonException("error.foundation.listQuery"))
//                .get("content");
//
//        List<Object> contentList = m.convertValue(content, List.class);
//        List<ObjectSchemeFieldDTO> fieldDTOS = new ArrayList<>();
//
//        if (content != null) {
//            contentList.forEach(k ->
//                    fieldDTOS.add(m.convertValue(k, ObjectSchemeFieldDTO.class)));
//        }
//
//        List<ObjectSchemeFieldDTO> userDefinedFieldDTOS = fieldDTOS.stream().
//                filter(v -> !v.getSystem()).collect(Collectors.toList());
//
//        if (exportFieldCodes != null && exportFieldCodes.size() != 0) {
//            Map<String, String> data = new HashMap<>(fields.length + userDefinedFieldDTOS.size());
//            for (int i = 0; i < fields.length; i++) {
//                data.put(fields[i], fieldsName[i]);
//            }
//            for (ObjectSchemeFieldDTO userDefinedFieldDTO : userDefinedFieldDTOS) {
//                data.put(userDefinedFieldDTO.getCode(), userDefinedFieldDTO.getName());
//            }
//
//            List<String> fieldCodes = new ArrayList<>(exportFieldCodes.size());
//            List<String> fieldNames = new ArrayList<>(exportFieldCodes.size());
//            exportFieldCodes.forEach(code -> {
//                String name = data.get(code);
//                if (name != null) {
//                    fieldCodes.add(code);
//                    fieldNames.add(name);
//                } else {
//                    throw new CommonException("error.issue.illegal.exportField", code);
//                }
//            });
//            fieldMap.put(FIELD_CODES, fieldCodes.stream().toArray(String[]::new));
//            fieldMap.put(FIELD_NAMES, fieldNames.stream().toArray(String[]::new));
//        } else {
//            if (!userDefinedFieldDTOS.isEmpty()) {
//                List<String> fieldCodes = new ArrayList(Arrays.asList(fields));
//                List<String> fieldNames = new ArrayList(Arrays.asList(fieldsName));
//                userDefinedFieldDTOS.forEach(fieldDTO -> {
//                    fieldCodes.add(fieldDTO.getCode());
//                    fieldNames.add(fieldDTO.getName());
//                });
//
//                fieldMap.put(FIELD_CODES, fieldCodes.stream().toArray(String[]::new));
//                fieldMap.put(FIELD_NAMES, fieldNames.stream().toArray(String[]::new));
//            } else {
//                fieldMap.put(FIELD_CODES, fields);
//                fieldMap.put(FIELD_NAMES, fieldsName);
//            }
//        }
//        return fieldMap;
//    }

    @Override
    public IssueVO cloneIssueByIssueId(Long projectId, Long issueId, CopyConditionVO copyConditionVO, Long organizationId, String applyType) {
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
            throw new CommonException("error.applyType.illegal");
        }
        IssueDetailDTO issueDetailDTO = issueMapper.queryIssueDetail(projectId, issueId);
        if (issueDetailDTO != null) {
            Long newIssueId;
            Long objectVersionNumber;
            issueDetailDTO.setSummary(copyConditionVO.getSummary());
            IssueTypeVO issueTypeVO = issueTypeService.queryById(issueDetailDTO.getIssueTypeId(), projectId);
            if (issueTypeVO.getTypeCode().equals(SUB_TASK)) {
                IssueSubCreateVO issueSubCreateVO = issueAssembler.issueDtoToIssueSubCreateDto(issueDetailDTO);
                IssueSubVO newIssue = stateMachineClientService.createSubIssue(issueSubCreateVO);
                newIssueId = newIssue.getIssueId();
                objectVersionNumber = newIssue.getObjectVersionNumber();
            } else {
                IssueCreateVO issueCreateVO = issueAssembler.issueDtoToIssueCreateDto(issueDetailDTO);
                if (ISSUE_EPIC.equals(issueCreateVO.getTypeCode())) {
                    setEpicName(projectId, copyConditionVO, issueCreateVO);
                }
                if (agilePluginService != null) {
                    agilePluginService.handlerCloneFeature(issueId,issueCreateVO, applyType, projectId);
                }
                IssueVO newIssue = stateMachineClientService.createIssue(issueCreateVO, applyType);
                newIssueId = newIssue.getIssueId();
                objectVersionNumber = newIssue.getObjectVersionNumber();
            }
            //复制链接
            batchCreateCopyIssueLink(copyConditionVO.getIssueLink(), issueId, newIssueId, projectId);
            // 复制项目群的特性和史诗都不会去创建关联关系
            if (!(applyType.equals("program") && (issueDetailDTO.getTypeCode().equals("issue_epic") || issueDetailDTO.getTypeCode().equals("feature")))) {
                //生成一条复制的关联
                createCopyIssueLink(issueDetailDTO.getIssueId(), newIssueId, projectId);
            }
            //复制故事点和剩余工作量并记录日志
            copyStoryPointAndRemainingTimeData(issueDetailDTO, projectId, newIssueId, objectVersionNumber);
            //复制冲刺
            handleCreateCopyIssueSprintRel(copyConditionVO.getSprintValues(), issueDetailDTO, newIssueId);
            if (copyConditionVO.getSubTask()) {
                List<IssueDTO> subIssueDTOList = issueDetailDTO.getSubIssueDTOList();
                if (subIssueDTOList != null && !subIssueDTOList.isEmpty()) {
                    subIssueDTOList.forEach(issueDO -> copySubIssue(issueDO, newIssueId, projectId,copyConditionVO));
                }
            }
            if (copyConditionVO.getCustomField()) {
                // 复制自定义字段的值
                fieldValueService.copyCustomFieldValue(projectId, issueDetailDTO, newIssueId);
            }
            return queryIssue(projectId, newIssueId, organizationId);
        } else {
            throw new CommonException("error.issue.copyIssueByIssueId");
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
        if (issueDetailDTO.getStoryPoints() == null && issueDetailDTO.getEstimateTime() == null) {
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
        if (Boolean.TRUE.equals(copyConditionVO.getCustomField())) {
            fieldValueService.copyCustomFieldValue(projectId, subIssueDetailDTO, newSubIssue.getIssueId());
        }
    }

    protected void handleCreateCopyIssueSprintRel(Boolean sprintValues, IssueDetailDTO issueDetailDTO, Long newIssueId) {
        if (sprintValues && issueDetailDTO.getActiveSprint() != null) {
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
                if (subIssueIds != null && !subIssueIds.isEmpty()) {
                    throw new CommonException("error.transformedSubTask.issueHaveSubIssue");
                }
                issueConvertDTO.setEpicSequence(null);
                issueConvertDTO.setStoryPoints(null);
                issueConvertDTO.setStatusId(issueTransformSubTask.getStatusId());
                issueConvertDTO.setTypeCode(SUB_TASK);
                issueConvertDTO.setIssueTypeId(issueTransformSubTask.getIssueTypeId());
                issueConvertDTO.setParentIssueId(issueTransformSubTask.getParentIssueId());
                List<String> fieldList = new ArrayList<>();
                List<String> list = Arrays.asList(TYPE_CODE_FIELD, ISSUE_TYPE_ID, RANK_FIELD, STATUS_ID, PARENT_ISSUE_ID, EPIC_SEQUENCE, STORY_POINTS_FIELD);
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
        if (originType.equals(SUB_TASK)) {
            issueConvertDTO.setParentIssueId(null);
        }
        if (STORY_TYPE.equals(issueConvertDTO.getTypeCode()) && issueConvertDTO.getStoryPoints() != null) {
            issueConvertDTO.setStoryPoints(null);
        }
        if (issueTransformTask.getTypeCode().equals(ISSUE_EPIC)) {
            issueConvertDTO.setRank(null);
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
            calculationRank(issueConvertDTO.getProjectId(), issueConvertDTO);
        } else {
            issueConvertDTO.setTypeCode(issueTransformTask.getTypeCode());
        }
        if (issueTransformTask.getStatusId() != null) {
            issueConvertDTO.setStatusId(issueTransformTask.getStatusId());
        }
        issueConvertDTO.setIssueTypeId(issueTransformTask.getIssueTypeId());
        issueAccessDataService.update(issueConvertDTO, new String[]{TYPE_CODE_FIELD, REMAIN_TIME_FIELD, PARENT_ISSUE_ID, EPIC_NAME_FIELD, COLOR_CODE_FIELD, EPIC_ID_FIELD, STORY_POINTS_FIELD, RANK_FIELD, EPIC_SEQUENCE, ISSUE_TYPE_ID, STATUS_ID});
        return queryIssue(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId(), organizationId);
    }

//    protected String exportIssuesVersionName(ExportIssuesVO exportIssuesVO) {
//        StringBuilder versionName = new StringBuilder();
//        if (exportIssuesVO.getFixVersionName() != null && !"".equals(exportIssuesVO.getFixVersionName())) {
//            versionName.append("修复的版本:").append(exportIssuesVO.getFixVersionName()).append("\r\n");
//        } else if (exportIssuesVO.getInfluenceVersionName() != null && !"".equals(exportIssuesVO.getInfluenceVersionName())) {
//            versionName.append("影响的版本:").append(exportIssuesVO.getInfluenceVersionName());
//        }
//        return versionName.toString();
//    }
//
//    protected String exportIssuesSprintName(ExportIssuesVO exportIssuesVO) {
//        StringBuilder sprintName = new StringBuilder(exportIssuesVO.getSprintName() != null ? "正在使用冲刺:" + exportIssuesVO.getSprintName() + "\r\n" : "");
//        sprintName.append(!Objects.equals(exportIssuesVO.getCloseSprintName(), "") ? "已关闭冲刺:" + exportIssuesVO.getCloseSprintName() : "");
//        return sprintName.toString();
//    }

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
        orders.put("issueNum","issue_num_convert");
        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), SEARCH, orders);
        //pageable.resetOrder(SEARCH, new HashMap<>());
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
        orders.put("issueNum","issue_num_convert");
        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), SEARCH, orders);
        //pageable.resetOrder(SEARCH, new HashMap<>());
        Page<IssueDTO> issueDOPage = PageHelper.doPageAndSort(pageRequest, () ->
                issueMapper.listIssueWithLinkedIssues(projectId, searchVO.getSearchArgs(),
                        searchVO.getAdvancedSearchArgs(), searchVO.getOtherArgs(), searchVO.getContents()));
        return handleILTDTOToILTWSVDTO(projectId, handleIssueListTestDoToDto(issueDOPage, organizationId, projectId));
    }

    private Page<IssueListTestWithSprintVersionVO> handleILTDTOToILTWSVDTO(Long projectId, Page<IssueListTestVO> issueListTestDTOSPage) {

//        Map<Long, ProductVersionDataVO> versionIssueRelDTOMap = productVersionService
//                .queryVersionByProjectId(projectId).stream().collect(
//                        Collectors.toMap(ProductVersionDataVO::getVersionId, x-> x));

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
    public Page<IssueNumVO> queryIssueByOptionForAgile(Long projectId, Long issueId, String issueNum, Boolean self, String content, PageRequest pageRequest) {
        Map<String, String> orders = new HashMap<>();
        orders.put("issueNum", "issue_num_convert");
        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), SEARCH, orders);
        pageRequest.setSort(sort);
        //pageable.resetOrder("search", new HashMap<>());
        IssueNumDTO issueNumDTO = null;
//        PageRequest pageRequest = PageRequest.of(pageable.getPageNumber(), pageable.getPageSize());
        if (self) {
            issueNumDTO = issueMapper.queryIssueByIssueNumOrIssueId(projectId, issueId, issueNum);
            if (issueNumDTO != null) {
                pageRequest = new PageRequest(pageRequest.getPage(), pageRequest.getSize() - 1);
            }
        }
        Page<IssueNumDTO> issueDOPage = PageHelper.doPageAndSort(pageRequest,
                () ->
                issueMapper.queryIssueByOptionForAgile(projectId, issueId, issueNum, self, content));
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

//    @Override
//    public List<PieChartVO> issueStatistic(Long projectId, String type, List<String> issueTypes) {
//        return reportAssembler.toTargetList(issueMapper.issueStatistic(projectId, type, issueTypes), PieChartVO.class);
//    }

//    @Override
//    public Page<IssueComponentDetailDTO> listIssueWithoutSubDetail(Long projectId, SearchVO searchVO, PageRequest pageRequest) {
//        //连表查询需要设置主表别名
//        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), SEARCH, new HashMap<>());
//        pageRequest.setSort(sort);
//        //pageable.resetOrder(SEARCH, new HashMap<>());
//        Page<Long> issueIds = PageHelper.doPageAndSort(pageRequest,
//                () -> issueMapper.listIssueIdsWithoutSubDetail(projectId, searchVO.getSearchArgs(),
//                searchVO.getAdvancedSearchArgs(), searchVO.getOtherArgs(), searchVO.getContents()));
//        List<IssueComponentDetailInfoDTO> issueComponentDetailInfoDTOS = new ArrayList<>(issueIds.getContent().size());
//        if (issueIds.getContent() != null && !issueIds.getContent().isEmpty()) {
//            issueComponentDetailInfoDTOS.addAll(issueMapper.listIssueWithoutSubDetailByIssueIds(issueIds.getContent()));
//        }
//        return PageUtil.buildPageInfoWithPageInfoList(issueIds, issueAssembler.issueComponentDetailDoToDto(projectId, issueComponentDetailInfoDTOS));
//    }


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

//    @Override
//    public synchronized List<Long> cloneIssuesByVersionId(Long projectId, Long versionId, List<Long> issueIds) {
//        List<IssueDetailDTO> issueDOList = issueMapper.queryByIssueIds(projectId, issueIds);
//        if (issueDOList.size() == issueIds.size()) {
//            return batchCreateIssue(issueDOList, projectId, versionId);
//        } else {
//            throw new CommonException("error.issueServiceImpl.issueTypeError");
//        }
//    }
//
//    private List<Long> batchCreateIssue(List<IssueDetailDTO> issueDOList, Long projectId, Long versionId) {
//        List<Long> issueIds = new ArrayList<>(issueDOList.size());
//        //获取issueTypeId
//        Long issueTypeId = issueDOList.get(0).getIssueTypeId();
//        //获取状态机id
//        Long organizationId = ConvertUtil.getOrganizationId(projectId);
//        Long stateMachineId = projectConfigService.queryStateMachineId(projectId, SchemeApplyType.TEST, issueTypeId);
//        if (stateMachineId == null) {
//            throw new CommonException(ERROR_ISSUE_STATE_MACHINE_NOT_FOUND);
//        }
//        //获取初始状态
//        Long initStatusId = instanceService.queryInitStatusId(organizationId, stateMachineId);
//
//        ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
//        projectInfoDTO.setProjectId(projectId);
//        ProjectInfoDTO projectInfo = modelMapper.map(projectInfoMapper.selectOne(projectInfoDTO), ProjectInfoDTO.class);
//        if (projectInfo == null) {
//            throw new CommonException(ERROR_PROJECT_INFO_NOT_FOUND);
//        }
//        issueDOList.forEach(issueDetailDTO -> {
//            IssueConvertDTO issueConvertDTO = issueAssembler.toTarget(issueDetailDTO, IssueConvertDTO.class);
//            //初始化创建issue设置issue编号、项目默认设置
//            issueConvertDTO.initializationIssueByCopy(initStatusId);
//            projectInfoService.updateIssueMaxNum(projectId, issueConvertDTO.getIssueNum());
//            issueConvertDTO.setApplyType(SchemeApplyType.TEST);
//            Long issueId = issueAccessDataService.create(issueConvertDTO).getIssueId();
//            handleCreateCopyLabelIssueRel(issueDetailDTO.getLabelIssueRelDTOList(), issueId);
//            handleCreateCopyComponentIssueRel(issueDetailDTO.getComponentIssueRelDTOList(), issueId);
//            issueIds.add(issueId);
//        });
//        VersionIssueRelDTO versionIssueRelDTO = new VersionIssueRelDTO();
//        versionIssueRelDTO.createBatchIssueToVersionDTO(projectId, versionId, issueIds);
//        issueAccessDataService.batchIssueToVersion(versionIssueRelDTO);
//        return issueIds;
//    }

    private void handleCreateCopyComponentIssueRel(List<ComponentIssueRelDTO> componentIssueRelDTOList, Long issueId) {
        componentIssueRelDTOList.forEach(componentIssueRelDO -> {
            ComponentIssueRelDTO componentIssueRelDTO = new ComponentIssueRelDTO();
            BeanUtils.copyProperties(componentIssueRelDO, componentIssueRelDTO);
            componentIssueRelDTO.setIssueId(issueId);
            componentIssueRelDTO.setObjectVersionNumber(null);
            if (issueValidator.existComponentIssueRel(componentIssueRelDTO)) {
                componentIssueRelService.create(componentIssueRelDTO);
            }
        });
    }

    private void handleCreateCopyLabelIssueRel(List<LabelIssueRelDTO> labelIssueRelDTOList, Long issueId) {
        labelIssueRelDTOList.forEach(labelIssueRel -> {
            LabelIssueRelDTO labelIssueRelDTO = new LabelIssueRelDTO();
            BeanUtils.copyProperties(labelIssueRel, labelIssueRelDTO);
            labelIssueRelDTO.setIssueId(issueId);
            labelIssueRelDTO.setObjectVersionNumber(null);
            labelIssueRelService.create(labelIssueRelDTO);
        });
    }

//    public String getDes(String str) {
//        StringBuilder result = new StringBuilder();
//        if (!"".equals(str) && str != null) {
//            String[] arrayLine = str.split(("\\},\\{"));
//            String regEx = "\"insert\":\"(.*)\"";
//            Pattern pattern = Pattern.compile(regEx);
//            for (String s : arrayLine) {
//                Matcher matcher = pattern.matcher(s);
//                if (matcher.find()) {
//                    result.append(StringEscapeUtils.unescapeJava(matcher.group(1)));
//                }
//            }
//        }
//        return result.toString();
//    }

//    @Override
//    public List<IssueProjectVO> queryIssueTestGroupByProject() {
//        return issueAssembler.toTargetList(issueMapper.queryIssueTestGroupByProject(), IssueProjectVO.class);
//    }

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
        orders.put("issueNum","issue_num_convert");
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
        if (Objects.equals(searchType, "myStarBeacon")) {
            allIssue = issueMapper.listMyStarIssuesByProjectIdsAndUserId(projectIds, parentIssues, userId);
        } else {
            allIssue = issueMapper.listIssuesByParentIssueIdsAndUserId(projectIds,parentIssues, userId, searchType);
        }
        Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
        Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMapByProjectIds(organizationId, projectIds);
        Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
        Map<Long, ProjectVO> projectVOMap = projects.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        List<IssueListFieldKVVO> list = new ArrayList<>();
        Set<Long> userIds = new HashSet<>();
        allIssue.forEach(v -> {
            IssueListFieldKVVO issueListFieldKVVO = new IssueListFieldKVVO();
            modelMapper.map(v,issueListFieldKVVO);
            issueListFieldKVVO.setIssueTypeVO(issueTypeDTOMap.get(v.getIssueTypeId()));
            issueListFieldKVVO.setStatusVO(statusMapDTOMap.get(v.getStatusId()));
            issueListFieldKVVO.setPriorityVO(priorityMap.get(v.getPriorityId()));
            issueListFieldKVVO.setProjectVO(projectVOMap.get(v.getProjectId()));
            // 设置父级issueId
            Long parentId = null;
            Long parentIssueId = v.getParentIssueId();
            Long relateIssueId = v.getRelateIssueId();
            if (!ObjectUtils.isEmpty(parentIssueId) && parentIssueId != 0) {
                parentId = parentIssueId;
            }
            if (!ObjectUtils.isEmpty(relateIssueId) && relateIssueId != 0) {
                parentId = relateIssueId;
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
        if (Objects.equals(searchType, "myStarBeacon")) {
            setListStarBeacon(list, userId, projectIds);
        }
        if (!userIds.isEmpty()) {
            setAssignee(list, userIds);
        }
        PageInfo pageInfo = new PageInfo(pageRequest.getPage(), pageRequest.getSize());
        return new Page<>(list, pageInfo, parentPage.getTotalElements());
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
                        .filter(v -> ((!Objects.isNull(type) && Objects.equals(type, "myStarBeacon"))
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
            Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId,"agile");
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
