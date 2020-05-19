package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.core.domain.Page;
import io.choerodon.core.domain.PageInfo;
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
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.TestFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.statemachineclient.dto.InputDTO;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.asgard.saga.dto.StartInstanceDTO;
import io.choerodon.asgard.saga.feign.SagaClient;
import io.choerodon.core.utils.PageableHelper;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import org.apache.commons.lang.StringEscapeUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.math.BigDecimal;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * 敏捷开发Issue
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 20:30:48
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueServiceImpl implements IssueService {

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
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private StatusService statusService;
    @Autowired
    private InstanceService instanceService;
    @Autowired
    private TestFeignClient testFeignClient;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private ProjectUtil projectUtil;

    private static final String SUB_TASK = "sub_task";
    private static final String ISSUE_EPIC = "issue_epic";
    private static final String COPY = "Copy";
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
    private static final String FIX_RELATION_TYPE = "fix";
    private static final String INFLUENCE_RELATION_TYPE = "influence";
    private static final String PROJECT_ERROR = "error.project.notFound";
    private static final String ERROR_ISSUE_NOT_FOUND = "error.Issue.queryIssue";
    private static final String ERROR_PROJECT_INFO_NOT_FOUND = "error.createIssue.projectInfoNotFound";
    private static final String ERROR_ISSUE_STATE_MACHINE_NOT_FOUND = "error.createIssue.stateMachineNotFound";
    private static final String SEARCH = "search";
    private static final String STORYMAP = "storymap";
    private static final String AGILE = "agile";
    private static final String FIELD_CODES = "fieldCodes";
    private static final String FIELD_NAMES = "fieldNames";
    private static final String BACKETNAME = "agile-service";

    private ModelMapper modelMapper = new ModelMapper();

    private static final String[] FIELDS_NAME;

    private static final String[] FIELDS;

    protected static Map<String, String> FIELD_MAP = new LinkedHashMap<>();

    protected static String[] AUTO_SIZE_WIDTH = {"summary", "epicName", "feature",
            "creationDate", "lastUpdateDate", "sprintName"};

    static {
        FIELD_MAP.put("typeName", "问题类型");
        FIELD_MAP.put("issueNum", "问题编号");
        FIELD_MAP.put("summary", "概要");
        FIELD_MAP.put("description", "描述");
        FIELD_MAP.put("priorityName", "优先级");
        FIELD_MAP.put("statusName", "状态");
        FIELD_MAP.put("resolution", "解决状态");
        FIELD_MAP.put("sprintName", "冲刺");
        FIELD_MAP.put("assigneeName", "经办人");
        FIELD_MAP.put("reporterName", "报告人");
        FIELD_MAP.put("storyPoints", "故事点");
        FIELD_MAP.put("remainingTime", "剩余预估时间");
        FIELD_MAP.put("versionName", "版本");
        FIELD_MAP.put("epicName", "所属史诗");
        FIELD_MAP.put("labelName", "标签");
        FIELD_MAP.put("componentName", "模块");
        FIELD_MAP.put("creationDate", "创建时间");
        FIELD_MAP.put("lastUpdateDate", "最后更新时间");
        FIELDS = new ArrayList<>(FIELD_MAP.keySet()).toArray(new String[FIELD_MAP.keySet().size()]);
        FIELDS_NAME = new ArrayList<>(FIELD_MAP.values()).toArray(new String[FIELD_MAP.values().size()]);
    }


    @PostConstruct
    public void init() {
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
    }

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

    @Override
    public void afterCreateIssue(Long issueId, IssueConvertDTO issueConvertDTO, IssueCreateVO issueCreateVO, ProjectInfoDTO projectInfoDTO) {
        handleCreateIssueRearAction(issueConvertDTO, issueId, projectInfoDTO, issueCreateVO.getLabelIssueRelVOList(), issueCreateVO.getComponentIssueRelVOList(), issueCreateVO.getVersionIssueRelVOList(), issueCreateVO.getIssueLinkCreateVOList());
    }

    private void handleCreateIssueRearAction(IssueConvertDTO issueConvertDTO, Long issueId, ProjectInfoDTO projectInfoDTO, List<LabelIssueRelVO> labelIssueRelVOList, List<ComponentIssueRelVO> componentIssueRelVOList, List<VersionIssueRelVO> versionIssueRelVOList, List<IssueLinkCreateVO> issueLinkCreateVOList) {
        //处理冲刺
        handleCreateSprintRel(issueConvertDTO.getSprintId(), issueConvertDTO.getProjectId(), issueId);
        handleCreateLabelIssue(labelIssueRelVOList, issueId);
        handleCreateComponentIssueRel(componentIssueRelVOList, projectInfoDTO.getProjectId(), issueId, projectInfoDTO, issueConvertDTO.getAssigneerCondtiion());
        handleCreateVersionIssueRel(versionIssueRelVOList, projectInfoDTO.getProjectId(), issueId);
        handleCreateIssueLink(issueLinkCreateVOList, projectInfoDTO.getProjectId(), issueId);
    }

    @Override
    public void afterCreateSubIssue(Long issueId, IssueConvertDTO subIssueConvertDTO, IssueSubCreateVO issueSubCreateVO, ProjectInfoDTO projectInfoDTO) {
        handleCreateIssueRearAction(subIssueConvertDTO, issueId, projectInfoDTO, issueSubCreateVO.getLabelIssueRelVOList(), issueSubCreateVO.getComponentIssueRelVOList(), issueSubCreateVO.getVersionIssueRelVOList(), issueSubCreateVO.getIssueLinkCreateVOList());
    }

    @Override
    public void handleInitIssue(IssueConvertDTO issueConvertDTO, Long statusId, ProjectInfoDTO projectInfoDTO) {
        //如果是epic，初始化颜色
        if (ISSUE_EPIC.equals(issueConvertDTO.getTypeCode())) {
            List<LookupValueDTO> colorList = lookupValueMapper.queryLookupValueByCode(EPIC_COLOR_TYPE).getLookupValues();
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
    public IssueVO queryIssueCreate(Long projectId, Long issueId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(attachmentUrl + "/" + BACKETNAME + "/" + issueAttachmentDO.getUrl()));
        }
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, issue.getApplyType());
        Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
        Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
        IssueVO result = issueAssembler.issueDetailDTOToVO(issue, issueTypeDTOMap, statusMapDTOMap, priorityDTOMap);
        sendMsgUtil.sendMsgByIssueCreate(projectId, result);
        return result;
    }

    @Override
    public IssueVO queryIssue(Long projectId, Long issueId, Long organizationId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(attachmentUrl + "/" + BACKETNAME + "/" + issueAttachmentDO.getUrl()));
        }
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, issue.getApplyType());
        Map<Long, StatusVO> statusMapDTOMap = ConvertUtil.getIssueStatusMap(projectId);
        Map<Long, PriorityVO> priorityDTOMap = ConvertUtil.getIssuePriorityMap(projectId);
        return issueAssembler.issueDetailDTOToVO(issue, issueTypeDTOMap, statusMapDTOMap, priorityDTOMap);
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
        sendMsgUtil.sendMsgByIssueAssignee(projectId, fieldList, result);
        sendMsgUtil.sendMsgByIssueComplete(projectId, fieldList, result);
        return result;
    }

    @Override
    public Page<IssueListFieldKVVO> listIssueWithSub(Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId) {
        if (organizationId == null) {
            organizationId = ConvertUtil.getOrganizationId(projectId);
        }
        //处理用户搜索
        Boolean condition = handleSearchUser(searchVO, projectId);
        if (condition) {
            Page<Long> issueIdPage;
            String filterSql = null;
            //处理自定义搜索
            if (searchVO.getQuickFilterIds() != null && !searchVO.getQuickFilterIds().isEmpty()) {
                filterSql = getQuickFilter(searchVO.getQuickFilterIds());
            }
            //处理未匹配的筛选
            handleOtherArgs(searchVO);
            final String searchSql = filterSql;
            if (!handleSortField(pageRequest).equals("")) {
                String fieldCode = handleSortField(pageRequest);
                Map<String, String> order = new HashMap<>(1);
                String sortCode = fieldCode.split("\\.")[1];
                order.put(fieldCode, sortCode);
                PageUtil.sortResetOrder(pageRequest.getSort(), null, order);
                List<Long> issueIdsWithSub =
                        issueMapper.queryIssueIdsListWithSub(projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds(), null)
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
                Page<IssueDTO> issues = PageHelper.doPage(pageRequest, () -> issueMapper.queryIssueIdsListWithSub(projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds(), orderStr));
                List<Long> issueIds = issues.getContent().stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
                issueIdPage = PageUtil.buildPageInfoWithPageInfoList(issues, issueIds);
            }

            Page<IssueListFieldKVVO> issueListDTOPage;
            if (issueIdPage.getContent() != null && !issueIdPage.getContent().isEmpty()) {
                List<Long> issueIds = issueIdPage.getContent();
                Set<Long> childrenIds = issueMapper.queryChildrenIdByParentId(issueIds, projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds());
                List<IssueDTO> issueDTOList = issueMapper.queryIssueListWithSubByIssueIds(issueIds, childrenIds, false);
                Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
                Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId);
                Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
                List<Long> allIssueIds = issueDTOList.stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
                Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, projectId, allIssueIds, false);
                issueListDTOPage = PageUtil.buildPageInfoWithPageInfoList(issueIdPage,
                        issueAssembler.issueDoToIssueListFieldKVDTO(issueDTOList, priorityMap, statusMapDTOMap, issueTypeDTOMap, foundationCodeValue));
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
        if (fieldList.contains("epicName")
                && issueUpdateVO.getEpicName() != null
                && checkEpicName(projectId, issueUpdateVO.getEpicName(), issueUpdateVO.getIssueId())) {
            throw new CommonException("error.epicName.exist");
        }
        if (!fieldList.isEmpty()) {
            //处理issue自己字段
            handleUpdateIssue(issueUpdateVO, fieldList, projectId);
        }
        Long issueId = issueUpdateVO.getIssueId();
        handleUpdateLabelIssue(issueUpdateVO.getLabelIssueRelVOList(), issueId, projectId);
        handleUpdateComponentIssueRel(issueUpdateVO.getComponentIssueRelVOList(), projectId, issueId);
        handleUpdateVersionIssueRel(issueUpdateVO.getVersionIssueRelVOList(), projectId, issueId, issueUpdateVO.getVersionType());
        return queryIssueByUpdate(projectId, issueId, fieldList);
    }

    @Override
    public IssueVO updateIssueStatus(Long projectId, Long issueId, Long transformId, Long objectVersionNumber, String applyType) {
        stateMachineClientService.executeTransform(projectId, issueId, transformId, objectVersionNumber, applyType, new InputDTO(issueId, "updateStatus", null));
        if ("agile".equals(applyType)) {
            IssueConvertDTO issueConvertDTO = new IssueConvertDTO();
            issueConvertDTO.setIssueId(issueId);
            issueConvertDTO.setStayDate(new Date());
            issueConvertDTO.setObjectVersionNumber(issueMapper.selectByPrimaryKey(issueId).getObjectVersionNumber());
            issueAccessDataService.updateSelective(issueConvertDTO);
        }
        return queryIssueByUpdate(projectId, issueId, Collections.singletonList("statusId"));
    }

    @Override
    public void handleUpdateIssue(IssueUpdateVO issueUpdateVO, List<String> fieldList, Long projectId) {
        CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
        IssueDTO originIssue = issueMapper.queryIssueWithNoCloseSprint(issueUpdateVO.getIssueId());
        IssueConvertDTO issueConvertDTO = issueAssembler.toTarget(issueUpdateVO, IssueConvertDTO.class);
        //处理用户，前端可能会传0，处理为null
        issueConvertDTO.initializationIssueUser();
        if (fieldList.contains(SPRINT_ID_FIELD)) {
            IssueConvertDTO oldIssue = modelMapper.map(originIssue, IssueConvertDTO.class);
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
        //删除日志信息
        dataLogDeleteByIssueId(projectId, issueId);
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
        testFeignClient.deleteTestRel(projectId, issueId);
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
            issueSprintRelDTO.setProjectId(projectId);
            issueSprintRelService.createIssueSprintRel(issueSprintRelDTO);
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
        return issueAssembler.toTargetList(issueMapper.queryIssueEpicSelectList(projectId), IssueEpicVO.class);
    }


    @Override
    public IssueSubVO queryIssueSub(Long projectId, Long organizationId, Long issueId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        issue.setPriorityVO(priorityService.queryById(organizationId, issue.getPriorityId()));
        issue.setIssueTypeVO(issueTypeService.queryById(organizationId, issue.getIssueTypeId()));
        issue.setStatusVO(statusService.queryStatusById(organizationId, issue.getStatusId()));
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(attachmentUrl + issueAttachmentDO.getUrl()));
        }
        return issueAssembler.issueDetailDoToIssueSubDto(issue);
    }

    @Override
    public IssueSubVO queryIssueSubByCreate(Long projectId, Long issueId) {
        IssueDetailDTO issue = issueMapper.queryIssueDetail(projectId, issueId);
        if (issue.getIssueAttachmentDTOList() != null && !issue.getIssueAttachmentDTOList().isEmpty()) {
            issue.getIssueAttachmentDTOList().forEach(issueAttachmentDO -> issueAttachmentDO.setUrl(attachmentUrl + issueAttachmentDO.getUrl()));
        }
        IssueSubVO result = issueAssembler.issueDetailDoToIssueSubDto(issue);
        sendMsgUtil.sendMsgBySubIssueCreate(projectId, result);
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
        return queryIssue(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId(), organizationId);
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

    protected void handleUpdateLabelIssue(List<LabelIssueRelVO> labelIssueRelVOList, Long issueId, Long projectId) {
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

    protected void handleUpdateVersionIssueRel(List<VersionIssueRelVO> versionIssueRelVOList, Long projectId, Long issueId, String versionType) {
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

    protected void handleUpdateComponentIssueRel(List<ComponentIssueRelVO> componentIssueRelVOList, Long projectId, Long issueId) {
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
        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), "ai", new HashMap<>());
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

    @Override
    public void exportIssues(Long projectId, SearchVO searchVO, HttpServletRequest request,
                             HttpServletResponse response, Long organizationId, Sort sort) {
        //处理根据界面筛选结果导出的字段
        Map<String, String[]> fieldMap =
                handleExportFields(searchVO.getExportFieldCodes(), projectId, organizationId, FIELDS_NAME, FIELDS);
        String[] fieldCodes = sortFieldCodes(fieldMap.get(FIELD_CODES));
        String[] fieldNames = sortFieldNames(fieldMap.get(FIELD_NAMES));
        ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
        projectInfoDTO.setProjectId(projectId);
        projectInfoDTO = projectInfoMapper.selectOne(projectInfoDTO);
        ProjectVO project = userService.queryProject(projectId);
        if (project == null) {
            throw new CommonException(PROJECT_ERROR);
        }
        project.setCode(projectInfoDTO.getProjectCode());
        Boolean condition = handleSearchUser(searchVO, projectId);
        if (condition) {
            String filterSql = null;
            if (searchVO.getQuickFilterIds() != null && !searchVO.getQuickFilterIds().isEmpty()) {
                filterSql = getQuickFilter(searchVO.getQuickFilterIds());
            }
            final String searchSql = filterSql;
            //查询所有父节点问题
            String orderStr = getOrderStrOfQueryingIssuesWithSub(sort);
            List<Long> parentIds =
                    issueMapper.queryIssueIdsListWithSub(projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds(), orderStr)
                            .stream().map(IssueDTO::getIssueId).collect(Collectors.toList());
            List<Long> issueIds = new ArrayList<>();
            Map<Long, Set<Long>> parentSonMap = new HashMap<>();
            List<IssueDTO> issues = null;
            if (!parentIds.isEmpty()) {
                Set<Long> childrenIds = issueMapper.queryChildrenIdByParentId(parentIds, projectId, searchVO, searchSql, searchVO.getAssigneeFilterIds());
                issues = issueMapper.queryIssueListWithSubByIssueIds(parentIds, childrenIds, true);
                issues.forEach(i -> {
                    issueIds.add(i.getIssueId());
                    processParentSonRelation(parentSonMap, i);
                });
            }
            Map<Long, ExportIssuesVO> issueMap = new LinkedHashMap<>();
            if (!issueIds.isEmpty()) {
                List<ExportIssuesVO> exportIssues = issueAssembler.exportIssuesDOListToExportIssuesDTO(issues, projectId);
                Map<Long, List<SprintNameDTO>> closeSprintNames = issueMapper.querySprintNameByIssueIds(projectId, issueIds).stream().collect(Collectors.groupingBy(SprintNameDTO::getIssueId));
                Map<Long, List<VersionIssueRelDTO>> fixVersionNames = issueMapper.queryVersionNameByIssueIds(projectId, issueIds, FIX_RELATION_TYPE).stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
                Map<Long, List<VersionIssueRelDTO>> influenceVersionNames = issueMapper.queryVersionNameByIssueIds(projectId, issueIds, INFLUENCE_RELATION_TYPE).stream().collect(Collectors.groupingBy(VersionIssueRelDTO::getIssueId));
                Map<Long, List<LabelIssueRelDTO>> labelNames = issueMapper.queryLabelIssueByIssueIds(projectId, issueIds).stream().collect(Collectors.groupingBy(LabelIssueRelDTO::getIssueId));
                Map<Long, List<ComponentIssueRelDTO>> componentMap = issueMapper.queryComponentIssueByIssueIds(projectId, issueIds).stream().collect(Collectors.groupingBy(ComponentIssueRelDTO::getIssueId));
                Map<Long, Map<String, Object>> foundationCodeValue = pageFieldService.queryFieldValueWithIssueIdsForAgileExport(organizationId, projectId, issueIds, true);
                exportIssues.forEach(exportIssue -> {
                    String closeSprintName = closeSprintNames.get(exportIssue.getIssueId()) != null ? closeSprintNames.get(exportIssue.getIssueId()).stream().map(SprintNameDTO::getSprintName).collect(Collectors.joining(",")) : "";
                    String fixVersionName = fixVersionNames.get(exportIssue.getIssueId()) != null ? fixVersionNames.get(exportIssue.getIssueId()).stream().map(VersionIssueRelDTO::getName).collect(Collectors.joining(",")) : "";
                    String influenceVersionName = influenceVersionNames.get(exportIssue.getIssueId()) != null ? influenceVersionNames.get(exportIssue.getIssueId()).stream().map(VersionIssueRelDTO::getName).collect(Collectors.joining(",")) : "";
                    String labelName = labelNames.get(exportIssue.getIssueId()) != null ? labelNames.get(exportIssue.getIssueId()).stream().map(LabelIssueRelDTO::getLabelName).collect(Collectors.joining(",")) : "";
                    String componentName = componentMap.get(exportIssue.getIssueId()) != null ? componentMap.get(exportIssue.getIssueId()).stream().map(ComponentIssueRelDTO::getName).collect(Collectors.joining(",")) : "";
                    Map<String, Object> fieldValue = foundationCodeValue.get(exportIssue.getIssueId()) != null ? foundationCodeValue.get(exportIssue.getIssueId()) : new HashMap<>();
                    exportIssue.setCloseSprintName(closeSprintName);
                    exportIssue.setProjectName(project.getName());
                    exportIssue.setSprintName(exportIssuesSprintName(exportIssue));
                    exportIssue.setFixVersionName(fixVersionName);
                    exportIssue.setInfluenceVersionName(influenceVersionName);
                    exportIssue.setVersionName(exportIssuesVersionName(exportIssue));
                    exportIssue.setDescription(getDes(exportIssue.getDescription()));
                    exportIssue.setLabelName(labelName);
                    exportIssue.setComponentName(componentName);
                    exportIssue.setFoundationFieldValue(fieldValue);
                    issueMap.put(exportIssue.getIssueId(), exportIssue);
                });
            }
            ExcelUtil.export(issueMap, parentSonMap, ExportIssuesVO.class, fieldNames, fieldCodes, project.getName(), Arrays.asList(AUTO_SIZE_WIDTH), response);
        } else {
            ExcelUtil.export(Collections.emptyMap(), Collections.emptyMap(), ExportIssuesVO.class, fieldNames, fieldCodes, project.getName(), Arrays.asList(AUTO_SIZE_WIDTH), response);
        }
    }

    protected String[] sortFieldNames(String[] fieldNames) {
        List<String> result = new ArrayList<>();
        result.add("问题类型");
        result.add("问题编号");
        result.add("概要");
        for (String str : fieldNames) {
            if (result.get(0).equals(str)
                    || result.get(1).equals(str)
                    || result.get(2).equals(str)) {
                continue;
            }
            result.add(str);
        }
        return result.toArray(new String[result.size()]);
    }

    protected String[] sortFieldCodes(String[] fieldCodes) {
        List<String> result = new ArrayList<>();
        result.add("typeName");
        result.add("issueNum");
        result.add("summary");
        for (String str : fieldCodes) {
            if (result.get(0).equals(str)
                    || result.get(1).equals(str)
                    || result.get(2).equals(str)) {
                continue;
            }
            result.add(str);
        }
        return result.toArray(new String[result.size()]);
    }

    protected void processParentSonRelation(Map<Long, Set<Long>> parentSonMap, IssueDTO issue) {
        String typeCode = issue.getTypeCode();
        Long issueId = issue.getIssueId();
        if (IssueTypeCode.isBug(typeCode)) {
            Long relateIssueId = issue.getRelateIssueId();
            if (!ObjectUtils.isEmpty(relateIssueId) && !Objects.equals(relateIssueId, 0L)) {
                appendToParentSonMap(relateIssueId, issueId, parentSonMap);
            }
        }
        if (IssueTypeCode.isSubTask(typeCode)) {
            Long parentIssueId = issue.getParentIssueId();
            if (!ObjectUtils.isEmpty(parentIssueId) && !Objects.equals(parentIssueId, 0L)) {
                appendToParentSonMap(parentIssueId, issueId, parentSonMap);
            }
        }
    }

    private void appendToParentSonMap(Long parentId, Long issueId, Map<Long, Set<Long>> parentSonMap) {
        Set<Long> childrenSet =  parentSonMap.get(parentId);
        if (childrenSet == null) {
            childrenSet = new HashSet<>();
            parentSonMap.put(parentId, childrenSet);
        }
        childrenSet.add(issueId);
    }


    /**
     * 处理根据界面筛选结果导出的字段
     *
     * @param exportFieldCodes
     * @return
     */
    protected Map<String, String[]> handleExportFields(List<String> exportFieldCodes,
                                                     Long projectId,
                                                     Long organizationId,
                                                     String[] fieldsName,
                                                     String[] fields) {
        Map<String, String[]> fieldMap = new HashMap<>(2);
        ObjectMapper m = new ObjectMapper();

        Object content = Optional.ofNullable(objectSchemeFieldService
                .listQuery(organizationId, projectId, ObjectSchemeCode.AGILE_ISSUE))
                .orElseThrow(() -> new CommonException("error.foundation.listQuery"))
                .get("content");

        List<Object> contentList = m.convertValue(content, List.class);
        List<ObjectSchemeFieldDTO> fieldDTOS = new ArrayList<>();

        if (content != null) {
            contentList.forEach(k ->
                    fieldDTOS.add(m.convertValue(k, ObjectSchemeFieldDTO.class)));
        }

        List<ObjectSchemeFieldDTO> userDefinedFieldDTOS = fieldDTOS.stream().
                filter(v -> !v.getSystem()).collect(Collectors.toList());

        if (exportFieldCodes != null && exportFieldCodes.size() != 0) {
            Map<String, String> data = new HashMap<>(fields.length + userDefinedFieldDTOS.size());
            for (int i = 0; i < fields.length; i++) {
                data.put(fields[i], fieldsName[i]);
            }
            for (ObjectSchemeFieldDTO userDefinedFieldDTO : userDefinedFieldDTOS) {
                data.put(userDefinedFieldDTO.getCode(), userDefinedFieldDTO.getName());
            }

            List<String> fieldCodes = new ArrayList<>(exportFieldCodes.size());
            List<String> fieldNames = new ArrayList<>(exportFieldCodes.size());
            exportFieldCodes.forEach(code -> {
                String name = data.get(code);
                if (name != null) {
                    fieldCodes.add(code);
                    fieldNames.add(name);
                } else {
                    throw new CommonException("error.issue.illegal.exportField", code);
                }
            });
            fieldMap.put(FIELD_CODES, fieldCodes.stream().toArray(String[]::new));
            fieldMap.put(FIELD_NAMES, fieldNames.stream().toArray(String[]::new));
        } else {
            if (!userDefinedFieldDTOS.isEmpty()) {
                List<String> fieldCodes = new ArrayList(Arrays.asList(fields));
                List<String> fieldNames = new ArrayList(Arrays.asList(fieldsName));
                userDefinedFieldDTOS.forEach(fieldDTO -> {
                    fieldCodes.add(fieldDTO.getCode());
                    fieldNames.add(fieldDTO.getName());
                });

                fieldMap.put(FIELD_CODES, fieldCodes.stream().toArray(String[]::new));
                fieldMap.put(FIELD_NAMES, fieldNames.stream().toArray(String[]::new));
            } else {
                fieldMap.put(FIELD_CODES, fields);
                fieldMap.put(FIELD_NAMES, fieldsName);
            }
        }
        return fieldMap;
    }

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
            IssueTypeVO issueTypeVO = issueTypeService.queryById(ConvertUtil.getOrganizationId(projectId), issueDetailDTO.getIssueTypeId());
            if (issueTypeVO.getTypeCode().equals(SUB_TASK)) {
                IssueSubCreateVO issueSubCreateVO = issueAssembler.issueDtoToIssueSubCreateDto(issueDetailDTO);
                IssueSubVO newIssue = stateMachineClientService.createSubIssue(issueSubCreateVO);
                newIssueId = newIssue.getIssueId();
                objectVersionNumber = newIssue.getObjectVersionNumber();
            } else {
                IssueCreateVO issueCreateVO = issueAssembler.issueDtoToIssueCreateDto(issueDetailDTO);
                issueCreateVO.setEpicName(issueCreateVO.getTypeCode().equals(ISSUE_EPIC) ? issueCreateVO.getEpicName() + COPY : null);
                IssueVO newIssue = stateMachineClientService.createIssue(issueCreateVO, applyType);
                newIssueId = newIssue.getIssueId();
                objectVersionNumber = newIssue.getObjectVersionNumber();
            }
            //复制链接
            batchCreateCopyIssueLink(copyConditionVO.getIssueLink(), issueId, newIssueId, projectId);
            //生成一条复制的关联
            createCopyIssueLink(issueDetailDTO.getIssueId(), newIssueId, projectId);
            //复制故事点和剩余工作量并记录日志
            copyStoryPointAndRemainingTimeData(issueDetailDTO, projectId, newIssueId, objectVersionNumber);
            //复制冲刺
            handleCreateCopyIssueSprintRel(copyConditionVO.getSprintValues(), issueDetailDTO, newIssueId);
            if (copyConditionVO.getSubTask()) {
                List<IssueDTO> subIssueDTOList = issueDetailDTO.getSubIssueDTOList();
                if (subIssueDTOList != null && !subIssueDTOList.isEmpty()) {
                    subIssueDTOList.forEach(issueDO -> copySubIssue(issueDO, newIssueId, projectId));
                }
            }
            return queryIssue(projectId, newIssueId, organizationId);
        } else {
            throw new CommonException("error.issue.copyIssueByIssueId");
        }
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

    protected void copySubIssue(IssueDTO issueDTO, Long newIssueId, Long projectId) {
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
                issueLinkService.deleteByIssueId(issueConvertDTO.getIssueId());
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

    protected String exportIssuesVersionName(ExportIssuesVO exportIssuesVO) {
        StringBuilder versionName = new StringBuilder();
        if (exportIssuesVO.getFixVersionName() != null && !"".equals(exportIssuesVO.getFixVersionName())) {
            versionName.append("修复的版本:").append(exportIssuesVO.getFixVersionName()).append("\r\n");
        } else if (exportIssuesVO.getInfluenceVersionName() != null && !"".equals(exportIssuesVO.getInfluenceVersionName())) {
            versionName.append("影响的版本:").append(exportIssuesVO.getInfluenceVersionName());
        }
        return versionName.toString();
    }

    protected String exportIssuesSprintName(ExportIssuesVO exportIssuesVO) {
        StringBuilder sprintName = new StringBuilder(exportIssuesVO.getSprintName() != null ? "正在使用冲刺:" + exportIssuesVO.getSprintName() + "\r\n" : "");
        sprintName.append(!Objects.equals(exportIssuesVO.getCloseSprintName(), "") ? "已关闭冲刺:" + exportIssuesVO.getCloseSprintName() : "");
        return sprintName.toString();
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

//    @Override
//    public Page<IssueListTestVO> listIssueWithoutSubToTestComponent(Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId) {
//        //连表查询需要设置主表别名
//        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), SEARCH, new HashMap<>());
//        //pageable.resetOrder(SEARCH, new HashMap<>());
//        Page<IssueDTO> issueDOPage = PageHelper.doPageAndSort(pageRequest,
//                () -> issueMapper.listIssueWithoutSubToTestComponent(projectId, searchVO.getSearchArgs(),
//                searchVO.getAdvancedSearchArgs(), searchVO.getOtherArgs(), searchVO.getContents()));
//        return handleIssueListTestDoToDto(issueDOPage, organizationId, projectId);
//    }

    private Page<IssueListTestVO> handleIssueListTestDoToDto(Page<IssueDTO> issueDOPage, Long organizationId, Long projectId) {
        Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
        Map<Long, StatusVO> statusMapDTOMap = statusService.queryAllStatusMap(organizationId);
        Map<Long, IssueTypeVO> issueTypeDTOMap = ConvertUtil.getIssueTypeMap(projectId, SchemeApplyType.TEST);
        return PageUtil.buildPageInfoWithPageInfoList(issueDOPage, issueAssembler.issueDoToIssueTestListDto(issueDOPage.getContent(), priorityMap, statusMapDTOMap, issueTypeDTOMap));
    }

//    @Override
//    public Page<IssueListTestWithSprintVersionVO> listIssueWithLinkedIssues(Long projectId, SearchVO searchVO, PageRequest pageRequest, Long organizationId) {
//        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), SEARCH, new HashMap<>());
//        //pageable.resetOrder(SEARCH, new HashMap<>());
//        Page<IssueDTO> issueDOPage = PageHelper.doPageAndSort(pageRequest, () ->
//                issueMapper.listIssueWithLinkedIssues(projectId, searchVO.getSearchArgs(),
//                        searchVO.getAdvancedSearchArgs(), searchVO.getOtherArgs(), searchVO.getContents()));
//        return handleILTDTOToILTWSVDTO(projectId, handleIssueListTestDoToDto(issueDOPage, organizationId, projectId));
//    }

    private Page<IssueListTestWithSprintVersionVO> handleILTDTOToILTWSVDTO(Long projectId, Page<IssueListTestVO> issueListTestDTOSPage) {

//        Map<Long, ProductVersionDataVO> versionIssueRelDTOMap = productVersionService
//                .queryVersionByProjectId(projectId).stream().collect(
//                        Collectors.toMap(ProductVersionDataVO::getVersionId, x-> x));

        Map<Long, SprintDTO> sprintDoMap = sprintMapper.getSprintByProjectId(projectId).stream().collect(
                Collectors.toMap(SprintDTO::getSprintId, x -> x));

        List<IssueListTestWithSprintVersionVO> issueListTestWithSprintVersionVOS = new ArrayList<>();

        for (int a = 0; a < issueListTestDTOSPage.getSize(); a++) {
            IssueListTestWithSprintVersionVO issueListTestWithSprintVersionVO = new IssueListTestWithSprintVersionVO(issueListTestDTOSPage.getContent().get(a));

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
        Sort sort = PageUtil.sortResetOrder(pageRequest.getSort(), SEARCH, new HashMap<>());
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

    protected String getQuickFilter(List<Long> quickFilterIds) {
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

    private List<Long> batchCreateIssue(List<IssueDetailDTO> issueDOList, Long projectId, Long versionId) {
        List<Long> issueIds = new ArrayList<>(issueDOList.size());
        //获取issueTypeId
        Long issueTypeId = issueDOList.get(0).getIssueTypeId();
        //获取状态机id
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Long stateMachineId = projectConfigService.queryStateMachineId(projectId, SchemeApplyType.TEST, issueTypeId);
        if (stateMachineId == null) {
            throw new CommonException(ERROR_ISSUE_STATE_MACHINE_NOT_FOUND);
        }
        //获取初始状态
        Long initStatusId = instanceService.queryInitStatusId(organizationId, stateMachineId);

        ProjectInfoDTO projectInfoDTO = new ProjectInfoDTO();
        projectInfoDTO.setProjectId(projectId);
        ProjectInfoDTO projectInfo = modelMapper.map(projectInfoMapper.selectOne(projectInfoDTO), ProjectInfoDTO.class);
        if (projectInfo == null) {
            throw new CommonException(ERROR_PROJECT_INFO_NOT_FOUND);
        }
        issueDOList.forEach(issueDetailDTO -> {
            IssueConvertDTO issueConvertDTO = issueAssembler.toTarget(issueDetailDTO, IssueConvertDTO.class);
            //初始化创建issue设置issue编号、项目默认设置
            issueConvertDTO.initializationIssueByCopy(initStatusId);
            projectInfoService.updateIssueMaxNum(projectId, issueConvertDTO.getIssueNum());
            issueConvertDTO.setApplyType(SchemeApplyType.TEST);
            Long issueId = issueAccessDataService.create(issueConvertDTO).getIssueId();
            handleCreateCopyLabelIssueRel(issueDetailDTO.getLabelIssueRelDTOList(), issueId);
            handleCreateCopyComponentIssueRel(issueDetailDTO.getComponentIssueRelDTOList(), issueId);
            issueIds.add(issueId);
        });
        VersionIssueRelDTO versionIssueRelDTO = new VersionIssueRelDTO();
        versionIssueRelDTO.createBatchIssueToVersionDTO(projectId, versionId, issueIds);
        issueAccessDataService.batchIssueToVersion(versionIssueRelDTO);
        return issueIds;
    }

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

    public String getDes(String str) {
        StringBuilder result = new StringBuilder();
        if (!"".equals(str) && str != null) {
            String[] arrayLine = str.split(("\\},\\{"));
            String regEx = "\"insert\":\"(.*)\"";
            Pattern pattern = Pattern.compile(regEx);
            for (String s : arrayLine) {
                Matcher matcher = pattern.matcher(s);
                if (matcher.find()) {
                    result.append(StringEscapeUtils.unescapeJava(matcher.group(1)));
                }
            }
        }
        return result.toString();
    }

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


//    @Override
//    public IssueNumDTO queryIssueByIssueNum(Long projectId, String issueNum) {
//        return issueMapper.queryIssueByIssueNum(projectId, issueNum);
//    }

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

        return issueAssembler.issueDTOTOVO(projectId, issueMapper.listIssueInfoByIssueIds(projectId, issueIds));
    }

    @Override
    public Page<IssueListFieldKVVO> queryStoryAndTask(Long projectId, PageRequest pageRequest, SearchVO searchVO) {
        Page<IssueDTO> pageInfo = PageHelper.doPageAndSort(pageRequest, () -> issueMapper.queryStoryAndTaskByProjectId(projectId, searchVO));
        List<IssueDTO> list = pageInfo.getContent();
        if (!CollectionUtils.isEmpty(list)) {
            Long organizationId = projectUtil.getOrganizationId(projectId);
            Map<Long, PriorityVO> priorityMap = priorityService.queryByOrganizationId(organizationId);
            Map<Long, IssueTypeVO> issueTypeDTOMap = issueTypeService.listIssueTypeMap(organizationId);
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
        Set<Long> userIds = issueMapper.selectUserIdsByProjectId(projectId);
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
}
