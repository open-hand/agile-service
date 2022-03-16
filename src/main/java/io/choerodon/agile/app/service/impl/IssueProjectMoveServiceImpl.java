package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.TypeReference;
import io.choerodon.agile.api.validator.SprintValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.TriggerCarrierVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.aspect.DataLogRedisUtil;
import io.choerodon.agile.infra.dto.FieldValueDTO;
import io.choerodon.agile.infra.dto.IssueSprintRelDTO;
import io.choerodon.agile.infra.dto.LookupValueDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.operator.TestServiceClientOperator;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.hzero.core.base.AopProxy;
import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-01-05 13:38
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueProjectMoveServiceImpl implements IssueProjectMoveService, AopProxy<IssueProjectMoveService> {
    protected static final Logger LOGGER = LoggerFactory.getLogger(IssueProjectMoveServiceImpl.class);
    private static final String ISSUE_EPIC = "issue_epic";
    private static final String TYPE_CODE_FIELD = "typeCode";
    private static final String EPIC_ID_FIELD = "epicId";
    private static final String TASK_TYPE = "task";
    private static final String SPRINT_ID_FIELD = "sprintId";
    private static final String STORY_TYPE = "story";
    private static final String EPIC_COLOR_TYPE = "epic_color";
    private static final String EPIC_NAME_FIELD = "epicName";
    private static final String COLOR_CODE_FIELD = "colorCode";
    private static final String STORY_POINTS_FIELD = "storyPoints";
    private static final String REMAIN_TIME_FIELD = "remainingTime";
    private static final String PARENT_ISSUE_ID = "parentIssueId";
    private static final String EPIC_SEQUENCE = "epicSequence";
    private static final String ISSUE_TYPE_ID = "issueTypeId";
    private static final String RELATE_ISSUE_ID = "relateIssueId";
    private static final String RANK_FIELD = "rank";
    private static final String ISSUE_NULL = "error.issue.is.null";
    private static final String AGILE_SCHEME_CODE = "agile_issue";
    private static final String FEATURE = "feature";
    private static final String[] FIELD_LIST_NO_RANK = new String[]{TYPE_CODE_FIELD, REMAIN_TIME_FIELD, PARENT_ISSUE_ID, EPIC_NAME_FIELD, COLOR_CODE_FIELD, EPIC_ID_FIELD, STORY_POINTS_FIELD, EPIC_SEQUENCE, ISSUE_TYPE_ID, RELATE_ISSUE_ID};
    private static final String FEATURE_ID = "featureId";
    private static final String ERROR_TRANSFER_PROJECT_ILLEGAL = "error.transfer.project.illegal";
    private static final String ISSUE_PROJECT_BATCH_MOVE = "agile-issue-project-batch-move";
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IssueService issueService;
    @Autowired
    private DataLogRedisUtil dataLogRedisUtil;
    @Autowired
    private FieldValueMapper fieldValueMapper;
    @Autowired
    private VerifyUpdateUtil verifyUpdateUtil;
    @Autowired
    private WorkLogMapper workLogMapper;
    @Autowired
    private IssueCommentMapper issueCommentMapper;
    @Autowired
    private IssueAttachmentMapper issueAttachmentMapper;
    @Autowired
    private DataLogMapper dataLogMapper;
    @Autowired
    private RankMapper rankMapper;
    @Autowired
    private IssueLinkMapper issueLinkMapper;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired
    private TestServiceClientOperator testServiceClientOperator;
    @Autowired(required = false)
    private AgileTriggerService agileTriggerService;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired
    private IssueAccessDataService issueAccessDataService;
    @Autowired
    private IssueLinkService issueLinkService;
    @Autowired
    private FieldDataLogMapper fieldDataLogMapper;
    @Autowired
    private WikiRelationMapper wikiRelationMapper;
    @Autowired
    private SprintMapper sprintMapper;
    @Autowired
    private LookupValueMapper lookupValueMapper;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private InstanceService instanceService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private RedisUtil redisUtil;
    @Autowired
    private SprintValidator sprintValidator;
    @Autowired
    private FieldValueService fieldValueService;
    @Autowired
    private IssueSprintRelMapper issueSprintRelMapper;
    @Autowired
    private MessageClientC7n messageClientC7n;
    @Autowired
    private ProjectInfoService projectInfoService;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;
    @Override
    public void issueProjectMove(Long projectId, Long issueId, Long targetProjectId, JSONObject jsonObject) {
        if (ObjectUtils.isEmpty(targetProjectId)) {
            throw new CommonException("error.transfer.project.is.null");
        }
        // 两个是否是在同一个组织下,并且项目群项目、普通项目、运维项目不能相互转换
        ProjectVO projectVO = ConvertUtil.queryProject(projectId);
        ProjectVO targetProjectVO = ConvertUtil.queryProject(targetProjectId);
        if (!Objects.equals(projectVO.getOrganizationId(), targetProjectVO.getOrganizationId())) {
            throw new CommonException("error.transfer.across.organizations");
        }
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (ObjectUtils.isEmpty(issueDTO)) {
            throw new CommonException(ISSUE_NULL);
        }
        checkProject(issueDTO.getTypeCode(), targetProjectVO);

        JSONArray subIssues = jsonObject.getJSONArray("subIssues");
        jsonObject.remove("subIssues");
        // 处理issue相关的数据
        handlerIssueValue(projectVO, issueId, targetProjectVO, jsonObject);
        // 处理子任务和子bug
        handlerSubTaskAndBug(projectId, issueId, targetProjectId, subIssues);
        // 清除原项目和目标项目的缓存
        dataLogRedisUtil.handleDeleteRedisByDeleteIssue(projectId);
        dataLogRedisUtil.handleDeleteRedisByDeleteIssue(targetProjectVO.getId());
    }

    private void checkProject(String typeCode, ProjectVO targetProjectVO) {
        List<String> codes = ProjectCategory.getProjectCategoryCodes(targetProjectVO);
        if (Objects.equals(typeCode, ISSUE_EPIC)) {
            if (!(codes.contains(ProjectCategory.MODULE_AGILE) || codes.contains(ProjectCategory.MODULE_PROGRAM))) {
                throw new CommonException(ERROR_TRANSFER_PROJECT_ILLEGAL);
            }
        } else if (Objects.equals(typeCode, FEATURE) && !codes.contains(ProjectCategory.MODULE_PROGRAM)) {
            throw new CommonException(ERROR_TRANSFER_PROJECT_ILLEGAL);
        } else {
            if (!codes.contains(ProjectCategory.MODULE_AGILE)) {
                throw new CommonException(ERROR_TRANSFER_PROJECT_ILLEGAL);
            }
        }
    }

    @Override
    public List<ProjectVO> listMoveProject(Long projectId, String typeCode) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        ProjectVO projectVO = ConvertUtil.queryProject(projectId);
        List<ProjectVO> projectVOS = baseFeignClient.queryOrgProjects(projectVO.getOrganizationId(), userId).getBody();
        if (!CollectionUtils.isEmpty(projectVOS)) {
            return projectVOS.stream()
                    .filter(v -> Boolean.TRUE.equals(v.getEnabled()) && !Objects.equals(v.getId(), projectId))
                    .filter(v -> {
                        List<String> codes = ProjectCategory.getProjectCategoryCodes(v);
                        return checkProjectCategory(codes, typeCode);
                    }).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    @Override
    @Async
    public void issueProjectBatchMove(Long projectId,
                                      Long targetProjectId,
                                      JSONObject jsonObject,
                                      ServletRequestAttributes requestAttributes,
                                      String encryptType) {
        EncryptContext.setEncryptType(encryptType);
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String messageCode = ISSUE_PROJECT_BATCH_MOVE + "-" + projectId;
        List<Long> issueIds = parsingIssueIdList(jsonObject);
        // 检验传入的issueIds
        issueIds = validateIssueIds(projectId, issueIds);
        if (CollectionUtils.isEmpty(issueIds)) {
            BatchUpdateFieldStatusVO batchUpdateFieldStatusVO = new BatchUpdateFieldStatusVO();
            batchUpdateFieldStatusVO.setProcess(1.0);
            batchUpdateFieldStatusVO.setKey(messageCode);
            batchUpdateFieldStatusVO.setUserId(userId);
            batchUpdateFieldStatusVO.setStatus("success");
            messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
            return;
        }
        // 处理问题类型-状态映射
        Map<Long, Map<Long, Map<Long, Long>>> issueTypeStatusMap = parsingIssueTypeStatusMap(jsonObject);
        BatchUpdateFieldStatusVO batchUpdateFieldStatusVO = new BatchUpdateFieldStatusVO();
        try {
            batchUpdateFieldStatusVO.setLastProcess(0.0);
            batchUpdateFieldStatusVO.setStatus("doing");
            batchUpdateFieldStatusVO.setKey(messageCode);
            batchUpdateFieldStatusVO.setUserId(userId);
            batchUpdateFieldStatusVO.setProcess(0.0);
            batchUpdateFieldStatusVO.setSuccessCount(0);
            batchUpdateFieldStatusVO.setFailedCount(0);
            messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
            if (ObjectUtils.isEmpty(targetProjectId)) {
                throw new CommonException("error.transfer.project.is.null");
            }
            // 两个是否是在同一个组织下,并且项目群项目、普通项目、运维项目不能相互转换
            ProjectVO projectVO = ConvertUtil.queryProject(projectId);
            ProjectVO targetProjectVO = ConvertUtil.queryProject(targetProjectId);
            if (!Objects.equals(projectVO.getOrganizationId(), targetProjectVO.getOrganizationId())) {
                throw new CommonException("error.transfer.across.organizations");
            }
            List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds, null);
            int currentProgress = 1;
            double lastProcess = 0.0;
            List<Long> transferBug = new ArrayList<>();
            List<IssueTypeVO> targetProjectIssueType = issueTypeService.queryByOrgId(projectVO.getOrganizationId(), targetProjectVO.getId());
            Map<Long, String> issueTypeCodeMap = targetProjectIssueType.stream().collect(Collectors.toMap(IssueTypeVO::getId, IssueTypeVO::getTypeCode));
            for (IssueDTO issueDTO : issueDTOS) {
                Map<String, Object> reuslt = new HashMap<>();
                buildJSONObject(reuslt, issueDTO, issueTypeStatusMap, jsonObject, transferBug, issueTypeCodeMap);
                issueService.handleData(reuslt, projectVO, issueDTO, targetProjectVO, projectId, batchUpdateFieldStatusVO);
                double progress = currentProgress * 1.0 / issueDTOS.size();
                if (progress - lastProcess >= 0.1) {
                    batchUpdateFieldStatusVO.setProcess(progress);
                    messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
                    lastProcess = progress;
                }
                currentProgress++;
            }
            // 清除原项目和目标项目的缓存
            dataLogRedisUtil.handleDeleteRedisByDeleteIssue(projectId);
            dataLogRedisUtil.handleDeleteRedisByDeleteIssue(targetProjectVO.getId());
            //发送websocket
            batchUpdateFieldStatusVO.setStatus("success");
            batchUpdateFieldStatusVO.setProcess(1.0);
        } catch (Exception e) {
            batchUpdateFieldStatusVO.setStatus("failed");
            batchUpdateFieldStatusVO.setError(e.getMessage());
            throw new CommonException("update field failed, exception: {}", e);
        } finally {
            messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
        }
    }

    private List<Long> validateIssueIds(Long projectId, List<Long> issueIds) {
        Set<Long> allIssueIds = new HashSet<>();
        allIssueIds.addAll(issueIds);
        // 传入的父级issue移动时需要把所有子级带上，issueIds中没有的需要补充上
        Set<Long> childIssues = issueMapper.queryChildrenIds(projectId, null, issueIds, null);
        if (!CollectionUtils.isEmpty(childIssues)) {
            allIssueIds.addAll(childIssues);
        }
        // 传入的issueIds中选中了子任务没有带上父级任务需要把子任务去掉
        List<IssueDTO> childDTOS = issueMapper.queryChildrenIssue(projectId, issueIds);
        if (!CollectionUtils.isEmpty(childDTOS)) {
            for (IssueDTO childDTO : childDTOS) {
                if (!allIssueIds.contains(childDTO.getParentIssueId())) {
                    allIssueIds.remove(childDTO.getIssueId());
                }
            }
        }
        return new ArrayList<>(allIssueIds);
    }

    @Override
    public Map<String, List<String>> issueTypeStatusMap(Long projectId, List<Long> issueIds) {
        Map<String, List<String>> result = new HashMap<>();
        issueIds = validateIssueIds(projectId, issueIds);
        if (!CollectionUtils.isEmpty(issueIds)) {
            List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds, null);
            Map<Long, List<Long>> map = new HashMap<>();
            for (IssueDTO issueDTO : issueDTOS) {
                List<Long> statusIds = map.getOrDefault(issueDTO.getIssueTypeId(), new ArrayList<>());
                if (!statusIds.contains(issueDTO.getStatusId())) {
                    statusIds.add(issueDTO.getStatusId());
                }
                map.put(issueDTO.getIssueTypeId(), statusIds);
            }
            result = EncryptionUtils.encryptMap(map);
        }
        return result;
    }

    private void buildJSONObject(Map<String, Object> reuslt,
                                 IssueDTO issueDTO,
                                 Map<Long, Map<Long, Map<Long, Long>>> issueTypeStatusMap,
                                 JSONObject json,
                                 List<Long> transferBug,
                                 Map<Long, String> issueTypeCodeMap) {
        JSONObject jsonObject = json.getJSONObject("issueInfo");
        if (ObjectUtils.isEmpty(jsonObject)) {
            jsonObject = new JSONObject();
        }
        Long issueTypeId = issueDTO.getIssueTypeId();
        Map<Long, Map<Long, Long>> newIssueTypeMap = issueTypeStatusMap.get(issueTypeId);
        if (!ObjectUtils.isEmpty(newIssueTypeMap)) {
            List<Long> issueType = newIssueTypeMap.keySet().stream().collect(Collectors.toList());
            issueTypeId = issueType.get(0);
        }
        // 父级移动时问题类型改成了bug，需要将它下面的子缺陷解除关联关系，不移动。
        Boolean isSubIssue = "sub_task".equals(issueDTO.getTypeCode()) || ("bug".equals(issueDTO.getTypeCode()) && !ObjectUtils.isEmpty(issueDTO.getRelateIssueId()) && !Objects.equals(0L, issueDTO.getRelateIssueId()));
        if (!isSubIssue && "bug".equals(issueTypeCodeMap.get(issueTypeId))) {
            transferBug.add(issueDTO.getIssueId());
        }

        if (isSubIssue && "bug".equals(issueDTO.getTypeCode()) && "bug".equals(issueTypeCodeMap.get(issueTypeId))) {
            Long parentIssue = issueDTO.getRelateIssueId();
            if (transferBug.contains(parentIssue)) {
                jsonObject.put("issueId", issueDTO.getIssueId());
                jsonObject.put("relateIssueId", 0L);
                jsonObject.put("objectVersionNumber", issueDTO.getObjectVersionNumber());

                reuslt.put("isMove", false);
                reuslt.put("jsonObj", jsonObject);
                return;
            }
        }

        Long statusId = issueDTO.getStatusId();
        Map<Long, Long> statusIdMap = newIssueTypeMap.getOrDefault(issueTypeId, new HashMap<>());
        if (!CollectionUtils.isEmpty(statusIdMap) && !ObjectUtils.isEmpty(statusIdMap.get(statusId))) {
            statusId = statusIdMap.get(statusId);
        }
        jsonObject.put("issueId", issueDTO.getIssueId());
        jsonObject.put("issueTypeId", issueTypeId);
        jsonObject.put("statusId", statusId);
        reuslt.put("isMove", true);
        reuslt.put("jsonObj", jsonObject);
        return;
    }

    private Map<Long, Map<Long, Map<Long, Long>>> parsingIssueTypeStatusMap(JSONObject jsonObject) {
        Object json = jsonObject.get("issueTypeStatusMap");
        if (ObjectUtils.isEmpty(json)) {
            return new HashMap<>();
        }
        Map<String, Map<String, Map<String, String>>> issueTypeStatusStringMap = JSONObject.parseObject(JSON.toJSONString(json), new TypeReference<Map<String, Map<String, Map<String, String>>>>() {
        });
        Map<Long, Map<Long, Map<Long, Long>>> issueTypeStatusMap = new HashMap<>();
        for (Map.Entry<String, Map<String, Map<String, String>>> entry : issueTypeStatusStringMap.entrySet()) {
            Map<Long, Map<Long, Long>> statusMap = new HashMap<>();
            Map<String, Map<String, String>> value = entry.getValue();
            for (Map.Entry<String, Map<String, String>> statusEntry : value.entrySet()) {
                Map<String, String> entryValue = statusEntry.getValue();
                Map<Long, Long> map = new HashMap<>();
                for (Map.Entry<String, String> stringEntry : entryValue.entrySet()) {
                    map.put(Long.valueOf(EncryptionUtils.decrypt(stringEntry.getKey())), Long.valueOf(EncryptionUtils.decrypt(stringEntry.getValue())));
                }
                statusMap.put(Long.valueOf(EncryptionUtils.decrypt(statusEntry.getKey())), map);
            }
            issueTypeStatusMap.put(Long.valueOf(EncryptionUtils.decrypt(entry.getKey())), statusMap);
        }
        return issueTypeStatusMap;
    }

    private List<Long> parsingIssueIdList(JSONObject jsonObject) {
        // 获取issueIds
        Object issueList = jsonObject.get("issueIds");
        if (ObjectUtils.isEmpty(issueList)) {
            return new ArrayList<>();
        }
        List<String> issueIdStrings = JSONObject.parseObject(JSON.toJSONString(issueList), new TypeReference<List<String>>(){});
        return EncryptionUtils.decryptList(issueIdStrings, EncryptionUtils.BLANK_KEY, null);
    }

    private boolean checkProjectCategory(List<String> codes, String typeCode) {
        if (Objects.equals(typeCode, ISSUE_EPIC)) {
            return codes.contains(ProjectCategory.MODULE_AGILE) || codes.contains(ProjectCategory.MODULE_PROGRAM);
        }
        else if (Objects.equals(typeCode, FEATURE)) {
            return codes.contains(ProjectCategory.MODULE_PROGRAM);
        }
        else {
            return codes.contains(ProjectCategory.MODULE_AGILE);
        }
    }

    @Override
    public List<ObjectSchemeFieldVO> listLostField(Long projectId, Long issueId, Long targetProject, Long issueTypeId) {
        List<FieldValueDTO> fieldValueDTOS = fieldValueMapper.queryList(projectId, issueId, AGILE_SCHEME_CODE, null);
        if (!CollectionUtils.isEmpty(fieldValueDTOS)) {
            // 查询目标项目指定问题类型的字段
            PageConfigVO pageConfigVO = objectSchemeFieldService.listConfigs(ConvertUtil.getOrganizationId(targetProject), targetProject, issueTypeId);
            List<PageConfigFieldVO> fields = pageConfigVO.getFields();
            if (CollectionUtils.isEmpty(fields)) {
                return new ArrayList<>();
            }
            // 将issue原有的自定义字段值和新项目指定问题类型的字段比较，相同的留下，不同的删除
            List<Long> fieldIds = fields.stream().map(PageConfigFieldVO::getFieldId).collect(Collectors.toList());
            Set<Long> olderFieldIds = fieldValueDTOS.stream().map(FieldValueDTO::getFieldId).collect(Collectors.toSet());
            olderFieldIds.removeAll(fieldIds);
            if (!CollectionUtils.isEmpty(olderFieldIds)) {
                List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS = objectSchemeFieldMapper.selectByIds(olderFieldIds.stream().map(String::valueOf).collect(Collectors.joining(",")));
                return modelMapper.map(objectSchemeFieldDTOS, new TypeToken<List<ObjectSchemeFieldVO>>() {
                }.getType());
            }
        }
        return new ArrayList<>();
    }

    private void handlerSubTaskAndBug(Long projectId, Long issueId, Long targetProjectId, JSONArray subIssues) {
        // 查询issue是否有子任务和子bug
        List<Long> taskAndSubBugIds = issueMapper.selectSubIssueIds(issueId);
        if (!CollectionUtils.isEmpty(taskAndSubBugIds)) {
            for (Long taskAndSubBugId : taskAndSubBugIds) {
                JSONObject subJsonObject = new JSONObject();
                if (!ObjectUtils.isEmpty(subIssues)) {
                    for (int i = 0; i < subIssues.size(); i++) {
                        JSONObject jsonObject1 = subIssues.getJSONObject(i);
                        String subIssue = EncryptionUtils.decrypt(jsonObject1.getString("issueId"));
                        if (Objects.equals(Long.valueOf(subIssue), taskAndSubBugId)) {
                            subJsonObject = jsonObject1;
                        }
                    }
                }
                issueProjectMove(projectId, taskAndSubBugId, targetProjectId, subJsonObject);
            }
        }
    }

    @Override
    public void handlerIssueValue(ProjectVO projectVO, Long issueId, ProjectVO targetProjectVO, JSONObject jsonObject) {
        IssueDetailDTO issueDTO = issueMapper.queryIssueDetail(projectVO.getId(), issueId);
        if (ObjectUtils.isEmpty(issueDTO)) {
            throw new CommonException(ISSUE_NULL);
        }
        Long issueTypeId = EncryptionUtils.decrypt(jsonObject.getString(ISSUE_TYPE_ID),EncryptionUtils.BLANK_KEY);
        jsonObject.remove(ISSUE_TYPE_ID);
        IssueTypeVO issueTypeVO = issueTypeService.queryByOrgId(projectVO.getOrganizationId(), targetProjectVO.getId()).stream()
                .filter(issueTypeVO1 -> Objects.equals(ObjectUtils.isEmpty(issueTypeId) ? issueDTO.getIssueTypeId() : issueTypeId, issueTypeVO1.getId()))
                .findAny().orElse(null);
        // 处理需要清空的值
        handlerNeedCleanValue(projectVO, issueDTO, issueTypeVO, targetProjectVO);
        // 处理issue表本身需要清空的值
        handlerIssueSelfValue(projectVO.getId(), issueDTO);
        // 处理issue需要转交的数据
        handlerNeedTransferValue(projectVO, issueId, targetProjectVO, jsonObject, issueTypeVO);
    }

    private void handlerChangeIssueType(Long issueId, JSONObject jsonObject, IssueTypeVO issueTypeVO) {
        List<String> fieldList = new ArrayList<>(Arrays.asList(FIELD_LIST_NO_RANK));
        if (ObjectUtils.isEmpty(issueTypeVO)) {
            return;
        }
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (Objects.equals(issueDTO.getIssueTypeId(), issueTypeVO.getId())) {
            return;
        }
        IssueConvertDTO issueConvertDTO = modelMapper.map(issueDTO, IssueConvertDTO.class);
        issueConvertDTO.setProjectId(issueDTO.getProjectId());
        if (!Objects.equals(issueDTO.getTypeCode(), issueTypeVO.getTypeCode())) {
            if ((Objects.equals(issueDTO.getTypeCode(), STORY_TYPE) || Objects.equals(issueDTO.getTypeCode(), TASK_TYPE))
                    && (!Objects.equals(issueTypeVO.getTypeCode(), STORY_TYPE) && !Objects.equals(issueTypeVO.getTypeCode(), TASK_TYPE))) {
                issueMapper.updateSubBugRelateIssueId(issueDTO.getProjectId(), issueDTO.getIssueId());
            }
            if (issueTypeVO.getTypeCode().equals(ISSUE_EPIC)) {
                issueConvertDTO.setRank(null);
                fieldList.add(RANK_FIELD);
                issueConvertDTO.setTypeCode(issueTypeVO.getTypeCode());
                issueConvertDTO.setEpicName(jsonObject.getString(EPIC_NAME_FIELD));
                List<LookupValueDTO> colorList = lookupValueMapper.queryLookupValueByCode(EPIC_COLOR_TYPE).getLookupValues();
                issueConvertDTO.initializationColor(colorList);
                issueConvertDTO.setRemainingTime(null);
                issueConvertDTO.setEpicId(0L);
                //排序编号
                Integer sequence = issueMapper.queryMaxEpicSequenceByProject(issueConvertDTO.getProjectId());
                issueConvertDTO.setEpicSequence(sequence == null ? 0 : sequence + 1);
            } else if (issueDTO.getTypeCode().equals(ISSUE_EPIC)) {
                // 如果之前类型是epic，会把该epic下的issue的epicId置为0
                issueAccessDataService.batchUpdateIssueEpicId(issueConvertDTO.getProjectId(), issueConvertDTO.getIssueId());
                issueConvertDTO.setTypeCode(issueTypeVO.getTypeCode());
                issueConvertDTO.setColorCode(null);
                issueConvertDTO.setEpicName(null);
                issueConvertDTO.setEpicSequence(null);
                //rank值重置
                calculationRank(issueConvertDTO.getProjectId(), issueConvertDTO);
                fieldList.add(RANK_FIELD);
            } else if ("bug".equals(issueDTO.getTypeCode()) && Objects.nonNull(issueDTO.getRelateIssueId())) {
                issueConvertDTO.setTypeCode(issueTypeVO.getTypeCode());
                issueConvertDTO.setRelateIssueId(0L);
            } else {
                issueConvertDTO.setTypeCode(issueTypeVO.getTypeCode());
            }
        }
        issueConvertDTO.setIssueTypeId(issueTypeVO.getId());
        issueConvertDTO.setTypeCode(issueTypeVO.getTypeCode());
        issueAccessDataService.update(issueConvertDTO, fieldList.toArray(new String[fieldList.size()]));
    }

    private void calculationRank(Long projectId, IssueConvertDTO issueConvertDTO) {
        Boolean hasIssue = sprintValidator.hasIssue(projectId, issueConvertDTO.getSprintId());
        if (Boolean.TRUE.equals(hasIssue)) {
            String rank = sprintMapper.queryMaxRank(projectId, issueConvertDTO.getSprintId());
            //处理rank值为null的脏数据
            if (StringUtils.isEmpty(rank)) {
                issueConvertDTO.setRank(RankUtil.mid());
            } else {
                issueConvertDTO.setRank(RankUtil.genNext(rank));
            }
            issueConvertDTO.setRank(RankUtil.genNext(rank));
        } else {
            issueConvertDTO.setRank(RankUtil.mid());
        }
    }

    private void handlerNeedTransferValue(ProjectVO projectVO, Long issueId, ProjectVO targetProjectVO, JSONObject jsonObject, IssueTypeVO issueTypeVO) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (ObjectUtils.isEmpty(issueDTO)) {
            throw new CommonException(ISSUE_NULL);
        }
        String applyType = getApplyType(targetProjectVO);
        Map<String,Object> programValueMap = new HashMap<>();
        if (agilePluginService != null) {
            agilePluginService.handlerProgramPredefinedFields(targetProjectVO.getId(),jsonObject,programValueMap,applyType);
        }
        Object influenceVersions = jsonObject.get("influenceVersionList");
        jsonObject.remove("influenceVersionList");
        Object customFields = jsonObject.get("customFields");
        jsonObject.remove("customFields");
        IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
        List<String> fieldList = verifyUpdateUtil.verifyUpdateData(jsonObject, issueUpdateVO);
        // 转换附件
        issueAttachmentMapper.updateProject(projectVO.getId(), issueId, targetProjectVO.getId());
        // 转换评论
        issueCommentMapper.updateTransferProject(projectVO.getId(), issueId, targetProjectVO.getId());
        // 转换工作日志
        workLogMapper.updateProject(projectVO.getId(), issueId, targetProjectVO.getId());
        // 日志
        dataLogMapper.updateProject(projectVO.getId(), issueId, targetProjectVO.getId());
        // 修改触发器日志表记录的projectId
        if (agileTriggerService != null) {
            agileTriggerService.updateRuleLogProjectId(projectVO.getId(), targetProjectVO.getId(), issueId);
        }
        // 修改issue项目并指定合适的状态
        issueDTO.setApplyType(applyType);
        IssueDTO issue = buildIssue(projectVO.getId(), issueDTO, issueUpdateVO.getStatusId(), targetProjectVO.getId());
        issueAccessDataService.transferProject(issue);
        // 处理移动的时候同时改变issue的问题类型
        handlerChangeIssueType(issueId, jsonObject, issueTypeVO);
        // 更新issue的值
        if (!ObjectUtils.isEmpty(jsonObject)) {
            IssueDTO issue1 = issueMapper.selectByPrimaryKey(issueId);
            if (!ObjectUtils.isEmpty(programValueMap.get(FEATURE_ID))) {
                Long featureId = Long.valueOf(programValueMap.get(FEATURE_ID).toString());
                issueUpdateVO.setFeatureId(featureId);
                fieldList.add(FEATURE_ID);
            }
            issueUpdateVO.setIssueId(issueId);
            issueUpdateVO.setObjectVersionNumber(issue1.getObjectVersionNumber());
            issueService.updateIssueWithoutRuleNotice(targetProjectVO.getId(), issueUpdateVO, fieldList);
            if (!ObjectUtils.isEmpty(influenceVersions)) {
                List<VersionIssueRelVO> list = EncryptionUtils.jsonToList(influenceVersions, VersionIssueRelVO.class);
                fieldList.add("versionId");
                issueService.handleUpdateVersionIssueRelWithoutRuleNotice(list, targetProjectVO.getId(), issueDTO.getIssueId(), "influence");
            }
        }
        issueService.addCollectionFieldIfNotNull(issueUpdateVO, fieldList);
        // 修改自定义字段的值
        TriggerCarrierVO triggerCarrierVO = null;
        if (!ObjectUtils.isEmpty(customFields)) {
            Map<Long, TriggerCarrierVO> triggerCarrierMap = new HashMap<>();
            addCustomFieldValues(targetProjectVO, issueDTO, customFields, triggerCarrierMap);
            triggerCarrierVO = triggerCarrierMap.getOrDefault(issueDTO.getIssueId(), null);
        }
        // 修改agile_feature和wsjf表中数据的projectId
        if (agilePluginService != null) {
            agilePluginService.projectMoveUpdateFeatureValue(projectVO.getId(), issueDTO, targetProjectVO.getId());
            agilePluginService.handlerFeatureField(targetProjectVO.getId(),issueDTO,programValueMap, triggerCarrierVO);
        }
        IssueDTO currentIssue = issueMapper.selectByPrimaryKey(issueId);
        triggerCarrierVO = buildTriggerCarrierVO(currentIssue.getProjectId(), issueId, fieldList, triggerCarrierVO, currentIssue);
        issueService.batchUpdateInvokeTrigger(Collections.singletonList(triggerCarrierVO));
    }

    private TriggerCarrierVO buildTriggerCarrierVO(Long projectId, Long issueId, List<String> fieldList, TriggerCarrierVO triggerCarrierVO, IssueDTO issueDTO) {
        TriggerCarrierVO triggerCarrier = triggerCarrierVO;
        if (ObjectUtils.isEmpty(triggerCarrier)) {
            triggerCarrier = new TriggerCarrierVO();
            triggerCarrier.setExecutedRules(new ArrayList<>());
            triggerCarrier.setMemberFieldIds(new HashSet<>());
            triggerCarrier.setFieldList(new ArrayList<>());
        }
        triggerCarrier.setInstanceId(issueId);
        triggerCarrier.setProjectId(projectId);
        triggerCarrier.setNoticeInstanceId(issueId);
        triggerCarrier.setIssueTypeId(issueDTO.getIssueTypeId());
        triggerCarrier.setAuditDomain(issueDTO);
        Set<String> fields = new HashSet<>();
        fields.addAll(fieldList);
        fields.addAll(triggerCarrier.getFieldList());
        triggerCarrier.setFieldList(new ArrayList<>(fields));
        return triggerCarrier;
    }

    private String getApplyType(ProjectVO targetProjectVO) {
        List<String> codes = ProjectCategory.getProjectCategoryCodes(targetProjectVO);
        if (codes.contains(ProjectCategory.MODULE_AGILE)) {
            return SchemeApplyType.AGILE;
        } else {
            return SchemeApplyType.PROGRAM;
        }
    }

    private void addCustomFieldValues(ProjectVO targetProjectVO, IssueDTO issueDTO, Object customFields, Map<Long, TriggerCarrierVO> triggerCarrierMap) {
        List<PageFieldViewUpdateVO> list = EncryptionUtils.jsonToList(customFields, PageFieldViewUpdateVO.class);
        List<Long> fieldIds = list.stream().map(PageFieldViewUpdateVO::getFieldId).collect(Collectors.toList());
        List<Long> existFields = objectSchemeFieldMapper.filterNotExistFields(fieldIds);
        if (CollectionUtils.isEmpty(existFields)) {
            return;
        }
        list = list.stream().filter(v -> existFields.contains(v.getFieldId())).collect(Collectors.toList());
        fieldValueService.handlerCustomFields(targetProjectVO.getId(), list, AGILE_SCHEME_CODE, Arrays.asList(issueDTO.getIssueId()), null, false, triggerCarrierMap);
    }

    private IssueDTO buildIssue(Long projectId, IssueDTO issueDTO, Long status, Long targetProjectId) {
        IssueDTO issue = new IssueDTO();
        issue.setProjectId(targetProjectId);
        if (!ObjectUtils.isEmpty(issueDTO.getProgramId())) {
            issue.setProgramId(targetProjectId);
        }
        issue.setIssueId(issueDTO.getIssueId());
        Long statusId = status;
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        //获取初始状态
        if (ObjectUtils.isEmpty(status)) {
            //获取状态机id
            Long stateMachineId = projectConfigService.queryStateMachineId(targetProjectId, issueDTO.getApplyType(), issueDTO.getIssueTypeId());
            if (stateMachineId == null) {
                throw new CommonException("error.createIssue.stateMachineNotFound");
            }
            statusId = instanceService.queryInitStatusId(organizationId, stateMachineId);
            if (statusId == null) {
                throw new CommonException("error.init.status.not.found");
            }
        }
        issue.setStatusId(statusId);
        // 设置issueNum
        String newIssueNum = IssueNumUtil.getNewIssueNum(targetProjectId).toString();
        issue.setIssueNum(newIssueNum);
        projectInfoService.updateIssueMaxNum(targetProjectId,newIssueNum);
        issue.setObjectVersionNumber(issueDTO.getObjectVersionNumber());
        if (Objects.equals(issueDTO.getTypeCode(), ISSUE_EPIC)) {
            Integer sequence = issueMapper.queryMaxEpicSequenceByProject(targetProjectId);
            issue.setEpicSequence(sequence);
        }
        issue.setApplyType(issueDTO.getApplyType());
        return issue;
    }

    private void handlerIssueSelfValue(Long projectId, IssueDetailDTO issueDTO) {
        IssueDetailDTO issueDetailDTO = issueMapper.queryIssueDetail(projectId, issueDTO.getIssueId());
        IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
        List<String> field = new ArrayList<>();
        // 清空史诗
        issueUpdateVO.setEpicId(0L);
        field.add(EPIC_ID_FIELD);
        if (agilePluginService != null) {
            agilePluginService.handlerFeatureSelfValue(issueDTO, issueUpdateVO, field);
        }
        // 乐观锁
        issueUpdateVO.setObjectVersionNumber(issueDetailDTO.getObjectVersionNumber());
        issueUpdateVO.setIssueId(issueDTO.getIssueId());
        field.add("objectVersionNumber");
        if (Objects.equals(SchemeApplyType.AGILE, issueDTO.getApplyType())) {
            // 清空冲刺
            issueUpdateVO.setSprintId(0L);
            field.add(SPRINT_ID_FIELD);
            // 清空模块，版本，标签
            if (!CollectionUtils.isEmpty(issueDTO.getLabelIssueRelDTOList())) {
                issueUpdateVO.setLabelIssueRelVOList(new ArrayList<>());
            }
            if(!CollectionUtils.isEmpty(issueDTO.getComponentIssueRelDTOList())){
                issueUpdateVO.setComponentIssueRelVOList(new ArrayList<>());
            }
            if (!CollectionUtils.isEmpty(issueDTO.getVersionIssueRelDTOList())) {
                issueUpdateVO.setVersionIssueRelVOList(new ArrayList<>());
                issueUpdateVO.setVersionType("fix");
            }
        }
        issueService.updateIssueWithoutRuleNotice(projectId, issueUpdateVO, field);
        if (Objects.equals(issueDTO.getTypeCode(), "bug")) {
            issueService.handleUpdateVersionIssueRelWithoutRuleNotice(new ArrayList<>(), projectId, issueDTO.getIssueId(), "influence");
        }
        // 清空原项目和冲刺的关系
        IssueSprintRelDTO issueSprintRelDTO = new IssueSprintRelDTO();
        issueSprintRelDTO.setIssueId(issueDTO.getIssueId());
        issueSprintRelDTO.setProjectId(projectId);
        issueSprintRelMapper.delete(issueSprintRelDTO);
        if ((ISSUE_EPIC).equals(issueDTO.getTypeCode())) {
            //如果是epic，会把该epic下的issue的epicId置为0
            issueAccessDataService.batchUpdateIssueEpicId(projectId, issueDTO.getIssueId());
        } else {
            redisUtil.deleteRedisCache(new String[]{"Agile:EpicChart" + projectId + ":" + issueDTO.getEpicId() + ":" + "*"});
        }
    }

    private void handlerNeedCleanValue(ProjectVO projectVO, IssueDetailDTO issueDTO, IssueTypeVO issueTypeVO, ProjectVO targetProjectVO) {
        // 清空测试用例
        testServiceClientOperator.deleteTestRel(projectVO.getId(), issueDTO.getIssueId());
        // 删除问题关联
        issueLinkService.deleteByIssueId(issueDTO.getIssueId());
        // 删除rank值
        rankMapper.deleteRankByIssueId(issueDTO.getIssueId());
        // 清空关联知识
        wikiRelationMapper.deleteByIssueId(projectVO.getId(), issueDTO.getIssueId());
        // 问题是否与需求相关联
        if (backlogExpandService != null) {
            backlogExpandService.deleteIssueBacklogRel(issueDTO.getIssueId());
        }
        // 处理项目群需要清空的值
        if (agilePluginService != null) {
            agilePluginService.handlerFeatureCleanValue(projectVO.getId(), issueDTO);
        }
        // 处理瀑布项目需要清空的值
        if (agileWaterfallService != null) {
            agileWaterfallService.deleteIssueForWaterfall(projectVO.getId(), issueDTO.getIssueId(), modelMapper.map(issueDTO, IssueConvertDTO.class));
        }
        // 清空自定义字段的值
        handlerCustomFieldValue(projectVO, issueDTO, issueTypeVO, targetProjectVO);
    }

    private void handlerCustomFieldValue(ProjectVO projectVO, IssueDetailDTO issueDTO, IssueTypeVO issueTypeVO, ProjectVO targetProjectVO) {
        List<FieldValueDTO> fieldValueDTOS = fieldValueMapper.queryList(projectVO.getId(), issueDTO.getIssueId(), AGILE_SCHEME_CODE, null);
        if(!CollectionUtils.isEmpty(fieldValueDTOS)){
            // 查询目标项目指定问题类型的字段
            Long issueTypeId = ObjectUtils.isEmpty(issueTypeVO) ? issueDTO.getIssueTypeId() : issueTypeVO.getId();
            PageConfigVO pageConfigVO = objectSchemeFieldService.listConfigs(targetProjectVO.getOrganizationId(), targetProjectVO.getId(), issueTypeId);
            List<PageConfigFieldVO> fields = pageConfigVO.getFields();
            if (CollectionUtils.isEmpty(fields)) {
                fieldValueMapper.deleteList(projectVO.getId(), issueDTO.getIssueId(), AGILE_SCHEME_CODE, null);
                return;
            }
            // 将issue原有的自定义字段值和新项目指定问题类型的字段比较，相同的留下，不同的删除
            List<Long> fieldIds = fields.stream().map(PageConfigFieldVO::getFieldId).collect(Collectors.toList());
            Set<Long> olderFieldIds = fieldValueDTOS.stream().map(FieldValueDTO::getFieldId).collect(Collectors.toSet());
            olderFieldIds.removeAll(fieldIds);
            if (!CollectionUtils.isEmpty(olderFieldIds)) {
                olderFieldIds.forEach(fieldId -> fieldValueMapper.deleteList(projectVO.getId(), issueDTO.getIssueId(), AGILE_SCHEME_CODE, fieldId));
                fieldDataLogMapper.deleteByInstanceIdAndFieldIds(projectVO.getId(), issueDTO.getIssueId(), AGILE_SCHEME_CODE, olderFieldIds);
            }
            fieldValueMapper.updateProjectId(projectVO.getId(), targetProjectVO.getId(), issueDTO.getIssueId(), AGILE_SCHEME_CODE);
            fieldDataLogMapper.updateProjectId(projectVO.getId(), targetProjectVO.getId(),issueDTO.getIssueId(), AGILE_SCHEME_CODE);
        }
    }

}
