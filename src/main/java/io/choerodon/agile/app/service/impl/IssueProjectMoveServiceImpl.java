package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.validator.SprintValidator;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueUpdateVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.aspect.DataLogRedisUtil;
import io.choerodon.agile.infra.dto.FieldValueDTO;
import io.choerodon.agile.infra.dto.LookupValueDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.TestFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.*;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-01-05 13:38
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueProjectMoveServiceImpl implements IssueProjectMoveService {
    private static final String SUB_TASK = "sub_task";
    private static final String ISSUE_EPIC = "issue_epic";
    private static final String TYPE_CODE_FIELD = "typeCode";
    private static final String EPIC_ID_FIELD = "epicId";
    private static final String BUG_TYPE = "bug";
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
    private static final  String[] AGILE_PROJECT_CATEGORY = {ProjectCategory.GENERAL, ProjectCategory.AGILE};

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
    private TestFeignClient testFeignClient;
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
    private FeignUtil feignUtil;

    @Override
    public void issueProjectMove(Long projectId, Long issueId, Long targetProjectId, JSONObject jsonObject) {
        if (ObjectUtils.isEmpty(targetProjectId)) {
            throw new CommonException("error.transfer.project.is.null");
        }
        // 两个是否是在同一个组织下,并且项目群项目、普通项目、运维项目不能相互转换
        Set<Long> projectIds = new HashSet<>();
        projectIds.add(projectId);
        projectIds.add(targetProjectId);
        List<ProjectVO> projectVOS = baseFeignClient.queryByIds(projectIds).getBody();
        if (CollectionUtils.isEmpty(projectVOS) || !Objects.equals(projectVOS.size(),projectIds.size())) {
            throw new CommonException("error.project.not.found");
        }
        Map<Long, ProjectVO> projectVOMap = projectVOS.stream().collect(Collectors.toMap(ProjectVO::getId, Function.identity()));
        ProjectVO projectVO = projectVOMap.get(projectId);
        ProjectVO targetProjectVO = projectVOMap.get(targetProjectId);
        if (!Objects.equals(projectVO.getOrganizationId(), targetProjectVO.getOrganizationId())) {
            throw new CommonException("error.transfer.across.organizations");
        }
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (ObjectUtils.isEmpty(issueDTO)) {
            throw new CommonException("");
        }
        List<String> agileProjectCategoryS = new ArrayList<>();
        agileProjectCategoryS.addAll(Arrays.asList(AGILE_PROJECT_CATEGORY));
        if (!Objects.equals(issueDTO.getTypeCode(), ISSUE_EPIC)) {
            if (!Objects.equals(projectVO.getCategory(), targetProjectVO.getCategory()) && (!agileProjectCategoryS.contains(projectVO.getCategory()) || !agileProjectCategoryS.contains(targetProjectVO.getCategory()))) {
                throw new CommonException("error.transfer.project.illegal");
            }
        } else {
            agileProjectCategoryS.add(ProjectCategory.PROGRAM);
            if (!agileProjectCategoryS.contains(targetProjectVO.getCategory())) {
                throw new CommonException("error.transfer.project.illegal");
            }
        }
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

    @Override
    public List<ProjectVO> listMoveProject(Long projectId, String typeCode) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        ProjectVO projectVO = baseFeignClient.queryProject(projectId).getBody();
        List<String> agileProjectCategoryS = Arrays.asList(AGILE_PROJECT_CATEGORY);
        List<ProjectVO> projectVOS = baseFeignClient.queryOrgProjects(projectVO.getOrganizationId(), userId).getBody();
        if (!Objects.equals(typeCode, ISSUE_EPIC)) {
            projectVOS = projectVOS.stream()
                    .filter(v -> Boolean.TRUE.equals(v.getEnabled()) && !Objects.equals(v.getId(),projectId))
                    .filter(v -> Objects.equals(v.getCategory(), projectVO.getCategory()) || (agileProjectCategoryS.contains(v.getCategory()) && agileProjectCategoryS.contains(projectVO.getCategory())))
                    .collect(Collectors.toList());
        } else {
            agileProjectCategoryS.add(ProjectCategory.PROGRAM);
            projectVOS = projectVOS.stream()
                    .filter(v -> Boolean.TRUE.equals(v.getEnabled()) && !Objects.equals(v.getId(),projectId))
                    .filter(v -> !agileProjectCategoryS.contains(v.getCategory()))
                    .collect(Collectors.toList());
        }
        return projectVOS;
    }

    @Override
    public List<ObjectSchemeFieldVO> listLostField(Long projectId, Long issueId, Long targetProject, String typeCode) {
        List<FieldValueDTO> fieldValueDTOS = fieldValueMapper.queryList(projectId, issueId, AGILE_SCHEME_CODE, null);
        if (!CollectionUtils.isEmpty(fieldValueDTOS)) {
            // 查询目标项目指定问题类型的字段
            PageConfigVO pageConfigVO = objectSchemeFieldService.listConfigs(ConvertUtil.getOrganizationId(targetProject), targetProject, typeCode);
            List<PageConfigFieldVO> fields = pageConfigVO.getFields();
            if (CollectionUtils.isEmpty(fields)) {
                return new ArrayList<>();
            }
            // 将issue原有的自定义字段值和新项目指定问题类型的字段比较，相同的留下，不同的删除
            List<Long> fieldIds = fields.stream().map(PageConfigFieldVO::getFieldId).collect(Collectors.toList());
            Set<Long> olderFieldIds = fieldValueDTOS.stream().map(FieldValueDTO::getFieldId).collect(Collectors.toSet());
            olderFieldIds.removeAll(fieldIds);
            if (!CollectionUtils.isEmpty(olderFieldIds)) {
                List<ObjectSchemeFieldDTO> objectSchemeFieldDTOS = objectSchemeFieldMapper.selectByIds(olderFieldIds.stream().map(v -> v.toString()).collect(Collectors.joining(",")));
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
                        if (Objects.equals(jsonObject1.getLong("issueId"), taskAndSubBugId)) {
                            subJsonObject = jsonObject1;
                        }
                    }
                }
                issueProjectMove(projectId, taskAndSubBugId, targetProjectId, subJsonObject);
            }
        }
    }

    private void handlerIssueValue(ProjectVO projectVO, Long issueId, ProjectVO targetProjectVO, JSONObject jsonObject) {
        IssueDetailDTO issueDTO = issueMapper.queryIssueDetail(projectVO.getId(), issueId);
        if (ObjectUtils.isEmpty(issueDTO)) {
            throw new CommonException(ISSUE_NULL);
        }
        String typeCode = jsonObject.getString(TYPE_CODE_FIELD);
        jsonObject.remove(TYPE_CODE_FIELD);
        IssueTypeVO issueTypeVO = issueTypeService.queryByOrgId(projectVO.getOrganizationId()).stream()
                .filter(issueTypeVO1 -> Objects.equals(ObjectUtils.isEmpty(typeCode) ? issueDTO.getTypeCode() : typeCode, issueTypeVO1.getTypeCode()))
                .findAny().orElse(null);
        // 处理需要清空的值
        handlerNeedCleanValue(projectVO, issueDTO, issueTypeVO, targetProjectVO);
        // 处理issue表本身需要清空的值
        handlerIssueSelfValue(projectVO.getId(), issueDTO);
        // 处理移动的时候同时改变issue的问题类型
        handlerChangeIssueType(issueId, jsonObject, issueTypeVO);
        // 处理issue需要转交的数据
        handlerNeedTransferValue(projectVO, issueId, targetProjectVO, jsonObject);
    }

    private void handlerChangeIssueType(Long issueId, JSONObject jsonObject, IssueTypeVO issueTypeVO) {
        if (ObjectUtils.isEmpty(issueTypeVO)) {
            return;
        }
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (Objects.equals(issueDTO.getTypeCode(), issueTypeVO.getTypeCode())) {
            return;
        }
        IssueConvertDTO issueConvertDTO = modelMapper.map(issueDTO, IssueConvertDTO.class);
        issueConvertDTO.setProjectId(issueDTO.getProjectId());
        if ((Objects.equals(issueDTO.getTypeCode(), STORY_TYPE) || Objects.equals(issueDTO.getTypeCode(), TASK_TYPE))
                && (!Objects.equals(issueTypeVO.getTypeCode(), STORY_TYPE) && !Objects.equals(issueTypeVO.getTypeCode(), TASK_TYPE))) {
            issueMapper.updateSubBugRelateIssueId(issueDTO.getProjectId(), issueDTO.getIssueId());
        }
        if (issueTypeVO.getTypeCode().equals(ISSUE_EPIC)) {
            issueConvertDTO.setRank(null);
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
        } else {
            issueConvertDTO.setTypeCode(issueTypeVO.getTypeCode());
        }
        issueConvertDTO.setIssueTypeId(issueTypeVO.getId());
        issueAccessDataService.update(issueConvertDTO, new String[]{TYPE_CODE_FIELD, REMAIN_TIME_FIELD, PARENT_ISSUE_ID, EPIC_NAME_FIELD, COLOR_CODE_FIELD, EPIC_ID_FIELD, STORY_POINTS_FIELD, RANK_FIELD, EPIC_SEQUENCE, ISSUE_TYPE_ID, RELATE_ISSUE_ID});
    }

    private void calculationRank(Long projectId, IssueConvertDTO issueConvertDTO) {
        if (sprintValidator.hasIssue(projectId, issueConvertDTO.getSprintId())) {
            String rank = sprintMapper.queryMaxRank(projectId, issueConvertDTO.getSprintId());
            issueConvertDTO.setRank(RankUtil.genNext(rank));
        } else {
            issueConvertDTO.setRank(RankUtil.mid());
        }
    }

    private void handlerNeedTransferValue(ProjectVO projectVO, Long issueId, ProjectVO targetProjectVO, JSONObject jsonObject) {
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueId);
        if (ObjectUtils.isEmpty(issueDTO)) {
            throw new CommonException(ISSUE_NULL);
        }
        Map<String,Object> programValueMap = new HashMap<>();
        if (agilePluginService != null) {
            agilePluginService.handlerProgramPredefinedFields(targetProjectVO.getId(),jsonObject,programValueMap,issueDTO.getApplyType());
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
        IssueDTO issue = buildIssue(projectVO.getId(), issueDTO, issueUpdateVO.getStatusId(), targetProjectVO.getId());
        IssueDTO dto = issueAccessDataService.transferProject(issue);
        // 更新issue的值
        if (!ObjectUtils.isEmpty(jsonObject)) {
            issueUpdateVO.setIssueId(issueId);
            issueUpdateVO.setObjectVersionNumber(dto.getObjectVersionNumber());
            issueService.updateIssue(targetProjectVO.getId(), issueUpdateVO, fieldList);
            if (!ObjectUtils.isEmpty(influenceVersions)) {
                List<VersionIssueRelVO> list = EncryptionUtils.jsonToList(influenceVersions, VersionIssueRelVO.class);
                IssueUpdateVO issueUpdateVO1 = new IssueUpdateVO();
                issueUpdateVO1.setIssueId(issueId);
                issueUpdateVO1.setVersionIssueRelVOList(list);
                issueUpdateVO1.setVersionType("influence");
                issueService.updateIssue(targetProjectVO.getId(), issueUpdateVO1, new ArrayList<>());
            }
        }
        // 修改自定义字段的值
        if (!ObjectUtils.isEmpty(customFields)) {
            addCustomFieldValues(targetProjectVO, issueDTO, customFields);
        }
        // 修改agile_feature和wsjf表中数据的projectId
        if (agilePluginService != null) {
            agilePluginService.projectMoveUpdateFeatureValue(projectVO.getId(), issueDTO, targetProjectVO.getId());
            agilePluginService.handlerFeatureField(targetProjectVO.getId(),issueDTO,programValueMap);
        }
    }

    private void addCustomFieldValues(ProjectVO targetProjectVO, IssueDTO issueDTO, Object customFields) {
        List<PageFieldViewUpdateVO> list = EncryptionUtils.jsonToList(customFields, PageFieldViewUpdateVO.class);
        List<Long> fieldIds = list.stream().map(PageFieldViewUpdateVO::getFieldId).collect(Collectors.toList());
        List<Long> existFields = objectSchemeFieldMapper.filterNotExistFields(fieldIds);
        if (CollectionUtils.isEmpty(existFields)) {
            return;
        }
        list = list.stream().filter(v -> existFields.contains(v.getFieldId())).collect(Collectors.toList());
        fieldValueService.handlerCustomFields(targetProjectVO.getId(), list, AGILE_SCHEME_CODE, Arrays.asList(issueDTO.getIssueId()), null, false);
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
        //获取状态机id
        Long stateMachineId = projectConfigService.queryStateMachineId(projectId, issueDTO.getApplyType(), issueDTO.getIssueTypeId());
        if (stateMachineId == null) {
            throw new CommonException("error.createIssue.stateMachineNotFound");
        }
        //获取初始状态
        if (ObjectUtils.isEmpty(status)) {
            statusId = instanceService.queryInitStatusId(organizationId, stateMachineId);
            if (statusId == null) {
                throw new CommonException("error.init.status.not.found");
            }
        }
        issue.setStatusId(statusId);
        // 设置issueNum
        String newIssueNum = IssueNumUtil.getNewIssueNum(targetProjectId).toString();
        issue.setIssueNum(newIssueNum);
        projectInfoMapper.updateIssueMaxNum(targetProjectId,newIssueNum);
        issue.setObjectVersionNumber(issueDTO.getObjectVersionNumber());
        if (Objects.equals(issueDTO.getTypeCode(), ISSUE_EPIC)) {
            Integer sequence = issueMapper.queryMaxEpicSequenceByProject(targetProjectId);
            issue.setEpicSequence(sequence);
        }
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
        IssueVO issueVO = issueService.updateIssue(projectId, issueUpdateVO, field);
        if (Objects.equals(issueDTO.getTypeCode(), "bug")) {
            IssueUpdateVO issueUpdateVO1 = new IssueUpdateVO();
            issueUpdateVO1.setVersionIssueRelVOList(new ArrayList<>());
            issueUpdateVO1.setVersionType("influence");
            issueUpdateVO1.setIssueId(issueDTO.getIssueId());
            issueUpdateVO1.setObjectVersionNumber(issueVO.getObjectVersionNumber());
            issueService.updateIssue(projectId, issueUpdateVO1, Collections.emptyList());
        }
        if ((ISSUE_EPIC).equals(issueDTO.getTypeCode())) {
            //如果是epic，会把该epic下的issue的epicId置为0
            issueAccessDataService.batchUpdateIssueEpicId(projectId, issueDTO.getIssueId());
        } else {
            redisUtil.deleteRedisCache(new String[]{"Agile:EpicChart" + projectId + ":" + issueDTO.getEpicId() + ":" + "*"});
        }
    }

    private void handlerNeedCleanValue(ProjectVO projectVO, IssueDetailDTO issueDTO, IssueTypeVO issueTypeVO, ProjectVO targetProjectVO) {
        // 清空测试用例
        if (feignUtil.isExist(FeignUtil.TEST_MANAGER_SERVICE)) {
            testFeignClient.deleteTestRel(projectVO.getId(), issueDTO.getIssueId());
        }
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
        // 清空自定义字段的值
        handlerCustomFieldValue(projectVO, issueDTO, issueTypeVO, targetProjectVO);
    }

    private void handlerCustomFieldValue(ProjectVO projectVO, IssueDetailDTO issueDTO, IssueTypeVO issueTypeVO, ProjectVO targetProjectVO) {
        List<FieldValueDTO> fieldValueDTOS = fieldValueMapper.queryList(projectVO.getId(), issueDTO.getIssueId(), AGILE_SCHEME_CODE, null);
        if(!CollectionUtils.isEmpty(fieldValueDTOS)){
            // 查询目标项目指定问题类型的字段
            String typeCode = ObjectUtils.isEmpty(issueTypeVO) ? issueDTO.getTypeCode() : issueTypeVO.getTypeCode();
            PageConfigVO pageConfigVO = objectSchemeFieldService.listConfigs(targetProjectVO.getOrganizationId(), targetProjectVO.getId(), typeCode);
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
