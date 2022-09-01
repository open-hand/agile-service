package io.choerodon.agile.api.validator;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.*;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.agile.infra.utils.EnumUtil;
import io.choerodon.core.exception.CommonException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/8/9.
 * Email: fuqianghuang01@gmail.com
 */
@Component
public class IssueValidator {

    @Autowired
    private ProductVersionMapper productVersionMapper;

    private static final String ERROR_ISSUE_GET = "error.issue.get";
    private static final String ERROR_TYPECODE_ISSUBTASK = "error.typeCode.isSubtask";
    private static final String ERROR_PARENT_ISSUE_ISSUBTASK = "error.parentIssue.isSubtask";
    private static final String ERROR_PARENT_ISSUE_ISTEST = "error.parentIssue.isTest";
    private static final String ERROR_PARENT_ISSUE_NOT_EXIST = "error.parentIssue.get";
    private static final String ERROR_ISSUE_RULE_TYPE_CODE= "error.IssueRule.typeCode";
    private static final String ERROR_ISSUE_TYPE_ID_IS_NULL_OR_ILLEGAL = "error.issueTypeId.isNull.or.illegal";
    private static final String ISSUE_ID = "issueId";
    private static final String COLOR = "color";
    private static final String EPIC_NAME = "epicName";
    private static final String ISSUE_EPIC = "issue_epic";
    private static final String SUB_TASK = "sub_task";
    private static final String STORY = "story";
    private static final String STATUS_ID = "status_id";
    private static final String ERROR_ISSUE_ID_NOT_FOUND = "error.IssueRule.issueId";
    private static final String ERROR_APPLY_TYPE_IS_NULL_OR_ILLEGAL= "error.applyType.isNull.or.illegal";
    private static final String AGILE = "agile";
    private static final String EPIC_ID = "epicId";
    private static final String SPRINT_ID_FIELD = "sprintId";
    private static final String STORY_POINTS_FIELD = "storyPoints";
    private static final String REMAIN_TIME_FIELD = "remainingTime";
    private static final String ASSIGNEE_ID = "assigneeId";
    private static final String FEATURE_ID = "featureId";
    private static final String ENVIRONMENT = "environment";
    private static final String MAIN_RESPONSIBLE_ID = "mainResponsibleId";
    private static final String ESTIMATED_START_TIME = "estimatedStartTime";
    private static final String ACTUAL_START_TIME = "actualStartTime";
    private static final String ACTUAL_END_TIME = "actualEndTime";
    private static final String ESTIMATED_END_TIME = "estimatedEndTime";
    private static final String COMPONENT = "component";
    private static final String FIX_VERSION = "fixVersion";
    private static final String INFLUENCE_VERSION = "influenceVersion";
    private static final String LABEL = "label";
    private static final String TAG = "tag";
    private static final String REPORTER_ID = "reporterId";
    private static final String PRIORITY_ID = "priorityId";
    private static final String STATUS_ID_FIELD = "statusId";

    @Autowired
    private IssueService issueService;

    @Autowired
    private IssueMapper issueMapper;

    @Autowired
    private IssueStatusMapper issueStatusMapper;

    @Autowired
    private VersionIssueRelMapper versionIssueRelMapper;

    @Autowired
    private ComponentIssueRelMapper componentIssueRelMapper;

    @Autowired
    private LabelIssueRelMapper labelIssueRelMapper;

    @Autowired
    private ProjectConfigService projectConfigService;

    @Autowired
    private StateMachineTransformService transformService;

    @Autowired
    private StatusTransferSettingService statusTransferSettingService;

    @Autowired(required = false)
    private AgilePluginService agilePluginService;

    @Autowired
    private PriorityMapper priorityMapper;

    @Autowired
    private IssueTypeMapper issueTypeMapper;


    public void verifyCreateData(IssueCreateVO issueCreateVO, Long projectId, String applyType) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        issueCreateVO.setProjectId(projectId);
        if (issueCreateVO.getTypeCode() == null) {
            throw new CommonException(ERROR_ISSUE_RULE_TYPE_CODE);
        }
        if (ObjectUtils.isEmpty(issueCreateVO.getSummary())) {
            throw new CommonException("error.IssueRule.Summary");
        }
        if (issueCreateVO.getSummary().length() > IssueConstant.SUMMARY_LENGTH) {
            throw new CommonException("error.summary.too.long");
        }
        if (issueCreateVO.getPriorityCode() == null) {
            throw new CommonException("error.IssueRule.PriorityCode");
        }
        if (issueCreateVO.getProjectId() == null) {
            throw new CommonException("error.IssueRule.ProjectId");
        }
        if (issueCreateVO.getEpicName() != null && !ISSUE_EPIC.equals(issueCreateVO.getTypeCode())) {
            throw new CommonException("error.IssueRule.EpicName");
        }
        if (issueCreateVO.getPriorityId() == null ||
                (Objects.equals(AGILE, applyType) && Objects.isNull(priorityMapper.selectOne(new PriorityDTO(organizationId, issueCreateVO.getPriorityId()))))) {
            throw new CommonException("error.priorityId.isNull.or.illegal");
        }
        if (issueCreateVO.getIssueTypeId() == null || Objects.isNull(issueTypeMapper.selectOne(new IssueTypeDTO(organizationId, issueCreateVO.getIssueTypeId())))) {
            throw new CommonException(ERROR_ISSUE_TYPE_ID_IS_NULL_OR_ILLEGAL);
        }
        if (issueCreateVO.getStatusId() != null && Objects.isNull(issueStatusMapper.selectByStatusId(projectId, issueCreateVO.getStatusId()))) {
            throw new CommonException("error.statusId.illegal");
        }
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
            throw new CommonException("error.applyType.illegal");
        }
        if (IssueTypeCode.isSubTask(issueCreateVO.getTypeCode())
                && (ObjectUtils.isEmpty(issueCreateVO.getParentIssueId())
                || Objects.equals(0L, issueCreateVO.getParentIssueId()))) {
            throw new CommonException("error.parentIssueId.null");
        }
    }

    public void verifyIssueUpdateStatus(Long projectId, Long issueId, Long transformId){
        StatusMachineTransformDTO statusMachineTransformDTO = transformService.queryDeployTransformForAgile(ConvertUtil.getOrganizationId(projectId), transformId);
        if (ObjectUtils.isEmpty(statusMachineTransformDTO)) {
            throw new CommonException("error.transfrom.not.exist");
        }
        Long endStatusId = statusMachineTransformDTO.getEndStatusId();
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setIssueId(issueId);
        issueDTO.setProjectId(projectId);
        issueDTO = issueMapper.selectByPrimaryKey(issueDTO);
        if (ObjectUtils.isEmpty(issueDTO)) {
            throw new CommonException(ERROR_ISSUE_ID_NOT_FOUND);
        }
        statusTransferSettingService.checkStatusTransferSetting(projectId,issueDTO,endStatusId);
    }

    public void verifyUpdateData(JSONObject issueUpdate, Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        if (issueUpdate.get(ISSUE_ID) == null) {
            throw new CommonException(ERROR_ISSUE_ID_NOT_FOUND);
        }
        Long issueId = decrypt(issueUpdate.get(ISSUE_ID).toString());
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setIssueId(issueId);
        issueDTO.setProjectId(projectId);
        issueDTO = issueMapper.selectByPrimaryKey(issueDTO);
        if (issueDTO == null) {
            throw new CommonException(ERROR_ISSUE_ID_NOT_FOUND);
        }
        //不是epic类型的，不能修改颜色
        if (issueUpdate.get(COLOR) != null && !ISSUE_EPIC.equals(issueDTO.getTypeCode())) {
            throw new CommonException("error.IssueRule.color");
        }
        //不是epic类型的，不能修改epicName
        if (issueUpdate.get(EPIC_NAME) != null && !ISSUE_EPIC.equals(issueDTO.getTypeCode())) {
            throw new CommonException("error.IssueRule.EpicName");
        }
        //修改状态要有当前状态
        if (issueUpdate.get(STATUS_ID) != null && issueStatusMapper.selectByStatusId(projectId, decrypt(issueUpdate.get(STATUS_ID).toString())) == null) {
            throw new CommonException("error.IssueRule.statusId");
        }

        if (issueUpdate.containsKey("rank") && ObjectUtils.isEmpty(issueUpdate.get("rank"))) {
            throw new CommonException("error.issue.rank.null");
        }

        if (issueUpdate.containsKey(PRIORITY_ID) && (ObjectUtils.isEmpty(issueUpdate.get(PRIORITY_ID))
                || Objects.isNull(priorityMapper.selectOne(new PriorityDTO(organizationId, decrypt(issueUpdate.get(PRIORITY_ID).toString())))))) {
            throw new CommonException("error.issue.priorityId.illegal");
        }

        if (issueUpdate.containsKey(STATUS_ID_FIELD) && (ObjectUtils.isEmpty(issueUpdate.get(STATUS_ID_FIELD))
                || Objects.isNull(issueStatusMapper.selectByStatusId(projectId, decrypt(issueUpdate.get(STATUS_ID_FIELD).toString()))))) {
            throw new CommonException("error.issue.statusId.illegal");
        }

        //史诗校验
        if (issueUpdate.containsKey(EPIC_ID) ) {
            if (ObjectUtils.isEmpty(issueUpdate.get(EPIC_ID))) {
                throw new CommonException("error.issue.epic.null");
            }
            judgeEpicCanUpdateAndExist(projectId, decrypt(issueUpdate.get(EPIC_ID).toString()));
        }

        // 概要校验
        if (issueUpdate.containsKey(FieldCode.SUMMARY) && ObjectUtils.isEmpty(issueUpdate.get(FieldCode.SUMMARY).toString())) {
            throw new CommonException("error.IssueRule.Summary");
        }
        if (issueUpdate.containsKey(FieldCode.SUMMARY) && issueUpdate.get(FieldCode.SUMMARY).toString().length() > IssueConstant.SUMMARY_LENGTH) {
            throw new CommonException("error.summary.too.long");
        }
    }

    public void verifySubCreateData(IssueSubCreateVO issueSubCreateVO, Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        if (ObjectUtils.isEmpty(issueSubCreateVO.getSummary())) {
            throw new CommonException("error.IssueRule.Summary");
        }
        // 校验概要长度
        if (issueSubCreateVO.getSummary().length() > IssueConstant.SUMMARY_LENGTH) {
            throw new CommonException("error.summary.too.long");
        }
        if (issueSubCreateVO.getPriorityCode() == null) {
            throw new CommonException("error.IssueRule.PriorityCode");
        }
        if (issueSubCreateVO.getProjectId() == null) {
            throw new CommonException("error.IssueRule.ProjectId");
        }
        if (issueSubCreateVO.getPriorityId() == null || Objects.isNull(priorityMapper.selectOne(new PriorityDTO(organizationId, issueSubCreateVO.getPriorityId())))) {
            throw new CommonException("error.priorityId.isNull.or.illegal");
        }
        if (issueSubCreateVO.getIssueTypeId() == null || Objects.isNull(issueTypeMapper.selectOne(new IssueTypeDTO(organizationId, issueSubCreateVO.getIssueTypeId())))) {
            throw new CommonException(ERROR_ISSUE_TYPE_ID_IS_NULL_OR_ILLEGAL);
        }
        if (issueSubCreateVO.getStatusId() != null && Objects.isNull(issueStatusMapper.selectByStatusId(projectId, issueSubCreateVO.getStatusId()))) {
            throw new CommonException("error.statusId.is.illegal");
        }
        if (issueSubCreateVO.getParentIssueId() == null) {
            throw new CommonException("error.IssueRule.ParentIssueId");
        }
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setProjectId(projectId);
        issueDTO.setIssueId(issueSubCreateVO.getParentIssueId());
        IssueDTO query = issueMapper.selectOne(issueDTO);
        if (query != null) {
            issueSubCreateVO.setProjectId(projectId);
        } else {
            throw new CommonException("error.IssueRule.issueNoFound");
        }
    }

    public void judgeEpicCanUpdateAndExist(Long projectId, Long epicId) {
        ProjectVO project = ConvertUtil.queryProject(projectId);
        Set<String> categoryCodes = new HashSet<>(ProjectCategory.getProjectCategoryCodes(project));
        boolean isOnlyAgile =
                categoryCodes.contains(ProjectCategory.MODULE_AGILE)
                        && !categoryCodes.contains(ProjectCategory.MODULE_PROGRAM);
        if (agilePluginService != null
                && isOnlyAgile
                && agilePluginService.isSubProjectAndArtDoing(projectId)) {
            throw new CommonException("error.issue.can.not.update.epic");
        }
        if (epicId != null && !Objects.equals(epicId, 0L)) {
            IssueDTO issueDTO = new IssueDTO();
            issueDTO.setProjectId(projectId);
            issueDTO.setTypeCode(ISSUE_EPIC);
            issueDTO.setIssueId(epicId);
            if (issueMapper.selectOne(issueDTO) == null) {
                throw new CommonException("error.epic.notFound");
            }
        }
    }


    public void verifyLabelIssueData(LabelIssueRelDTO labelIssueRelDTO) {
        if (labelIssueRelDTO.getProjectId() == null) {
            throw new CommonException("error.label.ProjectId");
        } else if (labelIssueRelDTO.getLabelName() == null && labelIssueRelDTO.getLabelId() == null) {
            throw new CommonException("error.label.LabelName");
        }
    }

    public void verifyVersionIssueRelData(VersionIssueRelDTO versionIssueRelDTO) {
        if (versionIssueRelDTO.getName() == null && versionIssueRelDTO.getVersionId() == null) {
            throw new CommonException("error.versionIssueRel.Name");
        }
    }

    public void verifyComponentIssueRelData(ComponentIssueRelDTO componentIssueRelDTO) {
        if (componentIssueRelDTO.getComponentId() == null && componentIssueRelDTO.getName() == null) {
            throw new CommonException("error.componentIssueRelE.Name");
        }
    }

    public IssueConvertDTO verifyUpdateTypeData(Long projectId, IssueUpdateTypeVO issueUpdateTypeVO) {
        if (issueUpdateTypeVO.getIssueId() == null) {
            throw new CommonException(ERROR_ISSUE_ID_NOT_FOUND);
        }
        if (issueUpdateTypeVO.getApplyType() == null || !EnumUtil.contain(SchemeApplyType.class, issueUpdateTypeVO.getApplyType())) {
            throw new CommonException(ERROR_APPLY_TYPE_IS_NULL_OR_ILLEGAL);
        }
        if (issueUpdateTypeVO.getIssueTypeId() == null ||
                Objects.isNull(issueTypeMapper.selectOne(new IssueTypeDTO(ConvertUtil.getOrganizationId(projectId), issueUpdateTypeVO.getIssueTypeId())))) {
            throw new CommonException(ERROR_ISSUE_TYPE_ID_IS_NULL_OR_ILLEGAL);
        }
        if (issueUpdateTypeVO.getTypeCode() == null) {
            throw new CommonException(ERROR_ISSUE_RULE_TYPE_CODE);
        }
        IssueConvertDTO issueConvertDTO = issueService.queryIssueByProjectIdAndIssueId(projectId, issueUpdateTypeVO.getIssueId());
        if (issueConvertDTO == null) {
            throw new CommonException("error.IssueUpdateTypeVO.issueDO");
        }
        Long originStateMachineId = projectConfigService.queryStateMachineId(projectId, issueUpdateTypeVO.getApplyType(), issueConvertDTO.getIssueTypeId());
        Long currentStateMachineId = projectConfigService.queryStateMachineId(projectId, issueUpdateTypeVO.getApplyType(), issueUpdateTypeVO.getIssueTypeId());
        if (originStateMachineId == null || currentStateMachineId == null) {
            throw new CommonException("error.IssueRule.stateMachineId");
        }
        return issueConvertDTO;
    }

    public Boolean existVersionIssueRel(VersionIssueRelDTO versionIssueRelDTO) {
        VersionIssueRelDTO versionIssueRel = new VersionIssueRelDTO();
        versionIssueRel.setVersionId(versionIssueRelDTO.getVersionId());
        versionIssueRel.setIssueId(versionIssueRelDTO.getIssueId());
        versionIssueRel.setRelationType(versionIssueRelDTO.getRelationType());
        return versionIssueRelMapper.selectOne(versionIssueRel) == null;
    }

    public Boolean existComponentIssueRel(ComponentIssueRelDTO componentIssueRelDTO) {
        ComponentIssueRelDTO componentIssueRel = new ComponentIssueRelDTO();
        componentIssueRel.setIssueId(componentIssueRelDTO.getIssueId());
        componentIssueRel.setComponentId(componentIssueRelDTO.getComponentId());
        return componentIssueRelMapper.selectOne(componentIssueRel) == null;
    }

    public Boolean existLabelIssue(LabelIssueRelDTO labelIssueRelDTO) {
        LabelIssueRelDTO labelIssueRel = new LabelIssueRelDTO();
        labelIssueRel.setLabelId(labelIssueRelDTO.getLabelId());
        labelIssueRel.setIssueId(labelIssueRelDTO.getIssueId());
        return labelIssueRelMapper.selectOne(labelIssueRel) == null;
    }

    public void verifyTransformedSubTask(Long projectId, IssueTransformSubTask issueTransformSubTask) {
        if (issueTransformSubTask.getIssueId() == null) {
            throw new CommonException(ERROR_ISSUE_ID_NOT_FOUND);
        }
        if (issueTransformSubTask.getIssueTypeId() == null ||
                Objects.isNull(issueTypeMapper.selectOne( new IssueTypeDTO(ConvertUtil.getOrganizationId(projectId), issueTransformSubTask.getIssueTypeId())))) {
            throw new CommonException(ERROR_ISSUE_TYPE_ID_IS_NULL_OR_ILLEGAL);
        }
        if (issueTransformSubTask.getStatusId() == null ||
                Objects.isNull(issueStatusMapper.selectByStatusId(projectId, issueTransformSubTask.getStatusId()))) {
            throw new CommonException("error.statusId.is.null.or.illegal");
        }
        if (issueTransformSubTask.getParentIssueId() == null) {
            throw new CommonException("error.IssueRule.parentIssueId");
        }
        if (issueTransformSubTask.getObjectVersionNumber() == null) {
            throw new CommonException("error.IssueRule.objectVersionNumber");
        }
    }

    public IssueConvertDTO verifyTransformedTask(Long projectId, IssueTransformTask issueTransformTask) {
        if (issueTransformTask.getIssueId() == null) {
            throw new CommonException(ERROR_ISSUE_ID_NOT_FOUND);
        }
        if (issueTransformTask.getIssueTypeId() == null ||
                Objects.isNull(issueTypeMapper.selectOne(new IssueTypeDTO(ConvertUtil.getOrganizationId(projectId), issueTransformTask.getIssueTypeId())))) {
            throw new CommonException(ERROR_ISSUE_TYPE_ID_IS_NULL_OR_ILLEGAL);
        }
        if (issueTransformTask.getTypeCode() == null) {
            throw new CommonException(ERROR_ISSUE_RULE_TYPE_CODE);
        }
        if (issueTransformTask.getTypeCode().equals(ISSUE_EPIC) && issueTransformTask.getEpicName() == null) {
            throw new CommonException("error.IssueRule.epicName");
        }
        IssueConvertDTO issueConvertDTO = issueService.queryIssueByProjectIdAndIssueId(projectId, issueTransformTask.getIssueId());
        if (issueConvertDTO == null) {
            throw new CommonException("error.IssueUpdateTypeVO.issueDO");
        }
        return issueConvertDTO;
    }

    public void verifySubTask(Long parentIssueId) {
        IssueDTO issueDTO = new IssueDTO();
        issueDTO.setIssueId(parentIssueId);
        IssueDTO query = issueMapper.selectByPrimaryKey(issueDTO);
        if (query == null) {
            throw new CommonException("error.IssueRule.issueNoFound");
        } else if (query.getTypeCode().equals(SUB_TASK)) {
            throw new CommonException("error.IssueRule.parentIssueId");
        }
    }

    public void verifyStoryPoints(IssueConvertDTO issueConvertDTO) {
        if (issueConvertDTO.getStoryPoints() != null && !(STORY.equals(issueConvertDTO.getTypeCode()))) {
            throw new CommonException("error.IssueRule.onlyStory");
        }
    }

    public static void checkParentIdUpdate(IssueDTO issueDTO, IssueDTO parentIssueDTO) {
        if (issueDTO == null) {
            throw new CommonException(ERROR_ISSUE_GET);
        }
        if (parentIssueDTO == null) {
            throw new CommonException(ERROR_PARENT_ISSUE_NOT_EXIST);
        }
        String typeCode = issueDTO.getTypeCode();
        if (!SUB_TASK.equals(typeCode)) {
            throw new CommonException(ERROR_TYPECODE_ISSUBTASK);
        }
        typeCode = parentIssueDTO.getTypeCode();
        if (SUB_TASK.equals(typeCode)) {
            throw new CommonException(ERROR_PARENT_ISSUE_ISSUBTASK);
        }
        if (SchemeApplyType.TEST.equals(issueDTO.getApplyType())) {
            throw new CommonException(ERROR_PARENT_ISSUE_ISTEST);
        }
    }

    public void checkIssueCreate(IssueCreateVO issueCreateVO, String applyType) {
        if (!EnumUtil.contain(SchemeApplyType.class, applyType)) {
            throw new CommonException("error.applyType.illegal");
        }
        if (SchemeApplyType.AGILE.equals(applyType) && issueCreateVO.getEpicName() != null
                && issueService.checkEpicName(issueCreateVO.getProjectId(), issueCreateVO.getEpicName(), null)) {
            throw new CommonException("error.epicName.exist");
        }
        if (issueCreateVO.getRankVO() != null) {
            RankVO rankVO = issueCreateVO.getRankVO();
            if (rankVO.getReferenceIssueId() == null) {
                throw new CommonException("error.referenceIssueId.isNull");
            }
            if (rankVO.getBefore() == null) {
                throw new CommonException("error.before.isNull");
            }
            if (rankVO.getType() == null) {
                throw new CommonException("error.type.isNull");
            }
            if (rankVO.getProjectId() == null) {
                throw new CommonException("error.projectId.isNull");
            }
        }
    }

    public void checkPredefinedFields(List<String> predefinedFieldNames) {
        if (!CollectionUtils.isEmpty(predefinedFieldNames)) {
            for (String fieldName: predefinedFieldNames) {
                String field = IssueFieldMapping.getCloneFieldCodeByInputField(fieldName);
                if (field == null) {
                    throw new CommonException("error.copy.issue.illegal.field");
                }
            }
        }
    }

    public void verifybatchUpdateFieldsValue(Long projectId, BatchUpdateFieldsValueVo batchUpdateFieldsValueVo, String applyType) {
        JSONObject predefinedFields = batchUpdateFieldsValueVo.getPredefinedFields();
        if (!CollectionUtils.isEmpty(predefinedFields)) {
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            if (predefinedFields.containsKey(STATUS_ID_FIELD) && (ObjectUtils.isEmpty(predefinedFields.get(STATUS_ID_FIELD))
                    || Objects.isNull(issueStatusMapper.selectByStatusId(projectId, decrypt(predefinedFields.get(STATUS_ID_FIELD).toString()))))) {
                throw new CommonException("error.statusId.illegal");
            }
            if (Objects.equals(AGILE, applyType) && predefinedFields.containsKey(PRIORITY_ID) && (ObjectUtils.isEmpty(predefinedFields.get(PRIORITY_ID))
                    || Objects.isNull(priorityMapper.selectOne(new PriorityDTO(organizationId, decrypt(predefinedFields.get(PRIORITY_ID).toString())))))) {
                    throw new CommonException("error.priorityId.illegal");
            }
         }
    }

    private Long decrypt(String id) {
        return EncryptionUtils.decrypt(id, EncryptionUtils.BLANK_KEY);
    }
}
