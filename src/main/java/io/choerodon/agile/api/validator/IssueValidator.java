package io.choerodon.agile.api.validator;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueCreateVO;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.choerodon.agile.app.service.StateMachineTransformService;
import io.choerodon.agile.app.service.StatusTransferSettingService;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.SchemeApplyType;
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
    private static final String ISSUE_ID = "issueId";
    private static final String COLOR = "color";
    private static final String EPIC_NAME = "epicName";
    private static final String ISSUE_EPIC = "issue_epic";
    private static final String SUB_TASK = "sub_task";
    private static final String STORY = "story";
    private static final String STATUS_ID = "status_id";
    private static final String ERROR_ISSUE_ID_NOT_FOUND = "error.IssueRule.issueId";
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
    private static final String ESTIMATED_END_TIME = "estimatedEndTime";
    private static final String COMPONENT = "component";
    private static final String FIX_VERSION = "fixVersion";
    private static final String INFLUENCE_VERSION = "influenceVersion";
    private static final String LABEL = "label";
    private static final String TAG = "tag";
    private static final String REPORTER_ID = "reporterId";
    private static final String PRIORITY_ID = "priorityId";
    private static final String STATUS_ID_FIELD = "statusId";

    private static final String[] LEGAL_COPY_PREDEFINED_FIELDS_NAME = new String[]
            {
                    ASSIGNEE_ID, EPIC_ID, STORY_POINTS_FIELD, FEATURE_ID,
                    ENVIRONMENT, MAIN_RESPONSIBLE_ID, REMAIN_TIME_FIELD,
                    ESTIMATED_START_TIME, ESTIMATED_END_TIME, SPRINT_ID_FIELD,
                    COMPONENT, LABEL, FIX_VERSION, INFLUENCE_VERSION, TAG,
                    REPORTER_ID, PRIORITY_ID, STATUS_ID_FIELD
            };

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


    public void verifyCreateData(IssueCreateVO issueCreateVO, Long projectId, String applyType) {
        issueCreateVO.setProjectId(projectId);
        if (issueCreateVO.getTypeCode() == null) {
            throw new CommonException(ERROR_ISSUE_RULE_TYPE_CODE);
        }
        if (issueCreateVO.getSummary() == null) {
            throw new CommonException("error.IssueRule.Summary");
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
        if (issueCreateVO.getPriorityId() == null) {
            throw new CommonException("error.priorityId.isNull");
        }
        if (issueCreateVO.getIssueTypeId() == null) {
            throw new CommonException("error.issueTypeId.isNull");
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
        if (issueUpdate.get(ISSUE_ID) == null) {
            throw new CommonException(ERROR_ISSUE_ID_NOT_FOUND);
        }
        Long issueId = EncryptionUtils.decrypt(issueUpdate.get(ISSUE_ID).toString(), EncryptionUtils.BLANK_KEY);
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
        if (issueUpdate.get(STATUS_ID) != null && issueStatusMapper.selectByPrimaryKey(EncryptionUtils.decrypt(issueUpdate.get(STATUS_ID).toString(), EncryptionUtils.BLANK_KEY)) == null) {
            throw new CommonException("error.IssueRule.statusId");
        }

        if (issueUpdate.containsKey("rank") && ObjectUtils.isEmpty(issueUpdate.get("rank"))) {
            throw new CommonException("error.issue.rank.null");
        }

        if (issueUpdate.containsKey(PRIORITY_ID) && ObjectUtils.isEmpty(issueUpdate.get(PRIORITY_ID))) {
            throw new CommonException("error.issue.priorityId.null");
        }

        if (issueUpdate.containsKey(STATUS_ID_FIELD) && ObjectUtils.isEmpty(issueUpdate.get(PRIORITY_ID))) {
            throw new CommonException("error.issue.statusId.null");
        }

        //判断要关联的史诗是否存在
        if (issueUpdate.containsKey(EPIC_ID) ) {
            if (ObjectUtils.isEmpty(issueUpdate.get(EPIC_ID))) {
                throw new CommonException("error.issue.epic.null");
            }
            //可能为项目群史诗，故不传projectId
            this.judgeExist(null, EncryptionUtils.decrypt(issueUpdate.get(EPIC_ID).toString(), EncryptionUtils.BLANK_KEY));
        }
    }

    public void verifySubCreateData(IssueSubCreateVO issueSubCreateVO, Long projectId) {
        if (issueSubCreateVO.getSummary() == null) {
            throw new CommonException("error.IssueRule.Summary");
        }
        if (issueSubCreateVO.getPriorityCode() == null) {
            throw new CommonException("error.IssueRule.PriorityCode");
        }
        if (issueSubCreateVO.getProjectId() == null) {
            throw new CommonException("error.IssueRule.ProjectId");
        }
        if (issueSubCreateVO.getPriorityId() == null) {
            throw new CommonException("error.priorityId.isNull");
        }
        if (issueSubCreateVO.getIssueTypeId() == null) {
            throw new CommonException("error.issueTypeId.isNull");
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

    public void judgeExist(Long projectId, Long epicId) {
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
        if (issueUpdateTypeVO.getIssueTypeId() == null) {
            throw new CommonException("error.issuetypeId.isNull");
        }
        if (issueUpdateTypeVO.getTypeCode() == null) {
            throw new CommonException(ERROR_ISSUE_RULE_TYPE_CODE);
        }
        IssueConvertDTO issueConvertDTO = issueService.queryIssueByProjectIdAndIssueId(projectId, issueUpdateTypeVO.getIssueId());
        if (issueConvertDTO == null) {
            throw new CommonException("error.IssueUpdateTypeVO.issueDO");
        }
        Long originStateMachineId = projectConfigService.queryStateMachineId(projectId, AGILE, issueConvertDTO.getIssueTypeId());
        Long currentStateMachineId = projectConfigService.queryStateMachineId(projectId, AGILE, issueUpdateTypeVO.getIssueTypeId());
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

    public void verifyTransformedSubTask(IssueTransformSubTask issueTransformSubTask) {
        if (issueTransformSubTask.getIssueId() == null) {
            throw new CommonException(ERROR_ISSUE_ID_NOT_FOUND);
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
        if (issueTransformTask.getIssueTypeId() == null) {
            throw new CommonException("error.issuetypeId.isNull");
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
            List<String> legalCopyPredefinedFields = Arrays.asList(LEGAL_COPY_PREDEFINED_FIELDS_NAME);
            for (String fieldName: predefinedFieldNames) {
                if (!legalCopyPredefinedFields.contains(fieldName)) {
                    throw new CommonException("error.copy.issue.illegal.field");
                }
            }
        }
    }
}
