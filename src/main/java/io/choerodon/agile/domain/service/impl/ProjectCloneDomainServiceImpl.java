package io.choerodon.agile.domain.service.impl;

import static io.choerodon.agile.domain.context.ProjectCloneContext.*;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.SetUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

import io.choerodon.agile.api.validator.StatusFieldSettingValidator;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.app.service.AgileWaterfallService;
import io.choerodon.agile.app.service.FilePathService;
import io.choerodon.agile.domain.context.ProjectCloneContext;
import io.choerodon.agile.domain.repository.IssueRepository;
import io.choerodon.agile.domain.service.ProjectCloneDomainService;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.enums.SchemeType;
import io.choerodon.agile.infra.feign.operator.CustomFileOperator;
import io.choerodon.agile.infra.feign.vo.FileVO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;

import org.hzero.boot.file.FileClient;
import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.AssertUtils;
import org.hzero.mybatis.domian.Condition;
import org.hzero.mybatis.util.Sqls;

/**
 * 项目复制 领域Service Impl
 *
 * @author gaokuo.dai@zknow.com 2023-05-06
 * @since 2.5
 */
@Service
public class ProjectCloneDomainServiceImpl implements ProjectCloneDomainService {

    @Autowired
    private IssueRepository issueRepository;
    @Autowired
    private ProductVersionMapper productVersionMapper;
    @Autowired
    private VersionIssueRelMapper versionIssueRelMapper;
    @Autowired
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private ComponentIssueRelMapper componentIssueRelMapper;
    @Autowired
    private IssueLabelMapper issueLabelMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired
    private IssueTypeExtendMapper issueTypeExtendMapper;
    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private FieldOptionMapper fieldOptionMapper;
    @Autowired
    private ObjectSchemeFieldExtendMapper objectSchemeFieldExtendMapper;
    @Autowired
    private IssueTypeFieldMapper issueTypeFieldMapper;
    @Autowired
    private FieldPermissionMapper fieldPermissionMapper;
    @Autowired
    private FieldCascadeRuleMapper fieldCascadeRuleMapper;
    @Autowired
    private FieldCascadeRuleOptionMapper fieldCascadeRuleOptionMapper;
    @Autowired
    private LabelIssueRelMapper labelIssueRelMapper;
    @Autowired
    private IssueLinkTypeMapper issueLinkTypeMapper;
    @Autowired
    private IssueLinkMapper issueLinkMapper;
    @Autowired
    private IssuePredecessorMapper issuePredecessorMapper;
    @Autowired
    private IssuePredecessorTreeClosureMapper issuePredecessorTreeClosureMapper;
    @Autowired
    private IssueAttachmentMapper issueAttachmentMapper;
    @Autowired
    private FilePathService filePathService;
    @Autowired
    private CustomFileOperator customFileOperator;
    @Autowired
    private FileClient fileClient;
    @Autowired
    private FieldValueMapper fieldValueMapper;
    @Autowired
    private ProjectConfigMapper projectConfigMapper;
    @Autowired
    private StateMachineSchemeMapper stateMachineSchemeMapper;
    @Autowired
    private StatusMachineSchemeConfigMapper statusMachineSchemeConfigMapper;
    @Autowired
    private StatusMachineMapper statusMachineMapper;
    @Autowired
    private StatusMachineNodeMapper statusMachineNodeMapper;
    @Autowired
    private StatusMachineTransformMapper statusMachineTransformMapper;
    @Autowired
    private IssueTypeSchemeMapper issueTypeSchemeMapper;
    @Autowired
    private IssueTypeSchemeConfigMapper issueTypeSchemeConfigMapper;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired
    private StatusBranchMergeSettingMapper statusBranchMergeSettingMapper;
    @Autowired
    private StatusTransferSettingMapper statusTransferSettingMapper;
    @Autowired
    private StatusLinkageMapper statusLinkageMapper;
    @Autowired
    private LinkIssueStatusLinkageMapper linkIssueStatusLinkageMapper;
    @Autowired
    private StatusFieldSettingMapper statusFieldSettingMapper;
    @Autowired
    private StatusFieldValueSettingMapper statusFieldValueSettingMapper;
    @Autowired
    private StatusNoticeSettingMapper statusNoticeSettingMapper;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;

    private final Logger logger = LoggerFactory.getLogger(ProjectCloneDomainServiceImpl.class);

    private static final String FIELD_CODE_COMPONENT = "component";
    private static final String FIELD_CODE_LABEL = "label";
    private static final String FIELD_CODE_INFLUENCE_VERSION = "influenceVersion";
    private static final String FIELD_CODE_FIX_VERSION = "fixVersion";

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void cloneProject(Long sourceProjectId, Long targetProjectId, ProjectCloneContext context) {
        if (sourceProjectId == null || targetProjectId == null) {
            return;
        }
        if (context == null) {
            context = new ProjectCloneContext();
        }
        final Set<String> categoryCodes = context.getCategoryCodes();
        // 复制projectInfo表
        cloneProjectInfo(sourceProjectId, targetProjectId, context);
        // 复制规划的版本
        cloneAgileProductVersion(sourceProjectId, targetProjectId, context);
        // 复制模块
        cloneAgileIssueComponent(sourceProjectId, targetProjectId, context);
        // 复制标签
        cloneAgileIssueLabel(sourceProjectId, targetProjectId, context);
        // 复制问题链接类型
        cloneAgileIssueLinkType(sourceProjectId, targetProjectId, context);
        // 复制工作项类型
        this.cloneIssueType(sourceProjectId, targetProjectId, context);
        // 复制自定义字段
        this.cloneField(sourceProjectId, targetProjectId, context);
        // 复制工作项类型的字段配置
        this.cloneIssueTypeFieldConfig(sourceProjectId, targetProjectId, context);
        // 复制状态和项目关系
        cloneAgileIssueStatus(sourceProjectId, targetProjectId, context);
        // 复制状态机
        cloneStatusMachine(sourceProjectId, targetProjectId, context);
        // 复制状态机自定义流转
        this.cloneStatusMachineCustomFlow(sourceProjectId, targetProjectId, context);
        // 复制issue本体
        cloneAgileIssue(sourceProjectId, targetProjectId, context);
        // 复制规划的版本与issue的关系
        cloneAgileVersionIssueRel(sourceProjectId, targetProjectId, context);
        // 复制模块与issue的关系
        cloneAgileComponentIssueRel(sourceProjectId, targetProjectId, context);
        // 复制标签与issue的关系
        cloneAgileLabelIssueRel(sourceProjectId, targetProjectId, context);
        // 复制关联工作项
        cloneAgileIssueLink(sourceProjectId, targetProjectId, context);
        // 复制工作项依赖关系
        cloneAgileIssuePredecessor(sourceProjectId, targetProjectId, context);
        // 复制工作项依赖关系树
        cloneAgileIssuePredecessorTreeClosure(sourceProjectId, targetProjectId, context);
        // 复制工作项附件
        cloneIssueAttachment(sourceProjectId, targetProjectId, context);
        // 复制工作项自定义字段值
        cloneCustomFieldValue(sourceProjectId, targetProjectId, context);

        // 复制瀑布插件数据
        if (categoryCodes.contains(ProjectCategory.MODULE_WATERFALL)) {
            if (agileWaterfallService != null) {
                agileWaterfallService.cloneProject(sourceProjectId, targetProjectId, context);
            }
        }
    }

    private void cloneProjectInfo(Long sourceProjectId,
                                  Long targetProjectId,
                                  ProjectCloneContext context) {
        ProjectInfoDTO sourceProjectInfo = projectInfoMapper.selectOne(new ProjectInfoDTO().setProjectId(sourceProjectId));
        Assert.notNull(sourceProjectInfo, "error.source.projectInfo.not.null");
        this.logger.debug("检测到可复制的 agile_project_info 数据1条, 开始复制");
        ProjectVO targetProject = context.queryProject(targetProjectId, TARGET_PROJECT);
        sourceProjectInfo.setInfoId(null);
        sourceProjectInfo.setProjectId(targetProjectId);
        sourceProjectInfo.setProjectCode(targetProject.getCode());
        if (projectInfoMapper.insert(sourceProjectInfo) != 1) {
            throw new CommonException("error.insert.agile_project_info");
        }
        this.logger.debug("agile_project_info 复制完成");
    }

    private void cloneAgileIssueStatus(Long sourceProjectId,
                                       Long targetProjectId,
                                       ProjectCloneContext context) {
        List<IssueStatusDTO> sourceIssueStatus = issueStatusMapper.select(new IssueStatusDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueStatus)) {
            this.logger.debug("没有检测到可复制的 agile_issue_status 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_status 数据{}条, 开始复制", sourceIssueStatus.size());
        for (IssueStatusDTO issueStatus : sourceIssueStatus) {
            issueStatus.setProjectId(targetProjectId);
            issueStatus.setId(null);
            if (issueStatusMapper.insert(issueStatus) != 1) {
                throw new CommonException("error.insert.agile_issue_status");
            }
        }
        this.logger.debug("agile_issue_status 复制完成");
    }

    private void cloneStatusMachine(Long sourceProjectId,
                                    Long targetProjectId,
                                    ProjectCloneContext context) {
        //查项目关联的状态机方案
        cloneFdProjectConfigAndFdStateMachineScheme(sourceProjectId, targetProjectId, context);
        //复制状态机方案配置、状态机、状态机节点、状态机节点转换
        cloneFdStatusMachineSchemeConfig(sourceProjectId, targetProjectId, context);
    }

    private void cloneFdStatusMachineSchemeConfig(Long sourceProjectId,
                                                  Long targetProjectId,
                                                  ProjectCloneContext context) {
        Set<Long> schemeIds =
                Optional.ofNullable(context.getByTable(TABLE_FD_STATE_MACHINE_SCHEME)).orElse(Collections.emptyMap()).keySet();
        for (Long schemeId : schemeIds) {
            List<StatusMachineSchemeConfigDTO> statusMachineSchemeConfigs = statusMachineSchemeConfigMapper.select(new StatusMachineSchemeConfigDTO().setSchemeId(schemeId));
            if (CollectionUtils.isEmpty(statusMachineSchemeConfigs)) {
                this.logger.debug("没有检测到可复制的 fd_status_machine_scheme_config 数据, 跳过此步骤");
                return;
            }
            this.logger.debug("检测到可复制的 fd_status_machine_scheme_config 数据{}条, 开始复制", statusMachineSchemeConfigs.size());
            for (StatusMachineSchemeConfigDTO statusMachineSchemeConfig : statusMachineSchemeConfigs) {
                Long sourceStateMachineId = statusMachineSchemeConfig.getStateMachineId();
                //复制状态机
                Long targetStateMachineId = cloneFdStatusMachine(sourceProjectId, targetProjectId, context, sourceStateMachineId);
                context.put(TABLE_FD_STATUS_MACHINE, sourceStateMachineId, targetStateMachineId);
                //复制状态机节点
                cloneFdStatusMachineNode(sourceStateMachineId, targetStateMachineId, context);
                //复制状态机转换
                cloneFdStatusMachineTransform(sourceStateMachineId, targetStateMachineId, context);
                Long sourceSchemeId = statusMachineSchemeConfig.getSchemeId();
                Long targetSchemeId = context.getByTableAndSourceId(TABLE_FD_STATE_MACHINE_SCHEME, sourceSchemeId);
                AssertUtils.notNull(targetSchemeId, "error.targetSchemeId.not.exist");
                statusMachineSchemeConfig.setId(null);
                statusMachineSchemeConfig.setSchemeId(targetSchemeId);
                statusMachineSchemeConfig.setStateMachineId(targetStateMachineId);
                Long sourceIssueTypeId = statusMachineSchemeConfig.getIssueTypeId();
                Long targetIssueTypeId;
                if (!Objects.equals(0L, sourceIssueTypeId)) {
                    targetIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceIssueTypeId);
                    if (targetIssueTypeId == null) {
                        targetIssueTypeId = sourceIssueTypeId;
                    }
                } else {
                    targetIssueTypeId = sourceIssueTypeId;
                }
                statusMachineSchemeConfig.setIssueTypeId(targetIssueTypeId);
                if (statusMachineSchemeConfigMapper.insert(statusMachineSchemeConfig) != 1) {
                    throw new CommonException("error.insert.fd_status_machine_scheme_config");
                }
            }
            this.logger.debug("fd_status_machine_scheme_config 复制完成");
        }
    }

    private void cloneFdStatusMachineTransform(Long sourceStateMachineId,
                                               Long targetStateMachineId,
                                               ProjectCloneContext context) {
        List<StatusMachineTransformDTO> statusMachineTransforms = statusMachineTransformMapper.select(new StatusMachineTransformDTO().setStateMachineId(sourceStateMachineId));
        if (CollectionUtils.isEmpty(statusMachineTransforms)) {
            this.logger.debug("没有检测到可复制的 fd_status_machine_transform 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_status_machine_transform 数据{}条, 开始复制", statusMachineTransforms.size());
        for (StatusMachineTransformDTO statusMachineTransform : statusMachineTransforms) {
            statusMachineTransform.setId(null);
            statusMachineTransform.setStateMachineId(targetStateMachineId);
            Long sourceStartNodeId = statusMachineTransform.getStartNodeId();
            if (!Objects.equals(0L, sourceStartNodeId)) {
                Long targetStartNodeId = context.getByTableAndSourceId(TABLE_FD_STATUS_MACHINE_NODE, sourceStartNodeId);
                if (targetStartNodeId == null) {
                    continue;
                }
                statusMachineTransform.setStartNodeId(targetStartNodeId);
            }
            Long sourceEndNodeId = statusMachineTransform.getEndNodeId();
            if (!Objects.equals(0L, sourceEndNodeId)) {
                Long targetEndNodeId = context.getByTableAndSourceId(TABLE_FD_STATUS_MACHINE_NODE, sourceEndNodeId);
                if (targetEndNodeId == null) {
                    continue;
                }
                statusMachineTransform.setEndNodeId(targetEndNodeId);
            }
            if (statusMachineTransformMapper.insert(statusMachineTransform) != 1) {
                throw new CommonException("error.insert.fd_status_machine_transform");
            }
        }
        this.logger.debug("fd_status_machine_transform 复制完成");
    }

    private void cloneFdStatusMachineNode(Long sourceStateMachineId,
                                          Long targetStateMachineId,
                                          ProjectCloneContext context) {
        List<StatusMachineNodeDTO> statusMachineNodes = statusMachineNodeMapper.select(new StatusMachineNodeDTO().setStateMachineId(sourceStateMachineId));
        if (CollectionUtils.isEmpty(statusMachineNodes)) {
            this.logger.debug("没有检测到可复制的 fd_status_machine_node 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_status_machine_node 数据{}条, 开始复制", statusMachineNodes.size());
        for (StatusMachineNodeDTO statusMachineNode : statusMachineNodes) {
            Long sourceId = statusMachineNode.getId();
            statusMachineNode.setId(null);
            statusMachineNode.setStateMachineId(targetStateMachineId);
            if (statusMachineNodeMapper.insert(statusMachineNode) != 1) {
                throw new CommonException("error.insert.fd_status_machine_node");
            }
            context.put(TABLE_FD_STATUS_MACHINE_NODE, sourceId, statusMachineNode.getId());
        }
        this.logger.debug("fd_status_machine_node 复制完成");
    }

    private Long cloneFdStatusMachine(Long sourceProjectId, Long targetProjectId, ProjectCloneContext context, Long sourceStateMachineId) {
        StatusMachineDTO statusMachine = statusMachineMapper.selectByPrimaryKey(sourceStateMachineId);
        AssertUtils.notNull(statusMachine, "error.statusMachine.not.exist");
        String newName = renameSchemeName(sourceProjectId, targetProjectId, context, statusMachine.getName());
        statusMachine.setId(null);
        statusMachine.setName(newName);
        statusMachine.setDescription(newName);
        if (statusMachineMapper.insert(statusMachine) != 1) {
            throw new CommonException("error.insert.fd_status_machine");
        }
        return statusMachine.getId();
    }

    private void cloneFdProjectConfigAndFdStateMachineScheme(Long sourceProjectId,
                                                             Long targetProjectId,
                                                             ProjectCloneContext context) {
        List<ProjectConfigDTO> projectConfigs = projectConfigMapper.select(new ProjectConfigDTO().setProjectId(sourceProjectId).setSchemeType(SchemeType.STATE_MACHINE));
        if (CollectionUtils.isEmpty(projectConfigs)) {
            this.logger.debug("没有检测到可复制的 fd_project_config 状态机数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_project_config 状态机数据{}条, 开始复制", projectConfigs.size());

        for (ProjectConfigDTO projectConfig : projectConfigs) {
            final Long sourceProjectConfigId = projectConfig.getId();
            Long sourceSchemeId = projectConfig.getSchemeId();
            StateMachineSchemeDTO stateMachineScheme = stateMachineSchemeMapper.selectByPrimaryKey(sourceSchemeId);
            AssertUtils.notNull(stateMachineScheme, "error.insert.fd_state_machine_scheme.not_exist");
            String newName = renameSchemeName(sourceProjectId, targetProjectId, context, stateMachineScheme.getName());
            stateMachineScheme.setId(null);
            stateMachineScheme.setName(newName);
            stateMachineScheme.setDescription(newName);
            if (stateMachineSchemeMapper.insert(stateMachineScheme) != 1) {
                throw new CommonException("error.insert.fd_state_machine_scheme");
            }
            final Long newSchemeId = stateMachineScheme.getId();
            context.put(TABLE_FD_STATE_MACHINE_SCHEME, sourceSchemeId, newSchemeId);
            //插入fd_project_config
            projectConfig.setId(null);
            projectConfig.setProjectId(targetProjectId);
            projectConfig.setSchemeId(newSchemeId);
            if (projectConfigMapper.insert(projectConfig) != 1) {
                throw new CommonException("error.insert.fd_project_config");
            }
            context.put(TABLE_FD_PROJECT_CONFIG, sourceProjectConfigId, projectConfig.getId());
        }
        this.logger.debug("fd_project_config 状态机数据复制完成");
    }

    private void cloneProjectConfigAndIssueTypeScheme(Long sourceProjectId,
                                                     Long targetProjectId,
                                                     ProjectCloneContext context) {
        List<ProjectConfigDTO> projectConfigs = projectConfigMapper.select(new ProjectConfigDTO().setProjectId(sourceProjectId).setSchemeType(SchemeType.ISSUE_TYPE));
        if (CollectionUtils.isEmpty(projectConfigs)) {
            this.logger.debug("没有检测到可复制的 fd_project_config 工作项类型数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_project_config 工作项类型数据{}条, 开始复制", projectConfigs.size());

        for (ProjectConfigDTO projectConfig : projectConfigs) {
            // 处理fd_issue_type_scheme
            final Long sourceProjectConfigId = projectConfig.getId();
            Long sourceSchemeId = projectConfig.getSchemeId();
            IssueTypeSchemeDTO sourceIssueTypeScheme = this.issueTypeSchemeMapper.selectByPrimaryKey(sourceSchemeId);
            AssertUtils.notNull(sourceIssueTypeScheme, "error.insert.fd_issue_type_scheme.not_exist");
            String newName = renameSchemeName(sourceProjectId, targetProjectId, context, sourceIssueTypeScheme.getName());
            sourceIssueTypeScheme.setId(null);
            sourceIssueTypeScheme.setName(newName);
            sourceIssueTypeScheme.setDescription(newName);

            // 处理方案默认类型
            final Long newDefaultIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceIssueTypeScheme.getDefaultIssueTypeId());
            if(newDefaultIssueTypeId != null) {
                sourceIssueTypeScheme.setDefaultIssueTypeId(newDefaultIssueTypeId);
            }

            if (issueTypeSchemeMapper.insert(sourceIssueTypeScheme) != 1) {
                throw new CommonException("error.insert.fd_issue_type_scheme");
            }
            final Long newSchemeId = sourceIssueTypeScheme.getId();
            context.put(TABLE_FD_ISSUE_TYPE_SCHEME, sourceSchemeId, newSchemeId);

            // 处理fd_project_config
            projectConfig.setId(null);
            projectConfig.setProjectId(targetProjectId);
            projectConfig.setSchemeId(newSchemeId);
            if (projectConfigMapper.insert(projectConfig) != 1) {
                throw new CommonException("error.insert.fd_project_config");
            }
            context.put(TABLE_FD_PROJECT_CONFIG, sourceProjectConfigId, projectConfig.getId());
        }

        // 处理fd_issue_type_scheme
        final Map<Long, Long> issueTypeSchemeIdMap = Optional.ofNullable(context.getByTable(TABLE_FD_ISSUE_TYPE_SCHEME)).orElse(Collections.emptyMap());
        final Set<Long> sourceSchemeIds = issueTypeSchemeIdMap.keySet();
        if(CollectionUtils.isNotEmpty(sourceSchemeIds)) {
            final List<IssueTypeSchemeConfigDTO> sourceIssueTypeSchemeConfigList = this.issueTypeSchemeConfigMapper.selectByCondition(Condition.builder(IssueTypeSchemeConfigDTO.class).andWhere(Sqls.custom()
                    .andIn(IssueTypeSchemeConfigDTO.FIELD_SCHEME_ID, sourceSchemeIds)
            ).build());
            if(CollectionUtils.isNotEmpty(sourceIssueTypeSchemeConfigList)) {
                this.logger.debug("检测到可复制的 fd_issue_type_scheme_config 数据{}条, 开始复制", projectConfigs.size());
                for (IssueTypeSchemeConfigDTO issueTypeSchemeConfig : sourceIssueTypeSchemeConfigList) {
                    final Long sourceSchemeConfigId = issueTypeSchemeConfig.getId();
                    issueTypeSchemeConfig.setId(null);

                    final Long sourceSchemeId = issueTypeSchemeConfig.getSchemeId();
                    final Long newSchemeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE_SCHEME, sourceSchemeId);
                    if(newSchemeId == null) {
                        continue;
                    }
                    issueTypeSchemeConfig.setSchemeId(newSchemeId);

                    final Long sourceIssueTypeId = issueTypeSchemeConfig.getIssueTypeId();
                    final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceIssueTypeId);
                    if(newIssueTypeId != null) {
                        issueTypeSchemeConfig.setIssueTypeId(newIssueTypeId);
                    }

                    if (this.issueTypeSchemeConfigMapper.insert(issueTypeSchemeConfig) != 1) {
                        throw new CommonException("error.insert.fd_project_config");
                    }
                    context.put(TABLE_FD_ISSUE_TYPE_SCHEME_CONFIG, sourceSchemeConfigId, issueTypeSchemeConfig.getId());
                }
                this.logger.debug("fd_issue_type_scheme_config 复制完成");
            }
        }

        this.logger.debug("fd_project_config 工作项类型数据复制完成");
    }

    private String renameSchemeName(Long sourceProjectId, Long targetProjectId, ProjectCloneContext context, String name) {
        ProjectVO sourceProject = context.queryProject(sourceProjectId, SOURCE_PROJECT);
        ProjectVO targetProject = context.queryProject(targetProjectId, TARGET_PROJECT);
        String sourceProjectCode = sourceProject.getCode();
        String suffix = name.substring(sourceProjectCode.length());
        return targetProject.getCode() + suffix;
    }

    /**
     * 复制issue表
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssue(Long sourceProjectId,
                                 Long targetProjectId,
                                 ProjectCloneContext context) {
        List<IssueDTO> sourceIssues = this.issueRepository.select(new IssueDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssues)) {
            this.logger.debug("没有检测到可复制的 agile_issue 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue 数据{}条, 开始复制", sourceIssues.size());
        for (IssueDTO issue : sourceIssues) {
            Long sourceIssueId = issue.getIssueId();
            issue.setProjectId(targetProjectId);
            issue.setIssueId(null);
            // 设置工作项类型ID
            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, issue.getIssueTypeId());
            if(newIssueTypeId != null) {
                issue.setIssueTypeId(newIssueTypeId);
            }
            // 设置工作项状态ID
            this.issueRepository.insert(issue);
            Long targetIssueId = issue.getIssueId();
            context.put(TABLE_AGILE_ISSUE, sourceIssueId, targetIssueId);
        }
        this.logger.debug("agile_issue 复制完成");
    }

    /**
     * 复制规划的版本 agile_product_version
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileProductVersion(Long sourceProjectId,
                                          Long targetProjectId,
                                          ProjectCloneContext context) {
        List<ProductVersionDTO> sourceProductVersions = productVersionMapper.select(new ProductVersionDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceProductVersions)) {
            this.logger.debug("没有检测到可复制的 agile_product_version 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_product_version 数据{}条, 开始复制", sourceProductVersions.size());
        for (ProductVersionDTO productVersionDTO : sourceProductVersions) {
            Long sourceVersionId = productVersionDTO.getVersionId();
            productVersionDTO.setVersionId(null);
            productVersionDTO.setProjectId(targetProjectId);
            if (productVersionMapper.insert(productVersionDTO) != 1) {
                throw new CommonException("error.insert.product_version");
            }
            context.put(TABLE_AGILE_PRODUCT_VERSION, sourceVersionId, productVersionDTO.getVersionId());
        }
        this.logger.debug("agile_product_version 复制完成");
    }

    /**
     * 复制规划的版本与issue的关系 agile_version_issue_rel
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileVersionIssueRel(Long sourceProjectId,
                                           Long targetProjectId,
                                           ProjectCloneContext context) {
        List<VersionIssueRelDTO> sourceVersionIssueRelList = versionIssueRelMapper.select(new VersionIssueRelDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceVersionIssueRelList)) {
            this.logger.debug("没有检测到可复制的 agile_version_issue_rel 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_version_issue_rel 数据{}条, 开始复制", sourceVersionIssueRelList.size());
        for (VersionIssueRelDTO versionIssueRelDTO : sourceVersionIssueRelList) {
            versionIssueRelDTO.setId(null);
            versionIssueRelDTO.setProjectId(targetProjectId);
            Long sourceVersionId = versionIssueRelDTO.getVersionId();
            Long targetVersionId = context.getByTableAndSourceId(TABLE_AGILE_PRODUCT_VERSION, sourceVersionId);
            if (targetVersionId == null) {
                continue;
            }
            versionIssueRelDTO.setVersionId(targetVersionId);
            Long sourceIssueId = versionIssueRelDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            versionIssueRelDTO.setIssueId(targetIssueId);
            if (versionIssueRelMapper.insert(versionIssueRelDTO) != 1) {
                throw new CommonException("error.insert.version_issue_rel");
            }
        }
        this.logger.debug("agile_version_issue_rel 复制完成");
    }

    /**
     * 复制模块 agile_issue_component
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssueComponent(Long sourceProjectId,
                                          Long targetProjectId,
                                          ProjectCloneContext context) {
        List<IssueComponentDTO> sourceIssueComponentList = issueComponentMapper.select(new IssueComponentDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueComponentList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_component 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_component 数据{}条, 开始复制", sourceIssueComponentList.size());
        for (IssueComponentDTO issueComponentDTO : sourceIssueComponentList) {
            Long componentId = issueComponentDTO.getComponentId();
            issueComponentDTO.setComponentId(null);
            issueComponentDTO.setProjectId(targetProjectId);
            if (issueComponentMapper.insert(issueComponentDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_component");
            }
            context.put(TABLE_AGILE_ISSUE_COMPONENT, componentId, issueComponentDTO.getComponentId());
        }
        this.logger.debug("agile_issue_component 复制完成");
    }

    /**
     * 复制模块与issue的关系 agile_component_issue_rel
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileComponentIssueRel(Long sourceProjectId,
                                             Long targetProjectId,
                                             ProjectCloneContext context) {
        List<ComponentIssueRelDTO> sourceComponentIssueRelList = componentIssueRelMapper.select(new ComponentIssueRelDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceComponentIssueRelList)) {
            this.logger.debug("没有检测到可复制的 agile_component_issue_rel 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_component_issue_rel 数据{}条, 开始复制", sourceComponentIssueRelList.size());
        for (ComponentIssueRelDTO componentIssueRelDTO : sourceComponentIssueRelList) {
            componentIssueRelDTO.setId(null);
            componentIssueRelDTO.setProjectId(targetProjectId);
            Long sourceComponentId = componentIssueRelDTO.getComponentId();
            Long targetComponentId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_COMPONENT, sourceComponentId);
            if (targetComponentId == null) {
                continue;
            }
            componentIssueRelDTO.setComponentId(targetComponentId);
            Long sourceIssueId = componentIssueRelDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            componentIssueRelDTO.setIssueId(targetIssueId);
            if (componentIssueRelMapper.insert(componentIssueRelDTO) != 1) {
                throw new CommonException("error.insert.agile_component_issue_rel");
            }
        }
        this.logger.debug("agile_component_issue_rel 复制完成");
    }

    /**
     * 复制标签 agile_issue_label
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssueLabel(Long sourceProjectId,
                                      Long targetProjectId,
                                      ProjectCloneContext context) {
        List<IssueLabelDTO> sourceIssueLabelList = issueLabelMapper.select(new IssueLabelDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueLabelList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_label 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_label 数据{}条, 开始复制", sourceIssueLabelList.size());
        for (IssueLabelDTO issueLabelDTO : sourceIssueLabelList) {
            Long labelId = issueLabelDTO.getLabelId();
            issueLabelDTO.setLabelId(null);
            issueLabelDTO.setProjectId(targetProjectId);
            if (issueLabelMapper.insert(issueLabelDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_label");
            }
            context.put(TABLE_AGILE_ISSUE_LABEL, labelId, issueLabelDTO.getLabelId());
        }
        this.logger.debug("agile_issue_label 复制完成");
    }

    /**
     * 复制标签与issue的关系 agile_label_issue_rel
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileLabelIssueRel(Long sourceProjectId,
                                         Long targetProjectId,
                                         ProjectCloneContext context) {
        List<LabelIssueRelDTO> sourceLabelIssueRelList = labelIssueRelMapper.select(new LabelIssueRelDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceLabelIssueRelList)) {
            this.logger.debug("没有检测到可复制的 agile_label_issue_rel 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_label_issue_rel 数据{}条, 开始复制", sourceLabelIssueRelList.size());
        for (LabelIssueRelDTO labelIssueRelDTO : sourceLabelIssueRelList) {
            labelIssueRelDTO.setProjectId(targetProjectId);
            Long sourceLabelId = labelIssueRelDTO.getLabelId();
            Long targetLabelId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_LABEL, sourceLabelId);
            if (targetLabelId == null) {
                continue;
            }
            labelIssueRelDTO.setLabelId(targetLabelId);
            Long sourceIssueId = labelIssueRelDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            labelIssueRelDTO.setIssueId(targetIssueId);
            if (labelIssueRelMapper.insert(labelIssueRelDTO) != 1) {
                throw new CommonException("error.insert.agile_label_issue_rel");
            }
        }
        this.logger.debug("agile_label_issue_rel 复制完成");
    }

    /**
     * 复制问题链接类型 agile_issue_link_type
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssueLinkType(Long sourceProjectId,
                                         Long targetProjectId,
                                         ProjectCloneContext context) {
        List<IssueLinkTypeDTO> sourceIssueLinkTypeList = issueLinkTypeMapper.select(new IssueLinkTypeDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueLinkTypeList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_link_type 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_link_type 数据{}条, 开始复制", sourceIssueLinkTypeList.size());
        for (IssueLinkTypeDTO issueLinkTypeDTO : sourceIssueLinkTypeList) {
            Long sourceLinkTypeId = issueLinkTypeDTO.getLinkTypeId();
            issueLinkTypeDTO.setLinkTypeId(null);
            issueLinkTypeDTO.setProjectId(targetProjectId);
            if (issueLinkTypeMapper.insert(issueLinkTypeDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_link_type");
            }
            context.put(TABLE_AGILE_ISSUE_LINK_TYPE, sourceLinkTypeId, issueLinkTypeDTO.getLinkTypeId());
        }
        this.logger.debug("agile_issue_link_type 复制完成");
    }

    /**
     * 复制工作项类型
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneIssueType(Long sourceProjectId,
                                Long targetProjectId,
                                ProjectCloneContext context) {
        this.cloneFdIssueType(sourceProjectId, targetProjectId, context);
        this.cloneFdIssueTypeExtend(sourceProjectId, targetProjectId, context);
        this.cloneProjectConfigAndIssueTypeScheme(sourceProjectId, targetProjectId, context);
    }

    /**
     * 复制工作项类型本体表 fd_issue_type
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneFdIssueType(Long sourceProjectId,
                                  Long targetProjectId,
                                  ProjectCloneContext context) {
        final List<IssueTypeDTO> sourceIssueTypes = this.issueTypeMapper.select(new IssueTypeDTO().setProjectId(sourceProjectId));
        if(CollectionUtils.isEmpty(sourceIssueTypes)) {
            this.logger.debug("没有检测到可复制的 fd_issue_type 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_issue_type 数据{}条, 开始复制", sourceIssueTypes.size());
        for (IssueTypeDTO issueTypeDTO : sourceIssueTypes) {
            final Long sourceIssueTypeId = issueTypeDTO.getId();
            issueTypeDTO.setId(null);
            issueTypeDTO.setProjectId(targetProjectId);
            if (this.issueTypeMapper.insert(issueTypeDTO) != 1) {
                throw new CommonException("error.insert.fd_issue_type");
            }
            context.put(TABLE_FD_ISSUE_TYPE, sourceIssueTypeId, issueTypeDTO.getId());
        }
        this.logger.debug("fd_issue_type 复制完成");
    }

    /**
     * 复制工作项类型项目扩展表 fd_issue_type_extend
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneFdIssueTypeExtend(Long sourceProjectId,
                                         Long targetProjectId,
                                         ProjectCloneContext context) {
        final List<IssueTypeExtendDTO> sourceIssueTypeExtends = this.issueTypeExtendMapper.select(new IssueTypeExtendDTO().setProjectId(sourceProjectId));
        if(CollectionUtils.isEmpty(sourceIssueTypeExtends)) {
            this.logger.debug("没有检测到可复制的 fd_issue_type_extend 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_issue_type_extend 数据{}条, 开始复制", sourceIssueTypeExtends.size());
        for (IssueTypeExtendDTO issueTypeExtendDTO : sourceIssueTypeExtends) {
            final Long sourceIssueTypeExtendId = issueTypeExtendDTO.getId();
            issueTypeExtendDTO.setId(null);
            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, issueTypeExtendDTO.getIssueTypeId());
            // extend表里操作的issueType不一定都是本项目下的
            if(newIssueTypeId != null) {
                issueTypeExtendDTO.setIssueTypeId(newIssueTypeId);
            }
            issueTypeExtendDTO.setProjectId(targetProjectId);
            if (this.issueTypeExtendMapper.insert(issueTypeExtendDTO) != 1) {
                throw new CommonException("error.insert.fd_issue_type_extend");
            }
            context.put(TABLE_FD_ISSUE_TYPE_EXTEND, sourceIssueTypeExtendId, issueTypeExtendDTO.getId());
        }
        this.logger.debug("fd_issue_type_extend 复制完成");
    }

    /**
     * 复制自定义字段
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneField(Long sourceProjectId,
                            Long targetProjectId,
                            ProjectCloneContext context) {
        this.cloneFdObjectSchemeField(sourceProjectId, targetProjectId, context);
        this.cloneFdFieldOption(sourceProjectId, targetProjectId, context);
        this.cloneFdObjectSchemeFieldDefaultValue(sourceProjectId, targetProjectId, context);
    }

    /**
     * 复制自定义字段定义 fd_object_scheme_field
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneFdObjectSchemeField(Long sourceProjectId,
                                        Long targetProjectId,
                                        ProjectCloneContext context) {
        final List<ObjectSchemeFieldDTO> allSystemFields = this.objectSchemeFieldMapper.select(new ObjectSchemeFieldDTO().setSystem(Boolean.TRUE));
        context.addMayBeUsedFields(allSystemFields);
        final List<ObjectSchemeFieldDTO> allOrganizationFields = this.objectSchemeFieldMapper.selectByCondition(Condition.builder(ObjectSchemeFieldDTO.class).andWhere(Sqls.custom()
                .andEqualTo(ObjectSchemeFieldDTO.FIELD_ORGANIZATION_ID, context.getOrganizationId())
                .andIsNull(ObjectSchemeFieldDTO.FIELD_PROJECT_ID)
        ).build());
        context.addMayBeUsedFields(allOrganizationFields);
        final List<ObjectSchemeFieldDTO> sourceObjectSchemeFields = this.objectSchemeFieldMapper.select(new ObjectSchemeFieldDTO().setProjectId(sourceProjectId));
        if(CollectionUtils.isEmpty(sourceObjectSchemeFields)) {
            this.logger.debug("没有检测到可复制的 fd_object_scheme_field 数据, 跳过此步骤");
            return;
        }
        // 复制一份ObjectSchemeFieldDTO对象放入mayBeUsedFields中
        // 不然后续处理之后fieldId会变成新插入的ID
        // 导致后续根据源fieldId查询field信息时查不到
        context.addMayBeUsedFields(sourceObjectSchemeFields.stream().map(field -> {ObjectSchemeFieldDTO res = new ObjectSchemeFieldDTO(); BeanUtils.copyProperties(field, res); return res;}).collect(Collectors.toList()));
        this.logger.debug("检测到可复制的 fd_object_scheme_field 数据{}条, 开始复制", sourceObjectSchemeFields.size());
        for (ObjectSchemeFieldDTO objectSchemeField : sourceObjectSchemeFields) {
            final Long sourceObjectSchemeFieldId = objectSchemeField.getId();
            objectSchemeField.setId(null);
            // 这里没有处理选项字段的默认值, 因为需要等选项表复制完成之后才能继续处理
            objectSchemeField.setProjectId(targetProjectId);
            if (this.objectSchemeFieldMapper.insert(objectSchemeField) != 1) {
                throw new CommonException("error.insert.fd_object_scheme_field");
            }
            context.put(TABLE_FD_OBJECT_SCHEME_FIELD, sourceObjectSchemeFieldId, objectSchemeField.getId());
        }
        this.logger.debug("fd_object_scheme_field 复制完成");
    }

    /**
     * 复制自定义字段选项 fd_field_option
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneFdFieldOption(Long sourceProjectId,
                                    Long targetProjectId,
                                    ProjectCloneContext context) {
        final Map<Long, Long> fieldIdMapping = Optional.ofNullable(context.getByTable(TABLE_FD_OBJECT_SCHEME_FIELD)).orElse(Collections.emptyMap());
        final Set<Long> sourceFieldIds = fieldIdMapping.keySet();
        if(CollectionUtils.isEmpty(sourceFieldIds)) {
            this.logger.debug("没有检测到可复制的 fd_field_option 数据, 跳过此步骤");
            return;
        }
        final List<FieldOptionDTO> sourceFieldOpthionList = this.fieldOptionMapper.selectByCondition(Condition.builder(FieldOptionDTO.class).andWhere(Sqls.custom()
                .andIn(FieldOptionDTO.FIELD_FIELD_ID, sourceFieldIds)
        ).build());
        if(CollectionUtils.isEmpty(sourceFieldOpthionList)) {
            this.logger.debug("没有检测到可复制的 fd_field_option 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_field_option 数据{}条, 开始复制", sourceFieldOpthionList.size());
        for (FieldOptionDTO fieldOption : sourceFieldOpthionList) {
            final Long sourceFieldOptionId = fieldOption.getId();
            fieldOption.setId(null);

            final Long newFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, fieldOption.getFieldId());
            // 只处理源项目自定义的选项, 其余的不复制
            if(newFieldId == null) {
                continue;
            }
            fieldOption.setFieldId(newFieldId);
            if (this.fieldOptionMapper.insert(fieldOption) != 1) {
                throw new CommonException("error.insert.fd_field_option");
            }
            context.put(TABLE_FD_FIELD_OPTION, sourceFieldOptionId, fieldOption.getId());
        }
        this.logger.debug("fd_field_option 复制完成");
    }

    /**
     * 复制自定义字段定义的默认值
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneFdObjectSchemeFieldDefaultValue(Long sourceProjectId,
                                                      Long targetProjectId,
                                                      ProjectCloneContext context) {
        final List<String> fieldTypeFilter = Arrays.asList(FieldType.RADIO, FieldType.CHECKBOX, FieldType.SINGLE, FieldType.MULTIPLE);
        final List<ObjectSchemeFieldDTO> objectSchemeFields = this.objectSchemeFieldMapper.selectByCondition(Condition.builder(ObjectSchemeFieldDTO.class).andWhere(Sqls.custom()
                .andEqualTo(ObjectSchemeFieldDTO.FIELD_PROJECT_ID, targetProjectId)
                .andIn(ObjectSchemeFieldDTO.FIELD_FIELD_TYPE, fieldTypeFilter)
        ).build());
        if(CollectionUtils.isEmpty(objectSchemeFields)) {
            this.logger.debug("没有检测到可复制默认值的 fd_object_scheme_field 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制默认值的 fd_object_scheme_field 数据{}条, 开始复制", objectSchemeFields.size());
        for (ObjectSchemeFieldDTO objectSchemeField : objectSchemeFields) {
            final String sourceDefaultValueCsv = objectSchemeField.getDefaultValue();
            if(StringUtils.isBlank(sourceDefaultValueCsv)) {
                continue;
            }
            final List<Long> defaultOptionIdList = Arrays.stream(sourceDefaultValueCsv.split(BaseConstants.Symbol.COMMA))
                    .filter(StringUtils::isNotBlank)
                    .filter(StringUtils::isNumeric)
                    .map(Long::parseLong)
                    .collect(Collectors.toList());
            if(CollectionUtils.isEmpty(defaultOptionIdList)) {
                continue;
            }
            final List<Long> newDefaultOptionIdList = new ArrayList<>(defaultOptionIdList.size());
            for (Long sourceDefaultOptionId : defaultOptionIdList) {
                final Long newDefaultOptionId = context.getByTableAndSourceId(TABLE_FD_FIELD_OPTION, sourceDefaultOptionId);
                if(newDefaultOptionId != null) {
                    newDefaultOptionIdList.add(newDefaultOptionId);
                }
            }
            String newDefaultValueCsv = null;
            if(CollectionUtils.isNotEmpty(newDefaultOptionIdList)) {
                newDefaultValueCsv = newDefaultOptionIdList.stream().map(String::valueOf).collect(Collectors.joining(BaseConstants.Symbol.COMMA));
            }
            objectSchemeField.setDefaultValue(newDefaultValueCsv);
            this.objectSchemeFieldMapper.updateOptional(objectSchemeField, ObjectSchemeFieldDTO.FIELD_DEFAULT_VALUE);
        }
        this.logger.debug("fd_object_scheme_field 默认值复制完成");
    }

    /**
     * 复制工作项类型的字段配置
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneIssueTypeFieldConfig(Long sourceProjectId,
                                           Long targetProjectId,
                                           ProjectCloneContext context) {
        this.cloneFdObjectSchemeFieldExtend(sourceProjectId, targetProjectId, context);
        this.cloneFdIssueTypeField(sourceProjectId, targetProjectId, context);
        this.cloneFdFieldPermission(sourceProjectId, targetProjectId, context);
        this.cloneIssueTypeFieldCascadeRule(sourceProjectId, targetProjectId, context);
    }

    /**
     * 复制工作项类型字段配置表 fd_object_scheme_field_extend
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneFdObjectSchemeFieldExtend(Long sourceProjectId,
                                                Long targetProjectId,
                                                ProjectCloneContext context) {
        final List<ObjectSchemeFieldExtendDTO> sourceObjectSchemeFieldExtendList = this.objectSchemeFieldExtendMapper.select(new ObjectSchemeFieldExtendDTO().setProjectId(sourceProjectId));
        if(CollectionUtils.isEmpty(sourceObjectSchemeFieldExtendList)) {
            this.logger.debug("没有检测到可复制的 fd_object_scheme_field_extend 数据, 跳过此步骤");
            return;
        }

        // 只处理选项型字段, 其余字段默认值暂时不用处理
        final Set<String> fieldTypeFilter = SetUtils.unmodifiableSet(FieldType.RADIO, FieldType.CHECKBOX, FieldType.SINGLE, FieldType.MULTIPLE);
        final List<ObjectSchemeFieldDTO> mayBeUsedFields = context.getMayBeUsedFields();
        final Map<Long, ObjectSchemeFieldDTO> fieldIdToEntityMap = mayBeUsedFields.stream()
                .filter(mayBeUsedField -> fieldTypeFilter.contains(mayBeUsedField.getFieldType()))
                .collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, Function.identity()));

        this.logger.debug("检测到可复制的 fd_object_scheme_field_extend 数据{}条, 开始复制", sourceObjectSchemeFieldExtendList.size());
        for (ObjectSchemeFieldExtendDTO objectSchemeFieldExtend : sourceObjectSchemeFieldExtendList) {
            final Long sourceObjectSchemeFieldExtendId = objectSchemeFieldExtend.getId();
            objectSchemeFieldExtend.setId(null);

            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, objectSchemeFieldExtend.getIssueTypeId());
            if(newIssueTypeId != null) {
                objectSchemeFieldExtend.setIssueTypeId(newIssueTypeId);
            }

            final Long sourceFieldId = objectSchemeFieldExtend.getFieldId();
            final Long newFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, sourceFieldId);
            if(newFieldId != null) {
                objectSchemeFieldExtend.setFieldId(newFieldId);
            }

            // 处理默认值
            final String sourceDefaultValue = objectSchemeFieldExtend.getDefaultValue();
            String newDefaultValue = sourceDefaultValue;
            // 根据ruleId获取对应的级联字段
            final ObjectSchemeFieldDTO sourceField = fieldIdToEntityMap.get(sourceFieldId);
            // 如果找到了, 说明需要处理; 否则不处理
            if(sourceField != null) {
                if(Boolean.TRUE.equals(sourceField.getSystem()) && FIELD_CODE_COMPONENT.equals(sourceField.getCode())) {
                    // 特殊处理模块
                    if(StringUtils.isNotBlank(sourceDefaultValue)) {
                        final List<Long> sourceComponentIds = Arrays.stream(sourceDefaultValue.split(BaseConstants.Symbol.COMMA))
                                .filter(StringUtils::isNotBlank)
                                .filter(StringUtils::isNumeric)
                                .map(Long::parseLong)
                                .collect(Collectors.toList());
                        if(CollectionUtils.isNotEmpty(sourceComponentIds)) {
                            final List<Long> newComponentIds = new ArrayList<>(sourceComponentIds.size());
                            for (Long sourceComponentId : sourceComponentIds) {
                                final Long newComponentId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_COMPONENT, sourceComponentId);
                                if(newComponentId != null) {
                                    newComponentIds.add(newComponentId);
                                } else {
                                    newComponentIds.add(sourceComponentId);
                                }
                            }
                            newDefaultValue = newComponentIds.stream().map(String::valueOf).collect(Collectors.joining(BaseConstants.Symbol.COMMA));
                        }
                    }
                } else if(Boolean.TRUE.equals(sourceField.getSystem()) && FIELD_CODE_LABEL.equals(sourceField.getCode())) {
                    // 特殊处理标签
                    if(StringUtils.isNotBlank(sourceDefaultValue)) {
                        final List<Long> sourceLabelIds = Arrays.stream(sourceDefaultValue.split(BaseConstants.Symbol.COMMA))
                                .filter(StringUtils::isNotBlank)
                                .filter(StringUtils::isNumeric)
                                .map(Long::parseLong)
                                .collect(Collectors.toList());
                        if(CollectionUtils.isNotEmpty(sourceLabelIds)) {
                            final List<Long> newLabelIds = new ArrayList<>(sourceLabelIds.size());
                            for (Long sourceLabelId : sourceLabelIds) {
                                final Long newLabelId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_LABEL, sourceLabelId);
                                if(newLabelId != null) {
                                    newLabelIds.add(newLabelId);
                                } else {
                                    newLabelIds.add(sourceLabelId);
                                }
                            }
                            newDefaultValue = newLabelIds.stream().map(String::valueOf).collect(Collectors.joining(BaseConstants.Symbol.COMMA));
                        }
                    }
                } else if(Boolean.TRUE.equals(sourceField.getSystem()) && (FIELD_CODE_INFLUENCE_VERSION.equals(sourceField.getCode()) || FIELD_CODE_FIX_VERSION.equals(sourceField.getCode()))) {
                    // 特殊影响/修复的版本
                    if(StringUtils.isNotBlank(sourceDefaultValue)) {
                        final List<Long> sourceProductVersionIds = Arrays.stream(sourceDefaultValue.split(BaseConstants.Symbol.COMMA))
                                .filter(StringUtils::isNotBlank)
                                .filter(StringUtils::isNumeric)
                                .map(Long::parseLong)
                                .collect(Collectors.toList());
                        if(CollectionUtils.isNotEmpty(sourceProductVersionIds)) {
                            final List<Long> newProductVersionIds = new ArrayList<>(sourceProductVersionIds.size());
                            for (Long sourceProductVersionId : sourceProductVersionIds) {
                                final Long newProductVersionId = context.getByTableAndSourceId(TABLE_AGILE_PRODUCT_VERSION, sourceProductVersionId);
                                if(newProductVersionId != null) {
                                    newProductVersionIds.add(newProductVersionId);
                                } else {
                                    newProductVersionIds.add(sourceProductVersionId);
                                }
                            }
                            newDefaultValue = newProductVersionIds.stream().map(String::valueOf).collect(Collectors.joining(BaseConstants.Symbol.COMMA));
                        }
                    }
                } else if(!Boolean.TRUE.equals(sourceField.getSystem()) && Objects.equals(sourceField.getProjectId(),sourceProjectId)) {
                    // 处理项目层自定义字段
                    if(FieldType.RADIO.equals(sourceField.getFieldType()) || FieldType.SINGLE.equals(sourceField.getFieldType())) {
                        // 处理单选类型
                        if(StringUtils.isNumeric(sourceDefaultValue)) {
                            final Long sourceOptionId = Long.parseLong(sourceDefaultValue);
                            final Long newOptionId = context.getByTableAndSourceId(TABLE_FD_FIELD_OPTION, sourceOptionId);
                            newDefaultValue = newOptionId == null ? sourceDefaultValue : String.valueOf(newOptionId);
                        }
                    } else {
                        // 处理多选类型
                        if(StringUtils.isNotBlank(sourceDefaultValue)) {
                            final List<Long> sourceOptionIds = Arrays.stream(sourceDefaultValue.split(BaseConstants.Symbol.COMMA))
                                    .filter(StringUtils::isNotBlank)
                                    .filter(StringUtils::isNumeric)
                                    .map(Long::parseLong)
                                    .collect(Collectors.toList());
                            if(CollectionUtils.isNotEmpty(sourceOptionIds)) {
                                final List<Long> newOptionIds = new ArrayList<>(sourceOptionIds.size());
                                for (Long sourceOptionId : sourceOptionIds) {
                                    final Long newOptionId = context.getByTableAndSourceId(TABLE_FD_FIELD_OPTION, sourceOptionId);
                                    if(newOptionId != null) {
                                        newOptionIds.add(newOptionId);
                                    } else {
                                        newOptionIds.add(sourceOptionId);
                                    }
                                }
                                newDefaultValue = newOptionIds.stream().map(String::valueOf).collect(Collectors.joining(BaseConstants.Symbol.COMMA));
                            }
                        }
                    }
                }
                // 不满足以上条件的, 暂时不需要处理
            }
            objectSchemeFieldExtend.setDefaultValue(newDefaultValue);

            objectSchemeFieldExtend.setProjectId(targetProjectId);
            if (this.objectSchemeFieldExtendMapper.insert(objectSchemeFieldExtend) != 1) {
                throw new CommonException("error.insert.fd_object_scheme_field_extend");
            }
            context.put(TABLE_FD_OBJECT_SCHEME_FIELD_EXTEND, sourceObjectSchemeFieldExtendId, objectSchemeFieldExtend.getId());
        }
        this.logger.debug("fd_object_scheme_field_extend 复制完成");
    }

    /**
     * 复制工作项类型描述默认值表 fd_issue_type_field
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneFdIssueTypeField(Long sourceProjectId,
                                       Long targetProjectId,
                                       ProjectCloneContext context) {
        final List<IssueTypeFieldDTO> sourceIssueTypeFieldList = this.issueTypeFieldMapper.select(new IssueTypeFieldDTO().setProjectId(sourceProjectId));
        if(CollectionUtils.isEmpty(sourceIssueTypeFieldList)) {
            this.logger.debug("没有检测到可复制的 fd_issue_type_field 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_issue_type_field 数据{}条, 开始复制", sourceIssueTypeFieldList.size());
        for (IssueTypeFieldDTO issueTypeField : sourceIssueTypeFieldList) {
            final Long sourceIssueTypeFieldId = issueTypeField.getId();
            issueTypeField.setId(null);

            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, issueTypeField.getIssueTypeId());
            if(newIssueTypeId != null) {
                issueTypeField.setIssueTypeId(newIssueTypeId);
            }

            issueTypeField.setProjectId(targetProjectId);
            if (this.issueTypeFieldMapper.insert(issueTypeField) != 1) {
                throw new CommonException("error.insert.fd_issue_type_field");
            }
            context.put(TABLE_FD_ISSUE_TYPE_FIELD, sourceIssueTypeFieldId, issueTypeField.getId());
        }
        this.logger.debug("fd_issue_type_field 复制完成");
    }

    /**
     * 复制工作项类型字段配置表权限配置 fd_field_permission
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneFdFieldPermission(Long sourceProjectId, Long targetProjectId, ProjectCloneContext context) {
        final List<FieldPermissionDTO> sourceFieldPermissionList = this.fieldPermissionMapper.select(new FieldPermissionDTO().setProjectId(sourceProjectId));
        if(CollectionUtils.isEmpty(sourceFieldPermissionList)) {
            this.logger.debug("没有检测到可复制的 fd_field_permission 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_field_permission 数据{}条, 开始复制", sourceFieldPermissionList.size());
        for (FieldPermissionDTO fieldPermission : sourceFieldPermissionList) {
            final Long sourceFieldPermissionId = fieldPermission.getId();
            fieldPermission.setId(null);

            final Long newFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, fieldPermission.getFieldId());
            if(newFieldId != null) {
                // 项目下可以对非本项目的字段设置权限, 所以如果查映射查不到就保持原值
                fieldPermission.setFieldId(newFieldId);
            }

            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, fieldPermission.getIssueTypeId());
            if(newIssueTypeId != null) {
                // 项目下可以对非本项目的工作项类型设置权限, 所以如果查映射查不到就保持原值
                fieldPermission.setIssueTypeId(newIssueTypeId);
            }


            fieldPermission.setProjectId(targetProjectId);
            if (this.fieldPermissionMapper.insert(fieldPermission) != 1) {
                throw new CommonException("error.insert.fd_field_permission");
            }
            context.put(TABLE_FD_FIELD_PERMISSION, sourceFieldPermissionId, fieldPermission.getId());
        }
        this.logger.debug("fd_field_permission 复制完成");
    }

    /**
     * 复制工作项类型的字段配置--级联规则
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneIssueTypeFieldCascadeRule(Long sourceProjectId,
                                                Long targetProjectId,
                                                ProjectCloneContext context) {
        final List<FieldCascadeRuleDTO> sourceFieldCascadeRuleList = this.cloneFdFieldCascadeRule(sourceProjectId, targetProjectId, context);
        this.cloneFdFieldCascadeRuleOption(sourceProjectId, targetProjectId, context, sourceFieldCascadeRuleList);
    }

    /**
     * 复制工作项类型的字段配置--级联规则头 fd_field_cascade_rule
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private List<FieldCascadeRuleDTO> cloneFdFieldCascadeRule(Long sourceProjectId,
                                                 Long targetProjectId,
                                                 ProjectCloneContext context) {
        final List<FieldCascadeRuleDTO> sourceFieldCascadeRuleList = this.fieldCascadeRuleMapper.select(new FieldCascadeRuleDTO().setProjectId(sourceProjectId));
        if(CollectionUtils.isEmpty(sourceFieldCascadeRuleList)) {
            this.logger.debug("没有检测到可复制的 fd_field_cascade_rule 数据, 跳过此步骤");
            return Collections.emptyList();
        }
        this.logger.debug("检测到可复制的 fd_field_cascade_rule 数据{}条, 开始复制", sourceFieldCascadeRuleList.size());
        for (FieldCascadeRuleDTO fieldCascadeRule : sourceFieldCascadeRuleList) {
            final Long sourceFieldCascadeRuleId = fieldCascadeRule.getId();
            fieldCascadeRule.setId(null);

            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, fieldCascadeRule.getIssueTypeId());
            if(newIssueTypeId != null) {
                // 项目下可以对非本项目的工作项类型设置级联规则, 所以如果查映射查不到就保持原值
                fieldCascadeRule.setIssueTypeId(newIssueTypeId);
            }

            final Long newFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, fieldCascadeRule.getFieldId());
            if(newFieldId != null) {
                // 项目下可以对非本项目的字段设置级联规则, 所以如果查映射查不到就保持原值
                fieldCascadeRule.setFieldId(newFieldId);
            }

            final Long newFieldOptionId = context.getByTableAndSourceId(TABLE_FD_FIELD_OPTION, fieldCascadeRule.getFieldOptionId());
            if(newFieldOptionId != null) {
                // 项目下可以对非本项目的字段选项设置级联规则, 所以如果查映射查不到就保持原值
                fieldCascadeRule.setFieldOptionId(newFieldOptionId);
            }

            final Long sourceCascadeFieldId = fieldCascadeRule.getCascadeFieldId();
            final Long newCascadeFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, sourceCascadeFieldId);
            if(newCascadeFieldId != null) {
                // 项目下可以对非本项目的字段设置级联规则, 所以如果查映射查不到就保持原值
                fieldCascadeRule.setCascadeFieldId(newCascadeFieldId);
            }


            fieldCascadeRule.setProjectId(targetProjectId);
            if (this.fieldCascadeRuleMapper.insert(fieldCascadeRule) != 1) {
                throw new CommonException("error.insert.fd_field_cascade_rule");
            }
            context.put(TABLE_FD_FIELD_CASCADE_RULE, sourceFieldCascadeRuleId, fieldCascadeRule.getId());
            fieldCascadeRule.setId(sourceFieldCascadeRuleId);
            fieldCascadeRule.setCascadeFieldId(sourceCascadeFieldId);
        }
        this.logger.debug("fd_field_cascade_rule 复制完成");
        return sourceFieldCascadeRuleList;
    }

    /**
     * 复制工作项类型的字段配置--级联规则选项 fd_field_cascade_rule_option
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context context
     */
    private void cloneFdFieldCascadeRuleOption(Long sourceProjectId,
                                               Long targetProjectId,
                                               ProjectCloneContext context,
                                               List<FieldCascadeRuleDTO> sourceFieldCascadeRuleList) {
        final List<FieldCascadeRuleOptionDTO> sourceFieldCascadeRuleOptionList = this.fieldCascadeRuleOptionMapper.select(new FieldCascadeRuleOptionDTO().setProjectId(sourceProjectId));
        if(CollectionUtils.isEmpty(sourceFieldCascadeRuleOptionList)) {
            this.logger.debug("没有检测到可复制的 fd_field_cascade_rule_option 数据, 跳过此步骤");
            return;
        }

        // 只处理选项型字段, 其余字段默认值暂时不用处理
        final Set<String> fieldTypeFilter = SetUtils.unmodifiableSet(FieldType.RADIO, FieldType.CHECKBOX, FieldType.SINGLE, FieldType.MULTIPLE);
        final List<ObjectSchemeFieldDTO> mayBeUsedFields = context.getMayBeUsedFields();
        final Map<Long, ObjectSchemeFieldDTO> fieldIdToEntityMap = mayBeUsedFields.stream()
                .filter(mayBeUsedField -> fieldTypeFilter.contains(mayBeUsedField.getFieldType()))
                .collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, Function.identity()));
        final Map<Long, Long> ruleIdToCascadeFieldIdMap = sourceFieldCascadeRuleList.stream().collect(Collectors.toMap(FieldCascadeRuleDTO::getId, FieldCascadeRuleDTO::getCascadeFieldId));

        this.logger.debug("检测到可复制的 fd_field_cascade_rule_option 数据{}条, 开始复制", sourceFieldCascadeRuleOptionList.size());
        for (FieldCascadeRuleOptionDTO fieldCascadeRuleOption : sourceFieldCascadeRuleOptionList) {
            final Long sourceFieldCascadeRuleId = fieldCascadeRuleOption.getId();
            fieldCascadeRuleOption.setId(null);

            final Long sourceRuleId = fieldCascadeRuleOption.getFieldCascadeRuleId();
            final Long newFieldCascadeRuleId = context.getByTableAndSourceId(TABLE_FD_FIELD_CASCADE_RULE, sourceRuleId);
            if(newFieldCascadeRuleId == null) {
                continue;
            }
            fieldCascadeRuleOption.setFieldCascadeRuleId(newFieldCascadeRuleId);

            // 处理选项ID
            final Long sourceCascadeOptionId = fieldCascadeRuleOption.getCascadeOptionId();
            Long newCascadeOptionId = sourceCascadeOptionId;
            // 根据ruleId获取对应的级联字段
            final ObjectSchemeFieldDTO cascadeField = Optional.ofNullable(ruleIdToCascadeFieldIdMap.get(sourceRuleId)).map(fieldIdToEntityMap::get).orElse(null);
            // 如果找到了, 说明需要处理; 否则不处理
            if(cascadeField != null) {
                if(Boolean.TRUE.equals(cascadeField.getSystem()) && FIELD_CODE_COMPONENT.equals(cascadeField.getCode())) {
                    // 特殊处理模块
                    final Long newComponentId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_COMPONENT, sourceCascadeOptionId);
                    if(newComponentId == null) {
                        continue;
                    }
                    newCascadeOptionId = newComponentId;
                } else if(Boolean.TRUE.equals(cascadeField.getSystem()) && (FIELD_CODE_INFLUENCE_VERSION.equals(cascadeField.getCode()) || FIELD_CODE_FIX_VERSION.equals(cascadeField.getCode()))) {
                    // 特殊影响/修复的版本
                    final Long newProductVersionId = context.getByTableAndSourceId(TABLE_AGILE_PRODUCT_VERSION, sourceCascadeOptionId);
                    if(newProductVersionId == null) {
                        continue;
                    }
                    newCascadeOptionId = newProductVersionId;
                } else if(!Boolean.TRUE.equals(cascadeField.getSystem()) && Objects.equals(cascadeField.getProjectId(), sourceProjectId)) {
                    // 处理项目层自定义字段
                    final Long newOptionId = context.getByTableAndSourceId(TABLE_FD_FIELD_OPTION, sourceCascadeOptionId);
                    if(newOptionId == null) {
                        continue;
                    }
                    newCascadeOptionId = newOptionId;
                }
                // 不满足以上条件的, 暂时不需要处理
            }
            fieldCascadeRuleOption.setCascadeOptionId(newCascadeOptionId);

            fieldCascadeRuleOption.setProjectId(targetProjectId);
            if (this.fieldCascadeRuleOptionMapper.insert(fieldCascadeRuleOption) != 1) {
                throw new CommonException("error.insert.fd_field_cascade_rule_option");
            }
            context.put(TABLE_FD_FIELD_CASCADE_RULE_OPTION, sourceFieldCascadeRuleId, fieldCascadeRuleOption.getId());
        }
        this.logger.debug("fd_field_cascade_rule_option 复制完成");
    }

    /**
     * 复制关联工作项 agile_issue_link
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssueLink(Long sourceProjectId,
                                     Long targetProjectId,
                                     ProjectCloneContext context) {
        List<IssueLinkDTO> sourceIssueLinkList = issueLinkMapper.select(new IssueLinkDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueLinkList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_link 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_link 数据{}条, 开始复制", sourceIssueLinkList.size());
        for (IssueLinkDTO issueLinkDTO : sourceIssueLinkList) {
            issueLinkDTO.setLinkId(null);
            issueLinkDTO.setProjectId(targetProjectId);
            Long sourceIssueId = issueLinkDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            issueLinkDTO.setIssueId(targetIssueId);
            Long sourceLinkedIssueId = issueLinkDTO.getLinkedIssueId();
            Long targetLinkedIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceLinkedIssueId);
            if (targetLinkedIssueId == null) {
                continue;
            }
            issueLinkDTO.setLinkedIssueId(targetLinkedIssueId);
            Long sourceLinkTypeId = issueLinkDTO.getLinkTypeId();
            Long targetLinkTypeId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_LINK_TYPE, sourceLinkTypeId);
            if (targetLinkTypeId == null) {
                continue;
            }
            issueLinkDTO.setLinkTypeId(targetLinkTypeId);
            if (issueLinkMapper.insert(issueLinkDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_link");
            }
        }
        this.logger.debug("agile_issue_link 复制完成");
    }

    /**
     * 复制工作项依赖关系 agile_issue_predecessor
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssuePredecessor(Long sourceProjectId,
                                            Long targetProjectId,
                                            ProjectCloneContext context) {
        List<IssuePredecessorDTO> sourceIssuePredecessorList = issuePredecessorMapper.select(new IssuePredecessorDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssuePredecessorList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_predecessor 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_predecessor 数据{}条, 开始复制", sourceIssuePredecessorList.size());
        for (IssuePredecessorDTO issuePredecessorDTO : sourceIssuePredecessorList) {
            issuePredecessorDTO.setId(null);
            issuePredecessorDTO.setProjectId(targetProjectId);
            Long sourceIssueId = issuePredecessorDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            issuePredecessorDTO.setIssueId(targetIssueId);
            Long sourcePredecessorId = issuePredecessorDTO.getPredecessorId();
            Long targetPredecessorId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourcePredecessorId);
            if (targetPredecessorId == null) {
                continue;
            }
            issuePredecessorDTO.setPredecessorId(targetPredecessorId);
            if (issuePredecessorMapper.insert(issuePredecessorDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_predecessor");
            }
        }
        this.logger.debug("agile_issue_predecessor 复制完成");
    }

    /**
     * 复制工作项依赖关系树 agile_issue_predecessor_tree_closure
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneAgileIssuePredecessorTreeClosure(Long sourceProjectId,
                                                       Long targetProjectId,
                                                       ProjectCloneContext context) {
        List<IssuePredecessorTreeClosureDTO> sourceIssuePredecessorTreeClosureList = issuePredecessorTreeClosureMapper.select(new IssuePredecessorTreeClosureDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssuePredecessorTreeClosureList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_predecessor_tree_closure 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_predecessor_tree_closure 数据{}条, 开始复制", sourceIssuePredecessorTreeClosureList.size());
        for (IssuePredecessorTreeClosureDTO issuePredecessorTreeClosureDTO : sourceIssuePredecessorTreeClosureList) {
            issuePredecessorTreeClosureDTO.setId(null);
            issuePredecessorTreeClosureDTO.setProjectId(targetProjectId);
            Long sourceAncestorId = issuePredecessorTreeClosureDTO.getAncestorId();
            Long targetAncestorId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceAncestorId);
            if (targetAncestorId == null) {
                continue;
            }
            issuePredecessorTreeClosureDTO.setAncestorId(targetAncestorId);
            Long sourceDescendantId = issuePredecessorTreeClosureDTO.getDescendantId();
            Long targetDescendantId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceDescendantId);
            if (targetDescendantId == null) {
                continue;
            }
            issuePredecessorTreeClosureDTO.setDescendantId(targetDescendantId);
            if (issuePredecessorTreeClosureMapper.insert(issuePredecessorTreeClosureDTO) != 1) {
                throw new CommonException("error.insert.agile_issue_predecessor_tree_closure");
            }
        }
        this.logger.debug("agile_issue_predecessor_tree_closure 复制完成");
    }

    /**
     * 复制工作项附件 agile_issue_attachment & OSS
     *
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneIssueAttachment(Long sourceProjectId,
                                      Long targetProjectId,
                                      ProjectCloneContext context) {
        List<IssueAttachmentDTO> sourceIssueAttachmentList = issueAttachmentMapper.select(new IssueAttachmentDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceIssueAttachmentList)) {
            this.logger.debug("没有检测到可复制的 agile_issue_attachment 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 agile_issue_attachment 数据{}条, 开始复制", sourceIssueAttachmentList.size());
        Set<String> urls = new HashSet<>();
        for (IssueAttachmentDTO issueAttachmentDTO : sourceIssueAttachmentList) {
            issueAttachmentDTO.setAttachmentId(null);
            issueAttachmentDTO.setProjectId(targetProjectId);
            Long sourceIssueId = issueAttachmentDTO.getIssueId();
            Long targetIssueId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceIssueId);
            if (targetIssueId == null) {
                continue;
            }
            issueAttachmentDTO.setIssueId(targetIssueId);
            String url = issueAttachmentDTO.getUrl();
            if (url != null) {
                String fullUrl = filePathService.generateFullPath(url);
                if (url.startsWith("/")) {
                    url = url.substring(1);
                }
                issueAttachmentDTO.setUrl(fullUrl);
                urls.add(url);
            }
        }
        List<FileVO> files = new ArrayList<>();
        Long organizationId = ConvertUtil.getOrganizationId(targetProjectId);
        if (!urls.isEmpty()) {
            files.addAll(customFileOperator.getFileByFileKey(organizationId, new ArrayList<>(urls)));
        }
        Map<String, String> urlMap = new HashMap<>();
        for (FileVO file : files) {
            String cloneUrl = fileClient.copyFileByUrl(organizationId, file.getFileUrl(), file.getBucketName(), file.getBucketName(), file.getFileName());
            urlMap.put(file.getFileUrl(), cloneUrl);
        }
        for (IssueAttachmentDTO issueAttachmentDTO : sourceIssueAttachmentList) {
            String url = issueAttachmentDTO.getUrl();
            if (url != null) {
                String cloneUrl = urlMap.get(url);
                if (cloneUrl != null) {
                    issueAttachmentDTO.setUrl(filePathService.generateRelativePath(cloneUrl));
                    issueAttachmentMapper.insert(issueAttachmentDTO);
                }
            }
        }
        this.logger.debug("agile_issue_predecessor_tree_closure 复制完成");
    }

    private void cloneCustomFieldValue(Long sourceProjectId,
                                       Long targetProjectId,
                                       ProjectCloneContext context) {
        List<FieldValueDTO> fieldValues = fieldValueMapper.select(new FieldValueDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(fieldValues)) {
            this.logger.debug("没有检测到可复制的 fd_field_value 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_field_value 数据{}条, 开始复制", fieldValues.size());
        final Map<Long, String> fieldIdToTypeMap = context.getMayBeUsedFields().stream().collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, ObjectSchemeFieldDTO::getFieldType));
        for (FieldValueDTO fieldValue : fieldValues) {
            fieldValue.setId(null);
            fieldValue.setProjectId(targetProjectId);
            String schemeCode = fieldValue.getSchemeCode();
            Long sourceInstanceId = fieldValue.getInstanceId();
            Long targetInstanceId = null;
            if (ObjectSchemeCode.AGILE_ISSUE.equals(schemeCode)) {
                targetInstanceId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE, sourceInstanceId);
            } else if (ObjectSchemeCode.BACKLOG.equals(schemeCode)) {
            }
            if (targetInstanceId == null) {
                continue;
            }
            fieldValue.setInstanceId(targetInstanceId);
            Long sourceFieldId = fieldValue.getFieldId();
            Long targetFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, sourceFieldId);
            if (targetFieldId == null) {
                targetFieldId = sourceFieldId;
            }
            String fieldType = fieldIdToTypeMap.get(sourceFieldId);
            Long sourceOptionId = fieldValue.getOptionId();
            if (sourceOptionId != null) {
                if (FieldType.MEMBER.equals(fieldType) || FieldType.MULTI_MEMBER.equals(fieldType)) {
                    Long currentUserId = DetailsHelper.getUserDetails().getUserId();
                    if (!Objects.equals(sourceOptionId, currentUserId)) {
                        //不是操作人，跳过
                        continue;
                    }
                } else {
                    Long targetOptionId = context.getByTableAndSourceId(TABLE_FD_FIELD_OPTION, sourceOptionId);
                    if (targetOptionId == null) {
                        targetOptionId = sourceOptionId;
                    }
                    fieldValue.setOptionValue(targetOptionId.toString());
                }
            }
            if (fieldValueMapper.insert(fieldValue) != 1) {
                throw new CommonException("error.insert.fd_field_value");
            }
        }
        this.logger.debug("fd_field_value 复制完成");
    }

    /**
     * 复制状态机自定义流转
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneStatusMachineCustomFlow(Long sourceProjectId,
                                              Long targetProjectId,
                                              ProjectCloneContext context) {
        this.cloneFdStatusBranchMergeSetting(sourceProjectId, targetProjectId, context);
        this.cloneFdStatusTransferSetting(sourceProjectId, targetProjectId, context);
        this.cloneFdStatusLinkage(sourceProjectId, targetProjectId, context);
        this.cloneFdLinkIssueStatusLinkage(sourceProjectId, targetProjectId, context);
        final List<StatusFieldSettingDTO> sourceStatusFieldSettingList = this.cloneFdStatusFieldSetting(sourceProjectId, targetProjectId, context);
        this.cloneFdStatusFieldValueSetting(sourceProjectId, targetProjectId, context, sourceStatusFieldSettingList);
        this.cloneFdStatusNoticeSetting(sourceProjectId, targetProjectId, context);
    }

    /**
     * 复制状态机自定义流转--自动流转 fd_status_branch_merge_setting
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneFdStatusBranchMergeSetting(Long sourceProjectId,
                                                 Long targetProjectId,
                                                 ProjectCloneContext context) {
        List<StatusBranchMergeSettingDTO> sourceStatusBranchMergeSettingList = this.statusBranchMergeSettingMapper.select(new StatusBranchMergeSettingDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceStatusBranchMergeSettingList)) {
            this.logger.debug("没有检测到可复制的 fd_status_branch_merge_setting 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_status_branch_merge_setting 数据{}条, 开始复制", sourceStatusBranchMergeSettingList.size());
        for (StatusBranchMergeSettingDTO sourceStatusBranchMergeSetting : sourceStatusBranchMergeSettingList) {
            final Long sourceStatusBranchMergeSettingId = sourceStatusBranchMergeSetting.getId();

            // issueTypeId可能不是本项目的, 所以无法映射时就不处理
            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceStatusBranchMergeSetting.getIssueTypeId());
            if(newIssueTypeId != null) {
                sourceStatusBranchMergeSetting.setIssueTypeId(newIssueTypeId);
            }
            // statusId都是组织层的, 不用复制

            sourceStatusBranchMergeSetting.setId(null);
            sourceStatusBranchMergeSetting.setProjectId(targetProjectId);
            if (this.statusBranchMergeSettingMapper.insert(sourceStatusBranchMergeSetting) != 1) {
                throw new CommonException("error.insert.fd_status_branch_merge_setting");
            }
            context.put(TABLE_FD_STATUS_BRANCH_MERGE_SETTING, sourceStatusBranchMergeSettingId, sourceStatusBranchMergeSetting.getId());
        }
        this.logger.debug("fd_status_branch_merge_setting 复制完成");
    }

    /**
     * 复制状态机自定义流转--流转条件 fd_status_transfer_setting
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneFdStatusTransferSetting(Long sourceProjectId,
                                              Long targetProjectId,
                                              ProjectCloneContext context) {
        List<StatusTransferSettingDTO> sourceStatusTransferSettingList = this.statusTransferSettingMapper.select(new StatusTransferSettingDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceStatusTransferSettingList)) {
            this.logger.debug("没有检测到可复制的 fd_status_transfer_setting 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_status_transfer_setting 数据{}条, 开始复制", sourceStatusTransferSettingList.size());
        for (StatusTransferSettingDTO sourceStatusTransferSetting : sourceStatusTransferSettingList) {
            final Long sourceStatusTransferSettingId = sourceStatusTransferSetting.getId();

            // issueTypeId可能不是本项目的, 所以无法映射时就不处理
            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceStatusTransferSetting.getIssueTypeId());
            if(newIssueTypeId != null) {
                sourceStatusTransferSetting.setIssueTypeId(newIssueTypeId);
            }
            // statusId都是组织层的, 不用复制

            sourceStatusTransferSetting.setId(null);
            sourceStatusTransferSetting.setProjectId(targetProjectId);
            if (this.statusTransferSettingMapper.insert(sourceStatusTransferSetting) != 1) {
                throw new CommonException("error.insert.fd_status_transfer_setting");
            }
            context.put(TABLE_FD_STATUS_TRANSFER_SETTING, sourceStatusTransferSettingId, sourceStatusTransferSetting.getId());
        }
        this.logger.debug("fd_status_transfer_setting 复制完成");
    }

    /**
     * 复制状态机自定义流转--状态联动--父子级联动 fd_status_linkage
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneFdStatusLinkage(Long sourceProjectId,
                                              Long targetProjectId,
                                              ProjectCloneContext context) {
        List<StatusLinkageDTO> sourceStatusLinkageList = this.statusLinkageMapper.select(new StatusLinkageDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceStatusLinkageList)) {
            this.logger.debug("没有检测到可复制的 fd_status_linkage 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_status_linkage 数据{}条, 开始复制", sourceStatusLinkageList.size());
        for (StatusLinkageDTO sourceStatusLinkage : sourceStatusLinkageList) {
            final Long sourceStatusLinkageId = sourceStatusLinkage.getId();

            // issueTypeId可能不是本项目的, 所以无法映射时就不处理
            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceStatusLinkage.getIssueTypeId());
            if(newIssueTypeId != null) {
                sourceStatusLinkage.setIssueTypeId(newIssueTypeId);
            }
            // statusId都是组织层的, 不用复制
            // parentIssueStatusSetting都是组织层的, 不用复制
            // parentIssueTypeId可能不是本项目的, 所以无法映射时就不处理
            final Long newParentIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceStatusLinkage.getParentIssueTypeId());
            if(newParentIssueTypeId != null) {
                sourceStatusLinkage.setParentIssueTypeId(newParentIssueTypeId);
            }

            sourceStatusLinkage.setId(null);
            sourceStatusLinkage.setProjectId(targetProjectId);
            if (this.statusLinkageMapper.insert(sourceStatusLinkage) != 1) {
                throw new CommonException("error.insert.fd_status_linkage");
            }
            context.put(TABLE_FD_STATUS_LINKAGE, sourceStatusLinkageId, sourceStatusLinkage.getId());
        }
        this.logger.debug("fd_status_linkage 复制完成");
    }

    /**
     * 复制状态机自定义流转--状态联动--关联工作项 fd_link_issue_status_linkage
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneFdLinkIssueStatusLinkage(Long sourceProjectId,
                                               Long targetProjectId,
                                               ProjectCloneContext context) {
        List<LinkIssueStatusLinkageDTO> sourceLinkIssueStatusLinkageList = this.linkIssueStatusLinkageMapper.select(new LinkIssueStatusLinkageDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceLinkIssueStatusLinkageList)) {
            this.logger.debug("没有检测到可复制的 fd_link_issue_status_linkage 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_link_issue_status_linkage 数据{}条, 开始复制", sourceLinkIssueStatusLinkageList.size());
        for (LinkIssueStatusLinkageDTO sourceLinkIssueStatusLinkage : sourceLinkIssueStatusLinkageList) {
            final Long sourceLinkIssueStatusLinkageId = sourceLinkIssueStatusLinkage.getId();

            // issueTypeId可能不是本项目的, 所以无法映射时就不处理
            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceLinkIssueStatusLinkage.getIssueTypeId());
            if(newIssueTypeId != null) {
                sourceLinkIssueStatusLinkage.setIssueTypeId(newIssueTypeId);
            }
            // statusId都是组织层的, 不用复制
            // parentIssueStatusSetting都是组织层的, 不用复制
            // 工作项关联类型一定是本项目下新复制的, 查不到说明有脏数据
            final Long newLinkTypeId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_LINK_TYPE, sourceLinkIssueStatusLinkage.getLinkTypeId());
            if(newLinkTypeId == null) {
                continue;
            }
            sourceLinkIssueStatusLinkage.setLinkTypeId(newLinkTypeId);
            // linkIssueTypeId可能不是本项目的, 所以无法映射时就不处理
            final Long newLinkIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceLinkIssueStatusLinkage.getLinkIssueTypeId());
            if(newLinkIssueTypeId != null) {
                sourceLinkIssueStatusLinkage.setLinkIssueTypeId(newLinkIssueTypeId);
            }
            // linkIssueStatusId都是组织层的, 不用复制

            sourceLinkIssueStatusLinkage.setId(null);
            sourceLinkIssueStatusLinkage.setProjectId(targetProjectId);
            if (this.linkIssueStatusLinkageMapper.insert(sourceLinkIssueStatusLinkage) != 1) {
                throw new CommonException("error.insert.fd_link_issue_status_linkage");
            }
            context.put(TABLE_FD_LINK_ISSUE_STATUS_LINKAGE, sourceLinkIssueStatusLinkageId, sourceLinkIssueStatusLinkage.getId());
        }
        this.logger.debug("fd_link_issue_status_linkage 复制完成");
    }

    /**
     * 复制状态机自定义流转--更新属性--字段声明 fd_status_field_setting
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private List<StatusFieldSettingDTO> cloneFdStatusFieldSetting(Long sourceProjectId,
                                           Long targetProjectId,
                                           ProjectCloneContext context) {
        List<StatusFieldSettingDTO> sourceStatusFieldSettingList = this.statusFieldSettingMapper.select(new StatusFieldSettingDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceStatusFieldSettingList)) {
            this.logger.debug("没有检测到可复制的 fd_status_field_setting 数据, 跳过此步骤");
            return Collections.emptyList();
        }
        this.logger.debug("检测到可复制的 fd_status_field_setting 数据{}条, 开始复制", sourceStatusFieldSettingList.size());
        for (StatusFieldSettingDTO sourceStatusFieldSetting : sourceStatusFieldSettingList) {
            final Long sourceStatusFieldSettingId = sourceStatusFieldSetting.getId();

            // issueTypeId可能不是本项目的, 所以无法映射时就不处理
            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceStatusFieldSetting.getIssueTypeId());
            if(newIssueTypeId != null) {
                sourceStatusFieldSetting.setIssueTypeId(newIssueTypeId);
            }
            // statusId都是组织层的, 不用复制
            // fieldId可能不是本项目的, 所以无法映射时就不处理
            final Long sourceFieldId = sourceStatusFieldSetting.getFieldId();
            final Long newFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, sourceFieldId);
            if(newFieldId != null) {
                sourceStatusFieldSetting.setFieldId(newFieldId);
            }

            sourceStatusFieldSetting.setId(null);
            sourceStatusFieldSetting.setProjectId(targetProjectId);
            if (this.statusFieldSettingMapper.insert(sourceStatusFieldSetting) != 1) {
                throw new CommonException("error.insert.fd_status_field_setting");
            }
            context.put(TABLE_FD_STATUS_FIELD_SETTING, sourceStatusFieldSettingId, sourceStatusFieldSetting.getId());
            sourceStatusFieldSetting.setId(sourceStatusFieldSettingId);
            sourceStatusFieldSetting.setFieldId(sourceFieldId);
        }
        this.logger.debug("fd_status_field_setting 复制完成");
        return sourceStatusFieldSettingList;
    }

    /**
     * 复制状态机自定义流转--更新属性--值声明 fd_status_field_value_setting
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param sourceStatusFieldSettingList  源字段声明集合, 用来在处理选项时反查字段定义
     * @param context         context
     */
    private void cloneFdStatusFieldValueSetting(Long sourceProjectId,
                                                Long targetProjectId,
                                                ProjectCloneContext context,
                                                List<StatusFieldSettingDTO> sourceStatusFieldSettingList) {
        List<StatusFieldValueSettingDTO> sourceStatusFieldValueSettingList = this.statusFieldValueSettingMapper.select(new StatusFieldValueSettingDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceStatusFieldValueSettingList)) {
            this.logger.debug("没有检测到可复制的 fd_status_field_value_setting 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_status_field_value_setting 数据{}条, 开始复制", sourceStatusFieldValueSettingList.size());
        final Set<String> fieldTypeFilter = SetUtils.hashSet(FieldType.RADIO, FieldType.CHECKBOX, FieldType.SINGLE, FieldType.MULTIPLE);
        final List<ObjectSchemeFieldDTO> mayBeUsedFields = context.getMayBeUsedFields();
        final Map<Long, ObjectSchemeFieldDTO> fieldIdToEntityMap = mayBeUsedFields.stream()
                .filter(mayBeUsedField -> fieldTypeFilter.contains(mayBeUsedField.getFieldType()))
                .collect(Collectors.toMap(ObjectSchemeFieldDTO::getId, Function.identity()));
        final Map<Long, Long> statusFieldSettingIdToFieldIdMap = sourceStatusFieldSettingList.stream().collect(Collectors.toMap(StatusFieldSettingDTO::getId, StatusFieldSettingDTO::getFieldId));
        for (StatusFieldValueSettingDTO sourceStatusFieldValueSetting : sourceStatusFieldValueSettingList) {
            final Long sourceStatusFieldValueSettingId = sourceStatusFieldValueSetting.getId();
            final Long sourceStatusFieldSettingId = sourceStatusFieldValueSetting.getStatusFieldSettingId();

            final Long newStatusFieldSettingId = context.getByTableAndSourceId(TABLE_FD_STATUS_FIELD_SETTING, sourceStatusFieldSettingId);
            if(newStatusFieldSettingId == null) {
                continue;
            }
            sourceStatusFieldValueSetting.setStatusFieldSettingId(newStatusFieldSettingId);

            // 处理选项
            // 只处理字段类型为选项类型&操作类型为指定值的
            if(fieldTypeFilter.contains(sourceStatusFieldValueSetting.getFieldType()) && StatusFieldSettingValidator.OPERATE_TYPE_SPECIFIER.equals(sourceStatusFieldValueSetting.getOperateType())) {
                final Long sourceOptionId = sourceStatusFieldValueSetting.getOptionId();
                Long newOptionId = sourceOptionId;
                // 根据ruleId获取对应的级联字段
                final ObjectSchemeFieldDTO field = Optional.ofNullable(statusFieldSettingIdToFieldIdMap.get(sourceStatusFieldSettingId)).map(fieldIdToEntityMap::get).orElse(null);
                // 如果找到了, 说明需要处理; 否则不处理
                if(field != null) {
                    if(Boolean.TRUE.equals(field.getSystem()) && FIELD_CODE_COMPONENT.equals(field.getCode())) {
                        // 特殊处理模块
                        final Long newComponentId = context.getByTableAndSourceId(TABLE_AGILE_ISSUE_COMPONENT, sourceOptionId);
                        if(newComponentId == null) {
                            continue;
                        }
                        newOptionId = newComponentId;
                    } else if(Boolean.TRUE.equals(field.getSystem()) && (FIELD_CODE_INFLUENCE_VERSION.equals(field.getCode()) || FIELD_CODE_FIX_VERSION.equals(field.getCode()))) {
                        // 特殊影响/修复的版本
                        final Long newProductVersionId = context.getByTableAndSourceId(TABLE_AGILE_PRODUCT_VERSION, sourceOptionId);
                        if(newProductVersionId == null) {
                            continue;
                        }
                        newOptionId = newProductVersionId;
                    } else if(!Boolean.TRUE.equals(field.getSystem()) && Objects.equals(field.getProjectId(), sourceProjectId)) {
                        // 处理项目层自定义字段
                        final Long newFieldOptionId = context.getByTableAndSourceId(TABLE_FD_FIELD_OPTION, sourceOptionId);
                        if(newFieldOptionId == null) {
                            continue;
                        }
                        newOptionId = newFieldOptionId;
                    }
                    // 不满足以上条件的, 暂时不需要处理
                }
                sourceStatusFieldValueSetting.setOptionId(newOptionId);
            }

            // customFieldId可能不是本项目的, 所以无法映射时就不处理
            final Long newCustomFieldId = context.getByTableAndSourceId(TABLE_FD_OBJECT_SCHEME_FIELD, sourceStatusFieldValueSetting.getCustomFieldId());
            if(newCustomFieldId != null) {
                sourceStatusFieldValueSetting.setCustomFieldId(newCustomFieldId);
            }

            sourceStatusFieldValueSetting.setId(null);
            sourceStatusFieldValueSetting.setProjectId(targetProjectId);
            if (this.statusFieldValueSettingMapper.insert(sourceStatusFieldValueSetting) != 1) {
                throw new CommonException("error.insert.fd_status_field_value_setting");
            }
            context.put(TABLE_FD_STATUS_FIELD_VALUE_SETTING, sourceStatusFieldValueSettingId, sourceStatusFieldValueSetting.getId());
        }
        this.logger.debug("fd_status_field_value_setting 复制完成");
    }


    /**
     * 复制状态机自定义流转--通知设置 fd_status_notice_setting
     * @param sourceProjectId sourceProjectId
     * @param targetProjectId targetProjectId
     * @param context         context
     */
    private void cloneFdStatusNoticeSetting(Long sourceProjectId,
                                            Long targetProjectId,
                                            ProjectCloneContext context) {
        List<StatusNoticeSettingDTO> sourceStatusNoticeSettingList = this.statusNoticeSettingMapper.select(new StatusNoticeSettingDTO().setProjectId(sourceProjectId));
        if (CollectionUtils.isEmpty(sourceStatusNoticeSettingList)) {
            this.logger.debug("没有检测到可复制的 fd_status_notice_setting 数据, 跳过此步骤");
            return;
        }
        this.logger.debug("检测到可复制的 fd_status_notice_setting 数据{}条, 开始复制", sourceStatusNoticeSettingList.size());
        for (StatusNoticeSettingDTO sourceStatusNoticeSetting : sourceStatusNoticeSettingList) {
            final Long sourceStatusNoticeSettingId = sourceStatusNoticeSetting.getId();

            // issueTypeId可能不是本项目的, 所以无法映射时就不处理
            final Long newIssueTypeId = context.getByTableAndSourceId(TABLE_FD_ISSUE_TYPE, sourceStatusNoticeSetting.getIssueTypeId());
            if(newIssueTypeId != null) {
                sourceStatusNoticeSetting.setIssueTypeId(newIssueTypeId);
            }
            // statusId都是组织层的, 不用复制

            sourceStatusNoticeSetting.setId(null);
            sourceStatusNoticeSetting.setProjectId(targetProjectId);
            if (this.statusNoticeSettingMapper.insert(sourceStatusNoticeSetting) != 1) {
                throw new CommonException("error.insert.fd_status_notice_setting");
            }
            context.put(TABLE_FD_STATUS_NOTICE_SETTING, sourceStatusNoticeSettingId, sourceStatusNoticeSetting.getId());
        }
        this.logger.debug("fd_status_notice_setting 复制完成");
    }

}
