package io.choerodon.agile.app.eventhandler;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import com.alibaba.fastjson.JSON;
import io.choerodon.agile.api.vo.event.DevopsMergeRequestPayload;
import io.choerodon.agile.api.vo.event.OrganizationCreateEventPayload;
import io.choerodon.agile.api.vo.event.ProjectEvent;
import io.choerodon.agile.api.vo.event.ProjectEventCategory;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.enums.SchemeApplyType;
import io.choerodon.agile.infra.feign.operator.TestServiceClientOperator;
import io.choerodon.agile.infra.mapper.ProjectInfoMapper;
import io.choerodon.agile.infra.utils.RedisUtil;
import io.choerodon.agile.infra.utils.SpringBeanUtil;
import io.choerodon.asgard.saga.annotation.SagaTask;
import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.hzero.starter.keyencrypt.core.EncryptType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import static io.choerodon.agile.infra.utils.SagaTopic.Organization.ORG_CREATE;
import static io.choerodon.agile.infra.utils.SagaTopic.Organization.TASK_ORG_CREATE;
import static io.choerodon.agile.infra.utils.SagaTopic.Project.*;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/22.
 * Email: fuqianghuang01@gmail.com
 */
@Component
public class AgileEventHandler {

    private static final Logger LOGGER = LoggerFactory.getLogger(AgileEventHandler.class);

    @Autowired
    private ProjectInfoService projectInfoService;
    @Autowired
    private ProjectInfoMapper projectInfoMapper;
    @Autowired
    private IssueLinkTypeService issueLinkTypeService;
    @Autowired
    private IssueTypeService issueTypeService;
    @Autowired
    private StateMachineSchemeService stateMachineSchemeService;
    @Autowired
    private IssueTypeSchemeService issueTypeSchemeService;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private InitService initService;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;

    @Autowired
    private ProjectConfigService projectConfigService;

    @Autowired
    private TestServiceClientOperator testServiceClientOperator;

    @Autowired
    private OrganizationConfigService organizationConfigService;

    @Autowired
    private BoardTemplateService boardTemplateService;
    @Autowired
    private StatusBranchMergeSettingService statusBranchMergeSettingService;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired(required = false)
    private AgileWaterfallService agileWaterfallService;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;

    @SagaTask(code = TASK_ORG_CREATE,
            description = "创建组织事件",
            sagaCode = ORG_CREATE,
            seq = 1)
    public String handleOrganizationCreateByConsumeSagaTask(String data) {
        LOGGER.info("消费创建组织消息{}", data);
        OrganizationCreateEventPayload organizationEventPayload = JSON.parseObject(data, OrganizationCreateEventPayload.class);
        Long organizationId = organizationEventPayload.getId();
        actualInitOrganization(organizationId);
        return data;
    }

    public void actualInitOrganization(Long organizationId) {
        //注册组织初始化问题类型
        issueTypeService.initIssueTypeIfNotExisted(organizationId);
        //注册组织初始化优先级
        priorityService.initPriorityIfNotExisted(organizationId);
        if (agilePluginService != null) {
            agilePluginService.initBusinessOrganization(organizationId);
        }
        //初始化状态
        initService.initStatusIfNotExisted(organizationId);
        //初始化默认状态机
        initService.initDefaultStateMachine(organizationId);
        //初始化页面配置数据
        objectSchemeFieldService.createSystemFieldIfNotExisted(organizationId);
    }

    /**
     * 创建项目事件
     *
     * @param message message
     */
    @SagaTask(code = TASK_PROJECT_CREATE,
            description = "agile消费创建项目事件初始化项目数据",
            sagaCode = PROJECT_CREATE,
            seq = 2)
    public String handleProjectInitByConsumeSagaTask(String message) {
        ProjectEvent projectEvent = JSON.parseObject(message, ProjectEvent.class);
        LOGGER.info("接受创建项目消息{}", message);
        List<ProjectEventCategory> projectEventCategories = projectEvent.getProjectCategoryVOS();
        if (!ObjectUtils.isEmpty(projectEventCategories)) {
            initIfAgileProject(projectEvent, projectEventCategories);
        }
        return message;
    }

    public void initIfAgileProject(ProjectEvent projectEvent, List<ProjectEventCategory> projectEventCategories) {
        Set<String> codes =
                projectEventCategories
                        .stream()
                        .map(ProjectEventCategory::getCode)
                        .collect(Collectors.toSet());
        if (ProjectCategory.consumeProjectCreatEvent(codes)) {
            LOGGER.info("初始化项目{}, code: {}", projectEvent.getProjectId(), projectEvent.getProjectCode());
            //创建projectInfo
            projectInfoService.initializationProjectInfo(projectEvent);
            //创建项目初始化issueLinkType
            issueLinkTypeService.initIssueLinkType(projectEvent.getProjectId());
            // 创建项目初始化风险状态机及问题类型方案
            if (!ObjectUtils.isEmpty(agilePluginService)) {
                agilePluginService.initProjectRiskIssueTypeScheme(projectEvent, codes);
            }
            if (codes.contains(ProjectCategory.MODULE_PROGRAM)) {
                //program + (program & agile)
                if (!ObjectUtils.isEmpty(agilePluginService)) {
                    agilePluginService.initProjectIssueTypeSchemeAndArt(projectEvent, codes);
                }
            } else if (codes.contains(ProjectCategory.MODULE_AGILE)) {
                //创建项目时创建默认状态机方案
                stateMachineSchemeService.initByConsumeCreateProject(projectEvent);
                //创建项目时创建默认问题类型方案
                issueTypeSchemeService.initByConsumeCreateProject(projectEvent.getProjectId(), projectEvent.getProjectCode());
                // 同步状态机模板和看板模板
                handlerOrganizationTemplate(projectEvent);
            } else if (codes.contains(ProjectCategory.MODULE_WATERFALL)) {
                if (!ObjectUtils.isEmpty(agileWaterfallService)) {
                    agileWaterfallService.initProject(projectEvent, codes);
                }
            }

            if (backlogExpandService != null && codes.contains(ProjectCategory.MODULE_BACKLOG)) {
                // 选择需求管理后默认开启需求池
                backlogExpandService.startBacklog(projectEvent.getProjectId());
            }
        }
    }

    public void handlerOrganizationTemplate(ProjectEvent projectEvent) {
        if (!ObjectUtils.isEmpty(projectEvent.getUseTemplate()) && Boolean.TRUE.equals(projectEvent.getUseTemplate())) {
            // 同步状态机模板
            organizationConfigService.syncStatusMachineTemplate(projectEvent, SchemeApplyType.AGILE);
            // 同步看板模板
            boardTemplateService.syncBoardTemplate(projectEvent, SchemeApplyType.AGILE);
        } else {
            // 初始化问题类型状态机
            projectConfigService.initIssueTypeStatusMachine(projectEvent.getProjectId(), SchemeApplyType.AGILE);
        }
    }

    /**
     * 更新项目事件
     *
     * @param message message
     */
    @SagaTask(code = TASK_PROJECT_UPDATE, sagaCode = PROJECT_UPDATE, seq = 2,
            description = "agile消费更新项目事件初始化项目数据")
    public String handleProjectUpdateByConsumeSagaTask(String message) {
        ProjectEvent projectEvent = JSON.parseObject(message, ProjectEvent.class);
        LOGGER.info("接受更新项目消息{}", message);
        Long projectId = projectEvent.getProjectId();
        // 删除redis的缓存
        SpringBeanUtil.getBean(RedisUtil.class).delete("projectInfo:" + projectId);
        List<ProjectEventCategory> projectEventCategories = projectEvent.getNewProjectCategoryVOS();
        if (!CollectionUtils.isEmpty(projectEventCategories)) {
            initIfAgileProject(projectEvent, projectEventCategories);
        } else {
            LOGGER.info("项目{}已初始化，跳过项目初始化", projectEvent.getProjectCode());
        }
        return message;
    }

    @SagaTask(code = TASK_BRANCH_MERGE_REQUEST_PASS, sagaCode = BRANCH_MERGE_REQUEST_PASS, seq = 1,
            description = "agile消费分支合并事件")
    public String handleBranchMergeEvent(String message) {
        DevopsMergeRequestPayload devopsMergeRequestPayload = JSON.parseObject(message, DevopsMergeRequestPayload.class);
        LOGGER.info("分支合并变更issue状态，{}", message);
        Long projectId = devopsMergeRequestPayload.getProjectId();
        List<Long> issueIds = devopsMergeRequestPayload.getIssueIds();
        EncryptContext.setEncryptType(EncryptType.TO_STRING.name());
        if (!ObjectUtils.isEmpty(issueIds)) {
            issueIds.forEach(x -> statusBranchMergeSettingService.handleBranchMergeEvent(projectId, x));
        }
        return message;
    }
}
