package io.choerodon.agile.api.controller.v1;

import java.util.List;
import java.util.Set;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.apache.commons.collections4.SetUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.api.vo.DataLogFixVO;
import io.choerodon.agile.api.vo.IssueLinkFixVO;
import io.choerodon.agile.api.vo.ProjectInfoFixVO;
import io.choerodon.agile.api.vo.TestVersionFixVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.domain.context.ProjectCloneContext;
import io.choerodon.agile.domain.service.ProjectCloneDomainService;
import io.choerodon.agile.infra.dto.MessageDetailDTO;
import io.choerodon.agile.infra.dto.TestCaseAttachmentDTO;
import io.choerodon.agile.infra.dto.TestCaseDTO;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.util.Results;


/**
 * Created by HuangFuqiang@choerodon.io on 2018/11/13.
 * Email: fuqianghuang01@gmail.com
 */
@RestController
@RequestMapping(value = "/v1/fix_data")
public class FixDataController {

    private static final Logger LOGGER = LoggerFactory.getLogger(FixDataController.class);

    @Autowired
    private FixDataService fixDataService;

    @Autowired
    private IssueService issueService;

    @Autowired
    private IssueAttachmentService issueAttachmentService;
    @Autowired
    private IssueLinkService issueLinkService;
    @Autowired
    private ProjectInfoService projectInfoService;
    @Autowired
    private DataLogService dataLogService;
    @Autowired
    private ProductVersionService productVersionService;
    @Autowired
    private NoticeService noticeService;
    @Autowired
    private ProjectCloneDomainService projectCloneDomainService;

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("修复0.19创建项目产生的脏数据【全部】")
    @GetMapping(value = "/fix_create_project_0.19")
    public ResponseEntity<Void> fixCreateProject() {
        fixDataService.fixCreateProject();
        return Results.success();
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("修复0.19创建项目产生的脏数据【单个】")
    @GetMapping(value = "/fix_create_project_0.19_single")
    public ResponseEntity<Void> fixCreateProjectSingle(@ApiParam(value = "项目id", required = true)
                                                 @RequestParam("projectId") Long projectId) {
        fixDataService.fixCreateProjectSingle(projectId);
        return Results.success();
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.20】【迁移数据专用】查询测试用例的projectId")
    @GetMapping(value = "/project_ids")
    public ResponseEntity<List<Long>> queryProjectId(){
        return new ResponseEntity<>(issueService.queryProjectIds(), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.20】【迁移数据专用】根据projectId分批次将测试用例数据传递给test_manager")
    @GetMapping(value = "/migrate_issue/{project_id}")
    public ResponseEntity<List<TestCaseDTO>> migrateIssue(@ApiParam(value = "组织项目id", required = true)
                                                          @PathVariable("project_id") Long projectId) {
        return new ResponseEntity<>(issueService.migrateTestCaseByProjectId(projectId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.20】【迁移数据专用，迁移测试用例的附件信息】")
    @GetMapping(value = "/migrate_attachment")
    public ResponseEntity<List<TestCaseAttachmentDTO>> migrateIssueAttachment(){
        return new ResponseEntity<>(issueAttachmentService.migrateIssueAttachment(), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.20】迁移项目下link")
    @GetMapping(value = "/migrate_issueLink/{project_id}")
    public ResponseEntity<List<IssueLinkFixVO>> listIssueLinkByIssueIds(@ApiParam(value = "项目id", required = true)
                                                                        @PathVariable(name = "project_id") Long projectId) {
        return new ResponseEntity<>(issueLinkService.listIssueLinkByIssuedIds(projectId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.20】迁移projectInfo")
    @GetMapping("/migrate_project_info")
    public ResponseEntity<List<ProjectInfoFixVO>> queryAllProjectInfo() {
        return new ResponseEntity<>(projectInfoService.queryAllProjectInfo(), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.20】迁移日志")
    @GetMapping("/migrate_data_log/{project_id}")
    public ResponseEntity<List<DataLogFixVO>> migrateDataLog(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable(name = "project_id") Long projectId) {
        return new ResponseEntity<>(dataLogService.queryListByProjectId(projectId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.20】迁移版本")
    @GetMapping("/migrate_version")
    public ResponseEntity<List<TestVersionFixVO>> migrateVersion() {
        return new ResponseEntity<>(productVersionService.queryByVersionId(), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.20 BASE】迁移agile_message_detail到框架")
    @GetMapping("/migrate_message")
    public ResponseEntity<List<MessageDetailDTO>> migrateMessageDetail() {
        return new ResponseEntity<>(noticeService.migrateMessageDetail(),HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.20 BASE】启动敏捷迁移到Base的task")
    @GetMapping("/migration_to_base")
    public ResponseEntity<Void> migrateToBase() {
        LOGGER.info("==============================>>>>>>>> AGILE Data Migrate Start In Controller <<<<<<<<=================================");
//        notifyFeignClient.checkLog("0.20.0", "agile");
        return Results.success();
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.24】修复状态机和页面数据")
    @GetMapping("/fix_data_state_machine_and_page")
    public ResponseEntity<Void> fixDataStateMachineAndPage() {
        fixDataService.fixDateStateMachineAndPage();
        return Results.success();
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.24】修复页面数据")
    @GetMapping("/fix_page")
    public ResponseEntity<Void> fixPage() {
        fixDataService.fixPage();
        return Results.success();
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("【0.25】修复问题类型数据")
    @GetMapping("/fix_issue_type")
    public ResponseEntity<Void> fixIssueType() {
        fixDataService.fixIssueTypeData();
        return Results.success();
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("修复项目群和项目融合数据以及需求状态机数据")
    @GetMapping("/fix_agile_and_program")
    public ResponseEntity<Void> fixDate() {
        fixDataService.fixAgileAndProgram();
        return Results.success();
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("修复状态机自定义流转角色数据")
    @GetMapping("/fix_status_transfer_role_data")
    public ResponseEntity<Void> fixStatusTransferRoleData() {
        fixDataService.fixStatusMachineCustomTransferRoleData();
        return Results.success();
    }

    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("修复高级筛选个人筛选数据")
    @PostMapping("/fix_personal_filter")
    public ResponseEntity<Void> fixPersonalFilter(@RequestBody Set<String> typeCodes) {
        fixDataService.fixPersonalFilter(typeCodes);
        return Results.success();
    }


    @Permission(level = ResourceLevel.SITE)
    @ApiOperation("根据项目复制数据测试接口")
    @PostMapping("/copy_project_template")
    public ResponseEntity<Void> copyProjectTemplate(@RequestParam Long sourceProjectId,
                                              @RequestParam Long targetProjectId) {
        projectCloneDomainService.cloneProject(
                sourceProjectId,
                targetProjectId,
                 new ProjectCloneContext()
                         .setCategoryCodes(SetUtils.unmodifiableSet(ProjectCategory.MODULE_WATERFALL))
                         .setOrganizationId(ConvertUtil.getOrganizationId(sourceProjectId))
        );
        return Results.success();
    }
}
