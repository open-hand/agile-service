package io.choerodon.agile.api.controller.v1;

import com.alibaba.fastjson.JSONObject;


import io.choerodon.agile.api.vo.business.*;
import io.choerodon.agile.app.service.IssueOperateService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.InitRoleCode;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import springfox.documentation.annotations.ApiIgnore;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.validator.IssueValidator;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.StateMachineClientService;
import io.choerodon.agile.infra.dto.business.IssueConvertDTO;
import io.choerodon.agile.infra.utils.VerifyUpdateUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.CustomPageRequest;

import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * 敏捷开发Issue
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 20:30:48
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/issues")
public class IssueController {

    private IssueService issueService;

    @Autowired
    private VerifyUpdateUtil verifyUpdateUtil;
    @Autowired
    private IssueValidator issueValidator;
    @Autowired
    private StateMachineClientService stateMachineClientService;
    @Autowired
    private IssueOperateService issueOperateService;

    public IssueController(IssueService issueService) {
        this.issueService = issueService;
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建issue")
    @PostMapping
    public ResponseEntity<IssueVO> createIssue(@ApiParam(value = "项目id", required = true)
                                                @PathVariable(name = "project_id") Long projectId,
                                               @ApiParam(value = "应用类型", required = true)
                                                @RequestParam(value = "applyType") String applyType,
                                               @ApiParam(value = "创建issue对象", required = true)
                                                @RequestBody IssueCreateVO issueCreateVO) {
        issueValidator.verifyCreateData(issueCreateVO, projectId, applyType);
        return Optional.ofNullable(stateMachineClientService.createIssue(issueCreateVO, applyType))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.Issue.createIssue"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("史诗名称重复校验")
    @GetMapping(value = "/check_epic_name")
    public ResponseEntity<Boolean> checkEpicName(@ApiParam(value = "项目id", required = true)
                                                 @PathVariable(name = "project_id") Long projectId,
                                                 @ApiParam(value = "史诗名称", required = true)
                                                 @RequestParam String epicName,
                                                 @ApiParam(value = "史诗id")
                                                 @RequestParam(required = false) @Encrypt Long epicId) {
        return Optional.ofNullable(issueService.checkEpicName(projectId, epicName, epicId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.checkEpicName.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建issue子任务")
    @PostMapping(value = "/sub_issue")
    public ResponseEntity<IssueSubVO> createSubIssue(@ApiParam(value = "项目id", required = true)
                                                      @PathVariable(name = "project_id") Long projectId,
                                                     @ApiParam(value = "创建issue子任务对象", required = true)
                                                      @RequestBody IssueSubCreateVO issueSubCreateVO) {
        issueValidator.verifySubCreateData(issueSubCreateVO, projectId);
        return Optional.ofNullable(stateMachineClientService.createSubIssue(issueSubCreateVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.Issue.createSubIssue"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新issue")
    @PutMapping
    public ResponseEntity<IssueVO> updateIssue(@ApiParam(value = "项目id", required = true)
                                                @PathVariable(name = "project_id") Long projectId,
                                               @ApiParam(value = "更新issue对象", required = true)
                                                @RequestBody JSONObject issueUpdate) {
        issueValidator.verifyUpdateData(issueUpdate, projectId);
        IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
        List<String> fieldList = verifyUpdateUtil.verifyUpdateData(issueUpdate,issueUpdateVO);
        return Optional.ofNullable(issueService.updateIssue(projectId, issueUpdateVO, fieldList))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.Issue.updateIssue"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新issue的状态")
    @PutMapping("/update_status")
    public ResponseEntity<IssueVO> updateIssueStatus(@ApiParam(value = "项目id", required = true)
                                                      @PathVariable(name = "project_id") Long projectId,
                                                     @ApiParam(value = "转换id", required = true)
                                                      @RequestParam @Encrypt Long transformId,
                                                     @ApiParam(value = "问题id", required = true)
                                                      @RequestParam @Encrypt Long issueId,
                                                     @ApiParam(value = "版本号", required = true)
                                                      @RequestParam Long objectVersionNumber,
                                                     @ApiParam(value = "应用类型", required = true)
                                                      @RequestParam String applyType) {
        issueValidator.verifyIssueUpdateStatus(projectId,issueId,transformId);
        return Optional.ofNullable(issueService.updateIssueStatus(projectId, issueId, transformId, objectVersionNumber, applyType))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.Issue.updateIssueStatus"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询单个issue")
    @GetMapping(value = "/{issueId}")
    public ResponseEntity<IssueVO> queryIssue(@ApiParam(value = "项目id", required = true)
                                               @PathVariable(name = "project_id") Long projectId,
                                              @ApiParam(value = "issueId", required = true)
                                               @PathVariable @Encrypt Long issueId,
                                              @ApiParam(value = "组织id")
                                               @RequestParam(required = false) Long organizationId) {
        return Optional.ofNullable(issueService.queryIssue(projectId, issueId, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.queryIssue"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询单个子任务issue")
    @GetMapping(value = "/sub_issue/{issueId}")
    public ResponseEntity<IssueSubVO> queryIssueSub(@ApiParam(value = "项目id", required = true)
                                                     @PathVariable(name = "project_id") Long projectId,
                                                    @ApiParam(value = "组织id", required = true)
                                                     @RequestParam Long organizationId,
                                                    @ApiParam(value = "issueId", required = true)
                                                     @PathVariable @Encrypt Long issueId) {
        return Optional.ofNullable(issueService.queryIssueSub(projectId, organizationId, issueId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.queryIssueSub"));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("分页查询问题列表，包含子任务")
    @PostMapping(value = "/include_sub")
    public ResponseEntity<Page<IssueListFieldKVVO>> listIssueWithSub(@ApiIgnore
                                                               @ApiParam(value = "分页信息", required = true)
                                                               @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                       PageRequest pageRequest,
                                                                     @ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "查询参数", required = true)
                                                               @RequestBody(required = false) SearchVO searchVO,
                                                                     @ApiParam(value = "查询参数", required = true)
                                                               @RequestParam(required = false) Long organizationId) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return Optional.ofNullable(issueService.listIssueWithSub(projectId, searchVO, pageRequest, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.listIssueWithSub"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("分页搜索查询issue列表(包含子任务)")
    @CustomPageRequest
    @PostMapping(value = "/summary")
    public ResponseEntity<Page<IssueNumVO>> queryIssueByOption(@ApiIgnore
                                                                @ApiParam(value = "分页信息", required = true)
                                                                @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                        PageRequest pageRequest,
                                                               @ApiParam(value = "项目id", required = true)
                                                                @PathVariable(name = "project_id") Long projectId,
                                                               @ApiParam(value = "筛选条件")
                                                                @RequestBody IssueFilterParamVO issueFilterParamVO) {
        return Optional.ofNullable(issueService.queryIssueByOption(projectId, issueFilterParamVO, pageRequest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.queryIssueByOption"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("分页搜索查询issue列表")
    @CustomPageRequest
    @PostMapping(value = "/agile/summary")
    public ResponseEntity<Page<IssueNumVO>> queryIssueByOptionForAgile(@ApiIgnore @ApiParam(value = "分页信息", required = true)
                                                                       @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                            PageRequest pageRequest,
                                                                       @ApiParam(value = "项目id", required = true)
                                                                       @PathVariable(name = "project_id") Long projectId,
                                                                       @ApiParam(value = "issueId")
                                                                       @RequestParam(required = false)  @Encrypt Long issueId,
                                                                       @ApiParam(value = "issueNum")
                                                                       @RequestParam(required = false) String issueNum,
                                                                       @ApiParam(value = "是否包含自身", required = true)
                                                                       @RequestParam() Boolean self,
                                                                       @ApiParam(value = "搜索内容")
                                                                       @RequestParam(required = false) String content,
                                                                       @ApiParam(value = "不包含的issueId")
                                                                       @RequestBody(required = false) @Encrypt
                                                                            List<Long> excludeIssueIds) {
        return Optional.ofNullable(issueService.queryIssueByOptionForAgile(projectId, issueId, issueNum, self, content, pageRequest, excludeIssueIds))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.queryIssueByOptionForAgile"));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询epic")
    @GetMapping(value = "/epics")
    public ResponseEntity<List<EpicDataVO>> listEpic(@ApiParam(value = "项目id", required = true)
                                                      @PathVariable(name = "project_id") Long projectId) {
        return Optional.ofNullable(issueService.listEpic(projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Epic.listEpic"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("通过issueId删除")
    @DeleteMapping(value = "/{issueId}")
    public ResponseEntity deleteIssue(@ApiParam(value = "项目id", required = true)
                                      @PathVariable(name = "project_id") Long projectId,
                                      @ApiParam(value = "issueId", required = true)
                                      @PathVariable @Encrypt Long issueId) {
        issueService.deleteIssue(projectId, issueId);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除自己创建的issue")
    @DeleteMapping(value = "/delete_self_issue/{issueId}")
    public ResponseEntity deleteSelfIssue(@ApiParam(value = "项目id", required = true)
                                      @PathVariable(name = "project_id") Long projectId,
                                      @ApiParam(value = "issueId", required = true)
                                      @PathVariable @Encrypt Long issueId) {
        issueService.deleteSelfIssue(projectId, issueId);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("批量删除Issue,给测试")
    @DeleteMapping(value = "/to_version_test")
    public ResponseEntity batchDeleteIssues(@ApiParam(value = "项目id", required = true)
                                            @PathVariable(name = "project_id") Long projectId,
                                            @ApiParam(value = "issue id", required = true)
                                            @RequestBody @Encrypt  List<Long> issueIds) {
        issueService.batchDeleteIssues(projectId, issueIds);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("issue批量加入版本")
    @PostMapping(value = "/to_version/{versionId}")
    public ResponseEntity<List<IssueSearchVO>> batchIssueToVersion(@ApiParam(value = "项目id", required = true)
                                                                    @PathVariable(name = "project_id") Long projectId,
                                                                   @ApiParam(value = "versionId", required = true)
                                                                    @PathVariable @Encrypt(ignoreValue = {"0"}) Long versionId,
                                                                   @ApiParam(value = "issue id", required = true)
                                                                    @RequestBody @Encrypt List<Long> issueIds) {
        return Optional.ofNullable(issueService.batchIssueToVersion(projectId, versionId, issueIds))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.issue.batchToVersion"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("批量替换issue版本,给测试")
    @PostMapping(value = "/to_version_test/{versionId}")
    public ResponseEntity batchIssueToVersionTest(@ApiParam(value = "项目id", required = true)
                                                  @PathVariable(name = "project_id") Long projectId,
                                                  @ApiParam(value = "versionId", required = true)
                                                  @PathVariable @Encrypt(ignoreValue = {"0"}) Long versionId,
                                                  @ApiParam(value = "issue id", required = true)
                                                  @RequestBody @Encrypt List<Long> issueIds) {
        issueService.batchIssueToVersionTest(projectId, versionId, issueIds);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("issue批量加入epic")
    @PostMapping(value = "/to_epic/{epicId}")
    public ResponseEntity<List<IssueSearchVO>> batchIssueToEpic(@ApiParam(value = "项目id", required = true)
                                                                 @PathVariable(name = "project_id") Long projectId,
                                                                @ApiParam(value = "epicId", required = true)
                                                                 @PathVariable @Encrypt(ignoreValue = {"0"})  Long epicId,
                                                                @ApiParam(value = "issue id", required = true)
                                                                 @RequestBody @Encrypt List<Long> issueIds) {
        return Optional.ofNullable(issueService.batchIssueToEpic(projectId, epicId, issueIds))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.issue.batchToEpic"));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("issue批量加入冲刺")
    @PostMapping(value = "/to_sprint/{sprintId}")
    public ResponseEntity<List<IssueSearchVO>> batchIssueToSprint(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                  @ApiParam(value = "sprintId", required = true)
                                                                   @PathVariable @Encrypt(ignoreValue = {"0"}) Long sprintId,
                                                                  @ApiParam(value = "移卡信息", required = true)
                                                                   @RequestBody MoveIssueVO moveIssueVO) {
        return Optional.ofNullable(issueService.batchIssueToSprint(projectId, sprintId, moveIssueVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.issue.batchToSprint"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("分页查询当前项目下的epic，提供给列表下拉，如果有传入的id，置前显示")
    @PostMapping(value = "/epics/select_data")
    public ResponseEntity<Page<IssueEpicVO>> listEpicSelectData(@ApiParam(value = "项目id", required = true)
                                                                @PathVariable(name = "project_id") Long projectId,
                                                                @ApiParam(value = "分页信息", required = true)
                                                                @ApiIgnore PageRequest pageRequest,
                                                                @ApiParam(value = "只查询未完成", defaultValue = "false")
                                                                @RequestParam(required = false, defaultValue = "false")
                                                                    Boolean onlyUnCompleted,
                                                                @ApiParam(value = "搜索内容")
                                                                @RequestParam(required = false) String param,
                                                                @ApiParam(value = "史诗ids")
                                                                @RequestBody(required = false) @Encrypt(ignoreValue = {"0"}) List<Long> epicIds) {
        return Optional.ofNullable(issueService.listEpicSelectData(projectId, pageRequest, onlyUnCompleted, param, epicIds))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.queryIssueEpicList"));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询当前项目下的epic，提供给列表下拉")
    @GetMapping(value = "/{issueId}/list_required_field")
    public ResponseEntity<List<PageFieldViewVO>> listRequiredFieldByIssueType(@ApiParam(value = "项目id", required = true)
                                                                              @PathVariable(name = "project_id") Long projectId,
                                                                              @PathVariable @Encrypt Long issueId,
                                                                              @ApiParam(value = "组织id", required = true)
                                                                              @RequestParam Long organizationId,
                                                                              @ApiParam(value = "问题类型id", required = true)
                                                                              @RequestParam @Encrypt Long issueTypeId) {
        return ResponseEntity.ok(issueService.listRequiredFieldByIssueType(projectId, organizationId, issueId, issueTypeId));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更改issue类型")
    @PostMapping("/update_type")
    public ResponseEntity<IssueVO> updateIssueTypeCode(@ApiParam(value = "项目id", required = true)
                                                       @PathVariable(name = "project_id") Long projectId,
                                                       @ApiParam(value = "组织id", required = true)
                                                       @RequestParam Long organizationId,
                                                       @ApiParam(value = "修改类型信息", required = true)
                                                       @RequestBody IssueUpdateTypeVO issueUpdateTypeVO) {
        IssueConvertDTO issueConvertDTO = issueValidator.verifyUpdateTypeData(projectId, issueUpdateTypeVO);
        return Optional.ofNullable(issueService.updateIssueTypeCode(issueConvertDTO, issueUpdateTypeVO, organizationId, projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.issue.updateIssueTypeCode"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("任务转换为子任务")
    @PostMapping("/transformed_sub_task")
    public ResponseEntity<IssueSubVO> transformedSubTask(@ApiParam(value = "项目id", required = true)
                                                          @PathVariable(name = "project_id") Long projectId,
                                                         @ApiParam(value = "组织id", required = true)
                                                          @RequestParam Long organizationId,
                                                         @ApiParam(value = "转换子任务信息", required = true)
                                                          @RequestBody IssueTransformSubTask issueTransformSubTask) {
        issueValidator.verifyTransformedSubTask(projectId, issueTransformSubTask);
        return Optional.ofNullable(issueService.transformedSubTask(projectId, organizationId, issueTransformSubTask))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.issue.transformedSubTask"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("子任务转换为任务")
    @PostMapping("/transformed_task")
    public ResponseEntity<IssueVO> transformedTask(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId,
                                                   @ApiParam(value = "组织id", required = true)
                                                    @RequestParam Long organizationId,
                                                   @ApiParam(value = "转换任务信息", required = true)
                                                    @RequestBody IssueTransformTask issueTransformTask) {
        IssueConvertDTO issueConvertDTO = issueValidator.verifyTransformedTask(projectId, issueTransformTask);
        return Optional.ofNullable(issueService.transformedTask(issueConvertDTO, issueTransformTask, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.issue.transformedTask"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("复制一个issue")
    @PostMapping("/{issueId}/clone_issue")
    public ResponseEntity<Void> cloneIssueByIssueId(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId,
                                                    @ApiParam(value = "issueId", required = true)
                                                    @PathVariable(name = "issueId") @Encrypt Long issueId,
                                                    @ApiParam(value = "组织id", required = true)
                                                    @RequestParam Long organizationId,
                                                    @ApiParam(value = "应用类型", required = true)
                                                    @RequestParam(value = "applyType") String applyType,
                                                    @ApiParam(value = "异步任务id", required = true)
                                                    @RequestParam(value = "asyncTraceId") String asyncTraceId,
                                                    @ApiParam(value = "复制条件", required = true)
                                                    @RequestBody CopyConditionVO copyConditionVO) {
        issueValidator.checkPredefinedFields(copyConditionVO.getPredefinedFieldNames());
        issueOperateService.cloneIssueByIssueId(projectId, issueId, copyConditionVO, organizationId, applyType, asyncTraceId, (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes());
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工作项复制异步任务执行状态")
    @GetMapping(value = "/{issueId}/query_async_clone_status")
    public ResponseEntity<String> queryAsyncCloneStatus(@ApiParam(value = "项目id", required = true)
                                                        @PathVariable("project_id") Long projectId,
                                                        @ApiParam(value = "issueId", required = true)
                                                        @PathVariable(name = "issueId") @Encrypt Long issueId,
                                                        @ApiParam(value = "异步任务id", required = true)
                                                        @RequestParam(value = "asyncTraceId") String asyncTraceId) {
        return ResponseEntity.ok(issueService.queryAsyncCloneStatus(projectId, issueId, asyncTraceId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issue ids查询issue相关信息")
    @PostMapping("/issue_infos")
    public ResponseEntity<List<IssueInfoVO>> listByIssueIds(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable(name = "project_id") Long projectId,
                                                            @ApiParam(value = "issue ids", required = true)
                                                             @RequestBody @Encrypt List<Long> issueIds) {
        return Optional.ofNullable(issueService.listByIssueIds(projectId, issueIds))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.issueNums.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("分页过滤查询issue列表提供给测试模块用")
    @CustomPageRequest
    @PostMapping(value = "/test_component/no_sub")
    public ResponseEntity<Page<IssueListTestVO>> listIssueWithoutSubToTestComponent(@ApiIgnore
                                                                                     @ApiParam(value = "分页信息", required = true)
                                                                                     @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                                     PageRequest pageRequest,
                                                                                     @ApiParam(value = "项目id", required = true)
                                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                                     @ApiParam(value = "组织id", required = true)
                                                                                     @RequestParam Long organizationId,
                                                                                     @ApiParam(value = "查询参数", required = true)
                                                                                     @RequestBody(required = false) SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return Optional.ofNullable(issueService.listIssueWithoutSubToTestComponent(projectId, searchVO, pageRequest, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.listIssueWithoutSubToTestComponent"));
    }


    @Permission(level = ResourceLevel.PROJECT, roles = {InitRoleCode.PROJECT_MEMBER, InitRoleCode.PROJECT_OWNER})
    @ApiOperation("分页过滤查询issue列表, 测试项目接口，过滤linked issue")
    @CustomPageRequest
    @PostMapping(value = "/test_component/filter_linked")
    public ResponseEntity<Page<IssueListTestWithSprintVersionVO>> listIssueWithLinkedIssues(@ApiIgnore
                                                                                             @ApiParam(value = "分页信息", required = true)
                                                                                             @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                                                     PageRequest pageable,
                                                                                                @ApiParam(value = "项目id", required = true)
                                                                                             @PathVariable(name = "project_id") Long projectId,
                                                                                                @ApiParam(value = "组织id", required = true)
                                                                                             @RequestParam Long organizationId,
                                                                                                @ApiParam(value = "查询参数", required = true)
                                                                                             @RequestBody(required = false) SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return Optional.ofNullable(issueService.listIssueWithLinkedIssues(projectId, searchVO, pageable, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.listIssueWithBlockedIssues"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据时间段查询问题类型的数量")
    @GetMapping(value = "/type/{typeCode}")
    public ResponseEntity<List<IssueCreationNumVO>> queryIssueNumByTimeSlot(@ApiParam(value = "项目id", required = true)
                                                                             @PathVariable(name = "project_id") Long projectId,
                                                                            @ApiParam(value = "type code", required = true)
                                                                             @PathVariable String typeCode,
                                                                            @ApiParam(value = "时间段", required = true)
                                                                             @RequestParam Integer timeSlot) {
        return Optional.ofNullable(issueService.queryIssueNumByTimeSlot(projectId, typeCode, timeSlot))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.timeSlotCount.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "拖动epic位置")
    @PutMapping(value = "/epic_drag")
    public ResponseEntity<EpicDataVO> dragEpic(@ApiParam(value = "项目id", required = true)
                                                @PathVariable(name = "project_id") Long projectId,
                                               @ApiParam(value = "排序对象", required = true)
                                                @RequestBody EpicSequenceVO epicSequenceVO) {
        return Optional.ofNullable(issueService.dragEpic(projectId, epicSequenceVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.issueController.dragEpic"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更改父任务")
    @PostMapping(value = "/update_parent")
    public ResponseEntity<IssueVO> updateIssueParentId(@ApiParam(value = "项目id", required = true)
                                                        @PathVariable(name = "project_id") Long projectId,
                                                       @ApiParam(value = "issue parent id update vo", required = true)
                                                        @RequestBody IssueUpdateParentIdVO issueUpdateParentIdVO) {
        return Optional.ofNullable(issueService.issueParentIdUpdate(projectId, issueUpdateParentIdVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.issueParentId.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("统计当前项目下未完成的任务数，包括故事、任务、缺陷")
    @GetMapping(value = "/count")
    public ResponseEntity<JSONObject> countUnResolveByProjectId(@ApiParam(value = "项目id", required = true)
                                                                @PathVariable(name = "project_id") Long projectId) {
        return Optional.ofNullable(issueService.countUnResolveByProjectId(projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.countUnResolveIssue.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据条件过滤查询返回issueIds，测试项目接口")
    @PostMapping(value = "/issue_ids")
    public ResponseEntity<List<Long>> queryIssueIdsByOptions(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable(name = "project_id") Long projectId,
                                                             @ApiParam(value = "查询参数", required = true)
                                                             @RequestBody SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return Optional.ofNullable(issueService.queryIssueIdsByOptions(projectId, searchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.issueIds.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询未分配的问题，类型为story,task,bug")
    @GetMapping(value = "/undistributed")
    public ResponseEntity<Page<UndistributedIssueVO>> queryUnDistributedIssues(@ApiParam(value = "项目id", required = true)
                                                                                @PathVariable(name = "project_id") Long projectId,
                                                                                   @ApiParam(value = "分页信息", required = true)
                                                                                @ApiIgnore PageRequest pageRequest) {
        return Optional.ofNullable(issueService.queryUnDistributedIssues(projectId, pageRequest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.UndistributedIssueList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询经办人未完成的问题，类型为story,task,bug")
    @GetMapping(value = "/unfinished/{assignee_id}")
    public ResponseEntity<List<UnfinishedIssueVO>> queryUnfinishedIssues(@ApiParam(value = "项目id", required = true)
                                                                          @PathVariable(name = "project_id") Long projectId,
                                                                         @ApiParam(value = "经办人id", required = true)
                                                                          @PathVariable(name = "assignee_id") Long assigneeId) {
        return Optional.ofNullable(issueService.queryUnfinishedIssues(projectId, assigneeId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.UnfinishedIssueList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询用户故事地图泳道")
    @GetMapping(value = "/storymap/swim_lane")
    public ResponseEntity<String> querySwimLaneCode(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId) {
        return Optional.ofNullable(issueService.querySwimLaneCode(projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.querySwimLaneCode.get"));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询项目下的故事和任务(不包含子任务以及子bug)")
    @PostMapping(value = "/query_story_task")
    public ResponseEntity<Page<IssueListFieldKVVO>> queryStoryAndTask(@ApiParam(value = "项目id", required = true)
                                                                      @PathVariable(name = "project_id") Long projectId,
                                                                      @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                              PageRequest pageRequest,
                                                                      @ApiParam(value = "筛选条件")
                                                                      @RequestBody(required = false) SearchVO searchVO) {
        return Optional.ofNullable(issueService.queryStoryAndTask(projectId, pageRequest, searchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.issue.queryIssueByIssueIds"));
    }


    @CustomPageRequest
    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询项目所有经办人")
    @PostMapping(value = "/users")
    public ResponseEntity<Page<UserDTO>> pagingQueryUsers(@ApiIgnore
                                                          @ApiParam(value = "分页信息", required = true)
                                                          PageRequest pageRequest,
                                                          @ApiParam(value = "项目id", required = true)
                                                          @PathVariable(name = "project_id") Long projectId,
                                                          @ApiParam(value = "用户筛选条件")
                                                          @RequestParam(value = "param", required = false) String param,
                                                          @ApiParam(value = "忽略的用户id")
                                                          @RequestBody(required = false) @Encrypt Set<Long> ignoredUserIds) {
        return ResponseEntity.ok(issueService.pagingQueryUsers(pageRequest, projectId, param, ignoredUserIds));
    }

    @CustomPageRequest
    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询项目所有报告人")
    @PostMapping(value = "/reporters")
    public ResponseEntity<Page<UserDTO>> pagingQueryReporters(@ApiIgnore
                                                              @ApiParam(value = "分页信息", required = true)
                                                              PageRequest pageRequest,
                                                              @ApiParam(value = "项目id", required = true)
                                                              @PathVariable(name = "project_id") Long projectId,
                                                              @ApiParam(value = "用户筛选条件")
                                                              @RequestParam(value = "param", required = false) String param,
                                                              @ApiParam(value = "忽略的用户id")
                                                              @RequestBody(required = false) @Encrypt Set<Long> ignoredUserIds) {
        return ResponseEntity.ok(issueService.pagingQueryReporters(pageRequest, projectId, param, ignoredUserIds));
    }

    @CustomPageRequest
    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据项目id和问题类型分页查询可以选择的父问题")
    @PostMapping(value = "/available_parents")
    public ResponseEntity<Page<IssueVO>> pagingQueryAvailableParents(@ApiIgnore
                                                                     @ApiParam(value = "分页信息", required = true)
                                                                             PageRequest pageRequest,
                                                                     @ApiParam(value = "项目id", required = true)
                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                     @ApiParam(value = "问题类型", required = true)
                                                                     @RequestParam String issueType,
                                                                     @ApiParam(value = "模糊查询参数")
                                                                     @RequestParam(value = "param", required = false) String param) {
        return ResponseEntity.ok(issueService.pagingQueryAvailableParents(pageRequest, projectId, issueType, param));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("批量删除issue")
    @PostMapping(value = "/batch_delete")
    public ResponseEntity batchDeleteIssue(@ApiParam(value = "项目id", required = true)
                                           @PathVariable(name = "project_id") Long projectId,
                                           @ApiParam(value = "issueIds", required = true)
                                           @RequestBody @Encrypt List<Long> issueIds) {
        issueOperateService.batchDeleteIssue(projectId, issueIds);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("测试专用-执行状态变更去修改issue状态")
    @PostMapping(value = "/execution_update_status")
    public ResponseEntity executionUpdateStatus(@ApiParam(value = "项目id", required = true)
                                                @PathVariable(name = "project_id") Long projectId,
                                                @ApiParam(value = "issueId", required = true)
                                                @RequestParam @Encrypt Long issueId,
                                                @ApiParam(value = "更新的issue状态")
                                                @RequestBody ExecutionUpdateIssueVO executionUpdateIssueVO) {
        issueService.executionUpdateStatus(projectId, issueId, executionUpdateIssueVO);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询当前项目下的epic，提供给列表下拉")
    @GetMapping(value = "/{issueId}/list_link_contents")
    public ResponseEntity<List<String>> listLinkContents(@ApiParam(value = "项目id", required = true)
                                                         @PathVariable(name = "project_id") Long projectId,
                                                         @ApiParam(value = "issue id", required = true)
                                                         @PathVariable @Encrypt Long issueId) {
        return ResponseEntity.ok(issueService.listLinkContents(projectId, issueId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询当前项目下的epic，提供给列表下拉")
    @GetMapping(value = "/{issueId}/all_required_field")
    public ResponseEntity<List<IssueRequiredFields>> listAllRequiredField(@ApiParam(value = "项目id", required = true)
                                                                          @PathVariable(name = "project_id") Long projectId,
                                                                          @PathVariable @Encrypt Long issueId,
                                                                          @ApiParam(value = "组织id", required = true)
                                                                          @RequestParam Long organizationId,
                                                                          @ApiParam(value = "是否复制子项", required = true)
                                                                          @RequestParam Boolean subTask) {
        return ResponseEntity.ok(issueService.listAllRequiredField(projectId, organizationId, issueId, subTask));
    }
}