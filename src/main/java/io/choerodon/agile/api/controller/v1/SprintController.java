package io.choerodon.agile.api.controller.v1;


import java.util.List;
import java.util.Map;
import java.util.Optional;
import javax.validation.Valid;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListVO;
import io.choerodon.agile.api.vo.business.IssueSearchVO;
import io.choerodon.agile.api.vo.business.SprintDetailVO;
import io.choerodon.agile.app.service.SprintService;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.base.BaseController;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * Created by jian_zhang02@163.com on 2018/5/14.
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/sprint")
public class SprintController extends BaseController {

    @Autowired
    private SprintService sprintService;

    private static final String CREATE_ERROR = "error.sprint.create";
    private static final String UPDATE_ERROR = "error.sprint.update";
    private static final String DELETE_ERROR = "error.sprint.delete";
    private static final String QUERY_ERROR = "error.spring.query";
    private static final String QUERY_NAME_ERROR = "error.sprintName.query";
    private static final String OPEN_ERROR = "error.sprint.open";
    private static final String CLOSE_ERROR = "error.sprint.close";
    private static final String QUERY_SPRINT_MESSAGE_ERROR = "error.sprintMessage.query";

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "创建冲刺")
    @PostMapping
    public ResponseEntity<SprintDetailVO> createSprint(@ApiParam(value = "项目id", required = true)
                                                       @PathVariable(name = "project_id") Long projectId,
                                                       @ApiParam(value = "冲刺创建VO", required = true)
                                                       @RequestBody @Valid SprintCreateVO sprintCreateVO) {
        return Optional.ofNullable(sprintService.createSprintByDetails(projectId, sprintCreateVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException(CREATE_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新冲刺部分字段")
    @PutMapping
    public ResponseEntity<SprintDetailVO> updateSprint(@ApiParam(value = "项目id", required = true)
                                                       @PathVariable(name = "project_id") Long projectId,
                                                       @ApiParam(value = "冲刺DTO对象", required = true)
                                                       @RequestBody @Valid SprintUpdateVO sprintUpdateVO) {
        return Optional.ofNullable(sprintService.updateSprint(projectId, sprintUpdateVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException(UPDATE_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id删除冲刺")
    @DeleteMapping(value = "/{sprintId}")
    public ResponseEntity<Boolean> deleteSprint(@ApiParam(value = "项目id", required = true)
                                                @PathVariable(name = "project_id") Long projectId,
                                                @ApiParam(value = "sprintId", required = true)
                                                @PathVariable @Encrypt Long sprintId) {
        return Optional.ofNullable(sprintService.deleteSprint(projectId, sprintId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.NO_CONTENT))
                .orElseThrow(() -> new CommonException(DELETE_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "联合查询sprint及其issue")
    @PostMapping(value = "/issues")
    public ResponseEntity<Map<String, Object>> queryByProjectId(@ApiParam(value = "项目id", required = true)
                                                                @PathVariable(name = "project_id") Long projectId,
                                                                @ApiParam(value = "组织id", required = true)
                                                                @RequestParam Long organizationId,
                                                                @ApiParam(value = "查询参数", required = false)
                                                                @RequestBody(required = false) Map<String, Object> searchParamMap,
                                                                @ApiParam(value = "quick filter")
                                                                @RequestParam(required = false) @Encrypt List<Long> quickFilterIds,
                                                                @ApiParam(value = "经办人搜索", required = false)
                                                                @RequestParam(required = false) @Encrypt List<Long> assigneeFilterIds) {
        EncryptionUtils.decryptSearchParamMap(searchParamMap);
        return Optional.ofNullable(sprintService.queryByProjectId(projectId, searchParamMap, quickFilterIds, organizationId, assigneeFilterIds))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException(QUERY_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询冲刺名")
    @PostMapping(value = "/names")
    public ResponseEntity<List<SprintNameVO>> queryNameByOptions(@ApiParam(value = "项目id", required = true)
                                                                 @PathVariable(name = "project_id") Long projectId,
                                                                 @ApiParam(value = "状态列表", required = false)
                                                                 @RequestBody(required = false) List<String> sprintStatusCodes) {
        return Optional.ofNullable(sprintService.queryNameByOptions(projectId, sprintStatusCodes))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException(QUERY_NAME_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "开启冲刺")
    @PostMapping(value = "/start")
    public ResponseEntity<SprintDetailVO> startSprint(@ApiParam(value = "项目id", required = true)
                                                      @PathVariable(name = "project_id") Long projectId,
                                                      @ApiParam(value = "冲刺DTO对象", required = true)
                                                      @RequestBody @Valid SprintUpdateVO sprintUpdateVO) {
        return Optional.ofNullable(sprintService.startSprint(projectId, sprintUpdateVO, true))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException(OPEN_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "完成冲刺")
    @PostMapping(value = "/complete")
    public ResponseEntity<Boolean> completeSprint(@ApiParam(value = "项目id", required = true)
                                                  @PathVariable(name = "project_id") Long projectId,
                                                  @ApiParam(value = "完成冲刺对象", required = true)
                                                  @RequestBody @Valid SprintCompleteVO sprintCompleteVO) {
        return Optional.ofNullable(sprintService.completeSprint(projectId, sprintCompleteVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException(CLOSE_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询sprint名及issue统计信息")
    @GetMapping(value = "/{sprintId}/names")
    public ResponseEntity<SprintCompleteMessageVO> queryCompleteMessageBySprintId(@ApiParam(value = "项目id", required = true)
                                                                                  @PathVariable(name = "project_id") Long projectId,
                                                                                  @ApiParam(value = "冲刺id", required = true)
                                                                                  @PathVariable @Encrypt Long sprintId) {
        return Optional.ofNullable(sprintService.queryCompleteMessageBySprintId(projectId, sprintId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException(QUERY_SPRINT_MESSAGE_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据sprintId查询冲刺信息")
    @GetMapping(value = "/{sprintId}")
    public ResponseEntity<SprintDetailVO> querySprintById(@ApiParam(value = "项目id", required = true)
                                                          @PathVariable(name = "project_id") Long projectId,
                                                          @ApiParam(value = "冲刺id", required = true)
                                                          @PathVariable @Encrypt Long sprintId) {
        return Optional.ofNullable(sprintService.querySprintById(projectId, sprintId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException(QUERY_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @CustomPageRequest
    @ApiOperation(value = "根据状态查已完成冲刺issue信息")
    @GetMapping(value = "/{sprintId}/issues")
    public ResponseEntity<Page<IssueListVO>> queryIssueByOptions(@ApiParam(value = "项目id", required = true)
                                                                     @PathVariable(name = "project_id") Long projectId,
                                                                 @ApiParam(value = "冲刺id", required = true)
                                                                     @PathVariable @Encrypt Long sprintId,
                                                                 @ApiParam(value = "状态", required = true)
                                                                     @RequestParam String status,
                                                                 @ApiParam(value = "组织id", required = true)
                                                                     @RequestParam Long organizationId,
                                                                 @ApiParam(value = "分页信息", required = true)
                                                                     @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                     @ApiIgnore PageRequest pageRequest) {
        return Optional.ofNullable(sprintService.queryIssueByOptions(projectId, sprintId, status, pageRequest, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException(QUERY_ERROR));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下创建冲刺时当前默认的名称")
    @GetMapping(value = "/current_create_name")
    public ResponseEntity<String> queryCurrentSprintCreateName(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId) {
        return Optional.ofNullable(sprintService.queryCurrentSprintCreateName(projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.CurrentSprintCreateName.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询未关闭的冲刺")
    @GetMapping(value = "/unclosed")
    public ResponseEntity<List<SprintUnClosedVO>> queryUnClosedSprint(@ApiParam(value = "项目id", required = true)
                                                                      @PathVariable(name = "project_id") Long projectId) {
        return Optional.ofNullable(sprintService.queryUnClosedSprint(projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.SprintUnClosedDTOList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询活跃冲刺")
    @GetMapping(value = "/active/{organization_id}")
    public ResponseEntity<ActiveSprintVO> queryActiveSprint(@ApiParam(value = "项目id", required = true)
                                                            @PathVariable(name = "project_id") Long projectId,
                                                            @ApiParam(value = "组织id", required = true)
                                                            @PathVariable(name = "organization_id") Long organizationId) {
        return Optional.ofNullable(sprintService.queryActiveSprint(projectId, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.activeSprint.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询冲刺的时间范围内非工作日(包含周六周天)")
    @GetMapping(value = "/query_non_workdays/{sprint_id}/{organization_id}")
    public ResponseEntity<List<String>> queryNonWorkdays(@ApiParam(value = "项目id", required = true)
                                                         @PathVariable(name = "project_id") Long projectId,
                                                         @ApiParam(value = "冲刺", required = true)
                                                         @PathVariable(name = "sprint_id") @Encrypt Long sprintId,
                                                         @ApiParam(value = "组织id", required = true)
                                                         @PathVariable(name = "organization_id") Long organizationId) {
        return Optional.ofNullable(sprintService.queryNonWorkdays(projectId, sprintId, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.queryNonWorkdays.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("冲刺重名校验")
    @PostMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "项目id", required = true)
                                             @PathVariable(name = "project_id") Long projectId,
                                             @RequestBody CheckSprintNameVO checkSprintNameVO) {
        return Optional.ofNullable(sprintService.checkName(projectId, checkSprintNameVO.getSprintName()))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.sprintName.check"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("代办事项-查询未完成的冲刺")
    @PostMapping(value = "/unclose_sprint")
    public ResponseEntity<List<SprintSearchVO>> unCloseSprint(@ApiParam(value = "项目id", required = true)
                                                              @PathVariable(name = "project_id") Long projectId,
                                                              @ApiParam(value = "搜索参数", required = true)
                                                              @RequestBody Map<String, Object> searchParamMap) {
        EncryptionUtils.decryptSearchParamMap(searchParamMap);
        return Optional.ofNullable(sprintService.unCloseSprint(projectId, searchParamMap))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.sprint.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("代办事项-查询冲刺下的issue")
    @PostMapping(value = "/sprint_issue_page")
    public ResponseEntity<Page<IssueSearchVO>> issuePageBySprint(@ApiParam(value = "项目id", required = true)
                                                                 @PathVariable(name = "project_id") Long projectId,
                                                                 @RequestParam @Encrypt Long sprintId,
                                                                 PageRequest pageRequest,
                                                                 @ApiParam(value = "搜索参数", required = true)
                                                                 @RequestBody Map<String, Object> searchParamMap) {
        EncryptionUtils.decryptSearchParamMap(searchParamMap);
        return Optional.ofNullable(sprintService.issuePageBySprint(projectId, sprintId, pageRequest, searchParamMap))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.sprint.issue.search"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("代办事项-分页查询代办的问题")
    @PostMapping(value = "/todo_issue_page")
    public ResponseEntity<Page<IssueSearchVO>> todoIssuePage(@ApiParam(value = "项目id", required = true)
                                                                 @PathVariable(name = "project_id") Long projectId,
                                                                 PageRequest pageRequest,
                                                             @ApiParam(value = "搜索参数", required = true)
                                                                 @RequestBody Map<String, Object> searchParamMap) {
        EncryptionUtils.decryptSearchParamMap(searchParamMap);
        return Optional.ofNullable(sprintService.todoIssuePage(projectId, pageRequest, searchParamMap))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.sprint.issue.search"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("开启冲刺之前提示未规划故事点和工时的个数")
    @GetMapping(value = "/sprint_start_message")
    public ResponseEntity<SprintStartMessageVO> selectSprintStartMessage(@ApiParam(value = "项目id", required = true)
                                                                         @PathVariable(name = "project_id") Long projectId,
                                                                         @ApiParam(value = "冲刺id", required = true)
                                                                         @RequestParam @Encrypt Long sprintId){
        return Optional.ofNullable(sprintService.selectSprintStartMessage(projectId, sprintId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.sprint.issue.search"));
    }
}
