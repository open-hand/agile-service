package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusParamVO;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import java.util.List;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2020-06-19 10:44
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/work_bench")
public class WorkBenchController {
    @Autowired
    private IssueService issueService;

    @Autowired
    private StatusService statusService;

    @Autowired
    private StarBeaconService starBeaconService;

    @Autowired
    private IssueTypeService issueTypeService;

    @Autowired
    private PriorityService priorityService;

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("查询工作台个人代办事项")
    @PostMapping("/personal/backlog_issues")
    public ResponseEntity<Page<IssueListFieldKVVO>> queryBackLogIssuesByPersonal(@ApiParam(value = "组织id", required = true)
                                                                                 @PathVariable(name = "organization_id") Long organizationId,
                                                                                 @RequestParam(required = false) Long projectId,
                                                                                 PageRequest pageRequest,
                                                                                 @RequestBody WorkBenchIssueSearchVO workBenchIssueSearchVO) {
        return Optional.ofNullable(issueService.queryBackLogIssuesByPersonal(organizationId, projectId, pageRequest, workBenchIssueSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueLabel.queryIssueLabelList"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("查询工作台我的报告")
    @PostMapping("/personal/my_reported")
    public ResponseEntity<Page<IssueListFieldKVVO>> pagedQueryMyReported(@ApiParam(value = "组织id", required = true)
                                                                         @PathVariable(name = "organization_id") Long organizationId,
                                                                         @RequestParam(required = false) Long projectId,
                                                                         PageRequest pageRequest,
                                                                         @RequestBody WorkBenchIssueSearchVO workBenchIssueSearchVO) {
        return ResponseEntity.ok(issueService.pagedQueryMyReported(organizationId, projectId, pageRequest, workBenchIssueSearchVO));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("查询工作台我经手的")
    @PostMapping("/personal/my_assigned")
    public ResponseEntity<Page<IssueListFieldKVVO>> pagedQueryMyAssigned(@ApiParam(value = "组织id", required = true)
                                                                         @PathVariable(name = "organization_id") Long organizationId,
                                                                         @RequestParam(required = false) Long projectId,
                                                                         PageRequest pageRequest,
                                                                         @RequestBody WorkBenchIssueSearchVO workBenchIssueSearchVO) {
        return ResponseEntity.ok(issueService.pagedQueryMyAssigned(organizationId, projectId, pageRequest, workBenchIssueSearchVO));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation(value = "分页查询权限项目下状态列表")
    @PostMapping("/status")
    public ResponseEntity<Page<StatusVO>> queryUserProjectStatus(@ApiIgnore
                                                                 @SortDefault(value = "id", direction = Sort.Direction.DESC) PageRequest pageRequest,
                                                                 @ApiParam(value = "组织id", required = true)
                                                                 @PathVariable("organization_id") Long organizationId,
                                                                 @ApiParam(value = "卡片类型")
                                                                 @RequestParam(required = false) String type,
                                                                 @RequestBody StatusParamVO statusParamVO) {
        return Optional.ofNullable(statusService.queryUserProjectStatus(pageRequest, organizationId, type, statusParamVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.statusList.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("查询项目所有经办人")
    @PostMapping(value = "/users")
    public ResponseEntity<Page<UserDTO>> pagingUserProjectUsers(@ApiIgnore
                                                                @ApiParam(value = "分页信息", required = true) PageRequest pageRequest,
                                                                @ApiParam(value = "组织id", required = true)
                                                                @PathVariable(name = "organization_id") Long organizationId,
                                                                @RequestBody AgileUserVO agileUserVO) {
        return ResponseEntity.ok(issueService.pagingUserProjectUsers(pageRequest, organizationId, agileUserVO));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("工作台取关Instance")
    @PostMapping("/star_beacon/unstar")
    public ResponseEntity<Void> unStarInstance(@ApiParam(value = "组织id", required = true)
                                               @PathVariable(name = "organization_id") Long organizationId,
                                               @ApiParam(value = "取关VO", required = true)
                                               @RequestBody StarBeaconVO starBeaconVO) {
        starBeaconVO.setOrganizationId(organizationId);
        starBeaconService.unStarInstance(starBeaconVO);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("分页查询有权限项目下的问题类型")
    @PostMapping(value = "/issue_type")
    public ResponseEntity<Page<IssueTypeVO>> pagingProjectIssueTypes(@ApiIgnore
                                                                     @ApiParam(value = "分页信息", required = true) PageRequest pageRequest,
                                                                     @ApiParam(value = "组织id", required = true)
                                                                     @PathVariable(name = "organization_id") Long organizationId,
                                                                     @RequestBody IssueTypeSearchVO issueTypeSearchVO) {
        return ResponseEntity.ok(issueTypeService.pagingProjectIssueTypes(pageRequest, organizationId, issueTypeSearchVO));
    }


    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation(value = "工作台查询优先级")
    @PostMapping(value = "/priority")
    public ResponseEntity<List<PriorityVO>> queryPriorities(@PathVariable("organization_id") Long organizationId,
                                                            @RequestParam String param,
                                                            @RequestBody PriorityVO priority) {
        priority.setOrganizationId(organizationId);
        return new ResponseEntity<>(priorityService.selectAll(priority, param), HttpStatus.OK);
    }

}
