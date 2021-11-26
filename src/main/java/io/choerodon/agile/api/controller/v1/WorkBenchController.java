package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusParamVO;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import springfox.documentation.annotations.ApiIgnore;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
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

    @Autowired
    private PersonalFilterService personalFilterService;


    @Autowired
    private BaseFeignClient baseFeignClient;

    @Autowired
    private ExcelService excelService;

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


    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("工作台创建我的筛选")
    @PostMapping("/personal_filter")
    public ResponseEntity<PersonalFilterVO> create(@ApiParam(value = "组织id", required = true)
                                                   @PathVariable(name = "organization_id") Long organizationId,
                                                   @ApiParam(value = "personal filter object", required = true)
                                                   @RequestBody @Encrypt PersonalFilterVO personalFilterVO) {
        return Optional.ofNullable(personalFilterService.create(organizationId, 0L, personalFilterVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.personalFilter.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("工作台修改我的筛选")
    @PutMapping(value = "/personal_filter/{filterId}")
    public ResponseEntity<PersonalFilterVO> update(@ApiParam(value = "组织id", required = true)
                                                   @PathVariable(name = "organization_id") Long organizationId,
                                                   @ApiParam(value = "filter id", required = true)
                                                   @PathVariable @Encrypt Long filterId,
                                                   @ApiParam(value = "personal filter object", required = true)
                                                   @RequestBody @Encrypt PersonalFilterVO personalFilterVO) {
        return Optional.ofNullable(personalFilterService.update(organizationId, 0L, filterId, personalFilterVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.personalFilter.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("工作台删除我的筛选")
    @DeleteMapping(value = "/personal_filter/{filterId}")
    public ResponseEntity<PersonalFilterVO> deleteById(@ApiParam(value = "组织id", required = true)
                                                       @PathVariable(name = "organization_id") Long organizationId,
                                                       @ApiParam(value = "filter id", required = true)
                                                       @PathVariable  @Encrypt Long filterId) {
        personalFilterService.deleteById(organizationId, 0L, filterId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("工作台查询我的筛选列表")
    @GetMapping(value = "/personal_filter/query_all/{userId}")
    public ResponseEntity<List<PersonalFilterVO>> listByProjectId(@ApiParam(value = "组织id", required = true)
                                                                  @PathVariable(name = "organization_id") Long organizationId,
                                                                  @ApiParam(value = "用户id", required = true)
                                                                  @PathVariable @Encrypt Long userId,
                                                                  @ApiParam(value = "查询参数")
                                                                  @RequestParam(name = "searchStr", required = false) String searchStr,
                                                                  @ApiParam(value = "类型code", required = true)
                                                                  @RequestParam(name = "filterTypeCode") String filterTypeCode) {
        return Optional.ofNullable(personalFilterService.listByUserId(organizationId, 0L, userId, searchStr, filterTypeCode))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.personalFilter.list"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("获取时区下的工作日历")
    @GetMapping(value = "/time_zone_work_calendars/detail")
    public ResponseEntity<TimeZoneWorkCalendarRefDetailVO> queryTimeZoneWorkCalendarDetail(@ApiParam(value = "组织id", required = true)
                                                                                           @PathVariable(name = "organization_id") Long organizationId,
                                                                                           @ApiParam(value = "年份", required = true)
                                                                                           @RequestParam(name = "year") Integer year) {
        return baseFeignClient.queryTimeZoneWorkCalendarDetail(organizationId, year);
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation(value = "分页模糊查询组织下的用户")
    @PostMapping(value = "/organizations/users")
    @CustomPageRequest
    public ResponseEntity<Page<UserDTO>> pagingQueryUsersOnOrganizationAgile(@PathVariable(name = "organization_id") Long organizationId,
                                                                             @SortDefault(value = "organizationId", direction = Sort.Direction.DESC) PageRequest pageRequest,
                                                                             @Encrypt @RequestParam(required = false, name = "id") Long userId,
                                                                             @RequestParam(required = false) String email,
                                                                             @RequestParam(required = false) String param,
                                                                             @RequestBody @Encrypt List<Long> notSelectUserIds) {
        return baseFeignClient.pagingQueryUsersOnOrganizationAgile(organizationId, pageRequest.getPage(), pageRequest.getSize(), userId, email, param, notSelectUserIds);
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("查询最近的上传/下载记录")
    @GetMapping(value = "/excel/latest")
    public ResponseEntity<FileOperationHistoryVO> queryLatestRecode(@ApiParam(value = "组织id", required = true)
                                                                    @PathVariable(name = "organization_id") Long organizationId,
                                                                    @RequestParam String action) {
        return Optional.ofNullable(excelService.queryOrgLatestRecode(organizationId, action))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.ImportHistoryRecode.get"));
    }

}
