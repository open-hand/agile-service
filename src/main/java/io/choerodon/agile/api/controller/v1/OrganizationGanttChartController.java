package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.OrganizationGanttChartService;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import java.util.List;

/**
 * @author superlee
 * @since 2021-10-18
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/gantt")
public class OrganizationGanttChartController {

    @Autowired
    private OrganizationGanttChartService organizationGanttChartService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询组织层甘特图列表数据")
    @PostMapping(value = "/list")
    public ResponseEntity<Page<GanttChartVO>> pagedQuery(@ApiIgnore
                                                         @ApiParam(value = "分页信息", required = true)
                                                         @SortDefault(value = "issueId", direction = Sort.Direction.DESC)
                                                                 PageRequest pageRequest,
                                                         @ApiParam(value = "组织id", required = true)
                                                         @PathVariable(name = "organization_id") Long organizationId,
                                                         @ApiParam(value = "查询参数", required = true)
                                                         @RequestBody(required = false) SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return ResponseEntity.ok(organizationGanttChartService.pagedQuery(organizationId, searchVO, pageRequest));
    }


    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("查询组织层项目列表数据")
    @GetMapping(value = "/agile_projects")
    public ResponseEntity<List<ProjectVO>> listAgileProjects(@ApiParam(value = "组织id", required = true)
                                                             @PathVariable(name = "organization_id") Long organizationId,
                                                             @RequestParam String name,
                                                             @RequestParam String code,
                                                             @RequestParam String param) {
        return ResponseEntity.ok(organizationGanttChartService.listAgileProjects(organizationId, name, code, param));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织甘特图获取自定义字段表头")
    @GetMapping("/header_field")
    public ResponseEntity<List<AgileIssueHeadVO>> getIssueHeaderFields(@PathVariable(name = "organization_id") Long organizationId,
                                                                       @ApiParam(value = "方案编码", required = true)
                                                                       @RequestParam String schemeCode) {
        return new ResponseEntity<>(organizationGanttChartService.getIssueHeaderFields(organizationId, schemeCode), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "组织甘特图界面获取自定义字段")
    @GetMapping("/custom_field")
    public ResponseEntity<List<ObjectSchemeFieldDetailVO>> listCustomFields(@PathVariable(name = "organization_id") Long organizationId) {
        return new ResponseEntity<>(organizationGanttChartService.listCustomFields(organizationId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation(value = "组织甘特图预计时间冲突统计")
    @PostMapping("/estimated_time/conflict")
    public ResponseEntity<List<EstimatedTimeConflictVO>> queryEstimatedTimeConflict(@PathVariable(name = "organization_id") Long organizationId,
                                                                                    @RequestBody SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return new ResponseEntity<>(organizationGanttChartService.queryEstimatedTimeConflict(organizationId, searchVO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation(value = "组织甘特图判断是否有资源冲突接口")
    @PostMapping("/estimated_time/is_conflicted")
    public ResponseEntity<Boolean> isEstimatedTimeConflicted(@PathVariable(name = "organization_id") Long organizationId,
                                                             @RequestBody SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return new ResponseEntity<>(organizationGanttChartService.isEstimatedTimeConflicted(organizationId, searchVO), HttpStatus.OK);
    }


    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation(value = "组织甘特图预计时间冲突统计用户的详情")
    @PostMapping("/estimated_time/conflict/details")
    public ResponseEntity<Page<GanttChartVO>> queryEstimatedTimeConflictDetails(@ApiIgnore
                                                                                @ApiParam(value = "分页信息", required = true)
                                                                                @SortDefault(value = "issueId", direction = Sort.Direction.DESC)
                                                                                        PageRequest pageRequest,
                                                                                @PathVariable(name = "organization_id") Long organizationId,
                                                                                @RequestBody SearchVO searchVO,
                                                                                @RequestParam @Encrypt Long assigneeId) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return new ResponseEntity<>(organizationGanttChartService.queryEstimatedTimeConflictDetails(organizationId, searchVO, assigneeId, pageRequest), HttpStatus.OK);
    }
}
