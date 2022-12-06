package io.choerodon.agile.api.controller.v2;

import java.util.List;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import io.choerodon.agile.api.vo.EstimatedTimeConflictVO;
import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.app.service.OrganizationGanttChartService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-12-06
 */
@RestController
@RequestMapping(value = "/v2/organizations/{organization_id}/gantt")
public class OrganizationGanttV2ChartController {

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
                                                         @RequestBody SearchParamVO searchParamVO) {
        return ResponseEntity.ok(organizationGanttChartService.pagedQueryV2(organizationId, searchParamVO, pageRequest));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation(value = "组织甘特图预计时间冲突统计")
    @PostMapping("/estimated_time/conflict")
    public ResponseEntity<List<EstimatedTimeConflictVO>> queryEstimatedTimeConflict(@ApiParam(value = "组织id", required = true)
                                                                                    @PathVariable(name = "organization_id") Long organizationId,
                                                                                    @ApiParam(value = "筛选条件", required = true)
                                                                                    @RequestBody SearchParamVO searchParamVO) {
        return new ResponseEntity<>(organizationGanttChartService.queryEstimatedTimeConflictV2(organizationId, searchParamVO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation(value = "组织甘特图判断是否有资源冲突接口")
    @PostMapping("/estimated_time/is_conflicted")
    public ResponseEntity<Boolean> isEstimatedTimeConflicted(@ApiParam(value = "组织id", required = true)
                                                             @PathVariable(name = "organization_id") Long organizationId,
                                                             @ApiParam(value = "筛选条件", required = true)
                                                             @RequestBody SearchParamVO searchParamVO) {
        return new ResponseEntity<>(organizationGanttChartService.isEstimatedTimeConflictedV2(organizationId, searchParamVO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation(value = "组织甘特图预计时间冲突统计用户的详情")
    @PostMapping("/estimated_time/conflict/details")
    public ResponseEntity<Page<GanttChartVO>> queryEstimatedTimeConflictDetails(@ApiIgnore
                                                                                @ApiParam(value = "分页信息", required = true)
                                                                                @SortDefault(value = "issueId", direction = Sort.Direction.DESC)
                                                                                        PageRequest pageRequest,
                                                                                @ApiParam(value = "组织id", required = true)
                                                                                @PathVariable(name = "organization_id") Long organizationId,
                                                                                @ApiParam(value = "筛选条件", required = true)
                                                                                @RequestBody SearchParamVO searchParamVO,
                                                                                @ApiParam(value = "分配人id", required = true)
                                                                                @RequestParam @Encrypt Long assigneeId) {
        return new ResponseEntity<>(organizationGanttChartService.queryEstimatedTimeConflictDetailsV2(organizationId, searchParamVO, assigneeId, pageRequest), HttpStatus.OK);
    }
}
