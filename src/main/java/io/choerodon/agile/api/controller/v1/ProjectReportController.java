package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.app.service.ProjectReportService;
import io.choerodon.agile.infra.dto.ProjectReportDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 上午10:48
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/project_reports")
public class ProjectReportController {

    @Autowired
    private ProjectReportService projectReportService;

    @ApiOperation(value = "项目报表列表")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping
    public ResponseEntity<Page<ProjectReportDTO>> page(@PathVariable("project_id") Long projectId,
                                                       @SortDefault(value = "id", direction = Sort.Direction.ASC)
                                                       PageRequest pageRequest) {
        return Results.success(projectReportService.page(projectId, pageRequest));
    }

    @ApiOperation(value = "项目报表详情")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @GetMapping("/{id}")
    public ResponseEntity<ProjectReportDTO> detail(@PathVariable("project_id") Long projectId,
                                                        @PathVariable("id") @Encrypt Long id) {
        return Results.success(projectReportService.detail(projectId, id));
    }

    @ApiOperation(value = "创建项目报表")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @PostMapping
    public ResponseEntity<Void> create(@PathVariable("project_id") Long projectId,
                                     @RequestBody ProjectReportDTO projectReportDTO,
                                     @ApiParam(value = "方案编码", required = true)
                                     @RequestParam String applyType) {
        projectReportService.create(projectId, projectReportDTO);
        return Results.success();
    }

    @ApiOperation(value = "删除项目报表")
    @Permission(level = ResourceLevel.ORGANIZATION)
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable("project_id") Long projectId,
                                       @PathVariable("id") Long id,
                                       @RequestBody @Encrypt ProjectReportDTO projectReportDTO) {
        projectReportService.delete(projectId, projectReportDTO);
        return Results.success();
    }
}
