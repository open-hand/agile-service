package io.choerodon.agile.api.controller.v1;

import java.util.List;
import java.util.Map;

import com.alibaba.fastjson.JSONObject;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.app.service.IssueProjectMoveService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.hzero.starter.keyencrypt.core.EncryptContext;

/**
 * @author zhaotianxin
 * @date 2021-01-05 13:34
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/project_move")
public class IssueProjectMoveController {
    @Autowired
    private IssueProjectMoveService issueProjectMoveService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("跨项目转交问题")
    @PostMapping(value = "/{issue_id}")
    public ResponseEntity<Void> issueProjectMove(@ApiParam(value = "项目id", required = true)
                                           @PathVariable(name = "project_id") Long projectId,
                                           @ApiParam(value = "问题Id", required = true)
                                           @PathVariable(name = "issue_id") @Encrypt Long issueId,
                                           @ApiParam(value = "目标项目id", required = true)
                                           @RequestParam  Long targetProjectId,
                                           @ApiParam(value = "请求数据", required = true)
                                           @RequestBody JSONObject jsonObject) {
        issueProjectMoveService.issueProjectMove(projectId, issueId, targetProjectId,jsonObject);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询能进行移动issue的项目")
    @GetMapping(value = "/list_move_projects")
    public ResponseEntity<List<ProjectVO>> listMoveProject(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable(name = "project_id") Long projectId,
                                                           @ApiParam(value = "问题类型编码", required = true)
                                                           @RequestParam String typeCode) {
        return Results.success(issueProjectMoveService.listMoveProject(projectId, typeCode));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询issue移动到目标项目会丢失的自定义字段")
    @GetMapping(value = "/list_lost_field")
    public ResponseEntity<List<ObjectSchemeFieldVO>> listLostField(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                   @ApiParam(value = "目标项目id", required = true)
                                                                   @RequestParam Long targetProject,
                                                                   @ApiParam(value = "问题id", required = true)
                                                                   @RequestParam @Encrypt Long issueId,
                                                                   @ApiParam(value = "问题类型id", required = true)
                                                                   @RequestParam @Encrypt Long issueTypeId) {
        return Results.success(issueProjectMoveService.listLostField(projectId, issueId, targetProject, issueTypeId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("跨项目转交问题")
    @PostMapping(value = "/batch_move")
    public ResponseEntity<Void> issueProjectBatchMove(@ApiParam(value = "项目id", required = true)
                                                @PathVariable(name = "project_id") Long projectId,
                                                @ApiParam(value = "目标项目id", required = true)
                                                @RequestParam Long targetProjectId,
                                                @ApiParam(value = "请求数据", required = true)
                                                @RequestBody JSONObject jsonObject) {
        issueProjectMoveService.issueProjectBatchMove(projectId, targetProjectId, jsonObject, (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes(), EncryptContext.encryptType().name());
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issueIds查询并统计问题类型下的状态")
    @PostMapping(value = "/issue_type_status_map")
    public ResponseEntity<Map<String, List<String>>> issueTypeStatusMap(@ApiParam(value = "项目id", required = true)
                                                                    @PathVariable(name = "project_id") Long projectId,
                                                                    @ApiParam(value = "问题id集合", required = true)
                                                                    @RequestBody @Encrypt List<Long> issueIds) {
        return Results.success(issueProjectMoveService.issueTypeStatusMap(projectId, issueIds));
    }
}
