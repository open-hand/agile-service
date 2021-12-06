package io.choerodon.agile.api.controller.v1;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.app.service.IssueProjectMoveService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.List;
import java.util.Map;

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
    public ResponseEntity issueProjectMove(@ApiParam(value = "项目id", required = true)
                                           @PathVariable(name = "project_id") Long projectId,
                                           @ApiParam(value = "问题Id", required = true)
                                           @PathVariable(name = "issue_id") @Encrypt Long issueId,
                                           @RequestParam  Long targetProjectId,
                                           @RequestBody JSONObject jsonObject) {
        issueProjectMoveService.issueProjectMove(projectId, issueId, targetProjectId,jsonObject);
        return new ResponseEntity(HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询能进行移动issue的项目")
    @GetMapping(value = "/list_move_projects")
    public ResponseEntity<List<ProjectVO>> listMoveProject(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable(name = "project_id") Long projectId,
                                                           @RequestParam String typeCode) {
        return new ResponseEntity(issueProjectMoveService.listMoveProject(projectId, typeCode), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询issue移动到目标项目会丢失的自定义字段")
    @GetMapping(value = "/list_lost_field")
    public ResponseEntity<List<ObjectSchemeFieldVO>> listLostField(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                   @RequestParam Long targetProject,
                                                                   @RequestParam @Encrypt Long issueId,
                                                                   @RequestParam @Encrypt Long issueTypeId) {
        return new ResponseEntity(issueProjectMoveService.listLostField(projectId, issueId, targetProject, issueTypeId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("跨项目转交问题")
    @PostMapping(value = "/batch_move")
    public ResponseEntity issueProjectBatchMove(@ApiParam(value = "项目id", required = true)
                                                @PathVariable(name = "project_id") Long projectId,
                                                @RequestParam Long targetProjectId,
                                                @RequestBody JSONObject jsonObject) {
        issueProjectMoveService.issueProjectBatchMove(projectId, targetProjectId, jsonObject, (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes(), EncryptContext.encryptType().name());
        return new ResponseEntity(HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issueIds查询并统计问题类型下的状态")
    @PostMapping(value = "/issue_type_status_map")
    public ResponseEntity<Map<String, List<String>>> issueTypeStatusMap(@ApiParam(value = "项目id", required = true)
                                                                    @PathVariable(name = "project_id") Long projectId,
                                                                    @RequestBody @Encrypt List<Long> issueIds) {
        return new ResponseEntity(issueProjectMoveService.issueTypeStatusMap(projectId, issueIds), HttpStatus.OK);
    }
}
