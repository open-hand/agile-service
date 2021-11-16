package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.MoveWorkGroupVO;
import io.choerodon.agile.api.vo.WorkGroupTreeVO;
import io.choerodon.agile.api.vo.WorkGroupVO;
import io.choerodon.agile.api.vo.business.MoveIssueVO;
import io.choerodon.agile.app.service.WorkGroupService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2021-11-08 19:29
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/work_group")
public class WorkGroupController {
    @Autowired
    private WorkGroupService workGroupService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工作组树形结构")
    @GetMapping(value = "/query_tree")
    public ResponseEntity<WorkGroupTreeVO> pageWorkHoursLogByProjectIds(@ApiParam(value = "组织Id", required = true)
                                                                        @PathVariable(name = "organization_id") Long organizationId) {
        return Optional.ofNullable(workGroupService.queryWorkGroupTree(organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.group.tree.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工作组详情")
    @GetMapping(value = "/{work_group_id}")
    public ResponseEntity<WorkGroupVO> queryById(@ApiParam(value = "组织Id", required = true)
                                                 @PathVariable(name = "organization_id") Long organizationId,
                                                 @PathVariable(name = "work_group_id") Long workGroupId) {
        return Optional.ofNullable(workGroupService.queryById(organizationId, workGroupId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.group.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建工作组")
    @PostMapping
    public ResponseEntity<WorkGroupVO> create(@ApiParam(value = "组织Id", required = true)
                                              @PathVariable(name = "organization_id") Long organizationId,
                                              @RequestBody WorkGroupVO workGroupVO) {
        return Optional.ofNullable(workGroupService.create(organizationId, workGroupVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.work.group.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更改工作组")
    @PutMapping
    public ResponseEntity<WorkGroupVO> update(@ApiParam(value = "组织Id", required = true)
                                              @PathVariable(name = "organization_id") Long organizationId,
                                              @RequestBody WorkGroupVO workGroupVO) {
        return Optional.ofNullable(workGroupService.update(organizationId, workGroupVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.work.group.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除工作组")
    @DeleteMapping("/{work_group_id}")
    public ResponseEntity delete(@ApiParam(value = "组织Id", required = true)
                                 @PathVariable(name = "organization_id") Long organizationId,
                                 @PathVariable(name = "work_group_id") @Encrypt Long workGroupId) {
        workGroupService.delete(organizationId, workGroupId);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("调整工作组顺序")
    @PostMapping("/move")
    public ResponseEntity<WorkGroupVO> move(@ApiParam(value = "组织Id", required = true)
                                            @PathVariable(name = "organization_id") Long organizationId,
                                            @RequestParam @Encrypt(ignoreValue = {"0"}) Long parentId,
                                            @RequestBody MoveWorkGroupVO moveWorkGroupVO) {
        return Optional.ofNullable(workGroupService.moveWorkGroup(organizationId, parentId, moveWorkGroupVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.work.group.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("校验名称")
    @GetMapping("/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "组织Id", required = true)
                                             @PathVariable(name = "organization_id") Long organizationId,
                                             @RequestParam @Encrypt(ignoreValue = {"0"}) Long parentId,
                                             @RequestParam String name) {
        return new ResponseEntity<>(workGroupService.checkName(organizationId, parentId, name), HttpStatus.OK);
    }
}