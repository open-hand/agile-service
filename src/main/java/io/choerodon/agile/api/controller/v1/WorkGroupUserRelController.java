package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.WorkGroupUserRelParamVO;
import io.choerodon.agile.api.vo.WorkGroupUserRelVO;
import io.choerodon.agile.app.service.WorkGroupUserRelService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
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
@RequestMapping(value = "/v1/organizations/{organization_id}/work_group_user_rel")
public class WorkGroupUserRelController {
    @Autowired
    private WorkGroupUserRelService workGroupUserRelService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("批量加入成员")
    @PostMapping("/batch_insert")
    public ResponseEntity batchInsert(@ApiParam(value = "组织Id", required = true)
                                      @PathVariable(name = "organization_id") Long organizationId,
                                      @RequestBody WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        workGroupUserRelService.batchInsertRel(organizationId, workGroupUserRelParamVO);
        return new ResponseEntity(HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("批量移除成员")
    @PostMapping("/batch_delete")
    public ResponseEntity batchDelete(@ApiParam(value = "组织Id", required = true)
                                      @PathVariable(name = "organization_id") Long organizationId,
                                      @RequestBody WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        workGroupUserRelService.batchDeleteRel(organizationId, workGroupUserRelParamVO);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工作组下面关联的成员")
    @PostMapping("/page")
    public ResponseEntity<Page<WorkGroupUserRelVO>> pageByQuery(@ApiParam(value = "组织Id", required = true)
                                                                @PathVariable(name = "organization_id") Long organizationId,
                                                                PageRequest pageRequest,
                                                                @RequestBody WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        return Optional.ofNullable(workGroupUserRelService.pageByQuery(organizationId, pageRequest, workGroupUserRelParamVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.group.user.rel.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询工作组未关联的成员")
    @PostMapping("/unlink")
    public ResponseEntity<Page<WorkGroupUserRelVO>> pageUnlinkUser(@ApiParam(value = "组织Id", required = true)
                                                                @PathVariable(name = "organization_id") Long organizationId,
                                                                PageRequest pageRequest,
                                                                @RequestBody WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        return Optional.ofNullable(workGroupUserRelService.pageUnlinkUser(organizationId, pageRequest, workGroupUserRelParamVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.group.user.rel.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询未关联工作组的成员")
    @PostMapping("/page_unassignee")
    public ResponseEntity<Page<WorkGroupUserRelVO>> pageUnAssignee(@ApiParam(value = "组织Id", required = true)
                                                                   @PathVariable(name = "organization_id") Long organizationId,
                                                                   PageRequest pageRequest,
                                                                   @RequestBody WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        return Optional.ofNullable(workGroupUserRelService.pageUnAssignee(organizationId, pageRequest, workGroupUserRelParamVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.work.group.user.rel.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("查询按工作组筛选的成员")
    @PostMapping("/page_by_groups")
    public ResponseEntity<Page<UserDTO>> pageByGroups(@ApiParam(value = "组织Id", required = true)
                                                      @PathVariable(name = "organization_id") Long organizationId,
                                                      PageRequest pageRequest,
                                                      @RequestBody WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        return Optional.ofNullable(workGroupUserRelService.pageByGroups(organizationId, pageRequest, workGroupUserRelParamVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.user.query.by.groups"));
    }
}
