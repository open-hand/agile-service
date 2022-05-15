package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.FieldPermissionIssueService;
import io.choerodon.agile.app.service.FieldPermissionService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * @author superlee
 * @since 2021-07-20
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/field_permission")
public class FieldPermissionController {

    @Autowired
    private FieldPermissionService fieldPermissionService;

    @Autowired
    private FieldPermissionIssueService fieldPermissionIssueService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "字段分配权限")
    @PostMapping("/create")
    public ResponseEntity create(@ApiParam(value = "项目id", required = true)
                                 @PathVariable("project_id") Long projectId,
                                 @ApiParam(value = "组织id", required = true)
                                 @RequestParam Long organizationId,
                                 @ApiParam(value = "字段权限体")
                                 @RequestBody @Validated FieldPermissionVO fieldPermissionVO) {
        fieldPermissionService.create(projectId, organizationId, fieldPermissionVO);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "批量字段分配权限")
    @PostMapping("/batch_create")
    public ResponseEntity batchCreate(@ApiParam(value = "项目id", required = true)
                                      @PathVariable("project_id") Long projectId,
                                      @ApiParam(value = "组织id", required = true)
                                      @RequestParam Long organizationId,
                                      @ApiParam(value = "字段权限体")
                                      @RequestBody @Validated FieldPermissionVO fieldPermissionVO) {
        fieldPermissionService.batchCreate(projectId, organizationId, fieldPermissionVO);
        return new ResponseEntity<>(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据字段id查询权限")
    @GetMapping("/filed_id/{filed_id}")
    public ResponseEntity<List<PermissionVO>> queryByFieldId(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable("project_id") Long projectId,
                                                             @PathVariable("filed_id") @Encrypt Long fieldId,
                                                             @ApiParam(value = "组织id", required = true)
                                                             @RequestParam Long organizationId,
                                                             @ApiParam(value = "问题类型id", required = true)
                                                             @RequestParam @Encrypt Long issueTypeId) {
        return ResponseEntity.ok(fieldPermissionService.queryByFieldId(projectId, organizationId, fieldId, issueTypeId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询必填但没有权限的字段")
    @PostMapping("/required_fields/no_permission")
    public ResponseEntity<List<PageFieldViewVO>> listNoPermissionRequiredFields(@ApiParam(value = "项目id", required = true)
                                                                                @PathVariable("project_id") Long projectId,
                                                                                @ApiParam(value = "组织id", required = true)
                                                                                @RequestParam Long organizationId,
                                                                                @RequestParam(required = false) @Encrypt Long issueId,
                                                                                @ApiParam(value = "参数对象", required = true)
                                                                                @RequestBody PageFieldViewParamVO paramDTO) {
        return new ResponseEntity<>(fieldPermissionIssueService.listNoPermissionRequiredFields(organizationId, projectId, paramDTO, issueId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询当前用户问题类型下没有权限的字段")
    @GetMapping("/no_permission_fields")
    public ResponseEntity<List<IssueTypeNoPermissionFields>> listNoPermissionFieldsByIssueType(@ApiParam(value = "项目id", required = true)
                                                                                               @PathVariable("project_id") Long projectId,
                                                                                               @ApiParam(value = "组织id", required = true)
                                                                                               @RequestParam Long organizationId) {
        return new ResponseEntity<>(fieldPermissionIssueService.listNoPermissionFieldsByIssueType(organizationId, projectId), HttpStatus.OK);
    }

}
