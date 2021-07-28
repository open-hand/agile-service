package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * @author huaxin.deng@hand-china.com 2021-07-27 15:01:59
 */
@RestController
@RequestMapping(value = "/v1/users")
public class UserController {

    @Autowired
    private UserService userService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("项目权限下根据id批量查询用户信息列表")
    @PostMapping(value = "/projects/{project_id}/list_by_ids")
    public ResponseEntity<List<UserDTO>> listProjectUsersByIds(@ApiParam(value = "项目id", required = true)
                                                               @PathVariable(name = "project_id") Long projectId,
                                                               @ApiParam(value = "用户ids", required = true)
                                                               @RequestBody @Encrypt Long[] userIds) {
        return ResponseEntity.ok(userService.listUsersByIds(userIds));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("组织权限下根据id批量查询用户信息列表")
    @PostMapping(value = "/organizations/{organization_id}/list_by_ids")
    public ResponseEntity<List<UserDTO>> listOrgUsersByIds(@ApiParam(value = "组织id", required = true)
                                                           @PathVariable(name = "organization_id") Long organizationId,
                                                           @ApiParam(value = "用户ids", required = true)
                                                           @RequestBody @Encrypt Long[] userIds) {
        return ResponseEntity.ok(userService.listUsersByIds(userIds));
    }

}
