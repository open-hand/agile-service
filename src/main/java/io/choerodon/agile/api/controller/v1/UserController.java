package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;


/**
 * @author huaxin.deng@hand-china.com 2021-07-27 15:01:59
 */
@RestController
@RequestMapping(value = "/v1/users")
public class UserController {

    @Autowired
    private UserService userService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("项目权限下根据id批量查询项目下用户信息列表")
    @PostMapping(value = "/projects/{project_id}/page_by_ids")
    public ResponseEntity<Page<UserVO>> listPageProjectUsersByIds(@ApiParam(value = "项目id", required = true)
                                                                  @PathVariable(name = "project_id") Long projectId,
                                                                  @ApiIgnore @ApiParam(value = "分页信息", required = true)
                                                                  PageRequest pageRequest,
                                                                  @ApiParam(value = "用户ids", required = true)
                                                                  @RequestBody @Encrypt Long[] userIds) {
        return ResponseEntity.ok(userService.listPageUsersByIds(pageRequest, userIds));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("组织权限下根据id批量查询用户信息列表")
    @PostMapping(value = "/organizations/{organization_id}/page_by_ids")
    public ResponseEntity<Page<UserVO>> listPageOrgUsersByIds(@ApiParam(value = "组织id", required = true)
                                                              @PathVariable(name = "organization_id") Long organizationId,
                                                              @ApiIgnore @ApiParam(value = "分页信息", required = true)
                                                              PageRequest pageRequest,
                                                              @ApiParam(value = "用户ids", required = true)
                                                              @RequestBody @Encrypt Long[] userIds) {
        return ResponseEntity.ok(userService.listPageUsersByIds(pageRequest, userIds));
    }

}
