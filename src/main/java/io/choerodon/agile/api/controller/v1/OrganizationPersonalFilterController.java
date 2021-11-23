package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.PersonalFilterVO;
import io.choerodon.agile.app.service.PersonalFilterService;
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

import java.util.List;
import java.util.Optional;

/**
 * @author huaxin.deng@hand-china.com 2021-11-16 16:18:57
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/personal_filter")
public class OrganizationPersonalFilterController {
    @Autowired
    private PersonalFilterService personalFilterService;

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("组织层创建我的筛选")
    @PostMapping
    public ResponseEntity<PersonalFilterVO> create(@ApiParam(value = "组织id", required = true)
                                                   @PathVariable(name = "organization_id") Long organizationId,
                                                   @ApiParam(value = "personal filter object", required = true)
                                                   @RequestBody @Encrypt PersonalFilterVO personalFilterVO) {
        return Optional.ofNullable(personalFilterService.create(organizationId, 0L, personalFilterVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.personalFilter.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("组织层修改我的筛选")
    @PutMapping(value = "/{filterId}")
    public ResponseEntity<PersonalFilterVO> update(@ApiParam(value = "组织id", required = true)
                                                   @PathVariable(name = "organization_id") Long organizationId,
                                                   @ApiParam(value = "filter id", required = true)
                                                   @PathVariable @Encrypt Long filterId,
                                                   @ApiParam(value = "personal filter object", required = true)
                                                   @RequestBody @Encrypt PersonalFilterVO personalFilterVO) {
        return Optional.ofNullable(personalFilterService.update(organizationId, 0L, filterId, personalFilterVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.personalFilter.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("组织层删除我的筛选")
    @DeleteMapping(value = "/{filterId}")
    public ResponseEntity<PersonalFilterVO> deleteById(@ApiParam(value = "组织id", required = true)
                                                       @PathVariable(name = "organization_id") Long organizationId,
                                                       @ApiParam(value = "filter id", required = true)
                                                       @PathVariable  @Encrypt Long filterId) {
        personalFilterService.deleteById(organizationId, 0L, filterId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("组织层查询我的筛选列表")
    @GetMapping(value = "/query_all/{userId}")
    public ResponseEntity<List<PersonalFilterVO>> listByProjectId(@ApiParam(value = "组织id", required = true)
                                                                  @PathVariable(name = "organization_id") Long organizationId,
                                                                  @ApiParam(value = "用户id", required = true)
                                                                  @PathVariable @Encrypt Long userId,
                                                                  @ApiParam(value = "查询参数")
                                                                  @RequestParam(name = "searchStr", required = false) String searchStr,
                                                                  @ApiParam(value = "类型code", required = true)
                                                                  @RequestParam(name = "filterTypeCode") String filterTypeCode) {
        return Optional.ofNullable(personalFilterService.listByUserId(organizationId, 0L, userId, searchStr, filterTypeCode))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.personalFilter.list"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("组织层根据id查询我的筛选")
    @GetMapping(value = "/{filterId}")
    public ResponseEntity<PersonalFilterVO> queryById(@ApiParam(value = "组织id", required = true)
                                                      @PathVariable(name = "organization_id") Long organizationId,
                                                      @ApiParam(value = "filter id", required = true)
                                                      @PathVariable @Encrypt Long filterId) {
        return Optional.ofNullable(personalFilterService.queryById(organizationId, 0L, filterId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.personalFilter.queryById"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("组织层我的筛选重名校验")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkName(@ApiParam(value = "组织id", required = true)
                                             @PathVariable(name = "organization_id") Long organizationId,
                                             @ApiParam(value = "用户id", required = true)
                                             @RequestParam @Encrypt Long userId,
                                             @ApiParam(value = "name", required = true)
                                             @RequestParam String name,
                                             @ApiParam(value = "类型code", required = true)
                                             @RequestParam String filterTypeCode) {
        return Optional.ofNullable(personalFilterService.checkName(organizationId, 0L, userId, name, filterTypeCode))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.checkName.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("组织层设置默认筛选")
    @PutMapping(value = "/set_default")
    public ResponseEntity<Boolean> setDefault(@ApiParam(value = "组织id", required = true)
                                             @PathVariable(name = "organization_id") Long organizationId,
                                             @ApiParam(value = "filter Id", required = true)
                                             @RequestParam @Encrypt Long filterId) {
        return Optional.ofNullable(personalFilterService.setDefault(organizationId, 0L, filterId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.personal.filter.set.default"));
    }
}
