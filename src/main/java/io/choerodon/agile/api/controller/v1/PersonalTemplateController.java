package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.PersonalTemplateService;
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

import javax.validation.Valid;
import java.util.List;
import java.util.Optional;

/**
 * @author huaxin.deng@hand-china.com 2021-02-02 13:54:38
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/personal_template")
public class PersonalTemplateController {

    @Autowired
    private PersonalTemplateService personalTemplateService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建模板")
    @PostMapping
    public ResponseEntity<PersonalTemplatelVO> create(@ApiParam(value = "项目id", required = true)
                                                      @PathVariable(name = "project_id") Long projectId,
                                                      @ApiParam(value = "模板创建对象", required = true)
                                                      @RequestBody @Valid PersonalTemplateCreateVO personalTemplateCreateVO) {
        return Optional.ofNullable(personalTemplateService.create(projectId, personalTemplateCreateVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.personal.template.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("修改模板")
    @PutMapping(value = "/{id}")
    public ResponseEntity<PersonalTemplatelVO> update(@ApiParam(value = "项目id", required = true)
                                                      @PathVariable(name = "project_id") Long projectId,
                                                      @ApiParam(value = "模板id", required = true)
                                                      @PathVariable @Encrypt  Long id,
                                                      @ApiParam(value = "模板修改对象", required = true)
                                                      @RequestBody @Valid PersonalTemplateUpdateVO personalTemplateUpdateVO) {
        return Optional.ofNullable(personalTemplateService.update(projectId, id, personalTemplateUpdateVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.personal.template.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除模板")
    @DeleteMapping(value = "/{id}")
    public ResponseEntity delete(@ApiParam(value = "项目id", required = true)
                                 @PathVariable(name = "project_id") Long projectId,
                                 @ApiParam(value = "模板id", required = true)
                                 @PathVariable @Encrypt Long id) {
        personalTemplateService.delete(projectId, id);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据用户和类型查询模板")
    @GetMapping(value = "/user/{userId}")
    public ResponseEntity<List<PersonalTemplatelVO>> queryByUserAndAction(@ApiParam(value = "项目id", required = true)
                                                                          @PathVariable(name = "project_id") Long projectId,
                                                                          @ApiParam(value = "用户id", required = true)
                                                                          @PathVariable(name = "userId") @Encrypt Long userId,
                                                                          @ApiParam(value = "action", required = true)
                                                                          @RequestParam String action,
                                                                          @ApiParam(value = "type", required = true)
                                                                          @RequestParam String type) {
        return Optional.ofNullable(personalTemplateService.queryByUserAndAction(projectId, userId, action, type))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.personal.template.get"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据用户和类型进行模板重名校验")
    @GetMapping(value = "/check_name")
    public ResponseEntity<Boolean> checkNameByAction(@ApiParam(value = "项目id", required = true)
                                                     @PathVariable(name = "project_id") Long projectId,
                                                     @ApiParam(value = "模板名称", required = true)
                                                     @RequestParam String name,
                                                     @ApiParam(value = "用户id", required = true)
                                                     @RequestParam @Encrypt Long userId,
                                                     @ApiParam(value = "action", required = true)
                                                     @RequestParam String action,
                                                     @ApiParam(value = "type", required = true)
                                                     @RequestParam String type) {
        return Optional.ofNullable(personalTemplateService.checkNameByAction(projectId, userId, name, action, type))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.checkName.get"));
    }

}
