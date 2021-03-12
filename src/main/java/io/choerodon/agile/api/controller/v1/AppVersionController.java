package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;
import javax.validation.Valid;

import io.choerodon.agile.api.vo.AppVersionVO;
import io.choerodon.agile.app.service.AppVersionService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author superlee
 * @since 2021-03-10
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/app_version")
public class AppVersionController {

    @Autowired
    private AppVersionService appVersionService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建应用版本")
    @PostMapping
    public ResponseEntity<AppVersionVO> createAppVersion(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "新增应用版本", required = true)
            @RequestBody @Valid AppVersionVO appVersionVO) {
        return Optional.ofNullable(appVersionService.createAppVersion(
                projectId, appVersionVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.appVersion.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新应用版本")
    @PutMapping(value = "/update/{appVersionId}")
    public ResponseEntity<AppVersionVO> updateVersion(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "appVersionId", required = true)
            @PathVariable @Encrypt Long appVersionId,
            @ApiParam(value = "version信息", required = true)
            @RequestBody AppVersionVO appVersionVO) {
        return Optional.ofNullable(appVersionService.updateAppVersion(projectId, appVersionId, appVersionVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.appVersion.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @CustomPageRequest
    @ApiOperation(value = "根据项目id查找version")
    @GetMapping(value = "/{appVersionId}")
    public ResponseEntity<AppVersionVO> queryAppVersionById(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "appVersionId", required = true)
            @PathVariable @Encrypt Long appVersionId) {
        return Optional.ofNullable(appVersionService.queryAppVersionById(projectId, appVersionId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.version.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id删除应用版本")
    @DeleteMapping(value = "/delete/{appVersionId}")
    public ResponseEntity<Void> deleteAppVersion(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "versionId", required = true)
            @PathVariable @Encrypt Long appVersionId) {
        appVersionService.deleteAppVersion(projectId, appVersionId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "应用版本TAG是否重复")
    @PostMapping(value = "/check")
    public ResponseEntity<Boolean> checkTag(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "name", required = true)
            @RequestBody @Valid AppVersionVO appVersionVO) {
        return Optional.ofNullable(appVersionService.checkTagRepeat(appVersionVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.appVersion.check"));
    }
}
