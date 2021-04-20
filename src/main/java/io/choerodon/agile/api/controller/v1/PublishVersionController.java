package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.TagCompareVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;
import javax.validation.Valid;

import io.choerodon.agile.api.vo.PublishVersionVO;
import io.choerodon.agile.app.service.PublishVersionService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.choerodon.swagger.annotation.Permission;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author superlee
 * @since 2021-03-10
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/publish_version")
public class PublishVersionController {

    @Autowired
    private PublishVersionService publishVersionService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建发布版本")
    @PostMapping
    public ResponseEntity<PublishVersionVO> create(@ApiParam(value = "项目id", required = true)
                                                   @PathVariable(name = "project_id") Long projectId,
                                                   @ApiParam(value = "新增发布版本", required = true)
                                                   @RequestBody @Valid PublishVersionVO publishVersionVO) {
        return Optional.ofNullable(publishVersionService.create(projectId, publishVersionVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.publishVersion.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("批量创建发布版本")
    @PostMapping("/{publish_version_id}/batch")
    public ResponseEntity<List<PublishVersionVO>> batchCreate(@ApiParam(value = "项目id", required = true)
                                                              @PathVariable(name = "project_id") Long projectId,
                                                              @PathVariable(name = "publish_version_id") Long publishVersionId,
                                                              @ApiParam(value = "新增发布版本", required = true)
                                                              @RequestBody @Valid List<PublishVersionVO> publishVersionList) {
        return Optional.ofNullable(publishVersionService.batchCreate(projectId, publishVersionId, publishVersionList))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.publishVersion.create"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新发布版本")
    @PutMapping(value = "/update/{publish_version_id}")
    public ResponseEntity<PublishVersionVO> update(@ApiParam(value = "项目id", required = true)
                                                   @PathVariable(name = "project_id") Long projectId,
                                                   @ApiParam(value = "publish_version_id", required = true)
                                                   @PathVariable(name = "publish_version_id") @Encrypt Long publishVersionId,
                                                   @ApiParam(value = "version信息", required = true)
                                                   @RequestBody @Valid PublishVersionVO publishVersionVO) {
        return Optional.ofNullable(publishVersionService.update(projectId, publishVersionId, publishVersionVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.publishVersion.update"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @CustomPageRequest
    @ApiOperation(value = "根据项目id查找version")
    @GetMapping(value = "/{publish_version_id}")
    public ResponseEntity<PublishVersionVO> query(@ApiParam(value = "项目id", required = true)
                                                  @PathVariable(name = "project_id") Long projectId,
                                                  @ApiParam(value = "publish_version_id", required = true)
                                                  @PathVariable(name = "publish_version_id") @Encrypt Long publishVersionId) {
        return Optional.ofNullable(publishVersionService.query(projectId, publishVersionId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.publishVersion.query"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @CustomPageRequest
    @ApiOperation(value = "分页查询项目下发布版本")
    @PostMapping("/list")
    public ResponseEntity<Page<PublishVersionVO>> list(@ApiParam(value = "项目id", required = true)
                                                       @PathVariable(name = "project_id") Long projectId,
                                                       @SortDefault(value = {"service_code", "version"}, direction = Sort.Direction.ASC)
                                                               PageRequest pageRequest,
                                                       @ApiParam(value = "筛选条件")
                                                       @RequestBody PublishVersionVO publishVersionVO) {
        return Optional.ofNullable(publishVersionService.list(projectId, publishVersionVO, pageRequest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.publishVersion.list"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据id删除发布版本")
    @DeleteMapping(value = "/delete/{publish_version_id}")
    public ResponseEntity<Void> delete(@ApiParam(value = "项目id", required = true)
                                       @PathVariable(name = "project_id") Long projectId,
                                       @ApiParam(value = "发布版本id", required = true)
                                       @PathVariable(name = "publish_version_id") @Encrypt Long publishVersionId) {
        publishVersionService.delete(projectId, publishVersionId);
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "发布版本是否重复")
    @PostMapping(value = "/existed")
    public ResponseEntity<Boolean> isExisted(@ApiParam(value = "项目id", required = true)
                                             @PathVariable(name = "project_id") Long projectId,
                                             @ApiParam(value = "publishVersionVO", required = true)
                                             @RequestBody @Valid PublishVersionVO publishVersionVO) {
        return Optional.ofNullable(publishVersionService.isExisted(projectId, publishVersionVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.publishVersion.isExisted"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "版本别名是否重复")
    @GetMapping(value = "/checkAlias")
    public ResponseEntity<Boolean> checkAlias(@ApiParam(value = "项目id", required = true)
                                              @PathVariable(name = "project_id") Long projectId,
                                              @ApiParam(value = "alias", required = true)
                                              @RequestParam String alias,
                                              @RequestParam(required = false) Long publishVersionId) {
        return Optional.ofNullable(publishVersionService.checkAlias(projectId, alias, publishVersionId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.publishVersion.isExisted"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "解析pom返回数据")
    @PostMapping(value = "/{publish_version_id}/parse_pom")
    public ResponseEntity<List<PublishVersionVO>> parsePom(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable(name = "project_id") Long projectId,
                                                           @ApiParam(value = "groupIds", required = true)
                                                           @RequestParam(required = false) String groupIds,
                                                           @RequestParam(required = false, defaultValue = "false") Boolean writeBack,
                                                           @PathVariable(name = "publish_version_id") Long publishVersionId,
                                                           @RequestBody MultipartFile file) {
        return Optional.ofNullable(publishVersionService.parsePom(projectId, groupIds, file, publishVersionId, writeBack))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.publishVersion.parsePom"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询发布版本关联的已完成故事")
    @PostMapping(value = "/{publish_version_id}/story")
    public ResponseEntity<Page<IssueListFieldKVVO>> listRelStoryByOption(@ApiParam(value = "项目id", required = true)
                                                                         @PathVariable(name = "project_id") Long projectId,
                                                                         @ApiParam(value = "产品版本id", required = true)
                                                                         @Encrypt @PathVariable(name = "publish_version_id") Long publishVersionId,
                                                                         @RequestParam Long organizationId,
                                                                         @ApiParam(value = "筛选条件")
                                                                         @RequestBody SearchVO searchVO,
                                                                         @SortDefault(direction = Sort.Direction.ASC) PageRequest pageRequest) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return ResponseEntity.ok(publishVersionService.listRelIssueByOption(projectId, organizationId, publishVersionId, searchVO, pageRequest, IssueTypeCode.STORY.value()));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询发布版本关联的已完成故事")
    @PostMapping(value = "/{publish_version_id}/bug")
    public ResponseEntity<Page<IssueListFieldKVVO>> listRelBugByOption(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable(name = "project_id") Long projectId,
                                                                       @ApiParam(value = "产品版本id", required = true)
                                                                       @Encrypt @PathVariable(name = "publish_version_id") Long publishVersionId,
                                                                       @RequestParam Long organizationId,
                                                                       @ApiParam(value = "筛选条件")
                                                                       @RequestBody SearchVO searchVO,
                                                                       @SortDefault(direction = Sort.Direction.ASC) PageRequest pageRequest) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return ResponseEntity.ok(publishVersionService.listRelIssueByOption(projectId, organizationId, publishVersionId, searchVO, pageRequest, IssueTypeCode.BUG.value()));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "发布版本tag对比")
    @PostMapping(value = "/{publish_version_id}/compare")
    public ResponseEntity compareTag(@ApiParam(value = "项目id", required = true)
                                     @PathVariable(name = "project_id") Long projectId,
                                     @ApiParam(value = "产品版本id", required = true)
                                     @Encrypt @PathVariable(name = "publish_version_id") Long publishVersionId,
                                     @RequestParam Long organizationId,
                                     @RequestBody @Validated List<TagCompareVO> tagCompareList) {
        publishVersionService.compareTag(projectId, organizationId, publishVersionId, tagCompareList);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新发布版本状态")
    @PutMapping(value = "/{publish_version_id}/update_status")
    public ResponseEntity updateStatus(@ApiParam(value = "项目id", required = true)
                                       @PathVariable(name = "project_id") Long projectId,
                                       @ApiParam(value = "publish_version_id", required = true)
                                       @PathVariable(name = "publish_version_id") @Encrypt Long publishVersionId,
                                       @RequestParam String statusCode) {
        publishVersionService.updateStatus(projectId, publishVersionId, statusCode);
        return Results.success();
    }

}
