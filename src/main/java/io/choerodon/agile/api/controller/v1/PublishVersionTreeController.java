package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.PublishVersionVO;
import io.choerodon.agile.api.vo.TagVO;
import io.choerodon.agile.api.vo.VersionTreeVO;
import io.choerodon.agile.app.service.PublishVersionTreeService;
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

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-19
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/publish_version_tree")
public class PublishVersionTreeController {

    @Autowired
    private PublishVersionTreeService publishVersionTreeService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询项目下某个版本的版本树")
    @GetMapping
    public ResponseEntity<List<VersionTreeVO>> tree(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId,
                                                    @ApiParam(value = "根节点id", required = true)
                                                    @RequestParam @Encrypt Long rootId,
                                                    @ApiParam(value = "组织id", required = true)
                                                    @RequestParam Long organizationId) {
        return ResponseEntity.ok(publishVersionTreeService.tree(new HashSet<>(Arrays.asList(projectId)), organizationId, new HashSet<>(Arrays.asList(rootId))));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "版本树中添加节点")
    @PostMapping(value = "/add")
    public ResponseEntity add(@ApiParam(value = "项目id", required = true)
                              @PathVariable(name = "project_id") Long projectId,
                              @ApiParam(value = "组织id", required = true)
                              @RequestParam Long organizationId,
                              @RequestBody @Validated VersionTreeVO versionTreeVO) {
        publishVersionTreeService.add(projectId, organizationId, versionTreeVO);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "版本树中删除节点")
    @DeleteMapping(value = "/delete")
    public ResponseEntity delete(@ApiParam(value = "项目id", required = true)
                                 @PathVariable(name = "project_id") Long projectId,
                                 @ApiParam(value = "组织id", required = true)
                                 @RequestParam Long organizationId,
                                 @RequestBody @Validated VersionTreeVO versionTreeVO) {
        publishVersionTreeService.delete(projectId, organizationId, versionTreeVO);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据可用的应用版本节点")
    @GetMapping("/available_publish_version")
    public ResponseEntity<List<PublishVersionVO>> availablePublishVersion(@ApiParam(value = "项目id", required = true)
                                                                          @PathVariable(name = "project_id") Long projectId,
                                                                          @ApiParam(value = "组织id", required = true)
                                                                          @RequestParam Long organizationId,
                                                                          @ApiParam(value = "根节点id", required = true)
                                                                          @RequestParam @Encrypt Long rootId) {
        return ResponseEntity.ok(publishVersionTreeService.availablePublishVersion(projectId, organizationId, rootId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询树节点的下一级")
    @GetMapping("/direct_descendants")
    public ResponseEntity<List<PublishVersionVO>> directDescendants(@ApiParam(value = "项目id", required = true)
                                                                    @PathVariable(name = "project_id") Long projectId,
                                                                    @ApiParam(value = "组织id", required = true)
                                                                    @RequestParam Long organizationId,
                                                                    @ApiParam(value = "根节点id", required = true)
                                                                    @RequestParam @Encrypt Long rootId) {
        return ResponseEntity.ok(publishVersionTreeService.directDescendants(projectId, organizationId, rootId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "发布版本树添加tag")
    @PostMapping(value = "/add_tag")
    public ResponseEntity addTag(@ApiParam(value = "项目id", required = true)
                                 @PathVariable(name = "project_id") Long projectId,
                                 @ApiParam(value = "组织id", required = true)
                                 @RequestParam Long organizationId,
                                 @RequestParam @Encrypt Long publishVersionId,
                                 @RequestBody @Validated Set<TagVO> tags) {
        publishVersionTreeService.addTag(projectId, organizationId, publishVersionId, tags);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "更新发布版本tag的别名")
    @PutMapping(value = "/tag/{tag_id}/update_alias")
    public ResponseEntity updateTagAlias(@ApiParam(value = "项目id", required = true)
                                         @PathVariable(name = "project_id") Long projectId,
                                         @ApiParam(value = "发布版本tag关系id", required = true)
                                         @PathVariable(name = "tag_id") @Encrypt Long tagId,
                                         @ApiParam(value = "发布版本id", required = true)
                                         @RequestParam @Encrypt Long publishVersionId,
                                         @ApiParam(value = "乐观锁", required = true)
                                         @RequestParam Long objectVersionNumber,
                                         @ApiParam(value = "别名", required = true)
                                         @RequestParam String alias) {
        publishVersionTreeService.updateTagAlias(projectId, tagId, publishVersionId, objectVersionNumber, alias);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "发布版本树删除tag")
    @DeleteMapping(value = "/delete_tag")
    public ResponseEntity deleteTag(@ApiParam(value = "项目id", required = true)
                                    @PathVariable(name = "project_id") Long projectId,
                                    @ApiParam(value = "组织id", required = true)
                                    @RequestParam Long organizationId,
                                    @RequestParam @Encrypt Long publishVersionId,
                                    @RequestBody @Validated Set<TagVO> tags) {
        publishVersionTreeService.deleteTag(projectId, organizationId, publishVersionId, tags);
        return new ResponseEntity(HttpStatus.OK);
    }

}