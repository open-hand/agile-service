package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.WebRequest;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.api.vo.StaticFileHeaderVO;
import io.choerodon.agile.api.vo.StaticFileRelatedVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.StaticFileService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:25
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/static_file")
public class StaticFileController {

    @Autowired
    StaticFileService staticFileService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("上传静态文件压缩包")
    @PostMapping
    public ResponseEntity<List<StaticFileHeaderVO>> uploadStaticFile(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "issue id", required = true)
            @RequestParam @Encrypt Long issueId,
            HttpServletRequest request) {
        return Optional.ofNullable(staticFileService.uploadStaticFile(projectId, issueId, request))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.attachment.upload"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("获取该项目所有的静态文件列表")
    @GetMapping
    public ResponseEntity<List<StaticFileHeaderVO>> getFileListByProject(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId){
        return Optional.ofNullable(staticFileService.selectFileListByProject(projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.attachment.select.project.list"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("获取静态文件信息")
    @GetMapping("/detail/{fileHeaderId}")
    public ResponseEntity<StaticFileHeaderVO> getFileHeaderDetail(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "静态文件头id", required = true)
            @PathVariable(name = "fileHeaderId") @Encrypt Long fileHeaderId){
        return Optional.ofNullable(staticFileService.selectFileHeaderById(fileHeaderId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.staticFile.select.file"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("获取该项目下没有关联传入id的所有的静态文件列表")
    @GetMapping("/{issueId}")
    public ResponseEntity<List<StaticFileHeaderVO>> getFileListExcludeIssue(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "问题id", required = true)
            @PathVariable(name = "issueId") @Encrypt Long issueId){
        return Optional.ofNullable(staticFileService.selectFileListExcludeIssue(projectId, issueId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.attachment.select.project.excludeIssue.list"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("获取问题下关联的静态文件列表")
    @GetMapping("/related/{issueId}")
    public ResponseEntity<List<StaticFileHeaderVO>> getFileListByIssue(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "问题id", required = true)
            @PathVariable(name = "issueId") @Encrypt Long issueId){
        return Optional.ofNullable(staticFileService.selectFileListByIssue(projectId, issueId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.attachment.select.issue.list"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("静态文件与问题相关联")
    @PostMapping("/related")
    public ResponseEntity<List<StaticFileHeaderVO>> updateStaticFileRelatedIssue(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @RequestBody @Encrypt StaticFileRelatedVO staticFileRelatedVO) {
        return Optional.ofNullable(staticFileService.updateStaticFileRelatedIssue(projectId, staticFileRelatedVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.attachment.upload"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除静态文件列表与问题的关联关系")
    @DeleteMapping("/related/{issueId}/{fileHeaderId}")
    public ResponseEntity deleteStaticFileRelated(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "问题id", required = true)
            @PathVariable(name = "issueId") @Encrypt Long issueId,
            @ApiParam(value = "静态文件头id", required = true)
            @PathVariable(name = "fileHeaderId") @Encrypt Long fileHeaderId){
        staticFileService.deleteStaticFileRelated(projectId, fileHeaderId, issueId);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除静态文件")
    @DeleteMapping("/{fileHeaderId}")
    public ResponseEntity deleteStaticFile(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "静态文件头id", required = true)
            @PathVariable(name = "fileHeaderId") @Encrypt Long fileHeaderId){
        staticFileService.deleteStaticFile(projectId, fileHeaderId);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除静态文件")
    @GetMapping("/test")
    public ResponseEntity<IssueVO> getentry(){
        IssueVO issueVO = new IssueVO();
        issueVO.setIssueId(78497555806457856L);
        return ResponseEntity.ok(issueVO);
    }
}
