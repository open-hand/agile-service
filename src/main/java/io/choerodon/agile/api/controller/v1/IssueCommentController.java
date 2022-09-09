package io.choerodon.agile.api.controller.v1;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.validator.IssueCommentValidator;
import io.choerodon.agile.app.service.IssueCommentService;

import io.choerodon.agile.infra.utils.VerifyUpdateUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import java.util.List;
import java.util.Optional;

/**
 * 敏捷开发Issue评论
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:59:45
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/issue_comment")
public class IssueCommentController {

    @Autowired
    private IssueCommentService issueCommentService;
    @Autowired
    private IssueCommentValidator issueCommentValidator;
    @Autowired
    private VerifyUpdateUtil verifyUpdateUtil;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建issue评论")
    @PostMapping
    public ResponseEntity<IssueCommentVO> createIssueComment(@ApiParam(value = "项目id", required = true)
                                                              @PathVariable(name = "project_id") Long projectId,
                                                             @ApiParam(value = "创建issue评论对象", required = true)
                                                              @RequestBody IssueCommentCreateVO issueCommentCreateVO) {
        issueCommentValidator.verifyCreateData(issueCommentCreateVO);
        return Optional.ofNullable(issueCommentService.createIssueComment(projectId, issueCommentCreateVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.IssueComment.createIssueComment"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建issue评论回复")
    @PostMapping("/reply")
    public ResponseEntity<IssueCommentVO> createIssueCommentReply(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable(name = "project_id") Long projectId,
                                                                  @ApiParam(value = "创建issue评论回复对象", required = true)
                                                             @RequestBody IssueCommentReplyCreateVO issueCommentReplyCreateVO) {
        issueCommentValidator.verifyCreateReplyData(issueCommentReplyCreateVO);
        return Optional.ofNullable(issueCommentService.createIssueCommentReply(projectId, issueCommentReplyCreateVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.IssueComment.createIssueCommentReply"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新issue评论")
    @PostMapping(value = "/update")
    public ResponseEntity<IssueCommentVO> updateIssueComment(@ApiParam(value = "项目id", required = true)
                                                              @PathVariable(name = "project_id") Long projectId,
                                                             @ApiParam(value = "更新issue对象", required = true)
                                                              @RequestBody JSONObject issueCommentUpdate) {
        issueCommentValidator.verifyUpdateData(projectId, issueCommentUpdate, false);
        IssueCommentUpdateVO issueCommentUpdateVO = new IssueCommentUpdateVO();
        List<String> stringList = verifyUpdateUtil.verifyUpdateData(issueCommentUpdate, issueCommentUpdateVO);
        return Optional.ofNullable(issueCommentService.updateIssueComment(issueCommentUpdateVO, stringList,projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueComment.updateIssueComment"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("更新issue评论")
    @PostMapping(value = "/self/update")
    public ResponseEntity<IssueCommentVO> updateSelfIssueComment(@ApiParam(value = "项目id", required = true)
                                                             @PathVariable(name = "project_id") Long projectId,
                                                             @ApiParam(value = "更新issue对象", required = true)
                                                             @RequestBody JSONObject issueCommentUpdate) {
        issueCommentValidator.verifyUpdateData(projectId, issueCommentUpdate, true);
        IssueCommentUpdateVO issueCommentUpdateVO = new IssueCommentUpdateVO();
        List<String> stringList = verifyUpdateUtil.verifyUpdateData(issueCommentUpdate, issueCommentUpdateVO);
        return Optional.ofNullable(issueCommentService.updateIssueComment(issueCommentUpdateVO, stringList,projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueComment.updateIssueComment"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("通过issueId查询issue评论列表")
    @GetMapping(value = "/{issueId}")
    public ResponseEntity<List<IssueCommentVO>> queryIssueCommentList(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable(name = "project_id") Long projectId,
                                                                      @ApiParam(value = "issueId", required = true)
                                                                       @PathVariable @Encrypt Long issueId) {
        return Optional.ofNullable(issueCommentService.queryIssueCommentList(projectId, issueId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueComment.queryIssueCommentList"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("通过commentId查询评论下对应回复")
    @GetMapping(value = "/reply/{comment_id}")
    public ResponseEntity<List<IssueCommentReplyVO>> queryIssueCommentReplyList(@ApiParam(value = "项目id", required = true)
                                                                                      @PathVariable(name = "project_id") Long projectId,
                                                                                @ApiParam(value = "comment_id", required = true)
                                                                                      @PathVariable(name = "comment_id") @Encrypt Long commentId) {
        return Optional.ofNullable(issueCommentService.queryIssueCommentReplyList(projectId, commentId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueComment.queryIssueCommentReplyList"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("通过commentId删除")
    @DeleteMapping(value = "/{commentId}")
    public ResponseEntity deleteIssueComment(@ApiParam(value = "项目id", required = true)
                                             @PathVariable(name = "project_id") Long projectId,
                                             @ApiParam(value = "commentId", required = true)
                                             @PathVariable @Encrypt Long commentId) {
        issueCommentService.deleteIssueComment(projectId, commentId, false);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("通过commentId删除评论及其回复")
    @DeleteMapping(value = "/reply/{commentId}")
    public ResponseEntity deleteIssueCommentReply(@ApiParam(value = "项目id", required = true)
                                             @PathVariable(name = "project_id") Long projectId,
                                                  @ApiParam(value = "评论id", required = true)
                                             @PathVariable @Encrypt Long commentId) {
        issueCommentService.deleteIssueCommentReply(projectId, commentId, false);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("通过commentId删除删除自己的评论")
    @DeleteMapping(value = "/self/{commentId}")
    public ResponseEntity deleteSelfIssueComment(@ApiParam(value = "项目id", required = true)
                                             @PathVariable(name = "project_id") Long projectId,
                                             @ApiParam(value = "commentId", required = true)
                                             @PathVariable @Encrypt Long commentId) {
        issueCommentService.deleteIssueComment(projectId, commentId, true);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("通过commentId删除自己的评论及其回复")
    @DeleteMapping(value = "/self/reply/{commentId}")
    public ResponseEntity deleteSelfIssueCommentReply(@ApiParam(value = "项目id", required = true)
                                                  @PathVariable(name = "project_id") Long projectId,
                                                  @ApiParam(value = "评论id", required = true)
                                                  @PathVariable @Encrypt Long commentId) {
        issueCommentService.deleteIssueCommentReply(projectId, commentId, true);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("分页查询issue下评论")
    @GetMapping(value = "/issue/{issue_id}/page")
    public ResponseEntity<Page<IssueCommentVO>> issueCommentPage(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "问题id", required = true)
            @Encrypt @PathVariable(name = "issue_id") Long issueId,
            @ApiParam(value = "分页参数")
            @ApiIgnore PageRequest pageRequest) {
        return Optional.ofNullable(issueCommentService.queryIssueCommentPage(pageRequest, issueId, projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.issueComment.page"));
    }

}