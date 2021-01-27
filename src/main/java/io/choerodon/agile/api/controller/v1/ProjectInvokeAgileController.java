package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.IssueLinkVO;
import io.choerodon.agile.api.vo.PageFieldViewParamVO;
import io.choerodon.agile.api.vo.PageFieldViewVO;
import io.choerodon.agile.api.vo.StaticFileHeaderVO;
import io.choerodon.agile.api.vo.business.DataLogVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.*;
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
 * @author huaxin.deng@hand-china.com 2020-12-29 15:27:18
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/project_invoke_agile")
public class ProjectInvokeAgileController {

    @Autowired
    private IssueService issueService;

    @Autowired
    private PageFieldService pageFieldService;

    @Autowired
    private DataLogService dataLogService;

    @Autowired
    private IssueLinkService issueLinkService;

    @Autowired
    private StaticFileService staticFileService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询单个issue")
    @GetMapping(value = "/issues/{issueId}")
    public ResponseEntity<IssueVO> queryIssue(@ApiParam(value = "项目id", required = true)
                                              @PathVariable(name = "project_id") Long projectId,
                                              @ApiParam(value = "issueId", required = true)
                                              @PathVariable @Encrypt Long issueId,
                                              @ApiParam(value = "所属项目id", required = true)
                                              @RequestParam Long instanceProjectId,
                                              @ApiParam(value = "组织id", required = true)
                                              @RequestParam(required = false) Long organizationId) {
        return Optional.ofNullable(issueService.queryIssue(instanceProjectId, issueId, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.queryIssue"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "根据实例id从界面上获取字段列表，带有字段值、字段选项")
    @PostMapping("/field_value/list/{instance_id}")
    public ResponseEntity<List<PageFieldViewVO>> queryPageFieldViewListWithInstanceId(@ApiParam(value = "项目id", required = true)
                                                                                      @PathVariable("project_id") Long projectId,
                                                                                      @ApiParam(value = "实例id", required = true)
                                                                                      @PathVariable("instance_id") @Encrypt Long instanceId,
                                                                                      @ApiParam(value = "组织id", required = true)
                                                                                      @RequestParam Long organizationId,
                                                                                      @ApiParam(value = "所属项目id", required = true)
                                                                                      @RequestParam Long instanceProjectId,
                                                                                      @ApiParam(value = "参数对象", required = true)
                                                                                      @RequestBody @Valid PageFieldViewParamVO paramDTO) {
        return new ResponseEntity<>(pageFieldService.queryPageFieldViewListWithInstanceId(organizationId, instanceProjectId, instanceId, paramDTO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询DataLog列表")
    @GetMapping("/data_log")
    public ResponseEntity<List<DataLogVO>> listByIssueId(@ApiParam(value = "项目id", required = true)
                                                         @PathVariable(name = "project_id") Long projectId,
                                                         @ApiParam(value = "issue id", required = true)
                                                         @RequestParam @Encrypt Long issueId,
                                                         @ApiParam(value = "所属项目id", required = true)
                                                         @RequestParam Long instanceProjectId) {
        return Optional.ofNullable(dataLogService.listByIssueId(instanceProjectId, issueId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.dataLogList.get"));
    }


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issueId查询issueLink")
    @GetMapping(value = "/issue_links/{issueId}")
    public ResponseEntity<List<IssueLinkVO>> listIssueLinkByIssueId(@ApiParam(value = "项目id", required = true)
                                                                    @PathVariable(name = "project_id") Long projectId,
                                                                    @ApiParam(value = "issueId", required = true)
                                                                    @PathVariable @Encrypt Long issueId,
                                                                    @ApiParam(value = "所属项目id", required = true)
                                                                    @RequestParam Long instanceProjectId,
                                                                    @ApiParam(value = "是否包含测试任务")
                                                                    @RequestParam(required = false,name = "no_issue_test",defaultValue = "false")
                                                                        Boolean noIssueTest) {
        return Optional.ofNullable(issueLinkService.listIssueLinkByIssueId(issueId, instanceProjectId, noIssueTest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueLink.listIssueLinkByIssueId"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("获取问题下关联的静态文件列表")
    @GetMapping(value = "/related_static_file/{issueId}")
    public ResponseEntity<List<StaticFileHeaderVO>> getFileListByIssue(@ApiParam(value = "项目id", required = true)
                                                                       @PathVariable(name = "project_id") Long projectId,
                                                                       @ApiParam(value = "问题id", required = true)
                                                                       @PathVariable @Encrypt Long issueId,
                                                                       @ApiParam(value = "所属项目id", required = true)
                                                                       @RequestParam Long instanceProjectId) {
        return Optional.ofNullable(staticFileService.selectFileListByIssue(instanceProjectId, issueId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.attachment.select.issue.list"));
    }
}
