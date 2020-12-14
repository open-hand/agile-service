package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.IssueLinkVO;
import io.choerodon.agile.api.vo.IssueQueryVO;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.Permission;
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
 * @author superlee
 * @since 2020-12-13
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/issues")
public class ForTestManagerController {

    @Autowired
    private IssueService issueService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("【测试专用】根据issueIds查询issue")
    @PostMapping(value = "/query_issue_ids")
    public ResponseEntity<List<IssueLinkVO>> queryIssues(@ApiParam(value = "项目id", required = true)
                                                         @PathVariable(name = "project_id") Long projectId,
                                                         @ApiParam(value = "issue编号", required = true)
                                                         @RequestBody @Encrypt List<Long> issueIds) {
        return Optional.ofNullable(issueService.queryIssueByIssueIds(projectId, issueIds))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.issue.queryIssueByIssueIds"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("【测试专用】根据issueIds分页查询issue")
    @PostMapping(value = "/paged_query")
    public ResponseEntity<Page<IssueLinkVO>> pagedQueryByOptions(@ApiParam(value = "项目id", required = true)
                                                                 @PathVariable(name = "project_id") Long projectId,
                                                                 @ApiIgnore @SortDefault PageRequest pageRequest,
                                                                 @RequestBody IssueQueryVO issueQueryVO) {
        return ResponseEntity.ok(issueService.pagedQueryByOptions(projectId, pageRequest, issueQueryVO));
    }

//    @Permission(level = ResourceLevel.ORGANIZATION)
//    @ApiOperation("【测试专用】批量复制issue并生成版本信息")
//    @PostMapping("/batch_clone_issue/{versionId}")
//    public ResponseEntity<List<Long>> cloneIssuesByVersionId(@ApiParam(value = "项目id", required = true)
//                                                             @PathVariable(name = "project_id") Long projectId,
//                                                             @ApiParam(value = "versionId", required = true)
//                                                             @PathVariable Long versionId,
//                                                             @ApiParam(value = "复制的issueIds", required = true)
//                                                             @RequestBody List<Long> issueIds) {
//        issueValidator.checkIssueIdsAndVersionId(projectId, issueIds, versionId);
//        return Optional.ofNullable(issueService.cloneIssuesByVersionId(projectId, versionId, issueIds))
//                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
//                .orElseThrow(() -> new CommonException("error.issue.cloneIssuesByVersionId"));
//    }

//    @Permission(level = ResourceLevel.ORGANIZATION)
//    @ApiOperation("【测试专用】issue按照项目分组接口")
//    @GetMapping("/list_issues_by_project")
//    public ResponseEntity<List<IssueProjectVO>> queryIssueTestGroupByProject(@ApiParam(value = "项目id", required = true)
//                                                                              @PathVariable(name = "project_id") Long projectId) {
//        return Optional.ofNullable(issueService.queryIssueTestGroupByProject())
//                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
//                .orElseThrow(() -> new CommonException("error.issue.queryIssueTestGroupByProject"));
//    }

//
//    @Permission(level = ResourceLevel.ORGANIZATION)
//    @ApiOperation("【测试专用】根据issueNum查询issue")
//    @PostMapping(value = "/query_by_issue_num")
//    public ResponseEntity<IssueNumDTO> queryIssueByIssueNum(@ApiParam(value = "项目id", required = true)
//                                                            @PathVariable(name = "project_id") Long projectId,
//                                                            @ApiParam(value = "issue编号", required = true)
//                                                            @RequestBody String issueNum) {
//        return Optional.ofNullable(issueService.queryIssueByIssueNum(projectId, issueNum))
//                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
//                .orElseThrow(() -> new CommonException("error.issue.queryIssueByIssueNum"));
//    }

}
