package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.PageFieldViewParamVO;
import io.choerodon.agile.api.vo.PageFieldViewVO;
import io.choerodon.agile.api.vo.business.DataLogVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.app.service.DataLogService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.PageFieldService;
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
@RequestMapping(value = "/v1/projects/{project_id}/cross_project_invoke")
public class CrossProjectInvokeController {

    @Autowired
    private IssueService issueService;

    @Autowired
    private PageFieldService pageFieldService;

    @Autowired
    private DataLogService dataLogService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询单个issue")
    @GetMapping(value = "/issues/{issueId}")
    public ResponseEntity<IssueVO> queryIssue(@ApiParam(value = "项目id", required = true)
                                              @PathVariable(name = "project_id") Long projectId,
                                              @ApiParam(value = "issueId", required = true)
                                              @PathVariable @Encrypt Long issueId,
                                              @ApiParam(value = "所属项目id", required = true)
                                              @RequestParam Long belongProjectId,
                                              @ApiParam(value = "组织id", required = true)
                                              @RequestParam(required = false) Long organizationId) {
        return Optional.ofNullable(issueService.queryIssue(belongProjectId, issueId, organizationId))
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
                                                                                      @RequestParam Long belongProjectId,
                                                                                      @ApiParam(value = "参数对象", required = true)
                                                                                      @RequestBody @Valid PageFieldViewParamVO paramDTO) {
        return new ResponseEntity<>(pageFieldService.queryPageFieldViewListWithInstanceId(organizationId, belongProjectId, instanceId, paramDTO), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询DataLog列表")
    @GetMapping("/data_log")
    public ResponseEntity<List<DataLogVO>> listByIssueId(@ApiParam(value = "项目id", required = true)
                                                         @PathVariable(name = "project_id") Long projectId,
                                                         @ApiParam(value = "issue id", required = true)
                                                         @RequestParam @Encrypt Long issueId,
                                                         @ApiParam(value = "所属项目id", required = true)
                                                         @RequestParam Long belongProjectId) {
        return Optional.ofNullable(dataLogService.listByIssueId(belongProjectId, issueId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.dataLogList.get"));
    }
}
