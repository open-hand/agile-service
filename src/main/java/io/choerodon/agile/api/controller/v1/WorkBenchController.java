package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.WorkBenchIssueSearchVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2020-06-19 10:44
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/work_bench")
public class WorkBenchController {
    @Autowired
    private IssueService issueService;

    @Permission(level = ResourceLevel.ORGANIZATION,permissionLogin = true)
    @ApiOperation("查询工作台个人代办事项")
    @PostMapping("/personal/backlog_issues")
    public ResponseEntity<Page<IssueListFieldKVVO>> queryBackLogIssuesByPersonal(@ApiParam(value = "组织id", required = true)
                                                                                 @PathVariable(name = "organization_id") Long organizationId,
                                                                                 @RequestParam(required = false) Long projectId,
                                                                                 PageRequest pageRequest,
                                                                                 @RequestBody WorkBenchIssueSearchVO workBenchIssueSearchVO) {
        return Optional.ofNullable(issueService.queryBackLogIssuesByPersonal(organizationId, projectId, pageRequest, workBenchIssueSearchVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueLabel.queryIssueLabelList"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("查询工作台我的报告")
    @PostMapping("/personal/my_reported")
    public ResponseEntity<Page<IssueListFieldKVVO>> pagedQueryMyReported(@ApiParam(value = "组织id", required = true)
                                                                         @PathVariable(name = "organization_id") Long organizationId,
                                                                         @RequestParam(required = false) Long projectId,
                                                                         PageRequest pageRequest) {
        return ResponseEntity.ok(issueService.pagedQueryMyReported(organizationId, projectId, pageRequest));
    }
}
