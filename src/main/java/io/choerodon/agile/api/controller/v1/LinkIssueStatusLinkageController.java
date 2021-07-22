package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.LinkIssueStatusLinkageVO;
import io.choerodon.agile.api.vo.StatusVO;
import io.choerodon.agile.app.service.LinkIssueStatusLinkageService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-06-10 10:52
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/link_issue_status_linkage")
public class LinkIssueStatusLinkageController {

    @Autowired
    private LinkIssueStatusLinkageService linkIssueStatusLinkageService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "配置关联问题联动")
    @PostMapping
    public ResponseEntity<List<LinkIssueStatusLinkageVO>> createOrUpdate(@PathVariable("project_id") Long projectId,
                                                       @RequestParam  Long organizationId,
                                                       @RequestParam @Encrypt Long issueTypeId,
                                                       @RequestParam @Encrypt Long statusId,
                                                       @RequestBody List<LinkIssueStatusLinkageVO> linkIssueStatusLinkageVOS) {
        return new ResponseEntity<>(linkIssueStatusLinkageService.createOrUpdate(projectId, organizationId, issueTypeId, statusId, linkIssueStatusLinkageVOS), HttpStatus.CREATED);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询状态机状态设置的关联问题联动")
    @GetMapping
    public ResponseEntity<List<LinkIssueStatusLinkageVO>> listByIssueTypeAndStatusId(@PathVariable("project_id") Long projectId,
                                                                         @RequestParam  Long organizationId,
                                                                         @RequestParam @Encrypt Long issueTypeId,
                                                                         @RequestParam @Encrypt Long statusId) {
        return new ResponseEntity<>(linkIssueStatusLinkageService.listByIssueTypeAndStatusId(projectId, organizationId, issueTypeId, statusId), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询状态")
    @PostMapping("/status")
    public ResponseEntity<List<StatusVO>> queryStatus(@PathVariable("project_id") Long projectId,
                                                                     @RequestParam  Long organizationId,
                                                                     @RequestBody LinkIssueStatusLinkageVO linkageVO) {
        return new ResponseEntity<>(linkIssueStatusLinkageService.queryStatus(projectId, organizationId, linkageVO), HttpStatus.OK);
    }
}
