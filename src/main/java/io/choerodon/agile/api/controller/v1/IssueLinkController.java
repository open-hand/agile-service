package io.choerodon.agile.api.controller.v1;

import java.util.List;
import java.util.Optional;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import io.choerodon.agile.api.vo.IssueLinkCreateVO;
import io.choerodon.agile.api.vo.IssueLinkResponseVO;
import io.choerodon.agile.api.vo.IssueLinkVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.service.IssueLinkService;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/14
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/issue_links")
public class IssueLinkController {

    @Autowired
    private IssueLinkService issueLinkService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("创建issueLink")
    @PostMapping(value = "/{issueId}")
    public ResponseEntity<IssueLinkResponseVO> createIssueLinkList(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                   @ApiParam(value = "issueId", required = true)
                                                                   @PathVariable @Encrypt Long issueId,
                                                                   @ApiParam(value = "issueLink创建对象", required = true)
                                                                   @RequestBody List<IssueLinkCreateVO> issueLinkCreateVOList) {
        return Optional.ofNullable(issueLinkService.createIssueLinkList(issueLinkCreateVOList, issueId, projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.IssueLink.createIssueLink"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("删除issueLink")
    @DeleteMapping(value = "/{issueLinkId}")
    public ResponseEntity deleteIssueLink(@ApiParam(value = "项目id", required = true)
                                          @PathVariable(name = "project_id") Long projectId,
                                          @ApiParam(value = "issueLinkId", required = true)
                                          @PathVariable @Encrypt Long issueLinkId) {
        issueLinkService.deleteIssueLink(issueLinkId);
        return new ResponseEntity(HttpStatus.NO_CONTENT);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issueId查询issueLink")
    @GetMapping(value = "/{issueId}")
    public ResponseEntity<List<IssueLinkVO>> listIssueLinkByIssueId(@ApiParam(value = "项目id", required = true)
                                                                    @PathVariable(name = "project_id") Long projectId,
                                                                    @ApiParam(value = "issueId", required = true)
                                                                    @PathVariable @Encrypt Long issueId,
                                                                    @ApiParam(value = "是否包含测试任务")
                                                                    @RequestParam(required = false, name = "no_issue_test", defaultValue = "false")
                                                                    Boolean noIssueTest) {
        return Optional.ofNullable(issueLinkService.listIssueLinkByIssueId(issueId, projectId, noIssueTest))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueLink.listIssueLinkByIssueId"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据issueId查询issueLink,外接测试项目")
    @PostMapping(value = "/issues")
    public ResponseEntity<List<IssueLinkVO>> listIssueLinkByBatch(@ApiParam(value = "项目id", required = true)
                                                                  @PathVariable(name = "project_id") Long projectId,
                                                                  @ApiParam(value = "issueIds", required = true)
                                                                  @RequestBody List<Long> issueIds) {
        return Optional.ofNullable(issueLinkService.listIssueLinkByBatch(projectId, issueIds))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.IssueLink.listIssueLinkByBatch"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("分页查询未关联问题列表")
    @PostMapping(value = "/un_link/{issueId}")
    public ResponseEntity<Page<IssueListFieldKVVO>> listUnLinkIssue(
            @ApiIgnore
            @ApiParam(value = "分页信息", required = true)
            @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
            PageRequest pageRequest,
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "查询信息的项目id")
            @RequestParam(name = "target_project_id", required = false) Long targetProjectId,
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "issueId") @Encrypt Long issueId,
            @ApiParam(value = "查询参数", required = true)
            @RequestBody(required = false) SearchVO searchVO,
            @ApiParam(value = "组织id", required = true)
            @RequestParam(required = false) Long organizationId) {
        EncryptionUtils.decryptSearchVO(searchVO);
        if (organizationId == null) {
            organizationId = ConvertUtil.getOrganizationId(projectId);
        }
        return Optional.ofNullable(issueLinkService.listUnLinkIssue(issueId, projectId, targetProjectId, searchVO, pageRequest, organizationId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.Issue.listUnLinkIssue"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("关联issue判断是否会形成环")
    @PostMapping(value = "/check_link_issue_cycle")
    public ResponseEntity<List<Long>> checkLinkIssueCycle(@ApiParam(value = "项目id", required = true)
                                                          @PathVariable(name = "project_id") Long projectId,
                                                          @ApiParam(value = "问题id", required = true)
                                                          @RequestParam @Encrypt Long issueId,
                                                          @ApiParam(value = "问题关联id", required = true)
                                                          @RequestParam @Encrypt Long linkTypeId,
                                                          @ApiParam(value = "问题关联集合", required = true)
                                                          @RequestBody List<Long> linkIssueIds) {
        return new ResponseEntity<>(issueLinkService.checkLinkIssueCycle(projectId, issueId, linkTypeId, linkIssueIds), HttpStatus.OK);
    }
}
