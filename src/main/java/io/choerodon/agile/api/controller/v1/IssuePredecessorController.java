package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.IssuePredecessorVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.service.IssuePredecessorService;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
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

/**
 * @author superlee
 * @since 2021-11-10
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/issue_predecessor")
public class IssuePredecessorController {

    @Autowired
    private IssuePredecessorService issuePredecessorService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询前置项类型")
    @GetMapping(value = "/types")
    public ResponseEntity queryPredecessorTypes(@ApiParam(value = "项目id", required = true)
                                                @PathVariable(name = "project_id") Long projectId) {
        return ResponseEntity.ok(issuePredecessorService.queryPredecessorTypes(projectId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("工作项添加前置项")
    @PostMapping(value = "/update")
    public ResponseEntity updatePredecessorTypes(@ApiParam(value = "项目id", required = true)
                                                 @PathVariable(name = "project_id") Long projectId,
                                                 @RequestBody List<IssuePredecessorVO> issuePredecessors,
                                                 @RequestParam @Encrypt Long currentIssueId) {
        issuePredecessorService.updatePredecessors(projectId, issuePredecessors, currentIssueId);
        return new ResponseEntity(HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("分页查询工作项列表，不包含循环的节点")
    @PostMapping(value = "/paged_query")
    public ResponseEntity<Page<IssueListFieldKVVO>> pagedQueryEnabledIssues(@ApiIgnore
                                                                            @ApiParam(value = "分页信息", required = true)
                                                                            @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                                    PageRequest pageRequest,
                                                                            @ApiParam(value = "项目id", required = true)
                                                                            @PathVariable(name = "project_id") Long projectId,
                                                                            @ApiParam(value = "查询参数", required = true)
                                                                            @RequestBody(required = false) SearchVO searchVO,
                                                                            @ApiParam(value = "查询参数", required = true)
                                                                            @RequestParam Long organizationId,
                                                                            @RequestParam @Encrypt Long currentIssueId) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return ResponseEntity.ok(issuePredecessorService.pagedQueryEnabledIssues(organizationId, projectId, searchVO, pageRequest, currentIssueId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询当前问题项的前置项")
    @GetMapping(value = "/query")
    public ResponseEntity<List<IssuePredecessorVO>> queryByIssueId(@ApiParam(value = "项目id", required = true)
                                                                   @PathVariable(name = "project_id") Long projectId,
                                                                   @RequestParam @Encrypt Long currentIssueId) {
        return ResponseEntity.ok(issuePredecessorService.queryByIssueId(projectId, currentIssueId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询当前问题项的前置项")
    @GetMapping(value = "/test_tree")
    public ResponseEntity<String> tree(@ApiParam(value = "项目id", required = true)
                                       @PathVariable(name = "project_id") Long projectId,
                                       @RequestParam(required = false) @Encrypt Long rootId) {
        return ResponseEntity.ok(issuePredecessorService.tree(projectId, rootId));
    }


}
