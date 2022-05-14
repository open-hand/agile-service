package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.IssueWithBranchVO;
import io.choerodon.agile.app.service.InnerFeignService;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author superlee
 * @since 2021-04-15
 */
@RestController
@RequestMapping(value = "/v1/inner")
public class InnerFeignController {

    @Autowired
    private InnerFeignService innerFeignService;

    @Permission(permissionWithin = true)
    @PostMapping(value = "/issues/by_ids")
    @ApiOperation(value = "根据issueIds查询issue")
    public ResponseEntity<List<IssueDTO>> listIssueByIds(@ApiParam(value = "问题id集合", required = true)
                                                         @RequestBody List<Long> issueIds) {
        return new ResponseEntity<>(innerFeignService.listIssueByIds(issueIds), HttpStatus.OK);
    }

    @Permission(permissionWithin = true)
    @ApiOperation("devops删除issue和branch关系时，判断是否要删除issue和tag关系")
    @PostMapping(value = "/projects/{project_id}/delete_tag_by_branch")
    public ResponseEntity deleteTagByBranch(@ApiParam(value = "项目id", required = true)
                                            @PathVariable(name = "project_id") Long projectId,
                                            @ApiParam(value = "问题和分支查询参数", required = true)
                                            @RequestBody IssueWithBranchVO issueWithBranchVO) {
        innerFeignService.deleteTagByBranch(projectId, issueWithBranchVO);
        return new ResponseEntity(HttpStatus.OK);
    }
}
