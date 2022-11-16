package io.choerodon.agile.api.controller.v2;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.core.domain.Page;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.util.Results;

/**
 * v2有各种各样的问题，切回v1
 *
 * @author superlee
 * @since 2022-11-03
 */
@RestController
@RequestMapping(value = "/v2/projects/{project_id}/issues")
public class IssueV2Controller {

    @Autowired
    private IssueService issueService;


    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("分页查询问题列表，包含子任务")
    @PostMapping(value = "/work_list")
    public ResponseEntity<Page<IssueListFieldKVVO>> pagedQueryWorkList(@ApiIgnore
                                                                       @ApiParam(value = "分页信息", required = true)
                                                                       @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                                                               PageRequest pageRequest,
                                                                       @ApiParam(value = "项目id", required = true)
                                                                       @PathVariable(name = "project_id") Long projectId,
                                                                       @ApiParam(value = "查询参数", required = true)
                                                                       @RequestBody SearchParamVO searchParamVO,
                                                                       @ApiParam(value = "查询参数", required = true)
                                                                       @RequestParam(required = false) Long organizationId) {
        return Results.success(issueService.pagedQueryWorkList(projectId, searchParamVO, pageRequest, organizationId));
    }

}
