package io.choerodon.agile.api.controller.v1;

import java.util.List;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.api.vo.IssuePersonalSortVO;
import io.choerodon.agile.app.service.PersonalSortService;
import io.choerodon.agile.domain.repository.PersonalSortRepository;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

import org.hzero.core.util.Results;

/**
 * 用户个性化排序规则 Controller
 * @author gaokuo.dai@zknow.com 2023-02-02
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organizationId}/personal-sort")
public class PersonalSortController {

    @Autowired
    private PersonalSortService personalSortService;
    @Autowired
    private PersonalSortRepository personalSortRepository;

    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("查询用户个性化排序规则")
    @GetMapping(value = "/{businessType}")
    public ResponseEntity<List<IssuePersonalSortVO>> listByBusinessType(@ApiParam(value = "组织ID", required = true) @PathVariable Long organizationId,
                                                                        @ApiParam(value = "业务类型", required = true) @PathVariable String businessType,
                                                                        @ApiParam(value = "项目ID", required = true, defaultValue = "0") @RequestParam(defaultValue = "0") Long projectId) {
        return Results.success(this.personalSortRepository.listByBusinessType(organizationId, projectId, businessType));
    }
    @Permission(level = ResourceLevel.ORGANIZATION, permissionLogin = true)
    @ApiOperation("保存用户个性化排序规则")
    @PostMapping(value = "/save")
    public ResponseEntity<Void> saveSort(@ApiParam(value = "组织ID", required = true) @PathVariable Long organizationId,
                                         @ApiParam(value = "项目ID", required = true, defaultValue = "0") @RequestParam(defaultValue = "0") Long projectId,
                                         @ApiParam(value = "业务类型", required = true) @RequestParam String businessType,
                                         @RequestBody @Validated List<IssuePersonalSortVO> issuePersonalSorts) {
        this.personalSortService.saveSort(organizationId, projectId, businessType, issuePersonalSorts);
        return Results.success();
    }

}
