package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.ListLayoutVO;
import io.choerodon.agile.app.service.OrganizationListLayoutService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


/**
 * @author superlee
 * @since 2021-10-19
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/list_layout")
public class OrganizationListLayoutController {

    @Autowired
    private OrganizationListLayoutService organizationListLayoutService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据列表编码查询展示列配置")
    @GetMapping("/{apply_type}")
    public ResponseEntity<ListLayoutVO> queryByApplyType(@ApiParam(value = "组织id", required = true)
                                                         @PathVariable(name = "organization_id") Long organizationId,
                                                         @ApiParam(value = "列表类型编码", required = true)
                                                         @PathVariable(name = "apply_type") String applyType) {
        return new ResponseEntity<>(organizationListLayoutService.queryByApplyType(organizationId, applyType), HttpStatus.OK);
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("保存列表布局配置")
    @PostMapping
    public ResponseEntity<ListLayoutVO> save(@ApiParam(value = "组织id", required = true)
                                             @PathVariable(name = "organization_id") Long organizationId,
                                             @RequestBody ListLayoutVO listLayoutVO) {
        return new ResponseEntity<>(organizationListLayoutService.save(organizationId, listLayoutVO), HttpStatus.OK);
    }
}
