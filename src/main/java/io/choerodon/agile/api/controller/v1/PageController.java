package io.choerodon.agile.api.controller.v1;

import com.github.pagehelper.PageInfo;
import io.choerodon.core.annotation.Permission;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import io.choerodon.core.enums.ResourceType;
import io.choerodon.core.base.BaseController;
import io.choerodon.core.iam.InitRoleCode;
import io.choerodon.agile.api.vo.PageSearchVO;
import io.choerodon.agile.api.vo.PageVO;
import io.choerodon.agile.app.service.PageService;
import org.springframework.data.web.SortDefault;
import io.choerodon.swagger.annotation.CustomPageRequest;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/page")
public class PageController extends BaseController {

    @Autowired
    private PageService pageService;

    @Permission(type = ResourceType.ORGANIZATION, roles = {InitRoleCode.ORGANIZATION_ADMINISTRATOR, InitRoleCode.ORGANIZATION_MEMBER})
    @ApiOperation(value = "分页查询页面列表")
    @CustomPageRequest
    @PostMapping
    public ResponseEntity<PageInfo<PageVO>> pageQuery(@ApiIgnore
                                                       @SortDefault(value = "id", direction = Sort.Direction.DESC) Pageable pageable,
                                                      @ApiParam(value = "组织id", required = true)
                                                       @PathVariable("organization_id") Long organizationId,
                                                      @ApiParam(value = "search dto", required = true)
                                                       @RequestBody(required = false) PageSearchVO searchDTO) {
        return new ResponseEntity<>(pageService.pageQuery(organizationId, pageable, searchDTO), HttpStatus.OK);
    }
}
