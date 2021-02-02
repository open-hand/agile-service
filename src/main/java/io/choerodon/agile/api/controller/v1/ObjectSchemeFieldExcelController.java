package io.choerodon.agile.api.controller.v1;


import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.app.service.ObjectSchemeFieldExcelService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
@RestController
@RequestMapping("/v1/organizations/{organization_id}/object_scheme_field/excel")
public class ObjectSchemeFieldExcelController {

    @Autowired
    private ObjectSchemeFieldExcelService objectSchemeFieldExcelService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("下载导入模版")
    @GetMapping(value = "/download")
    public void download(@ApiParam(value = "项目id", required = true)
                         @PathVariable(name = "organization_id") Long organizationId,
                         HttpServletResponse response) {
        objectSchemeFieldExcelService.download(organizationId, null, response);
    }
}
