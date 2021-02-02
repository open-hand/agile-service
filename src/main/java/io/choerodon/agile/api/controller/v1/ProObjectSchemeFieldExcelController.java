package io.choerodon.agile.api.controller.v1;


import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.ObjectSchemeFieldExcelService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
@RestController
@RequestMapping("/v1/projects/{project_id}/object_scheme_field/excel")
public class ProObjectSchemeFieldExcelController {

    @Autowired
    private ObjectSchemeFieldExcelService objectSchemeFieldExcelService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("下载导入模版")
    @GetMapping(value = "/download")
    public void download(@ApiParam(value = "项目id", required = true)
                         @PathVariable(name = "project_id") Long projectId,
                         @ApiParam(value = "组织id", required = true)
                         @RequestParam Long organizationId,
                         HttpServletResponse response) {
        objectSchemeFieldExcelService.download(organizationId, projectId, response);
    }
}
