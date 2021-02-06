package io.choerodon.agile.api.controller.v1;


import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.app.service.ObjectSchemeFieldExcelService;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.ExcelUtil;
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

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("导入自定义字段")
    @PostMapping(value = "/import")
    public ResponseEntity<Void> batchImport(@ApiParam(value = "项目id", required = true)
                                            @PathVariable(name = "project_id") Long projectId,
                                            @ApiParam(value = "导入文件", required = true)
                                            @RequestParam("file") MultipartFile file) {
        objectSchemeFieldExcelService.batchImport(
                ConvertUtil.getOrganizationId(projectId), projectId,
                ExcelUtil.getWorkbookFromMultipartFile(ExcelUtil.Mode.XSSF, file),
                RequestContextHolder.getRequestAttributes());
        return new ResponseEntity<>(HttpStatus.NO_CONTENT);
    }
}
