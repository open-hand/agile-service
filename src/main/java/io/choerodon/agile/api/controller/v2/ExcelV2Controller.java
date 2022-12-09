package io.choerodon.agile.api.controller.v2;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import springfox.documentation.annotations.ApiIgnore;

import io.choerodon.agile.api.vo.search.SearchParamVO;
import io.choerodon.agile.app.service.ExcelService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.mybatis.pagehelper.annotation.SortDefault;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.mybatis.pagehelper.domain.Sort;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author superlee
 * @since 2022-12-09
 */
@RestController
@RequestMapping(value = "/v2/projects/{project_id}/excel")
public class ExcelV2Controller {

    @Autowired
    private ExcelService excelService;

    @ResponseBody
    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("导出issue列表")
    @PostMapping(value = "/export")
    public void exportIssues(@ApiIgnore
                             @ApiParam(value = "分页信息", required = true)
                             @SortDefault(value = "issueNum", direction = Sort.Direction.DESC)
                                     PageRequest pageRequest,
                             @ApiParam(value = "项目id", required = true)
                             @PathVariable(name = "project_id") Long projectId,
                             @ApiParam(value = "组织id", required = true)
                             @RequestParam Long organizationId,
                             @ApiParam(value = "查询参数", required = true)
                             @RequestBody SearchParamVO searchParamVO,
                             @ApiParam(value = "http请求")
                                     HttpServletRequest request,
                             @ApiParam(value = "http响应")
                                     HttpServletResponse response) {
        excelService.asyncExportIssuesV2(projectId, searchParamVO, request, response, organizationId, pageRequest.getSort(), (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes());
    }
}
