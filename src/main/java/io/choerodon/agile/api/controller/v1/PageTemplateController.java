package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.choerodon.agile.api.vo.PageTemplateVO;
import io.choerodon.agile.app.service.PageTemplateService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/20 16:18
 */
@RestController
@RequestMapping("/v1/projects/{project_id}/object_scheme_field/page_template")
public class PageTemplateController {

    @Autowired
    private PageTemplateService pageTemplateService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "查询字段的页面模板数据")
    @GetMapping
    public ResponseEntity<PageTemplateVO> queryPageTemplate(
            @ApiParam(value = "项目id", required = true)
            @PathVariable(name = "project_id") Long projectId,
            @ApiParam(value = "组织id", required = true)
            @RequestParam Long organizationId,
            @ApiParam(value = "问题类型id", required = true)
            @RequestParam @Encrypt Long issueTypeId) {
        return new ResponseEntity<>(pageTemplateService.queryPageTemplate(organizationId, projectId, issueTypeId), HttpStatus.OK);
    }
}
