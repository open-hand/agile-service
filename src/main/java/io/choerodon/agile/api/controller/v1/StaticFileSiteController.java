package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.WebRequest;

import java.io.IOException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.app.service.StaticFileService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:25
 */
@RestController
@RequestMapping(value = "/v1/static_file")
public class StaticFileSiteController {

    @Autowired
    StaticFileService staticFileService;

    @ApiOperation(value = "请求资源")
    @Permission(level = ResourceLevel.SITE, permissionPublic = true)
    @GetMapping(value = "/resource/{fileHeaderId}/**")
    public ResponseEntity<byte[]> resource(
            @ApiParam(value = "静态资源头id", required = true)
            @PathVariable Long fileHeaderId,
            HttpServletResponse httpResponse,
            WebRequest webRequest,
            HttpServletRequest httpRequest) throws IOException {
        return staticFileService.selectStaticFileResult(fileHeaderId,
                webRequest, httpRequest, httpResponse
        );
    }
}
