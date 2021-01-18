package io.choerodon.agile.api.controller.v1;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.WebRequest;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.api.vo.StaticFileHeaderVO;
import io.choerodon.agile.api.vo.StaticFileRelatedVO;
import io.choerodon.agile.app.service.StaticFileService;
import io.choerodon.core.exception.CommonException;
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
            @PathVariable @Encrypt String fileHeaderId,
            HttpServletResponse httpResponse,
            WebRequest webRequest,
            HttpServletRequest httpRequest) throws IOException {
        return staticFileService.selectStaticFileResult(fileHeaderId,
                webRequest, httpRequest, httpResponse
        );
    }
}
