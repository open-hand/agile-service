package io.choerodon.agile.api.controller.v1;

import java.util.List;
import java.util.Optional;

import io.choerodon.agile.api.vo.ConfigCodeVO;
import io.choerodon.agile.app.service.ConfigCodeService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * @author shinan.chen
 * @date 2018/10/10
 */
@RestController
@RequestMapping(value = "/v1/organizations/{organization_id}/config_code")
public class ConfigCodeController {

    @Autowired
    private ConfigCodeService configCodeService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取未配置的条件，验证，后置动作等列表")
    @GetMapping(value = "/{transform_id}")
    public ResponseEntity<List<ConfigCodeVO>> queryByTransformId(@ApiParam(value = "组织id", required = true)
                                                                 @PathVariable("organization_id") Long organizationId,
                                                                 @ApiParam(value = "转换id", required = true)
                                                                 @PathVariable("transform_id") @Encrypt Long transformId,
                                                                 @ApiParam(value = "状态机类型", required = true)
                                                                 @RequestParam String type) {
        return Optional.ofNullable(configCodeService.queryByTransformId(organizationId, transformId, type))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.configCode.queryByTransformId"));
    }

}
