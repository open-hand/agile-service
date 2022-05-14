package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.LookupTypeWithValuesVO;
import io.choerodon.agile.app.service.LookupValueService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.core.exception.CommonException;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

/**
 * 敏捷开发code键值
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-15 09:40:27
 */
@RestController
@RequestMapping(value = "/v1/lookup_values")
public class LookupValueController {

    @Autowired
    private LookupValueService lookupValueService;

    @Permission(level = ResourceLevel.SITE, permissionLogin = true)
    @ApiOperation("根据type code查询其下的value值")
    @GetMapping(value = "/{typeCode}")
    public ResponseEntity<LookupTypeWithValuesVO> queryLookupValueByCode(@ApiParam(value = "type code", required = true)
                                                                         @PathVariable String typeCode,
                                                                         @ApiParam(value = "项目id")
                                                                         @RequestParam(required = false) Long projectId) {
        return Optional.ofNullable(lookupValueService.queryLookupValueByCode(typeCode, projectId))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.lookupValueList.get"));
    }

    @Permission(level = ResourceLevel.SITE, permissionLogin = true)
    @ApiOperation("查询列约束下的value值")
    @GetMapping(value = "/constraint/list")
    public ResponseEntity<LookupTypeWithValuesVO> queryConstraintLookupValue() {
        return Optional.ofNullable(lookupValueService.queryConstraintLookupValue())
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.lookupValueList.get"));
    }

}
