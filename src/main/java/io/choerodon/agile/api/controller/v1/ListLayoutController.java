package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.ListLayoutVO;
import io.choerodon.agile.api.vo.PriorityDistributeVO;
import io.choerodon.agile.app.service.ListLayoutService;
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

import java.util.List;
import java.util.Optional;

/**
 * @author zhaotianxin
 * @date 2021-05-07 14:50
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/list_layout")
public class ListLayoutController {

    @Autowired
    private ListLayoutService listLayoutService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("保存列表布局配置")
    @PostMapping
    public ResponseEntity<ListLayoutVO> save(@ApiParam(value = "项目id", required = true)
                                             @PathVariable(name = "project_id") Long projectId,
                                             @ApiParam(value = "组织id", required = true)
                                             @RequestParam Long organizationId,
                                             @RequestBody ListLayoutVO listLayoutVO) {
        return Optional.ofNullable(listLayoutService.save(organizationId, projectId, listLayoutVO))
                .map(result -> new ResponseEntity<>(result, HttpStatus.CREATED))
                .orElseThrow(() -> new CommonException("error.list.layout.save"));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据列表编码查询展示列配置")
    @GetMapping("/{type_code}")
    public ResponseEntity<ListLayoutVO> queryByTypeCode(@ApiParam(value = "项目id", required = true)
                                                        @PathVariable(name = "project_id") Long projectId,
                                                        @ApiParam(value = "列表类型编码", required = true)
                                                        @PathVariable(name = "type_code") String typeCode,
                                                        @ApiParam(value = "组织id", required = true)
                                                        @RequestParam Long organizationId) {
        return Optional.ofNullable(listLayoutService.queryByTypeCode(organizationId, projectId, typeCode))
                .map(result -> new ResponseEntity<>(result, HttpStatus.OK))
                .orElseThrow(() -> new CommonException("error.list.layout.query"));
    }
}
