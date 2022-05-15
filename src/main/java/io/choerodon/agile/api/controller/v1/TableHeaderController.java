package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.TableHeaderVO;
import io.choerodon.agile.app.service.TableHeaderService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author superlee
 * @since 2020-11-25
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/headers")
public class TableHeaderController {

    @Autowired
    private TableHeaderService tableHeaderService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("根据code查询table headers")
    @GetMapping(value = "/{code}")
    public ResponseEntity<List<TableHeaderVO>> list(@ApiParam(value = "项目id", required = true)
                                                    @PathVariable(name = "project_id") Long projectId,
                                                    @ApiParam(value = "编码", required = true)
                                                    @PathVariable(name = "code") String code) {
        return ResponseEntity.ok(tableHeaderService.listByCode(code));
    }
}
