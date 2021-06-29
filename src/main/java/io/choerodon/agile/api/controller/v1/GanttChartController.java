package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.GanttChartVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.app.service.GanttChartService;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author superlee
 * @since 2020-11-24
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/gantt")
public class GanttChartController {

    @Autowired
    private GanttChartService ganttChartService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("查询甘特图列表数据")
    @PostMapping(value = "/list")
    public ResponseEntity<List<GanttChartVO>> pagedQuery(@ApiParam(value = "项目id", required = true)
                                                       @PathVariable(name = "project_id") Long projectId,
                                                         @RequestParam(required = false) @Encrypt Long lastIssueId,
                                                         @RequestParam(defaultValue = "100") Integer size,
                                                         @ApiParam(value = "查询参数", required = true)
                                                       @RequestBody(required = false) SearchVO searchVO) {
        EncryptionUtils.decryptSearchVO(searchVO);
        return ResponseEntity.ok(ganttChartService.pagedQuery(projectId, searchVO, lastIssueId, size));
    }

//    @Permission(level = ResourceLevel.ORGANIZATION)
//    @ApiOperation("查询甘特图列表数据")
//    @PostMapping(value = "/list/by_user")
//    public ResponseEntity<List<GanttChartTreeVO>> listByUser(@ApiParam(value = "项目id", required = true)
//                                                             @PathVariable(name = "project_id") Long projectId,
//                                                             @ApiParam(value = "查询参数", required = true)
//                                                                 @RequestBody(required = false) SearchVO searchVO) {
//        EncryptionUtils.decryptSearchVO(searchVO);
//        return ResponseEntity.ok(ganttChartService.listByUser(projectId, searchVO));
//    }
}
