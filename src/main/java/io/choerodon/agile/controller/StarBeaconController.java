package io.choerodon.agile.controller;

import io.choerodon.agile.api.vo.StarBeaconVO;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiParam;
import org.hzero.core.util.Results;
import org.hzero.core.base.BaseController;
import io.choerodon.agile.service.StarBeaconService;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import io.swagger.annotations.ApiOperation;

/**
 * 星标controller
 * @author jiaxu.cui@hand-china.com
 */
@RestController("starBeaconController.v1")
@RequestMapping("/v1/projects/{project_id}/starbeacons")
public class StarBeaconController extends BaseController {

    @Autowired
    private StarBeaconService starBeaconService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("关注Issue")
    @PostMapping("/{instance_id}/star")
    public ResponseEntity<Void> starIssue(@ApiParam(value = "项目id", required = true)
                                          @PathVariable(name = "project_id") Long projectId,
                                          @PathVariable("instance_id") @Encrypt Long instanceId,
                                          @ApiParam(value = "issue信息", required = true)
                                          @RequestBody StarBeaconVO starBeaconVO) {
        starBeaconVO.setInstanceId(instanceId);
        starBeaconVO.setProjectId(projectId);
        starBeaconService.starIssue(starBeaconVO);
        return Results.success();
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation("取关Issue")
    @PostMapping("/{instance_id}/unstar")
    public ResponseEntity<Void> unstarIssue(@ApiParam(value = "项目id", required = true)
                                            @PathVariable(name = "project_id") Long projectId,
                                            @PathVariable("instance_id") @Encrypt Long instanceId,
                                            @RequestBody StarBeaconVO starBeaconVO) {
        starBeaconVO.setInstanceId(instanceId);
        starBeaconVO.setProjectId(projectId);
        starBeaconService.unStarIssue(starBeaconVO);
        return Results.success();
    }
}
