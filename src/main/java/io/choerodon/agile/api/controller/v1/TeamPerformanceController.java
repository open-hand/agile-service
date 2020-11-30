package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.api.vo.SprintStoryPointVO;
import io.choerodon.agile.api.vo.SprintTaskVO;
import io.choerodon.agile.app.service.TeamPerformanceService;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Created by jian.zhang02@hand-china.com on 2020/11/15
 */
@RestController
@RequestMapping(value = "/v1/projects/{project_id}/team_performance")
public class TeamPerformanceController {
    @Autowired
    private TeamPerformanceService teamPerformanceService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "团队绩效-当前进行冲刺故事点统计")
    @GetMapping(value = "/story_point")
    public ResponseEntity<List<SprintStoryPointVO>> querySprintStoryPoint(@ApiParam(value = "项目id", required = true)
                                                                          @PathVariable(name = "project_id") Long projectId) {
        return ResponseEntity.ok(teamPerformanceService.querySprintStoryPoint(projectId));
    }

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "团队绩效-当前进行冲刺任务工时统计")
    @GetMapping(value = "/task_time")
    public ResponseEntity<List<SprintTaskVO>> querySprintTaskTime(@ApiParam(value = "项目id", required = true)
                                                              @PathVariable(name = "project_id") Long projectId) {
        return ResponseEntity.ok(teamPerformanceService.querySprintTaskTime(projectId));
    }
}
