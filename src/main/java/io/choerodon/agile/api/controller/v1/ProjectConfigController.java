package io.choerodon.agile.api.controller.v1;

import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.choerodon.agile.api.vo.ProjectConfigDetailVO;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author shinan.chen
 * @date 2018/10/24
 */

@RestController
@RequestMapping("/v1/projects/{project_id}/project_config")
public class ProjectConfigController {

    @Autowired
    ProjectConfigService projectConfigService;

    @Permission(level = ResourceLevel.ORGANIZATION)
    @ApiOperation(value = "获取项目配置方案信息")
    @GetMapping
    public ResponseEntity<ProjectConfigDetailVO> queryById(@ApiParam(value = "项目id", required = true)
                                                           @PathVariable("project_id") Long projectId) {
        return new ResponseEntity<>(projectConfigService.queryById(projectId), HttpStatus.OK);
    }
}
