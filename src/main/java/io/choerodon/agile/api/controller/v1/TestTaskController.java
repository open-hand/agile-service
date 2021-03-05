package io.choerodon.agile.api.controller.v1;

import io.choerodon.agile.infra.task.SprintDelaySendMessageTask;
import io.choerodon.core.iam.ResourceLevel;
import io.choerodon.swagger.annotation.Permission;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author superlee
 * @since 2021-03-05
 */
@RestController
@RequestMapping(value = "/v1/test/task")
public class TestTaskController {

    @Autowired
    private SprintDelaySendMessageTask sprintDelaySendMessageTask;


    @Permission(level = ResourceLevel.SITE)
    @ApiOperation(value = "测试冲刺延期定时任务")
    @GetMapping(value = "/sprint_delay")
    public ResponseEntity sprintDelay() {
        sprintDelaySendMessageTask.run();
        return new ResponseEntity(HttpStatus.OK);
    }
}
