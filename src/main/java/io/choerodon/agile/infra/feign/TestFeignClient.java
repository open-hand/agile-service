package io.choerodon.agile.infra.feign;

import io.choerodon.agile.infra.feign.fallback.TestFeignClientFallback;
import io.swagger.annotations.ApiParam;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@FeignClient(value = "test-manager-service", fallback = TestFeignClientFallback.class)
public interface TestFeignClient {

    @DeleteMapping(value = "/v1/projects/{project_id}/defect/delete_relation/{defectId}")
    ResponseEntity deleteTestRel(@PathVariable(value = "project_id") Long projectId,
                                 @PathVariable(name = "defectId") Long defectId);

    @DeleteMapping(value = "/v1/projects/{project_id}/project_info")
    ResponseEntity<String> queryProjectInfo(@ApiParam(value = "项目id", required = true)
                                            @PathVariable(name = "project_id") Long projectId);

    @GetMapping(value = "/v1/projects/{project_id}/execution_status_change_setting/list")
    ResponseEntity<String> list(@ApiParam(value = "项目id", required = true)
                                @PathVariable(value = "project_id") Long projectId,
                                @ApiParam(value = "组织id", required = true)
                                @RequestParam Long organizationId,
                                @RequestParam Long issueTypeId,
                                @RequestBody List<Long> statusIds);
}
