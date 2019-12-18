package io.choerodon.agile.infra.feign;

import io.choerodon.agile.infra.feign.fallback.TestFeignClientFallback;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(value = "test-manager-service", fallback = TestFeignClientFallback.class)
public interface TestFeignClient {

    @DeleteMapping(value = "/v1/projects/{project_id}/defect/delete_relation/{defectId}")
    ResponseEntity deleteTestRel(@PathVariable(value = "project_id") Long projectId,
                                 @PathVariable(name = "defectId") Long defectId);
}
