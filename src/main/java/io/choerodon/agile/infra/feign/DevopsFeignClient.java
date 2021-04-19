package io.choerodon.agile.infra.feign;

import io.choerodon.agile.infra.feign.fallback.DevopsFeignClientFallback;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;


/**
 * @author superlee
 * @since 2021-03-12
 */
@FeignClient(
        value = "devops-service",
        fallback = DevopsFeignClientFallback.class
)
public interface DevopsFeignClient {

    @PostMapping({"/v1/projects/{project_id}/app_service/page_by_options"})
    ResponseEntity<String> listAppService(@PathVariable("project_id") Long projectId,
                                          @RequestParam int page,
                                          @RequestParam int size,
                                          @RequestParam boolean checkMember);

    @GetMapping({"/v1/projects/{project_id}/app_service/list_by_active"})
    ResponseEntity<String> listActiveAppService(@PathVariable("project_id") Long projectId);


    @GetMapping({"/v1/projects/{project_id}/app_service/{app_service_id}/git/tags/issue_ids"})
    ResponseEntity<String> getIssueIdsBetweenTags(@PathVariable(value = "project_id") Long projectId,
                                                  @PathVariable(value = "app_service_id") Long appServiceId,
                                                  @RequestParam String from,
                                                  @RequestParam String to);

}
