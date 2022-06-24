package io.choerodon.agile.infra.feign;

import io.choerodon.agile.api.vo.AppServiceSimpleVO;
import io.choerodon.agile.infra.feign.fallback.DevopsFeignClientFallback;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;


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
                                          @RequestParam Boolean checkMember,
                                          @RequestParam Boolean active);

    @GetMapping({"/v1/projects/{project_id}/app_service/list_by_active"})
    ResponseEntity<String> listActiveAppService(@PathVariable("project_id") Long projectId);


    @GetMapping({"/v1/projects/{project_id}/app_service/{app_service_id}/git/tags/issue_ids"})
    ResponseEntity<String> getIssueIdsBetweenTags(@PathVariable(value = "project_id") Long projectId,
                                                  @PathVariable(value = "app_service_id") Long appServiceId,
                                                  @RequestParam String from,
                                                  @RequestParam String to);

    @PostMapping({"/v1/organizations/{organization_id}/list_by_project_id_and_code"})
    ResponseEntity<String> listByProjectIdAndCode(@PathVariable(value = "organization_id") Long organizationId,
                                                  @RequestBody List<AppServiceSimpleVO> appServiceList);

    @GetMapping({"/v1/projects/{project_id}/branch/issue/check_rel_exist"})
    ResponseEntity<Boolean> checkIssueBranchRelExist(@PathVariable(value = "project_id") Long projectId,
                                                     @RequestParam("issue_id") Long issueId);

    @PostMapping({"/v1/projects/{project_id}/branch/issue/copy_rel"})
    ResponseEntity<Void> copyIssueBranchRel(@PathVariable(value = "project_id") Long projectId,
                                            @RequestParam("old_issue_id") Long oldIssueId,
                                            @RequestParam("new_issue_id") Long newIssueId);
}
