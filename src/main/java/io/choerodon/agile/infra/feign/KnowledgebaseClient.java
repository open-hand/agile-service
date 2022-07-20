package io.choerodon.agile.infra.feign;

import io.choerodon.agile.infra.feign.fallback.KnowledgeBaseFallbackFactory;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

@FeignClient(value = "knowledgebase-service", fallbackFactory = KnowledgeBaseFallbackFactory.class)
public interface KnowledgebaseClient {

    @PostMapping(value = "/v1/projects/{project_id}/work_space/query_by_space_ids")
    ResponseEntity<String> querySpaceByIds(@PathVariable(value = "project_id") Long projectId,
                                                      @RequestBody List<Long> spaceIds);
}
