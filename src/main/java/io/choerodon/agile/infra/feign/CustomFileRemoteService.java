package io.choerodon.agile.infra.feign;

import java.util.List;
import java.util.Map;

import io.choerodon.agile.infra.feign.fallback.CustomFileRemoteFallbackFactory;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * @author superlee
 * @since 2020-05-22
 */
@FeignClient(value = "choerodon-file", fallbackFactory = CustomFileRemoteFallbackFactory.class)
public interface CustomFileRemoteService {

    @PostMapping({"/choerodon/v1/{organizationId}/delete-by-url"})
    ResponseEntity<String> deleteFileByUrl(@PathVariable("organizationId") Long organizationId,
                                           @RequestParam("bucketName") String bucketName,
                                           @RequestBody List<String> urls);

    @PostMapping(value = {"/v1/{organizationId}/upload/fragment-combine"})
    ResponseEntity<String> fragmentCombineBlock(@PathVariable Long organizationId, @RequestParam String guid, @RequestParam String fileName, @RequestBody(required = false) Map<String, String> args);
}
