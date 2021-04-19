package io.choerodon.agile.infra.feign.fallback;

import io.choerodon.agile.infra.feign.DevopsFeignClient;
import io.choerodon.core.exception.CommonException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

/**
 * @author superlee
 * @since 2021-03-12
 */
@Component
public class DevopsFeignClientFallback implements DevopsFeignClient {
    @Override
    public ResponseEntity<String> listAppService(Long projectId, int page, int size, boolean checkMember) {
        throw new CommonException("error.devops.listAppService");
    }

    @Override
    public ResponseEntity<String> listActiveAppService(Long projectId) {
        throw new CommonException("error.devops.listActiveAppService");
    }

    @Override
    public ResponseEntity<String> getIssueIdsBetweenTags(Long projectId, Long appServiceId, String from, String to) {
        throw new CommonException("error.devops.getIssueIdsBetweenTags");    }
}
