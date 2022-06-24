package io.choerodon.agile.infra.feign.fallback;

import io.choerodon.agile.api.vo.AppServiceSimpleVO;
import io.choerodon.agile.infra.feign.DevopsFeignClient;
import io.choerodon.core.exception.CommonException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author superlee
 * @since 2021-03-12
 */
@Component
public class DevopsFeignClientFallback implements DevopsFeignClient {
    @Override
    public ResponseEntity<String> listAppService(Long projectId, int page, int size, Boolean checkMember, Boolean active) {
        throw new CommonException("error.devops.listAppService");
    }

    @Override
    public ResponseEntity<String> listActiveAppService(Long projectId) {
        throw new CommonException("error.devops.listActiveAppService");
    }

    @Override
    public ResponseEntity<String> getIssueIdsBetweenTags(Long projectId, Long appServiceId, String from, String to) {
        throw new CommonException("error.devops.getIssueIdsBetweenTags");
    }

    @Override
    public ResponseEntity<String> listByProjectIdAndCode(Long organizationId, List<AppServiceSimpleVO> appServiceList) {
        throw new CommonException("error.devops.listByProjectIdAndCode");
    }

    @Override
    public ResponseEntity<Boolean> checkIssueBranchRelExist(Long projectId, Long issueId) {
        throw new CommonException("error.check.exist.issueBranchRel");
    }

    @Override
    public ResponseEntity<Void> copyIssueBranchRel(Long projectId, Long oldIssueId, Long newIssueId) {
        throw new CommonException("error.copy.issue.relatedBranches");
    }
}
