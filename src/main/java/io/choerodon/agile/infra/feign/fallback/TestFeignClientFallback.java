package io.choerodon.agile.infra.feign.fallback;

import io.choerodon.agile.infra.feign.TestFeignClient;
import io.choerodon.core.exception.CommonException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author: 25499
 * @date: 2019/12/13 11:45
 * @description:
 */
@Component
public class TestFeignClientFallback implements TestFeignClient {
    @Override
    public ResponseEntity deleteTestRel(Long projectId, Long defectId) {
        throw new CommonException("error.delete.test.defect.rel");
    }

    @Override
    public ResponseEntity<String> queryProjectInfo(Long projectId) {
        throw new CommonException("error.query.test.project.info");
    }

    @Override
    public ResponseEntity<String> list(Long projectId, Long organizationId, Long issueTypeId, List<Long> statusIds) {
        throw new CommonException("error.execution.case.status.change.setting.list");
    }

    @Override
    public ResponseEntity<Boolean> checkTestCaseLinkExist(Long projectId, Long issueId) {
        throw new CommonException("error.check.test.case.link.exist");
    }
}
