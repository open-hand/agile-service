package io.choerodon.agile.infra.feign.operator;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.ExecutionCaseStatusChangeSettingVO;
import io.choerodon.agile.api.vo.ProjectInfoVO;
import io.choerodon.agile.infra.feign.TestFeignClient;
import io.choerodon.agile.infra.utils.ConvertUtil;
import org.apache.commons.lang3.BooleanUtils;
import org.hzero.core.util.ResponseUtils;
import org.springframework.stereotype.Component;

/**
 * @author zhaotianxin
 * @date 2021-01-08 14:11
 */
@Component
public class TestServiceClientOperator {

    private final TestFeignClient testFeignClient;

    public TestServiceClientOperator(TestFeignClient testFeignClient) {
        this.testFeignClient = testFeignClient;
    }

    public void deleteTestRel(Long projectId, Long defectId) {
        testFeignClient.deleteTestRel(projectId, defectId);
    }

    public ProjectInfoVO queryProjectInfo(Long projectId) {
        return ResponseUtils.getResponse(testFeignClient.queryProjectInfo(projectId), ProjectInfoVO.class);
    }

    public List<ExecutionCaseStatusChangeSettingVO> list(Long projectId, Long organizationId, Long issueTypeId, List<Long> statusIds) {
        if (Boolean.FALSE.equals(ConvertUtil.hasModule(projectId, "N_TEST"))) {
            return new ArrayList<>();
        }
        return ResponseUtils.getResponse(testFeignClient.list(projectId, organizationId, issueTypeId, statusIds), new TypeReference<List<ExecutionCaseStatusChangeSettingVO>>() {
        });
    }

    public Boolean checkExistTestCaseLink(Long projectId, Long issueId) {
        Boolean response = ResponseUtils.getResponse(testFeignClient.checkTestCaseLinkExist(projectId, issueId), Boolean.class);
        return BooleanUtils.isTrue(response);
    }

    public void copyIssueRelatedTestCases(Long projectId, Long issueId, Long newIssueId) {
        testFeignClient.copyIssueRelatedTestCases(projectId, issueId, newIssueId);
    }
}
