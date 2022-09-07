package io.choerodon.agile.infra.feign.operator;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.ExecutionCaseStatusChangeSettingVO;
import io.choerodon.agile.api.vo.ProjectInfoVO;
import io.choerodon.agile.infra.feign.TestFeignClient;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.ServiceUnavailableException;
import io.choerodon.core.utils.FeignClientUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-01-08 14:11
 */
@Component
public class TestServiceClientOperator {
    @Autowired
    private TestFeignClient testFeignClient;

    public void deleteTestRel(Long projectId, Long defectId){
        try {
             FeignClientUtils.doRequest(() -> testFeignClient.deleteTestRel(projectId, defectId), Void.class);
        } catch (ServiceUnavailableException e) {
            return;
        }
    }

    public ProjectInfoVO queryProjectInfo(Long projectId) {
        try {
            return FeignClientUtils.doRequest(() -> testFeignClient.queryProjectInfo(projectId), ProjectInfoVO.class);
        } catch (ServiceUnavailableException e) {
            return null;
        }
    }

    public List<ExecutionCaseStatusChangeSettingVO> list(Long projectId, Long organizationId, Long issueTypeId, List<Long> statusIds) {
        if (Boolean.FALSE.equals(ConvertUtil.hasModule(projectId, "N_TEST"))) {
            return new ArrayList<>();
        }
        try {
            return FeignClientUtils.doRequest(() -> testFeignClient.list(projectId, organizationId, issueTypeId, statusIds), new TypeReference<List<ExecutionCaseStatusChangeSettingVO>>() {});
        } catch (ServiceUnavailableException e) {
            return new ArrayList<>();
        }
    }

    public Boolean checkExistTestCaseLink(Long projectId, Long issueId) {
        try {
            return testFeignClient.checkTestCaseLinkExist(projectId, issueId).getBody();
        } catch (ServiceUnavailableException e) {
            return false;
        }
    }

    public void copyIssueRelatedTestCases(Long projectId, Long issueId, Long newIssueId) {
        try{
            testFeignClient.copyIssueRelatedTestCases(projectId, issueId, newIssueId).getBody();
        } catch (ServiceUnavailableException e) {
            return;
        }
    }
}
