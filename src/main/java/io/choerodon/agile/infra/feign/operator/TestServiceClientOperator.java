package io.choerodon.agile.infra.feign.operator;

import io.choerodon.agile.api.vo.ProjectInfoVO;
import io.choerodon.agile.infra.feign.TestFeignClient;
import io.choerodon.core.exception.ServiceUnavailableException;
import io.choerodon.core.utils.FeignClientUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

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
}
