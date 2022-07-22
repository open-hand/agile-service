package io.choerodon.agile.infra.feign.operator;

import java.util.List;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.WorkSpaceVO;
import io.choerodon.agile.infra.feign.KnowledgebaseFeignClient;
import io.choerodon.core.utils.FeignClientUtils;
import org.springframework.stereotype.Component;

/**
 * @author zhaotianxin
 * @date 2021-01-08 14:18
 */
@Component
public class KnowledgebaseClientOperator {

    private final KnowledgebaseFeignClient knowledgebaseFeignClient;

    public KnowledgebaseClientOperator(KnowledgebaseFeignClient knowledgebaseFeignClient) {
        this.knowledgebaseFeignClient = knowledgebaseFeignClient;
    }

    public List<WorkSpaceVO> querySpaceByIds(Long projectId, List<Long> spaceIds) {
        return FeignClientUtils.doRequest(() -> knowledgebaseFeignClient.querySpaceByIds(projectId, spaceIds), new TypeReference<List<WorkSpaceVO>>() {});
    }
}
