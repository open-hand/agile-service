package io.choerodon.agile.infra.feign.operator;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.WorkSpaceVO;
import io.choerodon.agile.infra.feign.KnowledgebaseClient;
import io.choerodon.core.exception.ServiceUnavailableException;
import io.choerodon.core.utils.FeignClientUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-01-08 14:18
 */
@Component
public class KnowledgebaseClientOperator {
    @Autowired
    private KnowledgebaseClient knowledgebaseClient;

   public List<WorkSpaceVO> querySpaceByIds(Long projectId, List<Long> spaceIds){
       try {
           return FeignClientUtils.doRequest(() -> knowledgebaseClient.querySpaceByIds(projectId, spaceIds), new TypeReference<List<WorkSpaceVO>>() {});
       } catch (ServiceUnavailableException e) {
           return new ArrayList<>();
       }
   }
}
