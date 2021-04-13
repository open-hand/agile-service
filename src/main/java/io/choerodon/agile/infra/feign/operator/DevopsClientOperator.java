package io.choerodon.agile.infra.feign.operator;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.AppServiceRepVO;
import io.choerodon.agile.infra.feign.DevopsFeignClient;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.ServiceUnavailableException;
import io.choerodon.core.utils.FeignClientUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * @author superlee
 * @since 2021-03-12
 */
@Component
public class DevopsClientOperator {
    @Autowired
    private DevopsFeignClient devopsFeignClient;

    public List<AppServiceRepVO> listAppService(Long projectId, int page, int size, boolean checkMember) {
        try {
            return FeignClientUtils.doRequest(() ->
                    devopsFeignClient.listAppService(projectId, page, size, checkMember), new TypeReference<Page<AppServiceRepVO>>() {
            }).getContent();
        } catch (ServiceUnavailableException e) {
            return new ArrayList<>();
        }
    }

    public List<AppServiceRepVO> listActiveAppService(Long projectId) {
        try {
            return FeignClientUtils.doRequest(() ->
                    devopsFeignClient.listActiveAppService(projectId), new TypeReference<Page<AppServiceRepVO>>() {
            }).getContent();
        } catch (ServiceUnavailableException e) {
            return new ArrayList<>();
        }
    }
}
