package io.choerodon.agile.infra.feign.fallback;

import io.choerodon.agile.infra.feign.FileFeignClient;
import io.choerodon.core.utils.FeignFallbackUtil;
import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.stereotype.Component;

/**
 * 项目字段
 *
 * @author 汪翔 2023-05-15
 */
@Component
public class FileFeignClientFallbackFactory implements FallbackFactory<FileFeignClient> {

    @Override
    public FileFeignClient create(Throwable cause) {
        return FeignFallbackUtil.get(cause, FileFeignClient.class);
    }
}
