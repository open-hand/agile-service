package io.choerodon.agile.infra.feign.fallback;

import io.choerodon.agile.infra.feign.RemoteIamFeignClient;
import io.choerodon.agile.infra.utils.FeignFallbackUtil;
import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.stereotype.Component;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/7/13
 */
@Component
public class BaseFallbackFactory implements FallbackFactory<RemoteIamFeignClient> {

    @Override
    public RemoteIamFeignClient create(Throwable cause) {
        return FeignFallbackUtil.get(cause, RemoteIamFeignClient.class);
    }
}
