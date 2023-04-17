package io.choerodon.agile.infra.feign.fallback;

import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.stereotype.Component;

import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.core.utils.FeignFallbackUtil;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/7/13
 */
@Component
public class BaseFallbackFactory implements FallbackFactory<BaseFeignClient> {

    @Override
    public BaseFeignClient create(Throwable cause) {
        return FeignFallbackUtil.get(cause, BaseFeignClient.class);
    }
}
