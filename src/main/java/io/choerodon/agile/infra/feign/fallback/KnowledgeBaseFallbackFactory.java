package io.choerodon.agile.infra.feign.fallback;

import io.choerodon.agile.infra.feign.KnowledgebaseFeignClient;
import io.choerodon.core.utils.FeignFallbackUtil;
import org.springframework.cloud.openfeign.FallbackFactory;
import org.springframework.stereotype.Component;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/7/13
 */
@Component
public class KnowledgeBaseFallbackFactory implements FallbackFactory<KnowledgebaseFeignClient> {

    @Override
    public KnowledgebaseFeignClient create(Throwable cause) {
        return FeignFallbackUtil.get(cause, KnowledgebaseFeignClient.class);
    }
}
