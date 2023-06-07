package io.choerodon.agile.infra.config;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;

import io.choerodon.core.common.ChoerodonService;
import io.choerodon.core.swagger.ChoerodonRouteData;
import io.choerodon.swagger.annotation.ChoerodonExtraData;
import io.choerodon.swagger.swagger.extra.ExtraData;

/**
 * 服务路由信息配置
 * @author superlee
 * @since 2020-05-18
 */
@ChoerodonExtraData
public class ExtraDataManager implements io.choerodon.swagger.swagger.extra.ExtraDataManager {

    @Value("${hzero.service.agile.code:agile}")
    private String routeName;

    @Value(ChoerodonService.AgileService.NAME)
    private String serviceName;

    @Override
    public ExtraData getData() {
        ChoerodonRouteData choerodonRouteData = new ChoerodonRouteData();
        String realRouteName = StringUtils.isBlank(this.routeName) ? ChoerodonService.AgileService.CODE : this.routeName;
        choerodonRouteData.setName(realRouteName);
        choerodonRouteData.setPath("/" + realRouteName + "/**");
        choerodonRouteData.setServiceId(this.serviceName);
        extraData.put(ExtraData.ZUUL_ROUTE_DATA, choerodonRouteData);
        return extraData;
    }
}
