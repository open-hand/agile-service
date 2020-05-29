package io.choerodon.agile.infra.config;

import io.choerodon.core.swagger.ChoerodonRouteData;
import io.choerodon.swagger.annotation.ChoerodonExtraData;
import io.choerodon.swagger.swagger.extra.ExtraData;
import io.choerodon.swagger.swagger.extra.ExtraDataManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;

/**
 * @author superlee
 * @since 2020-05-18
 */
@ChoerodonExtraData
public class RouteConfig implements ExtraDataManager {

    @Autowired
    private Environment environment;

    @Override
    public ExtraData getData() {
        ChoerodonRouteData choerodonRouteData = new ChoerodonRouteData();
        choerodonRouteData.setName(environment.getProperty("hzero.service.current.name", "agile"));
        choerodonRouteData.setPath(environment.getProperty("hzero.service.current.path", "/agile/**"));
        choerodonRouteData.setServiceId(environment.getProperty("hzero.service.current.service-name", "agile-service"));
        choerodonRouteData.setPackages("io.choerodon.agile");
        extraData.put(ExtraData.ZUUL_ROUTE_DATA, choerodonRouteData);
        return extraData;
    }
}
