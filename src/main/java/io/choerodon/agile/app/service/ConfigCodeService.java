package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.ConfigCodeVO;
import io.choerodon.agile.infra.statemachineclient.dto.PropertyData;

/**
 * @author shinan.chen
 * @date 2018/10/10
 */
public interface ConfigCodeService {

    /**
     * 根据类型获取ConfigCode
     *
     * @param type type
     * @return result
     */
    List<ConfigCodeVO> queryByType(String type);

    /**
     * 根据转换id获取未设置的ConfigCode
     *
     * @param organizationId organizationId
     * @param transformId transformId
     * @param type type
     * @return result
     */
    List<ConfigCodeVO> queryByTransformId(Long organizationId, Long transformId, String type);

    /**
     * 处理扫描到的ConfigCode
     *
     * @param propertyData propertyData
     */
    void handlePropertyData(PropertyData propertyData);
}
